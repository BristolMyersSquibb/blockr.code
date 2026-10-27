no_conditions <- function() {
  data.frame(
    block = character(),
    phase = character(),
    severity = character(),
    message = character(),
    id = character()
  )
}

test_that("new_code_extension builds a dock extension", {

  ext <- new_code_extension()

  expect_true(blockr.dock::is_dock_extension(ext))
  expect_s3_class(ext, "code_extension")
  expect_identical(blockr.dock::extension_name(ext), "Code")

  board <- blockr.dock::new_dock_board(extensions = ext)

  # A dock board keys the extension by its class, and saves it as a call to the
  # constructor, which restores it
  expect_identical(blockr.dock::dock_ext_ids(board), "code")

  expect_identical(
    blockr_deser(blockr_ser(board)),
    board,
    ignore_function_env = TRUE
  )
})

test_that("generate code", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("ChickWeight"),
      c = new_merge_block(by = "Time")
    ),
    links = links(
      from = c("a", "b"),
      to = c("c", "c"),
      input = c("x", "y")
    )
  )

  ext_args <- generate_plugin_args(board)
  ext_args$update <- reactiveVal()

  testServer(
    code_ext_srv,
    {
      expect_identical(code_export_state(board), "ready")

      expect_match(as.character(output$code_out), "merge", all = FALSE)

      # Showing the script asks nothing of the board
      expect_null(update())

      session$setInputs(code_eval = 1)

      expect_identical(update(), list(evaluate = c("a", "b", "c")))
    },
    args = ext_args
  )
})

test_that("export would emit `NA` for a board with unbuilt blocks", {

  board <- new_board(
    blocks = c(a = new_dataset_block("BOD"), b = new_dataset_block("BOD"))
  )

  # Only `a` carries an expression, as under deferred construction: `b` is on
  # the board but was never built. Handing this partial set to the exporter
  # indexes `b` out of the block list and assigns to a variable literally named
  # `NA` -- the junk this fix guards against.
  junk <- export_wrapped_code(list(a = quote(datasets::BOD)), board)

  expect_true(grepl("`NA` <-", junk, fixed = TRUE))

  # Gating on the built set is what keeps the panel from reaching the exporter
  # with `b` missing: it reports pending rather than exporting junk
  ro <- list(
    blocks = list(a = list(server = list(state_ready = TRUE))),
    conditions = no_conditions(),
    board = board
  )

  expect_identical(code_export_state(ro), "pending")

  body <- as.character(code_panel_body(code_export_state(ro), NS("gen")))

  expect_false(grepl("`NA` <-", body, fixed = TRUE))
  expect_match(body, "Waiting for every block")
})

test_that("the code panel waits for a deferred board without building it", {

  old <- options(blockr.background_construction_delay = Inf)
  on.exit(options(old))

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_dataset_block("ChickWeight")
    )
  )

  out <- NULL

  testServer(
    get_s3_method("board_server", board),
    {
      vis$visible[["a"]](TRUE)
      session$flushReact()

      waiting <- list(
        built = names(rv$blocks),
        body = as.character(output$`code-code_out`$html)
      )

      # Build the off-screen block, as the background pass of a board with a
      # finite construction delay would
      board_update(list(construct = "b"))
      session$flushReact()

      out <<- list(
        waiting = waiting,
        built = names(rv$blocks),
        status = reval_if(rv$eval[["b"]]),
        eager = rv$eager_blocks(),
        body = as.character(output$`code-code_out`$html)
      )
    },
    args = list(
      x = board,
      plugins = plugins(),
      callbacks = function(board, update, ...) {
        code_ext_srv("code", board, update)
        eager("front-end", "a")
      }
    )
  )

  # The panel builds nothing itself: the off-screen block stays unbuilt, and
  # the panel reports pending rather than exporting a partial script
  expect_identical(out$waiting$built, "a")
  expect_match(out$waiting$body, "Waiting for every block")

  # Once the block is built, the whole script is exported. The board stays as
  # lazy as it was: the block is left unevaluated, and held eager by no one
  expect_setequal(out$built, c("a", "b"))
  expect_identical(out$status, "unevaluated")
  expect_identical(out$eager, list(`front-end` = "a"))

  expect_match(out$body, "ChickWeight", fixed = TRUE)
  expect_false(grepl("`NA` <-", out$body, fixed = TRUE))
})

test_that("code_export_state distinguishes ready, blocked and pending", {

  board <- new_board(
    blocks = c(a = new_dataset_block("BOD"), b = new_dataset_block("BOD"))
  )

  blk <- function(state_ready = TRUE) {
    list(server = list(state_ready = state_ready))
  }

  errored <- function(id) {
    data.frame(
      block = id,
      phase = "eval",
      severity = "error",
      message = "boom",
      id = "cnd"
    )
  }

  make_ro <- function(blocks, conditions = no_conditions()) {
    list(blocks = blocks, conditions = conditions, board = board)
  }

  expect_identical(
    code_export_state(make_ro(list(a = blk(), b = blk()))),
    "ready"
  )

  # A block whose user inputs were never set holds the export back, whether or
  # not it has ever been evaluated
  expect_identical(
    code_export_state(make_ro(list(a = blk(), b = blk(FALSE)))),
    "blocked"
  )

  # So does an error a block reported the last time it ran, which outlives the
  # evaluation that raised it
  expect_identical(
    code_export_state(make_ro(list(a = blk(), b = blk()), errored("b"))),
    "blocked"
  )

  # A block still missing from the built set is pending, not blocked
  expect_identical(code_export_state(make_ro(list(a = blk()))), "pending")
})

test_that("the code panel offers a one-off evaluation and a copy", {

  ui <- as.character(code_ext_ui("gen", new_board()))

  expect_match(ui, "id=\"gen-code_eval\"", fixed = TRUE)
  expect_match(ui, "id=\"gen-code_out\"", fixed = TRUE)

  # The copy button sits beside the code rather than inside it, so copying the
  # target element yields the script alone
  expect_match(ui, "copyCode(&quot;gen-code_txt&quot;)", fixed = TRUE)
})

test_that("code panel body highlights a script or shows a note", {

  ready <- as.character(code_panel_body("ready", NS("gen"), "y <- 1"))

  # The script is highlighted, so it reaches the DOM as tokens rather than as a
  # contiguous string: read it back off the rendered `<pre>`
  code_txt <- xml2::xml_text(
    xml2::xml_find_first(xml2::read_html(ready), ".//pre")
  )

  expect_match(ready, "chroma", fixed = TRUE)
  expect_match(trimws(code_txt), "^y <- 1$")

  # The copy target holds the highlighted script and nothing else
  expect_match(ready, "id=\"gen-code_txt\"", fixed = TRUE)

  expect_match(
    as.character(code_panel_body("pending", NS("gen"))),
    "Waiting for every block"
  )

  expect_match(
    as.character(code_panel_body("blocked", NS("gen"))),
    "not configured"
  )

  # An empty board is ready, but has nothing to highlight
  expect_match(
    as.character(code_panel_body("ready", NS("gen"), "")),
    "No blocks"
  )
})
