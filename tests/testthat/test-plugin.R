no_conditions <- function() {
  data.frame(
    block = character(),
    phase = character(),
    severity = character(),
    message = character(),
    id = character()
  )
}

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

  plugin_args <- generate_plugin_args(board)
  plugin_args$update <- reactiveVal()

  testServer(
    generate_flat_code_server,
    {
      expect_identical(code_export_state(board), "ready")

      expect_match(as.character(output$code_out), "merge", all = FALSE)

      session$setInputs(code_mod = 1)

      expect_identical(update(), list(construct = c("a", "b", "c")))

      session$setInputs(code_eval = 1)

      expect_identical(update(), list(evaluate = c("a", "b", "c")))
    },
    args = plugin_args
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

  # Gating on the built set is what keeps the plugin from reaching the exporter
  # with `b` missing: it reports pending rather than exporting junk
  ro <- list(
    blocks = list(a = list(server = list(state_ready = TRUE))),
    conditions = no_conditions(),
    board = board
  )

  expect_identical(code_export_state(ro), "pending")

  body <- as.character(code_modal_body(code_export_state(ro), NS("gen")))

  expect_false(grepl("`NA` <-", body, fixed = TRUE))
  expect_match(body, "Preparing")
})

test_that("show code builds a deferred board without evaluating it", {

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
      vis$required[["a"]](TRUE)
      vis$visible[["a"]](TRUE)
      session$flushReact()

      before <- names(rv$blocks)

      session$setInputs(`generate_code-code_mod` = 1)
      session$flushReact()

      out <<- list(
        before = before,
        after = names(rv$blocks),
        status = reval_if(rv$eval[["b"]]),
        required = vis$required[["b"]](),
        body = as.character(output$`generate_code-code_out`$html)
      )
    },
    args = list(x = board, plugins = plugins(generate_flat_code()))
  )

  # The off-screen block starts unbuilt, and showing the code builds it
  expect_identical(out$before, "a")
  expect_setequal(out$after, c("a", "b"))

  # Built for its expression and left dormant: the export needs blocks present,
  # not run, so the board stays as lazy as it was
  expect_identical(out$status, "dormant")
  expect_true(is.na(out$required))

  # The whole script is exported, rather than an `NA` assignment standing in for
  # the block that was missing
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

test_that("the code modal offers a one-off evaluation", {

  modal <- as.character(code_modal(NS("gen")))

  expect_match(modal, "id=\"gen-code_eval\"", fixed = TRUE)
  expect_match(modal, "id=\"gen-code_out\"", fixed = TRUE)
})

test_that("code modal body highlights a script or shows a note", {

  ready <- as.character(code_modal_body("ready", NS("gen"), "y <- 1"))

  # The script is highlighted, so it reaches the DOM as tokens rather than as a
  # contiguous string: read it back off the rendered `<pre>`
  code_txt <- xml2::xml_text(
    xml2::xml_find_first(xml2::read_html(ready), ".//pre")
  )

  expect_match(ready, "chroma", fixed = TRUE)
  expect_match(trimws(code_txt), "^y <- 1$")

  # The copy button sits beside the code rather than inside it, so copying the
  # target element yields the script alone
  expect_match(ready, "copyCode(&quot;gen-code_txt&quot;)", fixed = TRUE)
  expect_match(ready, "id=\"gen-code_txt\"", fixed = TRUE)

  expect_match(as.character(code_modal_body("pending", NS("gen"))), "Preparing")

  expect_match(as.character(code_modal_body("blocked", NS("gen"))), "not ready")

  # An empty board is ready, but has nothing to highlight
  expect_match(
    as.character(code_modal_body("ready", NS("gen"), "")),
    "No code available"
  )
})

test_that("dummy gen code ui test", {
  expect_s3_class(generate_flat_code_ui("gen", new_board()), "shiny.tag.list")
})
