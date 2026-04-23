test_that("pipe_step drops first positional arg", {

  step <- pipe_step(
    expr = parse(text = "dplyr::filter(.(data), x > 0)")[[1L]],
    args = list(data = quote(prev)),
    previous_id = "prev"
  )

  expect_equal(deparse(step), "dplyr::filter(x > 0)")
})

test_that("pipe_step uses _ placeholder for named non-first arg", {

  step <- pipe_step(
    expr = parse(text = "lm(mpg ~ wt, data = .(data))")[[1L]],
    args = list(data = quote(prev)),
    previous_id = "prev"
  )

  expect_equal(deparse(step), "lm(mpg ~ wt, data = `_`)")
})

test_that("pipe_step refuses non-first unnamed arg", {

  expect_null(
    pipe_step(
      expr = parse(text = "merge(other, .(data))")[[1L]],
      args = list(data = quote(prev)),
      previous_id = "prev"
    )
  )
})

test_that("linear bquoted chain folds into a pipe", {

  board <- new_board(
    blocks = c(
      a = new_dataset_block("BOD"),
      b = new_subset_block(),
      c = new_subset_block(),
      d = new_subset_block()
    ),
    links = links(
      from  = c("a", "b", "c"),
      to    = c("b", "c", "d"),
      input = c("data", "data", "data")
    )
  )

  exprs <- list(
    a = quote(datasets::BOD),
    b = parse(text = "subset(.(data))")[[1L]],
    c = parse(text = "subset(.(data))")[[1L]],
    d = parse(text = "subset(.(data))")[[1L]]
  )

  out <- export_wrapped_code(exprs, board)

  expect_match(out, "|>", fixed = TRUE)
  expect_match(out, "^d <-")
  expect_false(grepl("\nb <-", out))
  expect_false(grepl("\nc <-", out))
})

test_that("non-bquoted block in chain prevents folding", {

  out <- fold_linear_paths(
    exprs = list(
      a = quote(datasets::BOD),
      b = bquote(subset(.(data)), splice = FALSE),
      c = quote(subset(data)),
      d = bquote(subset(.(data)), splice = FALSE)
    ),
    args = list(
      a = NULL,
      b = list(data = quote(a)),
      c = list(data = quote(b)),
      d = list(data = quote(c))
    ),
    types = c(a = "quoted", b = "bquoted", c = "quoted", d = "bquoted"),
    board = local({
      brd <- new_board(
        blocks = c(
          a = new_dataset_block("BOD"),
          b = new_subset_block(),
          c = new_subset_block(),
          d = new_subset_block()
        ),
        links = links(
          from  = c("a", "b", "c"),
          to    = c("b", "c", "d"),
          input = c("data", "data", "data")
        )
      )
      brd
    })
  )

  expect_length(out$replacements, 0L)
  expect_length(out$absorbed, 0L)
})
