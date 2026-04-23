test_that("pipe_step drops first positional arg (bquoted)", {

  step <- pipe_step(
    expr = parse(text = "dplyr::filter(.(data), x > 0)")[[1L]],
    args = list(data = quote(prev)),
    type = "bquoted",
    previous_id = "prev"
  )

  expect_equal(deparse(step), "dplyr::filter(x > 0)")
})

test_that("pipe_step uses _ placeholder for named non-first arg (bquoted)", {

  step <- pipe_step(
    expr = parse(text = "lm(mpg ~ wt, data = .(data))")[[1L]],
    args = list(data = quote(prev)),
    type = "bquoted",
    previous_id = "prev"
  )

  expect_equal(deparse(step), "lm(mpg ~ wt, data = `_`)")
})

test_that("pipe_step refuses non-first unnamed arg (bquoted)", {

  expect_null(
    pipe_step(
      expr = parse(text = "merge(other, .(data))")[[1L]],
      args = list(data = quote(prev)),
      type = "bquoted",
      previous_id = "prev"
    )
  )
})

test_that("pipe_step folds a quoted block via bare input symbol", {

  step <- pipe_step(
    expr = quote(blockr.bi::gt_table(data, title = "x")),
    args = list(data = quote(prev)),
    type = "quoted",
    previous_id = "prev"
  )

  expect_equal(deparse(step), 'blockr.bi::gt_table(title = "x")')
})

test_that("pipe_step folds a quoted block with named input via _", {

  step <- pipe_step(
    expr = quote(lm(mpg ~ wt, data = data)),
    args = list(data = quote(prev)),
    type = "quoted",
    previous_id = "prev"
  )

  expect_equal(deparse(step), "lm(mpg ~ wt, data = `_`)")
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

test_that("mixed quoted / bquoted chain still folds", {

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
    c = quote(subset(data)),
    d = parse(text = "subset(.(data))")[[1L]]
  )

  out <- fold_linear_paths(
    exprs,
    args = list(
      a = NULL,
      b = list(data = quote(a)),
      c = list(data = quote(b)),
      d = list(data = quote(c))
    ),
    types = c(a = "quoted", b = "bquoted", c = "quoted", d = "bquoted"),
    board = board
  )

  expect_length(out$replacements, 1L)
  expect_named(out$replacements, "d")
  expect_match(out$replacements[["d"]], "|>", fixed = TRUE)
})
