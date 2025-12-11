test_that("Simple linear path is identified correctly", {

  # DAG: 1 -> 2 -> 3 -> 4

  adj <- matrix(0, 4, 4)

  adj[1, 2] <- 1
  adj[2, 3] <- 1
  adj[3, 4] <- 1

  result <- identify_linear_subgraphs(adj)

  expect_length(result, 1)
  expect_equal(result[[1]], c(1, 2, 3, 4))
})

test_that("Multiple separate linear paths are identified", {

  # DAG: 1 -> 2 -> 3    4 -> 5 -> 6

  adj <- matrix(0, 6, 6)

  adj[1, 2] <- 1
  adj[2, 3] <- 1
  adj[4, 5] <- 1
  adj[5, 6] <- 1

  result <- identify_linear_subgraphs(adj)

  expect_length(result, 2)

  paths_as_strings <- lapply(result, paste, collapse = ",")

  expect_true("1,2,3" %in% paths_as_strings)
  expect_true("4,5,6" %in% paths_as_strings)
})

test_that("Branching point stops linear path", {

  # DAG: 1 -> 2 -> 3
  #           |
  #           v
  #           4

  adj <- matrix(0, 4, 4)

  adj[1, 2] <- 1
  adj[2, 3] <- 1
  adj[2, 4] <- 1

  expect_length(
    identify_linear_subgraphs(adj),
    0
  )
})

test_that("Merge point stops linear path", {

  # DAG: 1 -> 2 -> 4
  #                ^
  #      3 --------|

  adj <- matrix(0, 4, 4)

  adj[1, 2] <- 1
  adj[2, 4] <- 1
  adj[3, 4] <- 1

  expect_length(
    identify_linear_subgraphs(adj),
    0
  )
})

test_that("Single node is not returned", {

  # DAG: 1    2    3 (no edges)

  adj <- matrix(0, 3, 3)

  expect_length(
    identify_linear_subgraphs(adj),
    0
  )
})

test_that("Single edge creates a linear path", {

  # DAG: 1 -> 2

  adj <- matrix(0, 2, 2)

  adj[1, 2] <- 1

  expect_length(
    identify_linear_subgraphs(adj),
    0
  )
})

test_that("Complex DAG with branching and merging", {

  # DAG: 1 -> 2 -> 3 -> 4
  #           |
  #           v
  #      5 -> 6 -> 7

  adj <- matrix(0, 7, 7)

  adj[1, 2] <- 1
  adj[2, 3] <- 1
  adj[3, 4] <- 1
  adj[2, 6] <- 1
  adj[5, 6] <- 1
  adj[6, 7] <- 1

  result <- identify_linear_subgraphs(adj)

  expect_length(result, 2)

  paths_as_strings <- lapply(result, paste, collapse = ",")

  expect_true("2,3,4" %in% paths_as_strings)
  expect_true("2,6,7" %in% paths_as_strings)
})

test_that("Diamond pattern DAG", {

  # DAG:   1
  #       / \
  #      2   3
  #       \ /
  #        4

  adj <- matrix(0, 4, 4)

  adj[1, 2] <- 1
  adj[1, 3] <- 1
  adj[2, 4] <- 1
  adj[3, 4] <- 1

  expect_length(
    identify_linear_subgraphs(adj),
    0
  )
})

test_that("Long linear path", {

  # DAG: 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 8

  n <- 8
  adj <- matrix(0, n, n)

  for (i in 1:(n-1)) {
    adj[i, i+1] <- 1
  }

  result <- identify_linear_subgraphs(adj)

  expect_length(result, 1)
  expect_equal(result[[1]], 1:8)
})

test_that("Multiple short paths", {

  # DAG: 1 -> 2    3 -> 4    5 -> 6

  adj <- matrix(0, 6, 6)

  adj[1, 2] <- 1
  adj[3, 4] <- 1
  adj[5, 6] <- 1

  expect_length(
    identify_linear_subgraphs(adj),
    0
  )
})

test_that("Y-shaped DAG", {

  # DAG: 1 -> 2 -> 4
  #           ^
  #           3

  adj <- matrix(0, 4, 4)

  adj[1, 2] <- 1
  adj[3, 2] <- 1
  adj[2, 4] <- 1

  expect_length(
    identify_linear_subgraphs(adj),
    0
  )
})

test_that("Path with isolated source", {

  # DAG: 1 -> 2 -> 3 -> 4
  #      5

  adj <- matrix(0, 5, 5)

  adj[1, 2] <- 1
  adj[2, 3] <- 1
  adj[3, 4] <- 1

  result <- identify_linear_subgraphs(adj)

  expect_length(result, 1)
  expect_equal(result[[1]], c(1, 2, 3, 4))
})

test_that("Chain starting mid-DAG after branch", {

  # DAG: 1 -> 2 -> 3
  #           |
  #           v
  #           4 -> 5 -> 6

  adj <- matrix(0, 6, 6)

  adj[1, 2] <- 1
  adj[2, 3] <- 1
  adj[2, 4] <- 1
  adj[4, 5] <- 1
  adj[5, 6] <- 1

  result <- identify_linear_subgraphs(adj)

  expect_length(result, 1)
  expect_equal(result[[1]], c(2, 4, 5, 6))
})

test_that("Empty adjacency matrix", {
  expect_length(
    identify_linear_subgraphs(
      matrix(0, 0, 0)
    ),
    0
  )
})

test_that("Linear path after branching node", {

  # DAG: 1 -> 2
  #      |--> 3 -> 4

  adj <- matrix(0, 4, 4)
  adj[1, 2] <- 1
  adj[1, 3] <- 1
  adj[3, 4] <- 1

  result <- identify_linear_subgraphs(adj)

  expect_length(result, 1)
  expect_equal(result[[1]], c(1, 3, 4))
})

test_that("linear path through branch", {

  mat1 <- matrix(0, 5, 5, dimnames = list(letters[1:5], letters[1:5]))

  mat1[4, 2] <- 1
  mat1[4, 3] <- 1
  mat1[3, 1] <- 1
  mat1[1, 5] <- 1

  expect_identical(
    identify_linear_subgraphs(mat1),
    list(c("d", "c", "a", "e"))
  )

  mat2 <- matrix(0, 6, 6, dimnames = list(letters[1:6], letters[1:6]))

  mat2[1, 5] <- 1
  mat2[3, 1] <- 1
  mat2[4, 2] <- 1
  mat2[4, 3] <- 1
  mat2[6, 5] <- 1

  expect_identical(
    identify_linear_subgraphs(mat2),
    list(c("d", "c", "a"))
  )
})
