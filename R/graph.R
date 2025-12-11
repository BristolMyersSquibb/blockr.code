identify_linear_subgraphs <- function(x) {

  dfs <- function(node, current_path) {

    children <- which(x[node, ] > 0L)

    if (length(children) > 1L) {

      if (length(current_path) >= 3L) {
        linear_paths <<- c(list(current_path), linear_paths)
        in_linear_path[current_path] <<- TRUE
      }

      for (child in children) {
        if (!in_linear_path[child]) {
          dfs(child, c(node, child))
        }
      }

    } else if (length(children) == 1L) {

      child <- children

      if (in_degree[child] > 1L) {

        if (length(current_path) >= 3L) {
          linear_paths <<- c(list(current_path), linear_paths)
          in_linear_path[current_path] <<- TRUE
        }

        if (!in_linear_path[child]) {
          dfs(child, c(child))
        }

      } else {

        if (!in_linear_path[child]) {
          dfs(child, c(current_path, child))
        } else {
          if (length(current_path) >= 3) {
            linear_paths <<- c(list(current_path), linear_paths)
            in_linear_path[current_path] <<- TRUE
          }
        }
      }
    } else {

      if (length(current_path) >= 3) {
        linear_paths <<- c(list(current_path), linear_paths)
        in_linear_path[current_path] <<- TRUE
      }
    }
  }

  stopifnot(
    is.matrix(x),
    nrow(x) == ncol(x),
    identical(rownames(x), colnames(x)),
    all(x == 1L | x == 0L)
  )

  if (!is.null(rownames(x))) {
    stopifnot(
      length(rownames(x)) == nrow(x),
      anyDuplicated(rownames(x)) == 0L
    )
  }

  n <- nrow(x)

  if (n == 0) {
    return(list())
  }

  linear_paths <- list()

  in_degree <- colSums(x > 0)
  out_degree <- rowSums(x > 0)

  in_linear_path <- rep(FALSE, n)

  for (source in which(in_degree == 0 & out_degree > 0)) {
    if (!in_linear_path[source]) {
      dfs(source, source)
    }
  }

  if (!is.null(rownames(x))) {
    linear_paths <- lapply(linear_paths, function(i) rownames(x)[i])
  }

  linear_paths
}
