fold_linear_paths <- function(exprs, args, types, board) {

  paths <- identify_linear_subgraphs(as.matrix(board))

  absorbed <- character()
  replacements <- character()

  for (path in paths) {

    folded <- try_fold_path(path, exprs, args, types)

    if (is.null(folded)) {
      next
    }

    tail_id <- path[length(path)]
    replacements[[tail_id]] <- paste0(tail_id, " <- ", folded)
    absorbed <- union(absorbed, path[-length(path)])
  }

  list(absorbed = absorbed, replacements = replacements)
}

try_fold_path <- function(path, exprs, args, types) {

  head_id <- path[1L]

  head_expr <- wrap_expr(
    exprs[[head_id]],
    args[[head_id]],
    types[[head_id]]
  )

  pieces <- list(deparse_one(head_expr))

  for (i in seq_along(path)[-1L]) {

    this_id <- path[i]

    if (!identical(types[[this_id]], "bquoted")) {
      return(NULL)
    }

    step <- pipe_step(
      exprs[[this_id]],
      args[[this_id]],
      previous_id = path[i - 1L]
    )

    if (is.null(step)) {
      return(NULL)
    }

    pieces[[length(pieces) + 1L]] <- deparse_one(step)
  }

  paste(unlist(pieces), collapse = " |> ")
}

pipe_step <- function(expr, args, previous_id) {

  sub <- do.call(bquote, list(expr, args %||% list()))

  if (!is.call(sub)) {
    return(NULL)
  }

  prev_name <- as.name(previous_id)
  arg_count <- length(sub) - 1L

  if (arg_count == 0L) {
    return(NULL)
  }

  matches <- vapply(
    seq_len(arg_count),
    function(j) identical(sub[[j + 1L]], prev_name),
    logical(1L)
  )

  if (sum(matches) != 1L) {
    return(NULL)
  }

  pos <- which(matches)
  arg_names <- names(sub)
  this_name <- if (is.null(arg_names)) "" else arg_names[pos + 1L]

  if (pos == 1L && !nzchar(this_name)) {
    sub[-(pos + 1L)]
  } else if (nzchar(this_name)) {
    sub[[pos + 1L]] <- quote(`_`)
    sub
  } else {
    NULL
  }
}

deparse_one <- function(x) paste0(deparse(x), collapse = "\n")

`%||%` <- function(x, y) if (is.null(x)) y else x
