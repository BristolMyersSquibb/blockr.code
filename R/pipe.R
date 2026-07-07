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

    step <- pipe_step(
      exprs[[this_id]],
      args[[this_id]],
      types[[this_id]],
      previous_id = path[i - 1L]
    )

    if (is.null(step)) {
      return(NULL)
    }

    pieces[[length(pieces) + 1L]] <- deparse_one(step)
  }

  paste(unlist(pieces), collapse = " |> ")
}

pipe_step <- function(expr, args, type, previous_id) {

  if (identical(type, "bquoted")) {
    sub <- do.call(bquote, list(expr, args %||% list()))
    target <- as.name(previous_id)
  } else if (identical(type, "quoted")) {
    sub <- expr
    input_name <- input_name_for(args, previous_id)
    if (is.null(input_name)) return(NULL)
    target <- as.name(input_name)
  } else {
    return(NULL)
  }

  if (!is.call(sub)) {
    return(NULL)
  }

  arg_count <- length(sub) - 1L

  if (arg_count == 0L) {
    return(NULL)
  }

  matches <- vapply(
    seq_len(arg_count),
    function(j) identical(sub[[j + 1L]], target),
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

input_name_for <- function(args, previous_id) {

  if (!length(args)) return(NULL)

  prev <- as.name(previous_id)
  match <- vapply(args, function(a) identical(a, prev), logical(1L))

  if (sum(match) != 1L) return(NULL)
  names(args)[match]
}

deparse_one <- function(x) paste0(deparse(x), collapse = "\n")

`%||%` <- function(x, y) if (is.null(x)) y else x
