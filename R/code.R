export_wrapped_code <- function(expressions, board) {

  exprs <- do.call(
    Map,
    c(
      list(wrap_expr),
      export_code(expressions, board)
    )
  )

  exprs <- map(assignment, names(exprs), exprs)
  exprs <- lapply(exprs, deparse)
  exprs <- chr_ply(exprs, paste0, collapse = "\n")

  paste0(exprs, collapse = "\n\n")
}

wrap_expr <- function(exprs, args, types) {

  if (identical(types, "bquoted")) {
    exprs <- do.call(bquote, list(exprs, args))
  }

  if (length(args) && identical(types, "quoted")) {
    call("with", args, exprs)
  } else if (has_assignment(exprs)) {
    call("local", exprs)
  } else {
    exprs
  }
}

assignment <- function(name, value) {
  bquote(.(nme) <- .(val), list(nme = as.name(name), val = value))
}

has_assignment <- function(expr) {
  check_assignment_recursive(expr, local_scope = FALSE)
}

check_assignment_recursive <- function(expr, local_scope) {

  if (is.atomic(expr) || is.name(expr)) {
    return(FALSE)
  }

  if (is.call(expr)) {

    fn <- as.character(expr[[1]])

    if (fn == "<-" && !local_scope) {
      return(TRUE)
    }

    if (fn %in% c("<<-", "assign")) {

      blockr_warn(
        "Using the global assignment operator `<<-` or calling ",
        "assign() is discouraged, as it can lead to unreliable ",
        "code generation.",
        class = "code_generation_discouraged_assignments"
      )

      return(TRUE)
    }

    if (fn == "function" || fn == "\\") {

      if (length(expr) >= 3) {
        return(
          check_assignment_recursive(expr[[3L]], local_scope = TRUE)
        )
      }

      return(FALSE)
    }

    if (fn == "local") {

      if (length(expr) >= 2) {
        return(
          check_assignment_recursive(expr[[2]], local_scope = TRUE)
        )
      }

      return(FALSE)
    }

    if (fn == "{") {

      for (i in seq_along(expr)[-1]) {
        if (check_assignment_recursive(expr[[i]], local_scope)) {
          return(TRUE)
        }
      }

      return(FALSE)
    }

    for (i in seq_along(expr)[-1]) {
      if (check_assignment_recursive(expr[[i]], local_scope)) {
        return(TRUE)
      }
    }
  }

  if (is.pairlist(expr)) {
    for (i in seq_along(expr)) {
      if (check_assignment_recursive(expr[[i]], local_scope)) {
        return(TRUE)
      }
    }
  }

  FALSE
}
