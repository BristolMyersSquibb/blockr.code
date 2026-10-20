#' Code extension
#'
#' All code necessary for reproducing a data analysis as set up in blockr can
#' be made available to the user. While the code export bundled with
#' blockr.core can provide code to accurately reproduce all results (final and
#' intermediate), this extension is aimed at making the exported code as
#' idiomatic as possible. It shows the script in a panel of a
#' [blockr.dock::new_dock_board()], beside the blocks.
#'
#' The script is assembled from block expressions, which a block carries once
#' it is built, whether or not it has run. The panel therefore asks for nothing
#' to be evaluated, and for nothing to be built either: a board that defers its
#' off-screen blocks builds them in the background after first paint (see
#' [blockr.core::block_server()]). Until every block is built, the panel says
#' so rather than exporting a partial script. With the
#' `background_construction_delay` [blockr.core::blockr_option()] set to `Inf`,
#' a block that nothing needs is never built, and the panel keeps waiting.
#'
#' Export is held back while a block is not fully configured, or while one
#' reports an error from its last run — either would put code into the script
#' that does not reproduce the board. A block that has never run reports
#' neither, so the panel offers to evaluate the board, which is a one-off that
#' leaves the blocks parked again, reporting what they found.
#'
#' @param ... Forwarded to [blockr.dock::new_dock_extension()]
#'
#' @examples
#' ext <- new_code_extension()
#' blockr.dock::is_dock_extension(ext)
#'
#' @return A dock extension object that additionally inherits from
#' `code_extension`.
#'
#' @export
new_code_extension <- function(...) {
  blockr.dock::new_dock_extension(
    code_ext_srv,
    code_ext_ui,
    name = "Code",
    class = "code_extension",
    ...
  )
}

code_ext_srv <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      output$code_out <- renderUI(
        {
          state <- code_export_state(board)

          script <- if (identical(state, "ready")) {
            export_wrapped_code(
              lst_xtr_reval(board$blocks, "server", "expr"),
              board$board
            )
          }

          code_panel_body(state, session$ns, script)
        }
      )

      observeEvent(
        input$code_eval,
        update(list(evaluate = board_block_ids(board$board)))
      )

      NULL
    }
  )
}

code_ext_ui <- function(id, board) {
  div(
    div(
      class = "d-flex justify-content-end mb-2",
      actionButton(
        NS(id, "code_eval"),
        "Evaluate blocks",
        class = "btn-secondary btn-sm"
      )
    ),
    uiOutput(NS(id, "code_out"))
  )
}

code_export_state <- function(board) {

  ids <- board_block_ids(board$board)

  if (!setequal(names(board$blocks), ids)) {
    return("pending")
  }

  ready <- lgl_ply(board$blocks, block_state_ready)

  if (!all(ready) || nrow(export_block_errors(board)) > 0L) {
    return("blocked")
  }

  "ready"
}

block_state_ready <- function(blk) {
  isTRUE(reval_if(blk$server$state_ready))
}

export_block_errors <- function(board) {
  cnd <- reval_if(board$conditions)
  cnd[cnd$severity == "error", ]
}

code_panel_body <- function(state, ns, script = NULL) {

  if (!identical(state, "ready")) {
    return(code_status_note(state))
  }

  out <- paste0(script, collapse = "\n")

  if (!nchar(out)) {
    return(code_status_note("empty"))
  }

  pre <- downlit::highlight(
    paste0(styler::style_text(out), collapse = "\n"),
    classes = downlit::classes_chroma(),
    pre_class = "chroma"
  )

  div(
    class = "text-decoration-none position-relative",
    highlight_deps(),
    copy_to_clipboard(ns, "code_txt"),
    div(id = ns("code_txt"), HTML(add_blank_targets(pre)))
  )
}

code_status_note <- function(state) {

  div(
    class = "text-muted",
    switch(
      state,
      pending = "Preparing code...",
      empty = "No code available to display.",
      paste(
        "The board is not ready. Finish configuring all blocks, and fix any",
        "block reporting an error, before exporting code."
      )
    )
  )
}

copy_to_clipboard <- function(ns, id) {

  deps <- htmltools::htmlDependency(
    "copy-to-clipboard",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "copyToClipboard.js"
  )

  tagList(
    actionButton(
      ns("copy_code"),
      "",
      class = paste(
        "btn", "btn-outline-secondary", "btn-sm", "position-absolute",
        "top-0", "end-0", "m-2"
      ),
      icon = icon("copy", "fa-solid"),
      onclick = paste0("copyCode(\"", ns(id), "\");")
    ),
    deps
  )
}

highlight_deps <- function() {
  htmltools::htmlDependency(
    "chroma-highlighting",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "syntax-highlight.css"
  )
}

add_blank_targets <- function(html) {

  log_debug("adding blank targets to syntax highlighted code")

  doc <- xml2::read_html(html)

  links <- xml2::xml_find_all(doc, ".//pre//a")

  for (link in links) {

    xml2::xml_set_attr(link, "target", "_blank")

    existing_rel <- xml2::xml_attr(link, "rel")

    stopifnot(is_scalar(existing_rel))

    if (is.na(existing_rel)) {
      existing_rel <- character()
    } else {
      existing_rel <- strsplit(existing_rel, "\\s+")[[1]]
    }

    rel_parts <- paste(
      unique(c(existing_rel, "noopener", "noreferrer")),
      collapse = " "
    )

    xml2::xml_set_attr(link, "rel", rel_parts)
  }

  as.character(
    xml2::xml_children(xml2::xml_find_all(doc, "body"))
  )
}
