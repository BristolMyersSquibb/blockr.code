#' Code generation plugin module
#'
#' All code necessary for reproducing a data analysis as set up in blockr can
#' be made available to the user. While the `generate_code` plugin bundled with
#' blockr.core can provide code to accurately reproduce all results (final and
#' intermediate), this plugin is aimed at making this exported code as idiomatic
#' as possible.
#'
#' Opening the modal asks for every block on the board to be built, through the
#' `construct` board update component (see the "Evaluation requests" section of
#' [blockr.core::board_server()]), because the script is assembled from block
#' expressions and an unbuilt block carries none. Nothing is evaluated: a board
#' that defers its off-screen blocks stays deferred, and the front-end's gating
#' is untouched.
#'
#' Export is held back while a block is not fully configured, or while one
#' reports an error from its last run — either would put code into the script
#' that does not reproduce the board. A block that has never run reports
#' neither, so the modal offers to evaluate the board, which is a one-off that
#' leaves the blocks dormant again but has them report what they found.
#'
#' @param server,ui Server/UI for the plugin module
#'
#' @return A plugin container inheriting from `generate_code` is returned by
#' `generate_code()`, while the UI component (e.g. `generate_code_ui()`) is
#' expected to return shiny UI (i.e. [shiny::tagList()]) and the server
#' component (i.e. `generate_code_server()`) is expected to return `NULL`.
#'
#' @rdname generate_code
#' @export
generate_flat_code <- function(server = generate_flat_code_server,
                               ui = generate_flat_code_ui) {

  new_plugin(server, ui, class = "generate_code")
}

#' @param id Namespace ID
#' @param board Reactive values object
#' @param update Reactive value object to initiate board updates
#' @param ... Extra arguments passed from parent scope
#'
#' @rdname generate_code
#' @export
generate_flat_code_server <- function(id, board, update, ...) {
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

          code_modal_body(state, session$ns, script)
        }
      )

      observeEvent(
        input$code_mod,
        {
          update(list(construct = board_block_ids(board$board)))

          showModal(code_modal(session$ns))
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

#' @rdname generate_code
#' @export
generate_flat_code_ui <- function(id, board) {
  tagList(
    actionButton(
      NS(id, "code_mod"),
      "Show code",
      icon = icon("code")
    )
  )
}

code_modal <- function(ns) {
  modalDialog(
    title = "Generated code",
    uiOutput(ns("code_out")),
    easyClose = TRUE,
    footer = tagList(
      actionButton(ns("code_eval"), "Evaluate blocks", class = "btn-secondary"),
      modalButton("Close")
    ),
    size = "l"
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

code_modal_body <- function(state, ns, script = NULL) {

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
