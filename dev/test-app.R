options(shiny.port = 3838, shiny.host = "0.0.0.0")

pkgload::load_all("/workspace/blockr.core")
pkgload::load_all("/workspace/blockr.dplyr")
pkgload::load_all("/workspace/blockr.bi")
pkgload::load_all("/workspace/blockr.code")

library(blockr.core)

serve(
  new_board(
    blocks = c(
      a = new_dataset_block("ChickWeight"),
      b = new_filter_block(state = list(
        conditions = list(
          list(type = "values", column = "Diet",
               values = list("1"), mode = "include")
        ),
        operator = "&"
      )),
      c = new_summary_table_block(state = list(
        vars = c("weight", "Time"),
        by = "Chick",
        stats = "compact",
        add_overall = TRUE
      )),
      d = new_gt_table_block(title = "ChickWeight summary")
    ),
    links = links(
      from  = c("a", "b", "c"),
      to    = c("b", "c", "d"),
      input = c("data", "data", "data")
    )
  ),
  plugins = custom_plugins(generate_flat_code())
)
