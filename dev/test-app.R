options(shiny.port = 3838, shiny.host = "0.0.0.0")

pkgload::load_all("/workspace/blockr.core")
pkgload::load_all("/workspace/blockr.io")
pkgload::load_all("/workspace/blockr.dplyr")
pkgload::load_all("/workspace/blockr.bi")
pkgload::load_all("/workspace/blockr.code")

library(blockr.core)

serve(
  new_board(
    blocks = c(
      a = new_read_block(path = "/workspace/blockr.sandbox/dev/data/adsl.parquet"),
      b = new_filter_block(state = list(
        conditions = list(
          list(type = "values", column = "SEX",
               values = list("F", "M"), mode = "include")
        ),
        operator = "&"
      )),
      c = new_summary_table_block(state = list(
        vars = c("SEX", "AGEGR1"),
        by = "TRT01P",
        stats = "compact"
      )),
      d = new_gt_table_block(title = "Demographics")
    ),
    links = links(
      from  = c("a", "b", "c"),
      to    = c("b", "c", "d"),
      input = c("data", "data", "data")
    )
  ),
  plugins = custom_plugins(generate_flat_code())
)
