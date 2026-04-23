options(shiny.port = 3838, shiny.host = "0.0.0.0")

pkgload::load_all("/workspace/blockr.core")
pkgload::load_all("/workspace/blockr.code")

library(blockr.core)

serve(
  new_board(
    blocks = c(
      a = new_dataset_block("ChickWeight"),
      b = new_subset_block(subset = "Diet == 1"),
      c = new_subset_block(subset = "weight > 100"),
      d = new_subset_block(subset = "Time > 5")
    ),
    links = links(
      from  = c("a", "b", "c"),
      to    = c("b", "c", "d"),
      input = c("data", "data", "data")
    )
  ),
  plugins = custom_plugins(generate_flat_code())
)
