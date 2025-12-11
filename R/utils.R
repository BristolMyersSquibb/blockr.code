lst_xtr_reval <- function(x, ...) {
  lapply(lst_xtr(x, ...), reval)
}
