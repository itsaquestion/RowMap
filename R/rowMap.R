#' rowMap
#'
#'
#'
#' @param x object with "row", eg. data.frame, xts
#' @param fun function to apply.
#'
#' @return result
#' @export
#' @import data.table
#' @import checkmate
#' @importFrom  zoo index
#'
#' @examples
rowMap = function(x, fun) {
  UseMethod("rowMap",x)
}

rowMap.default = function(x, fun) {
  setDT(as.data.frame(x))[, fun(.SD), by = seq_len(nrow(x))][, -1]
}

rowMap.xts = function(x, fun) {
  ii = zoo::index(x)
  dt = as.data.table(x)
  assertDataFrame(dt, min.rows=1, min.cols = 1)
  as.xts.data.table(dt[, !"index"][, fun(.SD), by = seq_len(nrow(x))][, seq_len := ii])
}

rowMap.data.frame = function(x, fun) {
  assertDataFrame(x, min.rows=1, min.cols = 1)
  as.data.frame(as.data.table(x)[, fun(.SD), by = seq_len(nrow(x))][, !"seq_len"])
}

rowMap.data.table = function(x, fun) {
  assertDataFrame(x, min.rows=1, min.cols = 1)
  x[, fun(.SD), by = seq_len(nrow(x))][, !"seq_len"]
}
