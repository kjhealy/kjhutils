##' n-way contingency tables showing row/column/etc percents instead
##of raw frequencies.
##' @title ptable
##' @param ... Like table, the vectors to be tablulated.
##' @param by.pct Which dimension to calculate the perentatges
##on. 1 is row, 2 is column, etc.
##' @param dig Number of digits after the decimal point in the output
##' @return A nice table
##' @author Kieran Healy
##' @export
ptable <- function(..., by.pct=1,dig=2){
  out <- round(prop.table(table(...), by.pct)*100, dig)
  out
}
