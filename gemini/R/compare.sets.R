#' A compare sets function
#' 
#' This function allows you to compare two sets, get the number of unique
#' and common elements in each sets
#' @param x the first set to be compared
#' @param y the second set to be compared
#' @keywords comparison sets
#' @export
#' @examples()
#' compare.sets(c(1,1,2,3,4), c(2,2,3,4,5))

compare.sets <- function(x, y){
  in.both <- length(intersect(x, y))
  x.only <- length(unique(setdiff(x, y)))
  y.only <- length(unique(setdiff(y, x)))
  data.table(in.both, x.only, y.only)
}
