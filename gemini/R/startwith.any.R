#' A startwith.any Functino
#' 
#' This function allows you to see elements in one vector starts with any elements in another vector
#' @param x the vector to be compared
#' @param prefix the prefixs be be compared with
#' @keywords startwith, any
#' @export
#' @examples 
#' startwith.any(c("aa","bb", "cc", "dd"), c("a", "b"))
startwith.any <- function(x, prefix){
  mat <- matrix(0, nrow = length(x), ncol = length(prefix))
  for(i in 1:length(prefix)){
    mat[,i] <- startsWith(x, prefix[i])
  }
  return(as.vector(apply(mat, MARGIN = 1, FUN = sum))>0)
}