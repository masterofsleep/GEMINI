#' A Swdh Function
#' 
#' This functions allows you to set working directory to the local gemini data
#' @param sitefolder The site and subfolders in the site
#' @keywords working directory
#' @export
#' @examples 
#' swdr("SMH/CIHI")
#' 
swdh <- function(sitefolder = ""){
  wd <- paste("H:/GEMINI/Data", sitefolder, sep = "/")
  setwd(wd)
}