#' A Swdr Function
#' 
#' This functions allows you to set working directory to the R drive gemini data
#' @param sitefolder The site and subfolders in the site
#' @keywords working directory
#' @export
#' @examples 
#' swdr("SMH/CIHI")
#' 
swdr <- function(sitefolder = ""){
  wd <- paste("//vs-research/research/GEMINI/_RESTORE", sitefolder, sep = "/")
  setwd(wd)
}
