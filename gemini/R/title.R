#' A title function
#' 
#' this function allows you to create title for codes with date
#' @param what content you want to put in the title
#' @export
#' examples
#' title("nice") 

title <- function(what){
  len <- nchar(what)
  cat("# ------------------------------------------------------------------------------\n",
      paste(c(c("# -"), rep("-", (80 - 5 - len)/2), " ",what, " ", 
              rep("-", floor((80 - 4 - len)/2))),
            collapse = ""), "\n", 
      paste(c(c("# -"), rep("-", floor((80 - 5 - 10)/2)), " ",as.character(Sys.Date()), " ", 
              rep("-", (80 - 4 - 10)/2)),
            collapse = ""), "\n")
}

