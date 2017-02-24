#' a readg function
#' 
#' This function allows you to read any data files in GEMINI database easily
#' @param site the site of the data to be find, no "" required
#' @param data_name the key names of the data required, no "" required
#' @param dt whether merged with admit/discharge date/time or not, default false
#' @keywords gemini read
#' @export
#' @example 
readg <- function(site = site, data_name = data_name, dt = FALSE, colClasses = NULL,...){
  swdh()
  files <- list.files(recursive = T)
  site <- deparse(substitute(site))
  data_name <- deparse(substitute(data_name))
  filepath <- files[grepl(site, files)&grepl(data_name, files)]
  if(!dt)return(fread(filepath, na.strings = c(NA,NULL,"", "NA", " "),
               colClasses = colClasses, showProgress = T,...))
  else{
    dat <- fread(filepath, na.strings = c(NA,NULL,"", "NA", " "),
                 colClasses = colClasses, showProgress = T,...)
    dat$EncID.new <- as.character(dat$EncID.new)
    dadpath <- files[grepl(site, files)&grepl("dad", files)]
    dad <- fread(dadpath, na.strings = c(NA,NULL,"", "NA", " "),
                 colClasses = colClasses, showProgress = T,...)[
                   ,.(Admit.Date, Admit.Time, Discharge.Date, 
                               Discharge.Time, EncID.new)]
    dad$EncID.new <- as.character(dad$EncID.new)
    return(merge(dat, dad, by = "EncID.new"))
  }
}
