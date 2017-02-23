# ==============================================================================
# =================== REMOVE EXTRA UHN PATIENTS ================================
# ==============================================================================
rm(list = ls())
library(gemini)
lib.pa()
dad <- fread("H:/GEMINI/DataBackup/Data170214/UHN/CIHI/uhn.ip_dad.nophi.csv")
extra.enc <- dad[mdy(Discharge.Date)>ymd("20150331"), EncID.new]
fwrite(data.table(str_sub(extra.enc, 3, 8)), "H:/GEMINI/Results/Check/uhn.extra.enc.csv")
uhn.extra.rm <- function(){
  setwd("H:/GEMINI/Data/UHN")
  files <- list.files(recursive = T)
  for(i in files){
    dat <- NULL
    dat <- fread(i)
    if("NACRSRegistrationNumber"%in%names(dat))
      dat <- fread(i, colClasses = list(character = "NACRSRegistrationNumber"))
      dat <- dat[!EncID.new%in%extra.enc]
      fwrite(dat, i)
  }
}

uhn.extra.rm()

gim.extra.rm <- function(){
  setwd("H:/GEMINI/Data/GEMINI")
  files <- list.files(recursive = T)
  for(i in files){
    dat <- NULL
    dat <- fread(i)
    dat <- dat[!EncID.new%in%extra.enc]
    fwrite(dat, i)
    print(i)
  }
}


