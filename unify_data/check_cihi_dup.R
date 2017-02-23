
library(gemini)
lib.pa()
rm(list = ls())
check_dup <- function(folder){
  swdh(folder)
  files <- list.files()
  ndup <- NULL
  for(i in files){
    dat <- fread(i)
    ndup <- c(ndup, sum(duplicated(dat)))
  }
  return(cbind(files, ndup))
}
cihi_dup <- rbind(check_dup("SMH/CIHI"),
                  check_dup("SBK/CIHI"),
                  check_dup("UHN/CIHI"),
                  check_dup("MSH/CIHI"),
                  check_dup("THP/CIHI"))

write.csv(cihi_dup, "H:/GEMINI/Results/Variable Map/cihi_duplist.csv",
          row.names = F)

rm.dup <- function(folder){
  swdh(folder)
  files <- list.files()
  for(i in files){
    dat <- fread(i)
    if("NACRSRegistrationNumber"%in%names(dat)){
    dat <- fread(i, colClasses = list(character = c("NACRSRegistrationNumber")))
    }
    dat <- dat[!duplicated(dat)]
    write.csv(dat, i, row.names = F)
  }
}

rm.dup("SMH/CIHI1")
check_dup("SMH/CIHI1")
