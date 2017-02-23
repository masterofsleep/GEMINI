#============== Generate New GEMINI ID =========================================
  # Rule for the new GEMINI ID 
  # prefix 2 digits to specify site
  # 11 for smh
  # 12 for sbk
  # 13 for uhn
  # 14 for msh
  # 15 for thp
rm(list = ls())
new_encid <- function(site = site, data_name = ""){
  swdh("")
  if(site == "SMH"){
    prefix = "11"
  }else if(site=="SBK"){
    prefix = "12"
  }else if(site=="UHN"){
    prefix = "13"
  }else if(site=="MSH"){
    prefix = "14"
  }else if(site=="THP"){
    prefix = "15"
  }
  files <- list.files(recursive = T)
  filepath <- files[grepl(site, files)&grepl(data_name, files)]
  for(i in filepath){
    dat <- fread(i, na.strings = c("", NA, NULL, " "), 
                 colClasses = list(character = c("EncID.new")))
    if((site == "SBK"|site == "UHN")&("NACRSRegistrationNumber"%in%names(dat))){
      dat <- fread(i, na.strings = c("", NA, NULL, " "), 
                   colClasses = list(character = c("EncID.new", 
                                                   "NACRSRegistrationNumber")))
    }
    if("EncID.new"%in%names(dat)){
      dat$EncID.new <- paste(prefix, dat$EncID.new, sep = "")
      write.csv(dat, i, row.names = F, na = "")
    }else{
      print(i)
    }
  }
}

new_encid("SMH")

site <- "SMH"
#for smh, stoped at filepath[20] Lab Test Codes: Blood Bank Product Codes


new_encid("SBK")
site <- "SBK"
new_encid("UHN")
new_encid("MSH")
new_encid("THP")


new_encid("UHN", data_name = "labs_ip")
new_encid("UHN", data_name = "labs_er")
