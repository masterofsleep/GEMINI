#======================== Blood Transfusion  ===================================
#------------------  available for SMH, UHN, UHN  ------------------------------
library(gemini)
lib.pa()
rm(list = ls())
smh <- readg(smh, bb)
uhnip <- readg(uhn, txm_ip)
uhner <- readg(uhn, txm_er)

names(smh)
names(uhnip)
names(uhner)

sum(duplicated(smh))
sum(duplicated(uhner))
sum(duplicated(uhnip))

apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhner, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhnip, MARGIN = 2, FUN = function(x)sum(is.na(x)))

uhner <- uhner[!duplicated(uhner)]
uhnip <- uhnip[!duplicated(uhnip)]

write.csv(uhner, "H:/GEMINI/Data/UHN/Transfusion Medicine/uhn.txm_er.nophi.csv",
          row.names = F, na = "")
write.csv(uhnip, "H:/GEMINI/Data/UHN/Transfusion Medicine/uhn.txm_ip.nophi.csv",
          row.names = F, na = "")



#--------------------- sample Jan 20 2017 --------------------------------------
trans_sample <- smh[1:97]
trans_sample$EncID.new <- as.numeric(as.factor(trans_sample$EncID.new))
fwrite(trans_sample, "H:/GEMINI/Results/Ad Hoc/trans.sample.csv")




#-------------------- new sbk trans data ---------------------------------------
setwd("R:/GEMINI/_RESTORE/SBK/Transfers")
files <- list.files()
sbk.dob <- fread(files[1])
sbk <- fread(files[2])
sbk[,V1:=NULL]
sbk.dob[,V1:=NULL]
sum(duplicated(sbk))
sum(duplicated(sbk.dob))
sum(duplicated(rbind(sbk, sbk.dob)))
sbk.dob$EncID.new <- paste("12", sbk.dob$EncID.new, sep = "")
sbk$EncID.new <- paste("12", sbk$EncID.new, sep = "")


fwrite(sbk, "H:/GEMINI/Data/SBK/Transfusion/sbk.trans.csv")
fwrite(sbk.dob, "H:/GEMINI/Data/SBK/Transfusion/sbk.trans.dob.csv")
dad <- readg(sbk, dad)
dad$trans <- dad$EncID.new%in%sbk.dob$EncID.new

ggplot(dad, aes(ymd(Discharge.Date), fill = trans)) +
  geom_histogram(binwidth = 10) + ggtitle("sbk trans")

all <- rbind(sbk, sbk.dob)
all[!EncID.new%in%all[duplicated(all), EncID.new]] -> check
