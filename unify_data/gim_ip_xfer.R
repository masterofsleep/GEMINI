library(gemini)
lib.pa()
#===================GIM_IP_Transfers========================================
#------------available for SMH, SBK, UHN   --- -----------------------------
rm(list = ls())
smh <- readg(smh, ip_xfer)
sbk <- readg(sbk, ip_xfer)
uhn <- readg(uhn, ip_xfer)



names(smh)
names(sbk)
names(uhn)



names(uhn) <- names(smh)
names(sbk) <- c("Date.Check.in", "Time.Check.in", "Unit", "EncID.new")
sbk <- sbk[!is.na(EncID.new),]

smh <- smh[!duplicated(smh)]
write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.ip_xfer.nophi.csv",
          row.names = F, na = "")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_xfer.nophi.csv",
          row.names = F, na = "")
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.ip_xfer.nophi.csv",
          row.names = F, na = "")

names(smh)
names(sbk)
names(uhn)


apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(thp, MARGIN = 2, FUN = function(x)sum(is.na(x)))

uhn.check <- uhn[is.na(Unit)]




#adding the transfer code to the transfer files
rm(list = ls())
smh <- readg(smh, ip_xfer)
sbk <- readg(sbk, ip_xfer)
uhn <- readg(uhn, ip_xfer)

library(readxl)
smh.code <- read_excel("H:/GEMINI/Results/Shortadm/ip_xfer Units needing coding.xlsx",
                       sheet = 3)
sbk.code <- read_excel("H:/GEMINI/Results/Shortadm/ip_xfer Units needing coding.xlsx",
                       sheet = 2)
uhn.code <- read_excel("H:/GEMINI/Results/Shortadm/ip_xfer Units needing coding.xlsx",
                       sheet = 1)

names(smh.code) <- c("Unit", "Unit.Code")
names(sbk.code) <- c("Unit", "Unit.Code")
names(uhn.code) <- c("Unit", "Unit.Code")
sbk[sbk$Unit.Code =="?"]
smh.code[!smh.code$Unit%in%smh$Unit,]
sbk.code[!sbk.code$Unit%in%sbk$Unit,]
uhn.code[!uhn.code$Unit%in%uhn$Unit,]
unique(uhn[!Unit%in%uhn.code$Unit, Unit])

smh <- merge(smh, smh.code, by = "Unit", all.x = T, all.y = F)
sbk <- merge(sbk, sbk.code[,c(1,2)], by = "Unit", all.x = T, all.y = F)
uhn <- merge(uhn, uhn.code, by = "Unit", all.x = T, all.y = F)

check <- unique(uhn[is.na(Unit.Code), Unit])

smh[,Date.Check.in:=mdy(Date.Check.in)]
smh[,Date.Check.out:=mdy(Date.Check.out)]

sbk[, Date.Check.in:=mdy(Date.Check.in)]
sbk[, Time.Check.in:=paste(Time.Check.in, ":00", sep = "")]

#remove the 6 rows with encid only in uhn
uhn <- uhn[!is.na(Unit)]

write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.ip_xfer.nophi.csv",
          row.names = F, na = "")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_xfer.nophi.csv",
          row.names = F, na = "")
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.ip_xfer.nophi.csv",
          row.names = F, na = "")



## ------------------- march 10 new msh xfer data ------------------------------
library(gemini)
lib.pa()
msh <- fread("R:/GEMINI/_RESTORE/MSH/Transfers/msh.transfers.nophi.csv")
msh[, EncID.new := paste("14", EncID.new, sep = "")]
fwrite(msh, "H:/GEMINI/Data/MSH/CIHI/msh.xfer.csv")
