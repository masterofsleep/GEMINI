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



# ------------------------- sinai data -----------------------------------------
setwd("R:/GEMINI/_RESTORE/MSH/BB")
msh.bb <- fread("msh.bb.nophi.csv")
msh.bb$EncID.new <- paste("14", msh.bb$EncID.new, sep = "")
msh.bb[, V1 := NULL]
fwrite(msh.bb, "H:/GEMINI/Data/MSH/BB/msh.bb.nophi.csv")
msh <- readg(msh, bb)
apply(msh, 2, function(x)sum(is.na(x)))
# ------------------------ sbk data --------------------------------------------
sbk <- fread("R:/GEMINI/_RESTORE/SBK/Transfers/sbk.transfers.MRN_only_link.nophi.csv")
sbk[,V1:=NULL]
er <- readg(sbk, sbk.er.nophi)
dad <- readg(sbk, dad)
sum(duplicated(dad$EncID.new))
sum(duplicated(er$EncID.new))
er[EncID.new%in%er[duplicated(EncID.new), EncID.new]] -> check
sbktime <- merge(unique(er[,.(Triage.Date, Triage.Time, EncID.new)]),
                 dad[,.(Admit.Date, Admit.Time, Discharge.Date,
                        Discharge.Time, EncID.new)], 
                 by = "EncID.new", all.y = T)
sbktime[is.na(Triage.Date), ':='(
  Triage.Date = Admit.Date,
  Triage.Time = Admit.Time
)]
sbk[, EncID.new:=paste("12", EncID.new, sep = "")]
sbk[,':='(Issue.Date=dmy(Issue.Date))]

sbk <- merge(sbk, sbktime, by = "EncID.new",
             all.x = T)
sbk[ymd_hms(paste(Issue.Date, Issue.Time))<=ymd_hm(paste(Discharge.Date, Discharge.Time))&
      ymd_hms(paste(Issue.Date, Issue.Time))>=ymd_hm(paste(Triage.Date, Triage.Time))]


sbk[ymd(Issue.Date)<=ymd(Discharge.Date)&
      ymd(Issue.Date)>=ymd(Triage.Date)] -> sbk.bb
sbk.bb[, ':='(Triage.Date = NULL,
              Triage.Time = NULL,
              Admit.Date = NULL,
              Admit.Time = NULL,
              Discharge.Date = NULL,
              Discharge.Time = NULL)]

fwrite(sbk.bb, "H:/GEMINI/Data/SBK/BB/sbk.bb.csv")

sbk.bb <- readg(sbk, bb)


# ------------------------------ fix sinai bb time -----------------------------
msh.bb <- readg(msh, bb)
time.convert <- function(x){
  ifelse(str_sub(x, -2,-1)=="PM"&as.numeric(str_sub(x, 1, 2))<12, 
         paste(as.numeric(str_sub(x, 1, 2)) + 12, str_sub(x, 3, 5), sep = ""),
         ifelse((str_sub(x, -2,-1)=="AM"&as.numeric(str_sub(x, 1, 2))==12),
                paste(as.numeric(str_sub(x, 1, 2)) - 12, str_sub(x, 3, 5), sep = ""),
                paste(as.numeric(str_sub(x, 1, 2)), str_sub(x, 3, 5), sep = "")))
}
msh.bb$TIME.new <- time.convert(msh.bb$TIME)


fwrite(msh.bb, "H:/GEMINI/Data/MSH/BB/msh.bb.nophi.csv")
