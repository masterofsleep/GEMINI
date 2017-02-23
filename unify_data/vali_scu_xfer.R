#------------------------ Dec 16 2016 ------------------------------------------
#------------- Validate xfer files and scu files -------------------------------

rm(list = ls())
library(gemini)
lib.pa()
smh.xf <- readg(smh, ip_xfer)[Unit.Code =="1"]
sbk.xf <- readg(sbk, ip_xfer)[Unit.Code =="1"]
uhn.xf <- readg(uhn, ip_xfer)[Unit.Code =="1"]

smh.scu <- readg(smh, ip_scu)
sbk.scu <- readg(sbk, ip_scu)
uhn.scu <- readg(uhn, ip_scu)

sbk.scu <- sbk.scu[SCU.Unit.Number!="99"]

#------------------------- smh -------------------------------------------------

smh.xf$scu.dttm <- ymd_hm(paste(smh.xf$Date.Check.in, smh.xf$Time.Check.in))
smh.scu$scu.dttm <- mdy_hm(paste(smh.scu$SCU.Admit.Date, smh.scu$SCU.Admit.Time))
sum(duplicated(smh.scu$EncID.new))
compare.smh <- merge(smh.xf, smh.scu, by = c("EncID.new","scu.dttm"),all.x = T,
                     all.y = T)
apply(compare.smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
compare.smh[is.na(Unit.y)]
compare.smh[!duplicated(EncID.new)]%>%apply(MARGIN = 2, FUN = function(x)sum(is.na(x)))
compare.smh[duplicated(EncID.new)]%>%apply(MARGIN = 2, FUN = function(x)sum(is.na(x)))


##----------- sbk -----------------------------------------
head(sbk.xf)
head(sbk.scu)
sbk.xf$scu.dttm <- ymd_hm(paste(sbk.xf$Date.Check.in, sbk.xf$Time.Check.in))
sbk.scu$scu.dttm <- mdy_h(paste(sbk.scu$SCU.Admit.Date, sbk.scu$SCU.Admit.Time))
sum(duplicated(sbk.scu$EncID.new))
compare.sbk <- merge(sbk.xf, sbk.scu, by = c("EncID.new","scu.dttm"),all.x = T,
                     all.y = T)
apply(compare.sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
compare.sbk[is.na(Unit.y)]

compare.sbk[!duplicated(EncID.new)]%>%apply(MARGIN = 2, FUN = function(x)sum(is.na(x)))
compare.sbk[duplicated(EncID.new)]%>%apply(MARGIN = 2, FUN = function(x)sum(is.na(x)))

compare.sbk[duplicated(compare.sbk[,.(EncID.new, scu.dttm)])]

sum(duplicated(sbk.scu[,.(EncID.new, SCU.Admit.Date, SCU.Admit.Time)]))
sum(duplicated(sbk.xf[,.(EncID.new, Date.Check.in, Time.Check.in)]))

sum(duplicated(sbk.scu[,.(EncID.new, SCU.Admit.Date)]))
sbk.scu[duplicated(sbk.scu[,.(EncID.new, SCU.Admit.Date)])|duplicated(sbk.scu[,.(EncID.new, SCU.Admit.Date)], fromLast = T),] -> check
sbk.xf[duplicated(sbk.xf[,.(EncID.new, Date.Check.in)])|duplicated(sbk.xf[,.(EncID.new, Date.Check.in)], fromLast = T),]%>%
  arrange(EncID.new, Date.Check.in)
##--------------- uhn ---------------------------------------
head(uhn.xf)
head(uhn.scu)
uhn.xf$scu.dttm <- ymd_hm(paste(uhn.xf$Date.Check.in, uhn.xf$Time.Check.in))
uhn.scu$scu.dttm <- ymd_hm(paste(uhn.scu$SCU.Admit.Date, uhn.scu$SCU.Admit.Time))

compare.uhn <- merge(uhn.xf, uhn.scu, by = c("EncID.new","scu.dttm"),all.x = T,
                     all.y = T)
apply(compare.uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))
compare.uhn[is.na(Unit.y)]
compare.uhn[!duplicated(EncID.new)]%>%apply(MARGIN = 2, FUN = function(x)sum(is.na(x)))
compare.uhn[duplicated(EncID.new)]%>%apply(MARGIN = 2, FUN = function(x)sum(is.na(x)))


compare.uhn[SCU.Unit.Number=="20"]
compare.uhn[duplicated(compare.uhn[,.(EncID.new, scu.dttm)])]
compare.uhn[EncID.new=="13194409"]
compare.uhn[EncID.new=="13356229"]

sum(uhn.scu$EncID.new%in%uhn.xf$EncID.new)
uhn.scu[!EncID.new%in%uhn.xf$EncID.new]
sum(uhn.xf$EncID.new%in%uhn.scu$EncID.new)
uhn.xf[!EncID.new%in%uhn.scu$EncID.new]


uhn.scu[duplicated(uhn.scu[,.(EncID.new, SCU.Admit.Date)])|duplicated(uhn.scu[,.(EncID.new, SCU.Admit.Date)], fromLast = T),] -> check
uhn.xf[duplicated(uhn.xf[,.(EncID.new, Date.Check.in)])|duplicated(uhn.xf[,.(EncID.new, Date.Check.in)], fromLast = T),]%>%
  arrange(EncID.new, Date.Check.in) -> check2


uhn.scu[!EncID.new%in%uhn.xf$EncID.new] -> check











#======================== Validate with EncID only =============================
#=========================== Jan 12 2017========================================
smh.xf <- readg(smh, ip_xfer)[Unit.Code =="1"]
sbk.xf <- readg(sbk, ip_xfer)[Unit.Code %in%c("1", "2")]
uhn.xf <- readg(uhn, ip_xfer)[Unit.Code %in%c("1", "2")]

smh.scu <- readg(smh, ip_scu)
sbk.scu <- readg(sbk, ip_scu)
uhn.scu <- readg(uhn, ip_scu)
sbk.scu <- sbk.scu[SCU.Unit.Number!="99"]

#smh
length(unique(smh.scu$EncID.new))
length(unique(smh.xf$EncID.new))
sum(smh.xf$EncID.new%in%smh.scu$EncID.new)
sum(smh.scu$EncID.new%in%smh.xf$EncID.new)

#sbk
length(unique(sbk.scu$EncID.new))
length(unique(sbk.xf$EncID.new))
length(intersect(sbk.scu$EncID.new, sbk.xf$EncID.new))
length(setdiff(sbk.xf$EncID.new, sbk.scu$EncID.new))
length(setdiff(sbk.scu$EncID.new, sbk.xf$EncID.new))



#uhn
length(unique(uhn.scu$EncID.new))
length(unique(uhn.xf$EncID.new))
length(intersect(uhn.scu$EncID.new, uhn.xf$EncID.new))
length(setdiff(uhn.xf$EncID.new, uhn.scu$EncID.new))
length(setdiff(uhn.scu$EncID.new, uhn.xf$EncID.new))



#sbk and uhn, which codes are wrong?


sbk.scu[!EncID.new%in%sbk.xf[, EncID.new]] -> check
sbk.xf[EncID.new%in%sbk.scu$EncID.new]
sbk.scu$date <- mdy(sbk.scu$SCU.Admit.Date)
sbk.xf$date <- ymd(sbk.xf$Date.Check.in)
sbk.compare <- merge(sbk.scu, sbk.xf, by = c("EncID.new", "date"))
sbk.compare[,c("date", "SCU.Death", "SCU.Discharge.Date","SCU.Discharge.Time","GCS"):=NULL]
sbk.compare.bytime <- sbk.compare[hm(paste(SCU.Admit.Time, ":00"))==hm(Time.Check.in)]
table(sbk.compare.bytime$Unit, sbk.compare.bytime$Unit.Code)
uhn.scu[!EncID.new%in%uhn.xf$EncID.new]




uhn.scu[!EncID.new%in%uhn.xf[, EncID.new]] -> check
uhn.xf[EncID.new%in%uhn.scu$EncID.new]
uhn.scu$date <- ymd(uhn.scu$SCU.Admit.Date)
uhn.xf$date <- ymd(uhn.xf$Date.Check.in)
uhn.compare <- merge(uhn.scu, uhn.xf, by = c("EncID.new", "date"))
uhn.compare[,c("date", "SCU.Death", "SCU.Discharge.Date","SCU.Discharge.Time","GCS",
               "Date.Check.out", "Time.Check.out"):=NULL]
uhn.compare.bytime <- uhn.compare[hm(SCU.Admit.Time)==hm(Time.Check.in)]
table(uhn.compare.bytime$Unit, uhn.compare.bytime$Unit.Code)
uhn.scu[!EncID.new%in%uhn.xf$EncID.new]


uhn.xf[Unit=="EMERG"]



# --------------------------- Jan 17 2016 --------------------------------------
# --------------- Check all that are out of xfer code 1, 2 ---------------------
rm(list = ls())
library(gemini)
lib.pa()
sbk.xf <- readg(sbk, ip_xfer)#[Unit.Code %in%c("1", "2")]
uhn.xf <- readg(uhn, ip_xfer)#[Unit.Code %in%c("1", "2")]

sbk.scu <- readg(sbk, ip_scu)[SCU.Unit.Number!="99"]
uhn.scu <- readg(uhn, ip_scu)

sbk.scu$date <- mdy(sbk.scu$SCU.Admit.Date)
sbk.xf$date <- ymd(sbk.xf$Date.Check.in)
sbk.compare.sameday <- merge(sbk.scu[!EncID.new%in%sbk.xf[Unit.Code%in%c("1", "2"), EncID.new]], 
                     sbk.xf, 
                     by = c("EncID.new", "date"),
                     all.x = T, all.y = F)
fwrite(sbk.compare.sameday, "H:/GEMINI/Results/Check/sbk.compare.scu.xf.sameday.csv")
sbk.compare <- merge(sbk.scu[!EncID.new%in%sbk.xf[Unit.Code%in%c("1", "2"), EncID.new]], 
                             sbk.xf, 
                             by = c("EncID.new"),
                             all.x = T, all.y = F)
fwrite(sbk.compare, "H:/GEMINI/Results/Check/sbk.compare.scu.xf.csv")

uhn.scu$date <- ymd(uhn.scu$SCU.Admit.Date)
uhn.xf$date <- ymd(uhn.xf$Date.Check.in)
uhn.compare.sameday <- merge(uhn.scu[!EncID.new%in%uhn.xf[Unit.Code%in%c("1", "2"), EncID.new]],
                             uhn.xf,
                     by = c("EncID.new", "date"), all.x = T, all.y = F)
fwrite(uhn.compare.sameday, "H:/GEMINI/Results/Check/uhn.compare.scu.xf.sameday.csv")
uhn.compare <- merge(uhn.scu[!EncID.new%in%uhn.xf[Unit.Code%in%c("1", "2"), EncID.new]], 
                      uhn.xf, 
                     by = c("EncID.new"), all.x = T, all.y = F)
fwrite(uhn.compare, "H:/GEMINI/Results/Check/uhn.compare.scu.xf.csv")


sum(uhn.scu$EncID.new%in%uhn.xf$EncID.new)



