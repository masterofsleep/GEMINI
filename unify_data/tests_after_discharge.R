# ---------- check proportion of tests that are after discharge ----------------

library(gemini)
lib.pa()

#----------------------------- radiology ---------------------------------------
smh.rad <- readg(smh, rad)
sbk.rad <- readg(sbk, rad.csv)
uhn.rad <- rbind(
  readg(uhn, rad_ip),
  readg(uhn, rad_er))

tests.after.discharge <-function(dat, var.dt, time.fun){
  dat <- merge(dat, dad[,.(EncID.new, Discharge.Date, Discharge.Time)],
               by = "EncID.new")
  dat$timediff <- as.numeric(ymd_hm(paste(dat$Discharge.Date, dat$Discharge.Time)) - 
    time.fun(dat[,var.dt]))/3600
  sum(dat$timediff < 0, na.rm = T)
}

tests.after.discharge(smh.rad, "proc_dtime", ymd_h)

dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad <- dad[ymd(Discharge.Date) <= ymd("2015-03-31")]
dad$EncID.new <- as.character(dad$EncID.new)
smh.rad <- merge(smh.rad, dad[,.(EncID.new, Discharge.Date, Discharge.Time)],
                 by = "EncID.new")
sbk.rad <- merge(sbk.rad, dad[,.(EncID.new, Discharge.Date, Discharge.Time)],
                 by = "EncID.new")
uhn.rad <- merge(uhn.rad, dad[,.(EncID.new, Discharge.Date, Discharge.Time)],
                 by = "EncID.new")

smh.rad[, timediff := as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time)) - ymd_h(proc_dtime))/3600]
smh.rad[, timediff.ord := as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time)) - ymd_h(ord_for_dtime))/3600]

summary(smh.rad$timediff)

sum(smh.rad$timediff<0)
sum(smb)

# sbk
sbk.rad[, timediff := as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time)) - ymd_hms(Performed.DtTm))/3600]
sbk.rad[, timediff2 := as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time)) - ymd_hms(Ordered.DtTm))/3600]
sum(sbk.rad$timediff < 0, na.rm = T)
sum(sbk.rad$timediff2 < 0, na.rm = T)
sum(sbk.rad$timediff > sbk.rad$timediff2, na.rm = T)

# uhn
uhn.rad[, timediff := as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time)) - mdy_hm(ScanStartDateTime))/3600]
uhn.rad[, timediff2 := as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time)) - mdy_hm(OrderDateTime))/3600]
sum(uhn.rad$timediff<0)
sum(uhn.rad$timediff2<0)

