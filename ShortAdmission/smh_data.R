# -------------- Short admission cohort St. Michael's Hospital -----------------
library(gemini)
lib.pa()

dad.all <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")
smh.adm <- readg(smh, adm)
smh.er <- readg(smh.er, .er.nophi)
smh.inc <- smh.adm[startsWith(Admitting.Service, "TM")&EncID.new%in%smh.er$EncID.new&
                     EncID.new%in%dad.all$EncID.new, EncID.new]

swdh("SMH/CIHI")
cihi <- list.files()
for(i in cihi){
  dat <- fread(i)
  dat <- dat[EncID.new%in%smh.inc]
  fwrite(dat, paste("R:/GEMINI-Short_Admission_Project/Data/CIHI/", i, sep = ""))
}

rad <- readg(smh, rad, dt = T)
rad_before_adm <- rad[ymd_h(proc_dtime)<=ymd_hm(paste(Admit.Date, Admit.Time))]
# confirmed that all the admit/discharge date time in rad file are the same as dad
# rad[ymd_h(proc_dtime)<=dmy_hm(paste(ADMITDATE, ADMITTIME))]
rad_before_adm[,':='(ADMITDATE = NULL,
                      ADMITTIME = NULL,
                      DISCHARGEDATE = NULL,
                      DISCHARGETIME = NULL)]

fwrite(rad_before_adm, "R:/GEMINI-Short_Admission_Project/Data/Clinical/radiology.csv")


swdh("GEMINI/Lab")
lab <- list.files()
for(i in lab){
  print(i)
  dat <- fread(i)
  dat <- dat[EncID.new%in%smh.inc]
  dat <- merge(dat, dad.all[,.(EncID.new, Admit.Date, Admit.Time, 
                               Discharge.Date, Discharge.Time)], 
               all.x = T, all.y = F)
  dat[EncID.new=="11390574"&Collection.DtTm=="2060-02-22 00:07:00",
      Collection.DtTm := ymd_hms("2060-02-22 00:07:00")]
  print(sum(is.na(ymd_hms(dat$Collection.DtTm))))
  print(dat[is.na(ymd_hms(Collection.DtTm)), .(EncID.new, Collection.DtTm)])
  dat[EncID.new=="11390574"&Collection.DtTm=="2060-02-22 00:07:00",
      Collection.DtTm := ymd_hms("2060-02-22 00:07:00")]
  dat <- dat[ymd_hms(Collection.DtTm) <= ymd_hm(paste(Admit.Date, Admit.Time))]
  dat$Collection.DtTm <- as.character(ymd_hms(dat$Collection.DtTm))
  fwrite(dat, paste("R:/GEMINI-Short_Admission_Project/Data/Clinical/", i, sep = ""))
}

# dat[is.na(ymd_hms(Collection.DtTm) )]
# dat[EncID.new=="11390574"]
# smh.lab <- readg(smh, lab)
# smh.lab[EncID.new=="11390574"] -> check
# check[Test.ID=="ALB"]
# i = lab[1]
# ymd_hms("2060-02-22 00:07:00")
# smh.lab[Collection.DtTm=="2060-02-22 00:07:00"]


smh.phar <- readg(smh, phar, dt = T)
smh.phar <- smh.phar[EncID.new%in%smh.inc]
smh.phar <- smh.phar[ymd(start_date)==ymd(Admit.Date)]
smh.phar[,':='(ADMITDATE = NULL,
                     ADMIT.TIME = NULL,
                     DISCHARGE.DATE = NULL,
                     DISCHARGE.TIME = NULL)]
fwrite(smh.phar, "R:/GEMINI-Short_Admission_Project/Data/Clinical/medication_sameday.csv")
