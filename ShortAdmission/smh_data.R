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

# full lab 
smh.lab <- readg(smh, labs, dt = T)
smh.lab <- smh.lab[,.(EncID.new, Test.Name, Test.ID, 
                      Order.Number = Order., Result.Value,
                      Result.Unit, Reference.Range, 
                      Collection.DtTm = ymd_hms(Collection.DtTm), 
                      Admit.Date, Admit.Time,
                      Discharge.Date, Discharge.Time)]
smh.lab <- smh.lab[EncID.new%in%smh.inc]
fwrite(smh.lab, "R:/GEMINI-Short_Admission_Project/Data/Clinical/lab_all.csv")




# medication
smh.phar <- readg(smh, phar, dt = T)
smh.phar <- smh.phar[EncID.new%in%smh.inc]
smh.phar <- smh.phar[ymd(start_date)==ymd(Admit.Date)]
smh.phar[,':='(ADMITDATE = NULL,
                     ADMIT.TIME = NULL,
                     DISCHARGE.DATE = NULL,
                     DISCHARGE.TIME = NULL)]
fwrite(smh.phar, "R:/GEMINI-Short_Admission_Project/Data/Clinical/medication_sameday.csv")


# physician
gim.phy <- readg(gim, all.phy)
smh.phy <- gim.phy[str_sub(EncID.new, 1, 2)=="11"&EncID.new%in%smh.inc]
smh.phy[, ':='(mrp.code = NULL,
               adm.code = NULL,
               dis.code = NULL)]
fwrite(smh.phy, "R:/GEMINI-Short_Admission_Project/Data/CIHI/physicians.csv")


# bronchoscopy and endoscopy
smh.er.int <- readg(smh, er_int)
int.map <- readxl::read_excel("H:/GEMINI/Results/DesignPaper/int.freq_AV.xlsx", 
                              sheet = 2, col_names = F)
endo.code <- int.map$X2[4:171]

smh.er.int_endo <- smh.er.int[Occurrence.Type%in%endo.code]
smh.ip_int <- readg(smh, ip_int)
smh.ip_int[Intervention.Code%in%endo.code]



setwd("R:/GEMINI-Short_Admission_Project/Data/CIHI")
list.files()
smh.inc <- fread("smh.adm.nophi.csv")
adm <- readg(gim, adm)
smh.hash <- adm[EncID.new%in%smh.inc$EncID.new, .(EncID.new, Hash)]
fwrite(smh.hash, "smh.hash.new.csv")


smh.vitals <- readg(smh, vitals, dt = T)
smh.vitals_inc <- smh.vitals[mdy_hm(Collected.DT)<=ymd_hm(paste(Admit.Date, Admit.Time))]
smh.vitals_inc <- smh.vitals_inc[EncID.new%in%smh.inc$EncID.new]

fwrite(smh.vitals_inc, "R:/GEMINI-Short_Admission_Project/Data/Clinical/vitals.csv")



# readmission variable
adm <- readg(gim, adm)
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")
dad[, Hash:=NULL]
dad <- merge(dad, adm[,.(Hash, EncID.new)], by = "EncID.new")
dad[is.na(Hash), Hash:=""]
sum(smh.inc%in%dad$EncID.new)
dad <- dad %>% arrange(Hash, ymd_hm(paste(Admit.Date, Admit.Time))) %>% data.table
time.since.last.visit <- as.numeric(ymd_hm(dad[2:nrow(dad), 
                                                       paste(Admit.Date, Admit.Time)]) - 
                                   ymd_hm(dad[1:(nrow(dad)-1), 
                                                         paste(Discharge.Date, Discharge.Time)]) )/3600/24

dad$time.since.last.visit<- c(NA, time.since.last.visit)

dad[, .(Hash, time.since.last.visit)] -> check

sum(smh.inc%in%dad$EncID.new)

# dad[, readmission := time.since.last.visit<=30]
# dad[!duplicated(Hash), readmission:=F]
# dad[ymd(Admit.Date)<=ymd("2010-04-30")|Hash=="", readmission:=NA]
# 
dad[!duplicated(Hash), time.since.last.visit:=Inf]
dad[ymd(Admit.Date)<=ymd("2010-04-30")|Hash=="", time.since.last.visit:=NA]

dad[, readmission := time.since.last.visit<=30]

smh.read <- dad[EncID.new%in%smh.inc, .(EncID.new, Hash, Admit.Date, Admit.Time, Discharge.Date, Discharge.Time,
                                        time.since.last.visit, readmission)]
table(smh.read$readmission, useNA = "ifany")
sum(smh.read$Hash=="")

fwrite(smh.read, "R:/GEMINI-Short_Admission_Project/Data/CIHI/smh.read.csv")


# check dup er
smh.er <- fread("R:/GEMINI-Short_Admission_Project/Data/CIHI/smh.er.nophi.csv")
sum(duplicated(smh.er$EncID.new))

sum(is.na(smh.read$readmission))
ddply(dad, ~Institution.Number, summarize,
      sum(Hash==""),
      sum(read.in.30, na.rm =T))
table(smh.read$readmission)
