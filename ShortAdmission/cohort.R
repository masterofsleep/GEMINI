#==================== Short Admission ==========================================
#cohort selection
rm(list = ls())
library(gemini)
lib.pa()
#find cohort
adm <- readg(gim, adm)
smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
msh.adm <- readg(msh, adm)
table(smh.adm$Admitting.Service)
table(sbk.adm$Admitting.Service)
table(uhn.adm$Admitting.Service)



smh.er <- readg(smh.er, .er.nophi)
sbk.er <- readg(sbk.er, .er.nophi,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn.er <- readg(uhn.er, .er.nophi,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh.er <- readg(msh, er.nophi)

smh.adm <- smh.adm[startsWith(Admitting.Service, "TM")&EncID.new%in%smh.er$EncID.new]
sbk.adm <- sbk.adm[Admitting.Service == "GM"&EncID.new%in%sbk.er$EncID.new]
uhn.adm <- uhn.adm[Admitting.Service=="GIM"&EncID.new%in%uhn.er$EncID.new]

cohort.enc <- c(smh.adm$EncID.new, sbk.adm$EncID.new, uhn.adm$EncID.new)



#define short admission
smh.dad <- readg(smh, dad)
sbk.dad <- readg(sbk, dad)
uhn.dad <- readg(uhn, dad)

smh.dad$los <- as.numeric(ymd_hm(paste(smh.dad$Discharge.Date,smh.dad$Discharge.Time, sep = " "))-
  ymd_hm(paste(smh.dad$Admit.Date,smh.dad$Admit.Time, sep = " ")))
sbk.dad$los <- as.numeric(ymd_hm(paste(sbk.dad$Discharge.Date,sbk.dad$Discharge.Time, sep = " "))-
                            ymd_hm(paste(sbk.dad$Admit.Date,sbk.dad$Admit.Time, sep = " ")))/3600
uhn.dad$los <- as.numeric(ymd_hm(paste(uhn.dad$Discharge.Date,uhn.dad$Discharge.Time, sep = " "))-
                            ymd_hm(paste(uhn.dad$Admit.Date,uhn.dad$Admit.Time, sep = " ")))/60

dad <- rbind(smh.dad[,.(EncID.new, los, Discharge.Disposition, 
                             ymd_hm(paste(smh.dad$Admit.Date,smh.dad$Admit.Time, sep = " ")))],
                  sbk.dad[,.(EncID.new, los, Discharge.Disposition,
                             ymd_hm(paste(sbk.dad$Admit.Date,sbk.dad$Admit.Time, sep = " ")))],
                  uhn.dad[,.(EncID.new, los, Discharge.Disposition,
                             ymd_hm(paste(uhn.dad$Admit.Date,uhn.dad$Admit.Time, sep = " ")))])
names(dad)[4] <- "Admiting.Date.Time"
shortadm <- dad[los <= 72]
names(shortadm)[4] <- "Admiting.Date.Time"
table(shortadm$Discharge.Disposition, useNA = "ifany")


smh.scu <- readg(smh, scu)
sbk.scu <- readg(sbk, scu)
uhn.scu <- readg(uhn, scu)

scuadmit <- rbind(smh.scu[,.(EncID.new, mdy_hm(paste(SCU.Admit.Date,SCU.Admit.Time, sep = " ")))],
                  sbk.scu[!is.na(SCU.Admit.Date)&!is.na(EncID.new), 
                          .(EncID.new, mdy_h(paste(SCU.Admit.Date, SCU.Admit.Time, sep = " ")))],
                  uhn.scu[,.(EncID.new, ymd_hm(paste(SCU.Admit.Date,SCU.Admit.Time, sep = " ")))])
rm(smh.scu, sbk.scu, uhn.scu)
names(scuadmit)[2] <- "SCU.Admit.Date.Time"                          
scuadmit <- merge(scuadmit, dad[,.(EncID.new, Admiting.Date.Time)], 
                  by = "EncID.new", all.x = T)
scuadmit$Time.Before.ICU <- as.numeric(scuadmit$SCU.Admit.Date.Time - scuadmit$Admiting.Date.Time)/3600
##some patients have same time in admission and admission to SCU
To.ICU.within.72 <- scuadmit[Time.Before.ICU<=72&Time.Before.ICU>=0,]

shortadm <- shortadm[!shortadm$EncID.new%in%To.ICU.within.72$EncID.new]




##table of discharge disposition
#  1 to acute care inpatient institution
#  2 to continuing care
#  3 to other
#  4 to home or a home setting with support service
#  5 home with no support service from an external agency required
#  6 sign out (agains medical advice & absent without leave)
#  7 dead
#  8 Cadaveric donor admitted for organ
#  9 stillbirth
#  12 Patient who does not return from a pass

#no death and no transfer to another inpatient facility in first 48 hours
# disposition != 3, 7

shortadm <- shortadm[!Discharge.Disposition%in%c(3,7)]

write.csv(shortadm, "H:/GEMINI/Results/Shortadm/shortadm.csv", row.names = F)

#check xfer files
smh.xfer <- readg(smh,xfer)
sbk.xfer <- readg(sbk, xfer)
uhn.xfer <- readg(uhn, xfer)

xfer <- rbind(smh.xfer[,.(EncID.new, Unit.Code, ymd_hm(paste(Date.Check.in, Time.Check.in)))],
              sbk.xfer[,.(EncID.new, Unit.Code, ymd_hm(paste(Date.Check.in, Time.Check.in)))],
              uhn.xfer[,.(EncID.new, Unit.Code, ymd_hm(paste(Date.Check.in, Time.Check.in)))])
names(xfer)[3] <- "Checkin.Date.Time"
xfer <- merge(xfer, dad[,.(EncID.new, Admiting.Date.Time)], 
              by = "EncID.new", all.x = T)
xfer[,Time.to.Transfer := as.numeric(Checkin.Date.Time-Admiting.Date.Time)/3600]
To.ICU.within.48.xfer <- xfer[Time.to.Transfer<=48&Time.to.Transfer>=0&Unit.Code=="1"]
To.Palli.within.48.xfer <- xfer[Time.to.Transfer<=48&Time.to.Transfer>=0&Unit.Code=="7"]
sum(To.ICU.within.48$EncID.new%in%To.ICU.within.48.xfer$EncID.new)



check <- 
  xfer[xfer$EncID.new%in%To.ICU.within.48$EncID.new[!To.ICU.within.48$EncID.new%in%To.ICU.within.48.xfer$EncID.new]]

sum(shortadm$EncID.new%in%To.ICU.within.48.xfer$EncID.new)
sum(shortadm$EncID.new%in%To.Palli.within.48.xfer$EncID.new)

shortadm <- shortadm[!EncID.new%in%To.ICU.within.48.xfer$EncID.new]
shortadm <- shortadm[!EncID.new%in%To.Palli.within.48.xfer$EncID.new]







cohort <- rbind(smh.dad[EncID.new%in%cohort.enc, InstitutionFrom.Type, 
                        .(EncID.new, Age, Gender,InstitutionFrom, Admit.Date, 
                          Admit.Time, Discharge.Date, Discharge.Time, 
                          MostResponsible.DocterCode, los)],
                sbk.dad[EncID.new%in%cohort.enc, InstitutionFrom.Type, 
                        .(EncID.new, Age, Gender,InstitutionFrom, Admit.Date, 
                          Admit.Time, Discharge.Date, Discharge.Time, 
                          MostResponsible.DocterCode, los)],
                uhn.dad[EncID.new%in%cohort.enc, InstitutionFrom.Type, 
                        .(EncID.new, Age, Gender,InstitutionFrom, Admit.Date, 
                          Admit.Time, Discharge.Date, Discharge.Time, 
                          MostResponsible.DocterCode, los)])
cohort$Short.Admission <- cohort$EncID.new%in%shortadm$EncID.new
fwrite(cohort, "H:/GEMINI/Results/Shortadm/cohort.csv")



cohort <- fread("H:/GEMINI/Results/Shortadm/cohort.csv")

