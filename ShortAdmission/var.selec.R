#==================== Short Admission ==========================================
# variable selections
rm(list = ls())
library(gemini)
lib.pa()

cohort <- fread("H:/GEMINI/Results/Shortadm/cohort.csv")

# charlson comorbidity index 
smh.ip <- readg(smh, ip_diag)
sbk.ip  <- readg(sbk, ip_diag)
uhn.ip <- readg(uhn, ip_diag)

ip.diag <- rbind(smh.ip, sbk.ip[,c(1:4), with = F], uhn.ip)
rm(smh.ip, sbk.ip, uhn.ip)
library(icd)
cmd <- icd10_comorbid_quan_deyo(ip.diag, visit_name = "EncID.new",
                                icd_name = "Diagnosis.Code")
cci <- data.frame(icd_charlson_from_comorbid(cmd, visit_name = "EncID.new", 
                                  scoring_system = "charlson"))
colnames(cci)[1] <- "Charlson.Comorbidity.Index"
cci$EncID.new <- row.names(cci)

cohort <- merge(cohort, cci, by = "EncID.new", all.x = T, all.y = F)

table(cci$Charlson.Comorbidity.Index)
cohort$Charlson.Comorbidity.Index[cohort$Charlson.Comorbidity.Index>=3] <- "3+"


cohort$Admit.Day <- wday(ymd(cohort$Admit.Date), label = T)
cohort$Admit.Time.Cat <- ifelse(hm(cohort$Admit.Time)>=hm("8:00")&hm(cohort$Admit.Time)<hm("17:00"),
                                1, 
                                ifelse(hm(cohort$Admit.Time)>=hm("17:00")&hm(cohort$Admit.Time)<hm("24:00"),
                                       2, 3))

fwrite(cohort, "H:/GEMINI/Results/Shortadm/cohort1.csv", row.names = F)



cohort <- fread("H:/GEMINI/Results/Shortadm/cohort1.csv")
# merge var from gim.er
smh.er <- readg(smh.er, .er.nophi)
sbk.er <- readg(sbk.er, .er.nophi,
                colClasses = list(character = c("NACRSRegistrationNumber",
                                                "EncID.new")))
uhn.er <- readg(uhn.er, .er.nophi,
                colClasses = list(character = c("NACRSRegistrationNumber",
                                                "EncID.new")))
uhn.er$Blood.Transfusion.in.ED[is.na(uhn.er$Blood.Transfusion.in.ED)] <- "N"
sum(duplicated(sbk.er))
sum(duplicated(uhn.er))
sum(duplicated(smh.er))

table(smh.er$Admit.via.Ambulance)
table(sbk.er$Admit.via.Ambulance)
table(uhn.er$Admit.via.Ambulance)
table(smh.er$Blood.Transfusion.in.ED)
table(sbk.er$Blood.Transfusion.in.ED)
table(uhn.er$Blood.Transfusion.in.ED)
table(smh.er$Institution.Number, useNA = "always")
table(sbk.er$Institution.Number, useNA = "always")
table(uhn.er$Institution.Number, useNA = "always")
er1 <- rbind(smh.er[,.(EncID.new, Admit.via.Ambulance, Triage.Level, 
                       Blood.Transfusion.in.ED, Institution.Number,
                       Date.of.Physician.Initial.Assessment,
                       Time.of.Physician.Initial.Assessment,
                       Triage.Date, Triage.Time, 
                       Disposition.Date, Disposition.Time,
                       Date.Left.ER, Time.Left.ER)],
             sbk.er[,.(EncID.new, Admit.via.Ambulance, Triage.Level, 
                       Blood.Transfusion.in.ED, Institution.Number,
                       Date.of.Physician.Initial.Assessment,
                       Time.of.Physician.Initial.Assessment,
                       Triage.Date, Triage.Time, 
                       Disposition.Date, Disposition.Time,
                       Date.Left.ER, Time.Left.ER)],
             uhn.er[,.(EncID.new, Admit.via.Ambulance, Triage.Level, 
                       Blood.Transfusion.in.ED, Institution.Number,
                       Date.of.Physician.Initial.Assessment,
                       Time.of.Physician.Initial.Assessment,
                       Triage.Date, Triage.Time, 
                       Disposition.Date, Disposition.Time,
                       Date.Left.ER, Time.Left.ER)])

#duplicated rows in ER file
#sbk caused by multiple complaints
#uhn caused by blood components


er1 <- er1[!duplicated(er1)]
er1$Time.to.Physician.Initial.Assessment <- as.numeric(ymd_hm(paste(er1$Date.of.Physician.Initial.Assessment,
                                                                    er1$Time.of.Physician.Initial.Assessment, 
                                                      sep = " "))-
                                                        ymd_hm(paste(er1$Triage.Date, er1$Triage.Time, sep = " ")))/3600

er1$ER.LOS <- as.numeric(ymd_hm(paste(er1$Disposition.Date,
                                      er1$Disposition.Time, 
                                      sep = " "))-
                           ymd_hm(paste(er1$Triage.Date, er1$Triage.Time, sep = " ")))/3600


er1$Physical.Time.in.ER <- as.numeric(ymd_hm(paste(er1$Date.Left.ER, er1$Time.Left.ER, sep = " ")) - 
                                        ymd_hm(paste(er1$Triage.Date, er1$Triage.Time, sep = " ")))/3600
sum(duplicated(er1))
cohort$EncID.new <- as.character(cohort$EncID.new)



cohort <- merge(cohort, er1[,.(EncID.new, Admit.via.Ambulance,Triage.Level,
                               Institution.Number,
                               Time.to.Physician.Initial.Assessment,
                               ER.LOS, Physical.Time.in.ER, Blood.Transfusion.in.ED)], 
                by = "EncID.new", all.x = T, all.y =F)
#fill the 4 empty institution numbers ( All from smh)
cohort$Institution.Number[is.na(cohort$Institution.Number)] <- "53985"

cohort$Admit.via.Ambulance[cohort$Admit.via.Ambulance%in%c("A","C","G")] <- "Y"
fwrite(cohort, "H:/GEMINI/Results/Shortadm/cohort2.csv")




cohort <- fread("H:/GEMINI/Results/Shortadm/cohort2.csv")

apply(cohort, MARGIN = 2, FUN = function(x)sum(is.na(x)))
cohort$Short.Admission <- as.numeric(cohort$Short.Admission)
cci <- readg(gim, cci)
cohort[, Charlson.Comorbidity.Index:=NULL]
cohort <- merge(cohort, cci, by = "EncID.new", all.x =T, all.y = F)
cohort$Charlson.Comorbidity.Index[cohort$Charlson.Comorbidity.Index>=3] <- "3+"

fwrite(cohort, "H:/GEMINI/Results/Shortadm/cohort2.csv")
fit <- glm(Short.Admission ~ Age + Gender + Charlson.Comorbidity.Index + 
             Admit.Day + Admit.Time.Cat + Admit.via.Ambulance + factor(Triage.Level) + 
             Institution.Number + ER.LOS + Physical.Time.in.ER + 
             Blood.Transfusion.in.ED, data = cohort, family = binomial)

summary(fit)
sum(cohort$Short.Admission)

table(cohort$Charlson.Comorbidity.Index)

## =========== Institution From ================================================
cohort <- fread("H:/GEMINI/Results/Shortadm/cohort2.csv")
library(readxl)
inst.code <- read_excel("H:/GEMINI/Results/Shortadm/2015 Master Numbering System Code Book.xlsx", sheet = 2)

instfrom <- sapply(str_sub(cohort$InstitutionFrom,-4,-1),  function(y) sub('^0+([1-9])', '\\1', y))

table(instfrom)
unique(instfrom)[!unique(instfrom)%in% inst.code$`MASTER NUMBER`]

cohort$InstitutionFrom <- instfrom
cohort[!instfrom%in%inst.code$`MASTER NUMBER`&!is.na(instfrom)] -> check
table(str_sub(check$EncID.new,1,2), check$InstitutionFrom)






## ---------------- Find Hospital Level Factors --------------------------------
smh.adm <- readg(smh, adm, key = "EncID.new")
sbk.adm <- readg(sbk, adm, key = "EncID.new")
uhn.adm <- readg(uhn, adm, key = "EncID.new")
smh.dad <- readg(smh, dad, key = "EncID.new")
sbk.dad <- readg(sbk, dad, key = "EncID.new")
uhn.dad <- readg(uhn, dad, key = "EncID.new")
#fill in the empty smh 
smh.gim <- smh.dad[EncID.new%in%
                     smh.adm[startsWith(Admitting.Service, "TM"), EncID.new],
                   .(EncID.new, Admit.Date, Admit.Time)]
sbk.gim <- sbk.dad[EncID.new%in%
                     sbk.adm[startsWith(Admitting.Service, "GM"), EncID.new],
                   .(EncID.new, Admit.Date, Admit.Time)]
uhn.gim <- uhn.dad[EncID.new%in%
                     uhn.adm[startsWith(Admitting.Service, "GIM"), EncID.new],
                   .(EncID.new, Admit.Date, Admit.Time)]
# instnum.uhn <- fread("H:/GEMINI/Data/UHN/CIHI/missing ED info with Visit Facility_processed.csv",
#                      colClasses = list(character = c("Visit Number","EncID.new")))
# uhn.er <- readg(uhn.er, .er.nophi,
#                 colClasses = list(character = c("NACRSRegistrationNumber",
#                                                 "EncID.new")))
# instnum.uhn$Institution.Number <- ifelse(instnum.uhn$`Visit Facility`==
#                                            "TORONTO GENERAL & PRINCESS MARGARET HOSPITALS PROD",
#                                          "54265", "54266")
# instnum.uhn$EncID.new <- paste("13", instnum.uhn$EncID.new, sep = "")
# uhn.all.inst.num <- rbind(uhn.er[,.(EncID.new, Institution.Number)],
#                           instnum.uhn[,.(EncID.new, Institution.Number)])
# 
# uhn.all.inst.num <- uhn.all.inst.num[!duplicated(uhn.all.inst.num)]

institution <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv",
                     select = c("EncID.new", "Institution.Number"))
tgh.gim <- uhn.gim[EncID.new%in%institution[Institution.Number=="uhn-general", EncID.new]]
twh.gim <- uhn.gim[EncID.new%in%institution[Institution.Number=="uhn-western", EncID.new]]

GIM.adm.in12 <- function(x, y = smh.gim){
  x.dttm <- ymd_hm(x)
  y$dttm <- ymd_hm(paste(y$Admit.Date, y$Admit.Time))
  sapply(x.dttm, function(k)sum(y$dttm<=k &y$dttm>=k-hours(12)))
}

sbk.gim %>% arrange(ymd_hm(paste(sbk.gim$Admit.Date, sbk.gim$Admit.Time))) %>% 
  data.table -> sbk.gim

tgh.gim %>% arrange(ymd_hm(paste(tgh.gim$Admit.Date, tgh.gim$Admit.Time))) %>% 
  data.table -> tgh.gim

cohort[str_sub(EncID.new,1,2)=="11", 
       GIM.adm.in12:= GIM.adm.in12(paste(Admit.Date, Admit.Time))]
cohort[str_sub(EncID.new,1,2)=="12", 
       GIM.adm.in12:= GIM.adm.in12(paste(Admit.Date, Admit.Time), y = sbk.gim)]
cohort[Institution.Number=="54265", 
       GIM.adm.in12:= GIM.adm.in12(paste(Admit.Date, Admit.Time), y = tgh.gim)]
cohort[Institution.Number=="54266", 
       GIM.adm.in12:= GIM.adm.in12(paste(Admit.Date, Admit.Time), y = twh.gim)]



fwrite(cohort, "H:/GEMINI/Results/Shortadm/cohort3.csv", row.names = F)
cohort <- fread("H:/GEMINI/Results/Shortadm/cohort3.csv")







# -------------------- Dec 16 Radiology var selection --------------------------
smh.ct <- readg(smh, ct)
smh.mri <- readg(smh, mri)
smh.us <- readg(smh.us, us)
smh.xray <- readg(smh, xray)

##any xray before time of admission
smh.xray.er <- smh.xray[dmy_hm(paste(ADMITDATE, ADMITTIME))>=ymd_h(proc_dtime), EncID.new]
smh.xray.in48 <- smh.xray[dmy_hm(paste(ADMITDATE, ADMITTIME))<=ymd_h(ord_for_dtime)&
                            dmy_hm(paste(ADMITDATE, ADMITTIME)) + hours(48)>=ymd_h(ord_for_dtime), EncID.new] 

##any ct before admission
smh.ct.er <- smh.ct[dmy_hm(paste(ADMITDATE, ADMITTIME))>=ymd_h(proc_dtime), EncID.new]
smh.ct.in48 <- smh.ct[dmy_hm(paste(ADMITDATE, ADMITTIME))<=ymd_h(ord_for_dtime)&
                        dmy_hm(paste(ADMITDATE, ADMITTIME)) + hours(48)>=ymd_h(ord_for_dtime), EncID.new] 

##any us before admission
smh.us.er <- smh.us[dmy_hm(paste(ADMITDATE, ADMITTIME))>=ymd_h(proc_dtime), EncID.new]
smh.us.in48 <- smh.us[dmy_hm(paste(ADMITDATE, ADMITTIME))<=ymd_h(ord_for_dtime)&
                      dmy_hm(paste(ADMITDATE, ADMITTIME)) + hours(48)>=ymd_h(ord_for_dtime), EncID.new] 

##any mri before admission
smh.mri.er <- smh.mri[dmy_hm(paste(ADMITDATE, ADMITTIME))>=ymd_h(proc_dtime), EncID.new]
smh.mri.in48 <- smh.mri[dmy_hm(paste(ADMITDATE, ADMITTIME))<=ymd_h(ord_for_dtime)&
                      dmy_hm(paste(ADMITDATE, ADMITTIME)) + hours(48)>=ymd_h(ord_for_dtime), EncID.new] 






#sbk
sbk.rad <- readg(sbk, rad.csv)
map.sbk <- read_excel("H:/GEMINI/mapping/rad.freq.table_AV.xlsx", sheet = 1)
sum(sbk.rad$Test.Name%in%map.sbk$Test.Name)
sbk.rad <- merge(sbk.rad, map.sbk[,c("Test.Name", "Test.Type")], by = "Test.Name",
                 all.x = T, all.y = F)
sbk.rad <- merge(sbk.rad, cohort[,.(EncID.new, Admit.Date, Admit.Time)], 
                 by = "EncID.new") %>% data.table

sbk.rad.er <- sbk.rad[ymd_hms(Performed.DtTm)<=ymd_hm(paste(Admit.Date, Admit.Time))]
sbk.rad.in48 <- sbk.rad[ymd_hms(Ordered.DtTm)>=ymd_hm(paste(Admit.Date, Admit.Time))&
                          ymd_hms(Ordered.DtTm)<=ymd_hm(paste(Admit.Date, Admit.Time))+hours(48)]
sbk.us.er <- sbk.rad.er[Test.Type==2, EncID.new]
sbk.xray.er <- sbk.rad.er[Test.Type==1, EncID.new]
sbk.ct.er <- sbk.rad.er[Test.Type==3, EncID.new]
sbk.mri.er <- sbk.rad.er[Test.Type==4, EncID.new]

sbk.us.in48 <- sbk.rad.in48[Test.Type==2, EncID.new]
sbk.xray.in48 <- sbk.rad.in48[Test.Type==1, EncID.new]
sbk.ct.in48 <- sbk.rad.in48[Test.Type==3, EncID.new]
sbk.mri.in48 <- sbk.rad.in48[Test.Type==4, EncID.new]


#uhn
# UHN 
uhnip <- readg(UHN, rad_ip)
uhner <- readg(UHN, rad_er)
uhn.rad <- rbind(uhnip, uhner)

# uhn.freq <- data.table(table(uhn$ProcedureName))
# fwrite(uhn.freq, "H:/GEMINI/Results/Shortadm/rad.uhn.freq.csv", showProgress = T)
# setwd("H:/GEMINI/Results/Shortadm")
# uhn[startsWith(ProcedureName, "Int"), .N,by = ProcedureName]%>%write.csv("rad.uhn.int.csv", row.names= F)
# uhn[startsWith(ProcedureName, "PET"), .N,by = ProcedureName]%>%write.csv("rad.uhn.PET.csv", row.names= F)
# uhn[startsWith(ProcedureName, "BI"), .N,by = ProcedureName]%>%write.csv("rad.uhn.BI.csv", row.names= F)
# uhn[startsWith(ProcedureName, "Gas"), .N,by = ProcedureName]%>%write.csv("rad.uhn.Gas.csv", row.names= F)
# uhn[startsWith(ProcedureName, "Gen"), .N,by = ProcedureName]%>%write.csv("rad.uhn.Gen.csv", row.names= F)

uhn.rad <- merge(uhn.rad, cohort[,.(EncID.new, Admit.Date, Admit.Time)], 
                 by = "EncID.new")
uhn.rad.er <- uhn.rad[mdy_hm(ScanStartDateTime)<=ymd_hm(paste(Admit.Date, Admit.Time))]
uhn.rad.in48 <- uhn.rad[mdy_hm(OrderDateTime)>=ymd_hm(paste(Admit.Date, Admit.Time))&
                          mdy_hm(OrderDateTime)<=ymd_hm(paste(Admit.Date, Admit.Time))+hours(48)]
uhn.ct.er <- uhn.rad.er[str_sub(ProcedureName,1,2) =="CT", EncID.new]
uhn.us.er <- uhn.rad.er[str_sub(ProcedureName,1,2) =="US", EncID.new]
uhn.xray.er <- uhn.rad.er[str_sub(ProcedureName,1,2) =="XR", EncID.new]
uhn.mri.er <- uhn.rad.er[str_sub(ProcedureName,1,3) =="MRI", EncID.new]

uhn.ct.in48 <- uhn.rad.er[str_sub(ProcedureName,1,2) =="CT", EncID.new]
uhn.us.in48 <- uhn.rad.er[str_sub(ProcedureName,1,2) =="US",  EncID.new]
uhn.xray.in48 <- uhn.rad.er[str_sub(ProcedureName,1,2) =="XR", EncID.new]
uhn.mri.in48 <- uhn.rad.er[str_sub(ProcedureName,1,3) =="MRI",EncID.new]


ct.er <- c(smh.ct.er, sbk.ct.er, uhn.ct.er)
ct.in48 <- c(smh.ct.in48, sbk.ct.in48, uhn.ct.in48)

us.er <- c(smh.us.er, sbk.us.er, uhn.us.er)
us.in48 <- c(smh.us.in48, sbk.us.in48, uhn.us.in48)

mri.er <- c(smh.mri.er, sbk.mri.er, uhn.mri.er)
mri.in48 <- c(smh.mri.in48, sbk.mri.in48, uhn.mri.in48)

xray.er <- c(smh.xray.er, sbk.xray.er, uhn.xray.er)
xray.in48 <- c(smh.xray.in48, sbk.xray.in48, uhn.xray.in48)

cohort[,':='(ct.er = EncID.new%in%ct.er,
             xray.er= EncID.new%in%xray.er,
             us.er = EncID.new%in%us.er,
             mri.er = EncID.new%in%mri.er,
             ct.in48 = EncID.new%in%ct.in48,
             xray.in48 = EncID.new%in%xray.in48,
             mri.in48 = EncID.new%in%mri.in48)]

fwrite(cohort, "H:/GEMINI/Results/Shortadm/cohort5.csv")


# ----------------------- Pharmacy variables -----------------------------------

smh.phar <- readg(smh, phar ,dt = T)
sbk.phar <- readg(sbk, phar, dt = T)
uhn.phar <- readg(uhn, uhn.phar, dt = T)
table(smh.phar$route[str_detect(smh.phar$route, "IV")])
table(smh.phar$ord_frequency[str_detect(smh.phar$ord_frequency, "IV")])
table(sbk.phar$route)


# #phar.freq.tables 
# smh.route <- table(smh.phar$route) %>% data.table %>% 
#   fwrite("H:/GEMINI/Results/Shortadm/route.smh.csv")
# sbk.route <- table(sbk.phar$route) %>% data.table %>%
#   fwrite("H:/GEMINI/Results/Shortadm/route.sbk.csv")
# uhn.route <- table(uhn.phar$Route_Code) %>% data.table %>%
#   fwrite("H:/GEMINI/Results/Shortadm/route.uhn.csv")
# smh.phar <- merge(smh.phar,  cohort[,.(EncID.new, Admit.Date, Admit.Time)], 
#                   by = "EncID.new")

smh.ivmed.in48 <- smh.phar[
  ymd_hm(paste(start_date, start_time))>=
    ymd_hm(paste(Admit.Date, Admit.Time))&
  ymd_hm(paste(start_date, start_time))<=
    ymd_hm(paste(Admit.Date, Admit.Time))+ hours(48)&
    route%in%c("IV", "IV/IM", "IV/SC/IM", "SUBCUT/IV")]


sbk.ivmed.in48 <- sbk.phar[
  route%in%c("IV", "INTRAVEN")&
    mdy_hms(paste(start_date, start_time))>=
    ymd_hm(paste(Admit.Date, Admit.Time))&
    mdy_hms(paste(start_date, start_time))<=
    ymd_hm(paste(Admit.Date, Admit.Time)) + hours(48)]

uhn.ivmed.in48 <- uhn.phar[
    Route_Code%in%c("IV", "IVMED", "IM/IV", "SC/IV")&
    dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))>=
    ymd_hm(paste(Admit.Date, Admit.Time))&
    dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))<=
    ymd_hm(paste(Admit.Date, Admit.Time)) + hours(48)
]
table(smh.ivmed.in48$route)
table(sbk.ivmed.in48$route)
table(uhn.ivmed.in48$Route_Code)

cohort <- fread("H:/GEMINI/Results/Shortadm/cohort5.csv")
cohort[, ivmed.in.48 := EncID.new%in%
         c(smh.ivmed.in48$EncID.new, 
           sbk.ivmed.in48$EncID.new, 
           uhn.ivmed.in48$EncID.new)]

din.shortadm <- readxl::read_excel("H:/GEMINI/Protocols/shortadmission/ID_DIN_LIST_1.xls", sheet = 2)
din.shortadm$din <- gsub("(?<![0-9])0+", "", din.shortadm$din, perl = TRUE)
smh.inc <- smh.phar[din%in% din.shortadm$din &
                    ymd_hm(paste(start_date, start_time))>=
                    mdy_hms(paste(ADMITDATE, ADMIT.TIME))&
                    ymd_hm(paste(start_date, start_time))<=
                    mdy_hms(paste(ADMITDATE, ADMIT.TIME)) + hours(48)]
sbk.inc <- sbk.phar[ndc_din%in%din.shortadm$din&
                    mdy_hms(paste(start_date, start_time))>=
                    ymd_hm(paste(Admit.Date, Admit.Time))&
                    mdy_hms(paste(start_date, start_time))<=
                    ymd_hm(paste(Admit.Date, Admit.Time)) + hours(48)]
uhn.inc <- uhn.phar[DIN%in%din.shortadm$din&
                    dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))>=
                    ymd_hm(paste(Admit.Date, Admit.Time))&
                    dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))<=
                    ymd_hm(paste(Admit.Date, Admit.Time)) + hours(24)]
cohort[, iv.med.could.be.sub.for.oral := 
         EncID.new%in%c(smh.inc$EncID.new,
                        sbk.inc$EncID.new,
                        uhn.inc$EncID.new)]

fwrite(cohort, "H:/GEMINI/Results/Shortadm/cohort6.csv")


# ----------------------------- diag variables ---------------------------------
cohort <- fread("H:/GEMINI/Results/Shortadm/cohort6.csv")
ip.diag <- readg(gim, ip_diag, key = "EncID.new")
er.diag <- readg(gim, er_diag, key = "EncID.new")

ip.mrd <- ip.diag[Diagnosis.Type=="M", 
                  .(str_sub(Diagnosis.Code,1,3), EncID.new)]
names(ip.mrd)[1] <- "Diagnosis.Code"

er.mrd <- er.diag[ER.Diagnosis.Type=="M", 
                  .(str_sub(ER.Diagnosis.Code,1,3), EncID.new)]
names(er.mrd)[1] <- "ER.Diagnosis.Code"
mrd <- merge(ip.mrd, er.mrd, all.x = T)
mrd <- unique(mrd)
mrd[EncID.new%in%mrd[duplicated(EncID.new), EncID.new]]

cohort <- merge(cohort, mrd, by = "EncID.new", all.x = T, all.y = F)
fwrite(cohort, "H:/GEMINI/Results/Shortadm/cohort7.csv")


#---------------------------- Intervention variables ---------------------------
ip.int <- readg(gim, ip_int)
er.int <- readg(gim, er_int)
