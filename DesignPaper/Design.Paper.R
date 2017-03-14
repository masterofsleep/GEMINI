library(gemini)
lib.pa()
#================== Design Paper Statistics ====================================
#========================= Jan 10 2017 =========================================

#=================== Jan 9 2017 GEMINI Design Paper ============================
smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
thp.adm <- readg(thp, adm)
length(unique(c(smh.adm$Hash, sbk.adm$Hash, uhn.adm$Hash, thp.adm$Hash)))       # number of unique patients in cohort
smh.adm$Institution.Number <- "smh"
sbk.adm$Institution.Number <- "sbk"
msh.adm <- msh.dad[,.(EncID.new, Institution.Number="msh")]
thp.adm$Institution.Number <- thp.adm$Institution
adm <- rbind(smh.adm[,.(EncID.new, Institution.Number)],
      sbk.adm[,.(EncID.new, Institution.Number)],
      uhn.adm[,.(EncID.new, Institution.Number)],
      msh.adm[,.(EncID.new, Institution.Number)],
      thp.adm[,.(EncID.new, Institution.Number)])

smh.dad <- readg(smh, dad)
sbk.dad <- readg(sbk, dad)
uhn.dad <- readg(uhn, dad)
msh.dad <- readg(msh, dad)
thp.dad <- readg(thp, dad)

dad <- rbind(smh.dad[,.(EncID.new, Age, Gender, InstitutionFrom.Type,
                        Discharge.Disposition, 
                        Admit.Date, Admit.Time,
                        Discharge.Date, Discharge.Time, 
                        Number.of.ALC.Days)],
             sbk.dad[,.(EncID.new, Age, Gender, InstitutionFrom.Type,
                        Discharge.Disposition, 
                        Admit.Date, Admit.Time,
                        Discharge.Date, Discharge.Time, 
                        Number.of.ALC.Days)], 
             uhn.dad[,.(EncID.new, Age, Gender, InstitutionFrom.Type,
                        Discharge.Disposition, 
                        Admit.Date, Admit.Time,
                        Discharge.Date, Discharge.Time, 
                        Number.of.ALC.Days)], 
             msh.dad[,.(EncID.new, Age, Gender, InstitutionFrom.Type,
                        Discharge.Disposition, 
                        Admit.Date, Admit.Time,
                        Discharge.Date, Discharge.Time, 
                        Number.of.ALC.Days)], 
             thp.dad[,.(EncID.new, Age, Gender, InstitutionFrom.Type,
                        Discharge.Disposition, 
                        Admit.Date, Admit.Time,
                        Discharge.Date, Discharge.Time, 
                        Number.of.ALC.Days)])
median(dad$Age);IQR(dad$Age)                                                    # median and IQR of age: 73, 28
dad$age.cat <- cut(dad$Age, breaks = c(0, 50,60,70,80,90,200), 
                   labels = c("<=50", "50-60", "60-70","70-80",
                              "80-90", ">90"), 
                   right = T, include.lowest = T)
cbind(table(dad$age.cat, useNA = "always"), table(dad$age.cat, useNA = "always")/139151)
table(dad$Gender);table(dad$Gender)/139151                                                        # %of female
inst.code <- readxl::read_excel("H:/GEMINI/Results/Shortadm/InstitutionFrom_Type_GEMINI.xlsx",
                                sheet = 1)
dad <- merge(dad, inst.code[,c("Code","GEMINI Classifier")], by.x = "InstitutionFrom.Type",
             by.y = "Code", all.x = T, all.y = F)
dad <- merge(dad, adm, by = "EncID.new")

dad[,InstitutionFrom.Type:= `GEMINI Classifier`]
dad[,`GEMINI Classifier`:=NULL]
dad[InstitutionFrom.Type%in%c(3,5,7), InstitutionFrom.Type:=5]
dad[is.na(InstitutionFrom.Type), InstitutionFrom.Type:=6]
table(dad$InstitutionFrom.Type, dad$Institution.Number, useNA = "always")
inst.from.by.year <-table(dad$InstitutionFrom.Type, year(ymd(dad$Admit.Date)), useNA = "always")
fwrite(inst.from.by.year, "H:/GEMINI/Results/DesignPaper/instfrom.byyear.csv")
cbind(table(dad$InstitutionFrom.Type, useNA = "always"),
  table(dad$InstitutionFrom.Type, useNA = "always")/139151)                        # institution from type 

dad[Discharge.Disposition==5, Discharge.Disposition:=4]
dad[Discharge.Disposition%in%c(3, 8, 9 ,12), Discharge.Disposition:=3]
cbind(table(dad$Discharge.Disposition, useNA = "always"),                            # discharge disposition
table(dad$Discharge.Disposition, useNA = "always")/139151)


dad$LoS <- as.numeric((ymd_hm(paste(dad$Discharge.Date, dad$Discharge.Time)) - 
                         ymd_hm(paste(dad$Admit.Date, dad$Admit.Time)))/(3600*24))# Length - of - stay (calculate by time and convert to days)
median(dad$LoS);IQR(dad$LoS);mean(dad$LoS)

dad$Number.of.ALC.Days <- str_replace(dad$Number.of.ALC.Days, ",", "")
dad$LOS.without.ALC <- dad$LoS - as.numeric(dad$Number.of.ALC.Days)
median(dad$LOS.without.ALC);IQR(dad$LOS.without.ALC)                            # Length - of - Stay without ALC days

fwrite(dad, "H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")

#---------------------- IP Diag ------------------------------------------------
rm(list = ls())
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
ip.diag <- readg(gim, ip_diag, 
                 colClasses = list(character = c("EncID.new")))
#fwrite(ip.diag, "H:/GEMINI/Data/GEMINI/gim.ip_diag.csv")
n.comorb <- table(ip.diag$EncID.new)                                          # number of comorbidities
n.comorb <- data.table(n.comorb)
dad[, n.comorb:=NULL]
names(n.comorb) <- c("EncID.new", "n.comorb")
median(n.comorb$n.comorb);IQR(n.comorb$n.comorb)
dad$EncID.new <- as.character(dad$EncID.new)
dad <- merge(dad, n.comorb, by = "EncID.new")

library(icd)
                                                                                # charlson index
cmd <- icd10_comorbid_quan_deyo(ip.diag, visit_name = "EncID.new",
                                icd_name = "Diagnosis.Code")
cci <- data.frame(icd_charlson_from_comorbid(cmd, visit_name = "EncID.new", 
                                             scoring_system = "charlson"))
colnames(cci)[1] <- "Charlson.Comorbidity.Index"
cci$EncID.new <- row.names(cci)

dad <- merge(dad, cci, by = "EncID.new")
dad$Charlson.Comorbidity.Index[dad$Charlson.Comorbidity.Index>=3]<-"3+"

fwrite(dad, "H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")

cbind(table(dad$Charlson.Comorbidity.Index), table(dad$Charlson.Comorbidity.Index)/139151)
# Top Diagnosis Codes
# ip.diag[Diagnosis.Type=="M", str_sub(Diagnosis.Code, 1, 3)] %>% table %>% 
#   data.table %>% arrange(desc(N)) -> mrd.freq
# names(mrd.freq) <- c("Diagnosis", "N")
# mrd.freq$prop <- round(mrd.freq$N/139151 * 100, 2)
# fwrite(mrd.freq[1:20,], "H:/GEMINI/Results/DesignPaper/diag20.csv")

# dad <- merge(dad, ip.diag[Diagnosis.Type=="M", 
#                           .(EncID.new, str_sub(Diagnosis.Code, 1, 3))],
#              by = "EncID.new")
# names(dad)[13] <- "Diag.Code"
 diag.names <- readxl::read_excel("H:/GEMINI/Results/Diabetes/MRD.freqtable.xlsx")
# 
# mrd.by.beddays <- dad[, sum(LoS), by = Diag.Code]
# names(mrd.by.beddays)[2] <- "Bed.Days"
# mrd.by.beddays <- merge(mrd.by.beddays, 
#                         diag.names[,c("Diagnosis.Code", "Diagnosis")],
#                         by.x = "Diag.Code", by.y = "Diagnosis.Code")
# mrd.by.beddays$prop <- mrd.by.beddays$Bed.Days/sum(mrd.by.beddays$Bed.Days) *100
# mrd.by.beddays <- arrange(mrd.by.beddays, desc(Bed.Days))
# fwrite(mrd.by.beddays, "H:/GEMINI/Results/DesignPaper/mrd.by.beddays.csv")

diag.top10 <- table(str_sub(ip.diag$Diagnosis.Code, 1, 3)) %>% data.table %>%
  arrange(desc(N))
diag.top10$prop <- diag.top10$N/139151 *100  
diag.names <- readxl::read_excel("H:/GEMINI/Results/Diabetes/MRD.freqtable.xlsx")

diag.top10 <- merge(diag.top10, diag.names[,c("Diagnosis.Code", "Diagnosis")],
                    by.x = "V1", by.y = "Diagnosis.Code") %>% arrange(desc(N))
fwrite(diag.top10, "H:/GEMINI/Results/DesignPaper/diag.top10.csv")


dad <- merge(dad, ip.diag[Diagnosis.Type=="M",.(EncID.new, str_sub(Diagnosis.Code,1,3))],
             by = "EncID.new")
fwrite(dad, "H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")




# --------------------- scu admission ------------------------------------------

library(gemini)
lib.pa()
smh.xf <- readg(smh, ip_xfer)[Unit.Code =="1"]
sbk.xf <- readg(sbk, ip_xfer)[Unit.Code %in%c("1", "2")]
uhn.xf <- readg(uhn, ip_xfer)[Unit.Code %in%c("1", "2")]
smh.scu <- readg(smh, ip_scu)
sbk.scu <- readg(sbk, ip_scu)
uhn.scu <- readg(uhn, ip_scu)
msh.scu <- readg(msh, ip_scu)
thp.scu <- readg(thp, ip_scu)

sbk.scu <- sbk.scu[SCU.Unit.Number!="99"]
thp.scu <- thp.scu[SCU.Unit.Number!="99"]

scu.admit <- unique(c(smh.xf$EncID.new, smh.scu$EncID.new,
               sbk.xf$EncID.new, sbk.scu$EncID.new,
               uhn.xf$EncID.new, uhn.scu$EncID.new,
               msh.scu$EncID.new, thp.scu$EncID.new))



dad$SCU.adm <- as.numeric(dad$EncID.new%in%scu.admit)
names(dad)[17] <- "Diag.Code"
fwrite(dad, "H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")

#----------------------- table 3 -----------------------------------------------

dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad$EncID.new <- as.character(dad$EncID.new)

# Find Cost
smh.cmg <- readg(smh, ip_cmg)
sbk.cmg <- readg(sbk, ip_cmg)
uhn.cmg <- readg(uhn, ip_cmg)
msh.cmg <- readg(msh, ip_cmg)
thp.cmg <- readg(thp, ip_cmg)
ip_cmg <- rbind(smh.cmg[,.(EncID.new, RIW.15)],
                sbk.cmg[,.(EncID.new, RIW.15)],
                uhn.cmg[,.(EncID.new, RIW.15)],
                msh.cmg[,.(EncID.new, RIW.15)],
                thp.cmg[,.(EncID.new, RIW.15)])

dad <- merge(dad, ip_cmg, by = "EncID.new",all.x = T)
table(dad$Institution.Number)
dad[Institution.Number=="smh",CPWC:= 6541] 
dad[Institution.Number=="sbk",CPWC:= 5960] 
dad[Institution.Number=="msh",CPWC:= 6397] 
dad[Institution.Number=="C",CPWC:= 4912] 
dad[Institution.Number=="M",CPWC:= 5259] 
dad[Institution.Number=="54265",CPWC:= 6521] 
dad[Institution.Number=="54266",CPWC:= 6521] 
dad$Cost <- as.numeric(dad$RIW.15) * dad$CPWC
dad[is.na(as.numeric(RIW.15))]
summary(dad$Cost)

ip.diag[Diagnosis.Type=="M", str_sub(Diagnosis.Code, 1, 3)] %>% table %>% 
  data.table %>% arrange(desc(N)) -> mrd.freq
names(mrd.freq) <- c("Diagnosis", "N")
mrd.freq$prop <- round(mrd.freq$N/139151 * 100, 2)

#design paper table 3 function
dpt3 <- function(dat){
  age <- paste(median(dat$Age), IQR(dat$Age), sep = ", ")
  fromhome <- paste(sum(dat$InstitutionFrom.Type==6, na.rm = T), " (",
                    round(sum(dat$InstitutionFrom.Type==6, na.rm = T)/dim(dat)[1]*100,digits = 3), 
                    ")", sep = "")
  icu <- paste(sum(dat$SCU.adm), " (",
               round(sum(dat$SCU.adm)/dim(dat)[1]*100,digits = 3), 
               ")", sep = "")
  death <- paste(sum(dat$Discharge.Disposition==7), " (",
                 round(sum(dat$Discharge.Disposition==7)/dim(dat)[1]*100, digits = 3),
                 ")", sep = "")
  comorb <- paste(median(dat$n.comorb), IQR(dat$n.comorb), sep = ", ")
  ALC <- paste(sum(dat$Number.of.ALC.Days>0), " (", 
               round(sum(dat$Number.of.ALC.Days>0)/dim(dat)[1]*100, digits = 3),
               ")", sep = "")
  total.ALC.days <- paste(sum(dat$Number.of.ALC.Days), " (", 
                    round(sum(dat$Number.of.ALC.Days)/sum(dad$Number.of.ALC.Days), digits = 3),
                    ")",sep = "")
  LOS <- paste(round(median(dat$LoS), digits = 3), round(IQR(dat$LoS),digits=3), sep = ", ")
  bed.day <- paste(round(sum(dat$LoS), digits = 3), 
                   round(sum(dat$LoS)/1346470*100, digits = 3), sep = ", ")
  Cost <- paste(median(dat$Cost, na.rm = T), IQR(dat$Cost,na.rm = T), sep = ", ")
  cbind(age, fromhome, icu, death, comorb, ALC, total.ALC.days, LOS, bed.day, Cost)
}

dpt3(dad)
tab3 <- NULL
for(i in diag.names$Diagnosis.Code[1:20]){
  tab3 <- rbind(tab3, dpt3(dad[Diag.Code==i]))
}
fwrite(data.table(tab3), "H:/GEMINI/Results/DesignPaper/table3.csv") 

dpt.new <- function(dat){
  total.los <- round(sum(dat$LoS), 2)
  los.2 <- paste(sum(dat$LoS<2), "(", 
                 round(sum(dat$LoS<2)/dim(dat)[1], digits = 2),")", sep = "" )
  los.25 <- paste(sum(dat$LoS>=2&dat$LoS<5), "(", 
                 round(sum(dat$LoS>=2&dat$LoS<5)/dim(dat)[1], digits = 2),")", sep = "")
  los.59 <- paste(sum(dat$LoS>=5&dat$LoS<9), "(", 
                 round(sum(dat$LoS>=5&dat$LoS<9)/dim(dat)[1], digits = 2),")", sep = "" )
  los.10 <- paste(sum(dat$LoS>=9), "(", 
                 round(sum(dat$LoS>=9)/dim(dat)[1], digits = 2),")", sep = "" )
  cbind(total.los, los.2, los.25, los.59, los.10)
}
dpt.new(dad)
tab.new <- dpt.new(dad)
for(i in diag.names$Diagnosis.Code[1:20]){
  tab.new <- rbind(tab.new, dpt.new(dad[Diag.Code==i]))
}
tab.new <- data.table(tab.new)
names(tab.new) <- c("totol.los", "los <2", "los 2-5", "los 5-9", "los >=9")
fwrite(tab.new, "H:/GEMINI/Results/DesignPaper/table.new.csv") 

#---------------------- Interventions ------------------------------------------
rm(list = ls())

# code to create a csv file for the CCI codes
# cci.code <- readr::read_table("R:/GEMINI/Coding/CIHI/CCI_Code_Eng_Desc_2014_V1_0.txt")
# names(cci.code) <- c("desc1", "desc2")
# cci.code$code <- str_sub(cci.code$desc1, 1, 10)
# cci.code$desc1 <- str_sub(cci.code$desc1, 11, -1)
# cci.code <- cci.code[,c("code", "desc1", "desc2")]
# fwrite(cci.code, "R:/GEMINI/Coding/CIHI/CCI_Code_Eng_Desc_2014_V1_0.csv")

ip.int <- readg(gim, ip_int)
cci.code <- fread("R:/GEMINI/Coding/CIHI/CCI_Code_Eng_Desc_2014_V1_0.csv")
ip.int.freq <- table(ip.int$Intervention.Code) %>% data.table
ip.int.freq <- merge(ip.int.freq, cci.code,
                     by.x = "V1", by.y = "code", all.x = T, all.y = F)
names(ip.int.freq) <- c("Intervention.Code", "Freq", "Desc1", "Desc2")
er.int <- readg(gim, er_int)
er.int.freq <- table(er.int$Occurrence.Type) %>% data.table
er.int.freq <- merge(er.int.freq, cci.code,
                     by.x = "V1", by.y = "code", all.x = T, all.y = F)
names(er.int.freq) <- c("Intervention.Code", "Freq", "Desc1", "Desc2")
int.freq <- table(c(ip.int$Intervention.Code, er.int$Occurrence.Type)) %>% 
  data.table
int.freq <- merge(int.freq, cci.code,
                     by.x = "V1", by.y = "code", all.x = T, all.y = F)
names(int.freq) <- c("Intervention.Code", "Freq", "Desc1", "Desc2")
fwrite(ip.int.freq, "H:/GEMINI/Results/DesignPaper/ip.int.freq.csv")
fwrite(er.int.freq, "H:/GEMINI/Results/DesignPaper/er.int.freq.csv")
fwrite(int.freq, "H:/GEMINI/Results/DesignPaper/int.freq.csv")

cci.code[str_sub(code, 1, 1)=="3"&str_sub(code, 4, 5)=="20", type:="CT"]        # validated, true for all CT
cci.code[str_sub(code, 1, 1)=="3"&str_sub(code, 4, 5)=="40", type:="MRI"]       # validated, true for all MRI
cci.code[str_sub(code, 1, 1)=="3"&str_sub(code, 4, 5)=="10", type:="Xray"]      # validated, true for all
cci.code[str_sub(code, 1, 1)=="3"&str_sub(code, 4, 5)=="30", type:="US"]
table(cci.code$type)

int.ip <- rbind(smh.intip[,.(EncID.new, Intervention.Code)],
                sbk.intip[,.(EncID.new, Intervention.Code)],
                uhn.intip[,.(EncID.new, Intervention.Code)],
                msh.intip[,.(EncID.new, Intervention.Code)],
                thp.intip[,.(EncID.new, Intervention.Code)])
int.ip <- int.ip[!is.na(Intervention.Code)]

int.er <- rbind(smh.inter[,.(EncID.new, Occurrence.Type)],
                sbk.inter[,.(EncID.new, Occurrence.Type)],
                uhn.inter[,.(EncID.new, Occurrence.Type)],
                msh.inter[,.(EncID.new, Occurrence.Type)],
                thp.inter[,.(EncID.new, Occurrence.Type)])
int.er <- int.er[!is.na(Occurrence.Type)]
names(int.er)[2] <- "Intervention.Code"
cci.code[str_sub(code, 1, 1)=="3"&str_sub(code, 4, 5)=="20"]

interv <- rbind(int.ip, int.er)
sum(interv$Intervention.Code%in%cci.code$code)                                  # check if all code from cci list
interv[!Intervention.Code%in%cci.code$code] -> check                            # which ones are not in cci list

interv[str_sub(Intervention.Code, 1, 1)=="3"&str_sub(Intervention.Code, 4, 5)=="20", type:="CT"]
interv[str_sub(Intervention.Code, 1, 1)=="3"&str_sub(Intervention.Code, 4, 5)=="40", type:="MRI"]
interv[str_sub(Intervention.Code, 1, 1)=="3"&str_sub(Intervention.Code, 4, 5)=="10", type:="Xray"]
interv[str_sub(Intervention.Code, 1, 1)=="3"&str_sub(Intervention.Code, 4, 5)=="30", type:="US"]

table(interv$type, useNA = "always")

interv[type=="CT", EncID.new]%>%table %>% as.numeric-> ct.int
ct.int <- c(ct.int, rep(0, 139151-length(ct.int)))
median(ct.int);IQR(ct.int)
quantile(ct.int)

interv[type=="MRI", EncID.new]%>%table %>% as.numeric-> mri.int
mri.int <- c(mri.int, rep(0, 139151-length(mri.int)))
median(mri.int);IQR(mri.int);quantile(mri.int)

interv[type=="Xray", EncID.new]%>%table %>% as.numeric-> xray.int
xray.int <- c(xray.int, rep(0, 139151-length(xray.int)))
median(xray.int);IQR(xray.int);quantile(xray.int)

interv[type=="US", EncID.new]%>%table %>% as.numeric-> us.int
us.int <- c(us.int, rep(0, 139151-length(us.int)))
median(us.int);IQR(us.int);quantile(us.int)


#----------------- Lab (Biochemistry and Hematology) ---------------------------
rm(list = ls())
smh <- readg(smh, corelabs)
sbkip <- readg(sbk, labs_ip)
sbker <- readg(sbk, labs_er)
uhn <- readg(uhn.labs, labs.csv)

lab.table <- table(c(smh$EncID.new, sbker$EncID.new, sbkip$EncID.new, uhn$EncID.new))
lab.table <- c(lab.table, rep(0, 86033-length(lab.table)))
median(lab.table); IQR(lab.table);                                              # median and IQR of number of lab tests










#------------------------ Radiology tests---------------------------------------
#------------------------ St Michael's Hospital --------------------------------

library(gemini)
library(readxl)
lib.pa()
rm(list = ls())

smh.rad <- readg(smh, rad)
smh.ct <- readg(smh, ct)
smh.mri <- readg(smh, mri)
smh.us <- readg(smh.us, us)
smh.xray <- readg(smh, xray)
smh.ir <- fread("H:/GEMINI/Results/DesignPaper/smh.rad.freq.csv")

smh.xray <- smh.xray[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
smh.ct<- smh.ct[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
smh.mri <- smh.mri[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
smh.us <- smh.us[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
smh.ir <- smh.rad[proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]










map.sbk <- readxl::read_excel("H:/GEMINI/Results/DesignPaper/rad.freq.table.new_AV.xlsx", sheet = 1)
sbk.rad <- readg(sbk.rad, rad.csv)

sum(sbk.rad$Test.Name%in%map.sbk$Test.Name)
sbk.rad <- merge(sbk.rad, 
                 map.sbk[,c("Test.Name", "Test.Type", 
                            "Interventional Procedure")], 
                 by = "Test.Name",
                 all.x = T, all.y = F)
sbk.us <- sbk.rad[Test.Type==2&is.na(`Interventional Procedure`), EncID.new]
sbk.xray <- sbk.rad[Test.Type==1&is.na(`Interventional Procedure`), EncID.new]
sbk.ct <- sbk.rad[Test.Type==3&is.na(`Interventional Procedure`), EncID.new]
sbk.mri <- sbk.rad[Test.Type==4&is.na(`Interventional Procedure`), EncID.new]
sbk.ir <- sbk.rad[`Interventional Procedure`==1, EncID.new]

uhn.radip <- readg(uhn, rad_ip)
uhn.rader <- readg(uhn, rad_er)
uhn.rad <- rbind(uhn.radip, uhn.rader)
map.uhn <- 
  readxl::read_excel("H:/GEMINI/Results/DesignPaper/rad.freq.table.new_AV.xlsx", sheet = 2)%>%
  data.table
uhn.ir.names <- map.uhn[Interventional==1,Test.Name]

uhn.ct <- uhn.rad[str_sub(ProcedureName,1,2) =="CT"&
                    !ProcedureName%in%uhn.ir.names, EncID.new]
uhn.us <- uhn.rad[str_sub(ProcedureName,1,2) =="US"&
                    !ProcedureName%in%uhn.ir.names, EncID.new]
uhn.xray <- uhn.rad[str_sub(ProcedureName,1,2) =="XR"&
                      !ProcedureName%in%uhn.ir.names, EncID.new]
uhn.mri <- uhn.rad[str_sub(ProcedureName,1,3) =="MRI"&
                     !ProcedureName%in%uhn.ir.names, EncID.new]
uhn.ir <- uhn.rad[ProcedureName%in%uhn.ir.names, EncID.new]

msh.rader <- readg(msh, rad_er)
msh.radip <- readg(msh, rad_ip)
msh.rad <- rbind(msh.rader, msh.radip)

msh.ct <- msh.rad[str_sub(ProcedureName,1,2) =="CT"&
                    !ProcedureName%in%uhn.ir.names, EncID.new]
msh.us <- msh.rad[str_sub(ProcedureName,1,2) =="US"&
                    !ProcedureName%in%uhn.ir.names, EncID.new]
msh.xray <- msh.rad[str_sub(ProcedureName,1,2) =="XR"&
                      !ProcedureName%in%uhn.ir.names, EncID.new]
msh.mri <- msh.rad[str_sub(ProcedureName,1,3) =="MRI"&
                     !ProcedureName%in%uhn.ir.names, EncID.new]
msh.ir <- msh.rad[ProcedureName%in%uhn.ir.names, EncID.new]


ct.enc <- c(smh.ct, sbk.ct, uhn.ct, msh.ct)
us.enc <- c(smh.us, sbk.us, uhn.us, msh.us)
xray.enc <- c(smh.xray, sbk.xray, uhn.xray, msh.xray)
mri.enc <- c(smh.mri, sbk.mri, uhn.mri, msh.mri)
ir.enc <- c(smh.ir, sbk.ir, uhn.ir, msh.ir)

# 104698 from smh, sbk, uhn, msh
ct.full <- c(ct$N, rep(0, 104698-length(ct$N))) 
median(ct.full); IQR(ct.full) ; length(ct$N);length(ct$N)/104698
us.full <- c(us$N, rep(0, 104698-length(us$N)))
median(us.full); IQR(us.full); length(us$N);length(us$N)/104698
xray.full <- c(xray$N, rep(0, 104698-length(xray$N)))
median(xray.full); IQR(xray.full); length(xray$N);length(xray$N)/104698
mri.full <- c(mri$N, rep(0, 104698-length(mri$N)))
median(mri.full); IQR(mri.full); length(mri$N);length(mri$N)/104698





# ------------------------- echo -----------------------------------------------
rm(list = ls())
smh.echo <- readg(smh, echo)
sbk.echo <- readg(sbk, echo)
uhn.echo <- readg(uhn, echo)

echo.enc <- c(smh.echo[,EncID.new],
              sbk.echo[,EncID.new],
              uhn.echo[,EncID.new])


# -------------- Number of Medication Orders per admission ---------------------
rm(list = ls())
smh.phar <- readg(smh, phar)
sbk.phar <- readg(sbk, phar)
uhn.phar <- readg(uhn.phar, phar.nophi)
msh.phar <- readg(msh, phar)

smh.phar <- smh.phar[!duplicated(paste(din, EncID.new, sep = ""))]
sbk.phar <- sbk.phar[!duplicated(paste(ndc_din, EncID.new, sep = ""))]
uhn.phar <- uhn.phar[!duplicated(paste(DIN, EncID.new, sep = ""))]
msh.phar <- msh.phar[!duplicated(paste(DIN, EncID.new, sep = ""))]
lab.table <- table(c(smh.phar$EncID.new, sbk.phar $EncID.new, uhn.phar$EncID.new,
                     msh.phar$EncID.new)) %>%
  data.table
names(lab.table) <- c("EncID.new", "nmed")
dad <- merge(dad, lab.table, by = "EncID.new", all.x = T)
dad$nmed[is.na(dad$nmed)] <- 0
dad[str_sub(EncID.new,1, 2) =="15", nmed:= NA]
fwrite(dad, "H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dim(lab.table);dim(lab.table)/86033
lab.table <- c(lab.table, rep(0, 86033-length(lab.table)))
median(lab.table); IQR(lab.table); quantile(lab.table)



#-------------- Dialysis and Endoscopy -----------------------------------------
int.map <- readxl::read_excel("H:/GEMINI/Results/DesignPaper/int.freq_AV.xlsx", 
                      sheet = 2, col_names = F)
dia.code <- int.map$X2[1:3]
endo.code <- int.map$X2[4:171]

int.ip <- readg(gim, ip_int)
int.er <- readg(gim, er_int)

names(int.er)[3] <- "Intervention.Code"
interv <- rbind(int.er[,.(EncID.new, Intervention.Code)],
                int.ip[,.(EncID.new, Intervention.Code)])

dia.enc <- interv[Intervention.Code%in%dia.code, EncID.new]
endo.enc <- interv[Intervention.Code%in%endo.code, EncID.new]



# ---------------------- Transfusion -------------------------------------------
smh.trans <- readg(smh, bb)
uhnip.trans <- readg(uhn, txm_ip)
uhner.trans <- readg(uhn, txm_er)
trans.enc <- c(smh.trans$EncID.new, uhnip.trans$EncID.new, uhner.trans$EncID.new)

#---------------------- Create Table 5 -----------------------------------------
diag.names <- readxl::read_excel("H:/GEMINI/Results/Diabetes/MRD.freqtable.xlsx")
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad$EncID.new <- as.character(dad$EncID.new)

# a function to create design paper table 5
dpt5 <- function(dat=dad){
  xray <- paste(sum(dat$EncID.new%in%xray.enc), " (",
                round(sum(dat$EncID.new%in%xray.enc)/
                        dim(dat[str_sub(EncID.new,1,2)%in%
                                  c("11", "12", "13", "14")])[1]*100, digits = 3),
                ")",sep = "")
  ct <- paste(sum(dat$EncID.new%in%ct.enc), " (",
              round(sum(dat$EncID.new%in%ct.enc)/
                      dim(dat[str_sub(EncID.new,1,2)%in%
                                c("11", "12", "13", "14")])[1]*100, digits = 3),
              ")",sep = "")
  mri <- paste(sum(dat$EncID.new%in%mri.enc), " (",
              round(sum(dat$EncID.new%in%mri.enc)/
                      dim(dat[str_sub(EncID.new,1,2)%in%
                                c("11", "12", "13", "14")])[1]*100, digits = 3),
              ")",sep = "")
  us <- paste(sum(dat$EncID.new%in%us.enc), " (",
               round(sum(dat$EncID.new%in%us.enc)/
                       dim(dat[str_sub(EncID.new,1,2)%in%
                                 c("11", "12", "13", "14")])[1]*100, digits = 3),
               ")",sep = "")
  ir <- paste(sum(dat$EncID.new%in%ir.enc), " (",
               round(sum(dat$EncID.new%in%ir.enc)/
                       dim(dat[str_sub(EncID.new,1,2)%in%
                                 c("11", "12", "13", "14")])[1]*100, digits = 3),
               ")",sep = "")
  echo <- paste(sum(dat$EncID.new%in%echo.enc), " (",
               round(sum(dat$EncID.new%in%echo.enc)/
                       dim(dat[str_sub(EncID.new,1,2)%in%c("11","12", "13")])[1]*100, digits = 3),
               ")",sep = "")
  nmed <- paste(median(dat[str_sub(EncID.new,1,2)%in%c("11","12", "13"), nmed]), 
                IQR(dat[str_sub(EncID.new,1,2)%in%c("11","12", "13"),nmed]), sep = ", ")
  trans <- paste(sum(dat$EncID.new%in%trans.enc), " (",
                 round(sum(dat$EncID.new%in%trans.enc)/
                         dim(dat[str_sub(EncID.new,1,2)%in%c("11", "13")])[1]*100, digits = 3),
                 ")",sep = "")
  dialysis <-paste(sum(dat$EncID.new%in%dia.enc), " (",
                   round(sum(dat$EncID.new%in%dia.enc)/dim(dat)[1]*100, digits = 3),
                   ")",sep = "")
  endo <-paste(sum(dat$EncID.new%in%endo.enc), " (",
                   round(sum(dat$EncID.new%in%endo.enc)/dim(dat)[1]*100, digits = 3),
                   ")",sep = "")
  data.table(xray, ct, mri, us, ir, echo, nmed, trans, dialysis, endo)
}


dpt5(dad)
tab5 <- NULL
for(i in diag.names$Diagnosis.Code[1:20]){
  tab5 <- rbind(tab5, dpt5(dad[Diag.Code==i]))
}
tab5 <- data.table(cbind(cbind(paste(diag.names$Diagnosis.Code, diag.names$Diagnosis, sep = " "),
              paste(diag.names$Frequency, " (", diag.names$Prevalence*100,
                    ")", sep = ""))[1:20,], tab5))

fwrite(data.table(tab5), "H:/GEMINI/Results/DesignPaper/table5.csv") 




# ----------------- graph ------------------------------------------------------
library(googleVis)

table(dad$Institution.Number)
dad[Institution.Number=="M", Institution.Number:="THP-M"]
dad[Institution.Number=="C", Institution.Number:="THP-C"]
dad[Institution.Number=="54265", Institution.Number:="TGH"]
dad[Institution.Number=="54266", Institution.Number:="TWH"]


table(dad$InstitutionFrom.Type)
dad[InstitutionFrom.Type==1, Institution.From.Type:= "Acute.Care.Hospital"]
dad[InstitutionFrom.Type==2, Institution.From.Type:= "Nursing.Home"]
dad[InstitutionFrom.Type==4, Institution.From.Type:= "Rehabilitation"]
dad[InstitutionFrom.Type==5, Institution.From.Type:= "Other."]
dad[InstitutionFrom.Type==6, Institution.From.Type:= "Home."]

table(dad$Discharge.Disposition)
dad[Discharge.Disposition==1, Discharge.Disposition1:= "Acute Care Hospital"]
dad[Discharge.Disposition==2, Discharge.Disposition1:= "Continuing Care"]
dad[Discharge.Disposition==3, Discharge.Disposition1:= "Other"]
dad[Discharge.Disposition==4, Discharge.Disposition1:= "Home"]
dad[Discharge.Disposition==6, Discharge.Disposition1:= "Against Medical Advice"]
dad[Discharge.Disposition==7, Discharge.Disposition1:= "Death"]



dis.disp <- data.table(table(dad$Institution.From.Type, dad$Discharge.Disposition1, useNA = "ifany"))
names(dis.disp) <- c("Institution.From", "Discharge.Disposition","N")

Sankey <- gvisSankey(dis.disp, from = "Institution.From", 
                     to = "Discharge.Disposition", 
                     weight = "N")
plot(Sankey)



datSK <- data.frame(From=c(rep("A",3), rep("B", 3)),
                    To=c(rep(c("X", "Y", "Z"),2)),
                    Weight=c(5,7,6,2,9,4))

Sankey <- gvisSankey(datSK, from="From", to="To", weight="Weight")
plot(Sankey)


#--------------------- Feb 1 2017 ----------------------------------------------
rm(list = ls())
library(gemini)
lib.pa()
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad$Institution.Number[dad$Institution.Number=="54265"] <- "uhn-general"
dad$Institution.Number[dad$Institution.Number=="54266"] <- "uhn-western"
dad$Institution.Number[dad$Institution.Number=="M"] <- "thp-m"
dad$Institution.Number[dad$Institution.Number=="C"] <- "thp-c"
dad[ymd(Discharge.Date)>=ymd("2010-04-01")&ymd(Discharge.Date)<ymd("2011-04-01"),
    fiscal.year := "2010"]
dad[ymd(Discharge.Date)>=ymd("2011-04-01")&ymd(Discharge.Date)<ymd("2012-04-01"),
    fiscal.year := "2011"]
dad[ymd(Discharge.Date)>=ymd("2012-04-01")&ymd(Discharge.Date)<ymd("2013-04-01"),
    fiscal.year := "2012"]
dad[ymd(Discharge.Date)>=ymd("2013-04-01")&ymd(Discharge.Date)<ymd("2014-04-01"),
    fiscal.year := "2013"]
dad[ymd(Discharge.Date)>=ymd("2014-04-01")&ymd(Discharge.Date)<ymd("2015-04-01"),
    fiscal.year := "2014"]
dad[ymd(Discharge.Date)>=ymd("2015-04-01")&ymd(Discharge.Date)<ymd("2016-04-01"),
    fiscal.year := "2015"]
table(dad$fiscal.year)
dad[is.na(fiscal.year)]
by(dad$Discharge.Date, dad$fiscal.year, FUN = function(x) range(ymd(x)))
site7 <- xtabs(~Institution.Number + fiscal.year, data = dad)
write.csv(site7, "H:/GEMINI/Results/DesignPaper/year.site7.csv")
site5 <- xtabs(~str_sub(EncID.new, 1, 2) + fiscal.year, data = dad)
write.csv(site5, "H:/GEMINI/Results/DesignPaper/year.site5.csv")




# ----------------------- Feb 6 2017 -------------------------------------------
dad[Institution.Number=="smh", range(ymd(Discharge.Date))]
dad[Institution.Number=="sbk", range(ymd(Discharge.Date))]
dad[Institution.Number=="msh", range(ymd(Discharge.Date))]
dad[Institution.Number%in%c("thp-c", "thp-m"), range(ymd(Discharge.Date))]
dad[Institution.Number%in%c("uhn-western", "uhn-general"), range(ymd(Discharge.Date))]



# ------------------- feb 14 remove extra patients uhn -------------------------
uhn.dad <- fread("H:/GEMINI/DataBackup/Data170214/UHN/CIHI/uhn.ip_dad.nophi.csv")
extra.enc <- uhn.dad[mdy(Discharge.Date)>ymd("20150331"), EncID.new]
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad$Institution.Number[dad$Institution.Number=="54265"] <- "uhn-general"
dad$Institution.Number[dad$Institution.Number=="54266"] <- "uhn-western"
dad$Institution.Number[dad$Institution.Number=="M"] <- "thp-m"
dad$Institution.Number[dad$Institution.Number=="C"] <- "thp-c"

dad <- dad[ymd(Discharge.Date)<=ymd("20150331")]
fwrite(dad, "H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")



# ------------------- feb 15 remove discordant msh patients --------------------
msh <- fread("R:/GEMINI/_RESTORE/MSH/CIHI/msh.adm.nophi.csv")
# compare with dad
dad <- fread("H:/GEMINI/DataBackup/Data170214/MSH/CIHI/msh.ip_dad.nophi.csv")
msh$EncID.new <- paste("14", msh$EncID.new, sep = "")
enc.new <- msh$EncID.new[!msh$EncID.new%in%dad$EncID.new]
enc.old <- dad$EncID.new[!dad$EncID.new%in%msh$EncID.new]
discor <- unique(c(enc.new, enc.old))

dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad <- dad[!EncID.new%in%discor]
fwrite(dad, "H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")







# ---------------------- march 8 create table by fiscal year -------------------
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad[ymd(Discharge.Date)>=ymd("2010-04-01")&ymd(Discharge.Date)<ymd("2011-04-01"),
    fiscal.year := "2010"]
dad[ymd(Discharge.Date)>=ymd("2011-04-01")&ymd(Discharge.Date)<ymd("2012-04-01"),
    fiscal.year := "2011"]
dad[ymd(Discharge.Date)>=ymd("2012-04-01")&ymd(Discharge.Date)<ymd("2013-04-01"),
    fiscal.year := "2012"]
dad[ymd(Discharge.Date)>=ymd("2013-04-01")&ymd(Discharge.Date)<ymd("2014-04-01"),
    fiscal.year := "2013"]
dad[ymd(Discharge.Date)>=ymd("2014-04-01")&ymd(Discharge.Date)<ymd("2015-04-01"),
    fiscal.year := "2014"]
dad[ymd(Discharge.Date)>=ymd("2015-04-01")&ymd(Discharge.Date)<ymd("2016-04-01"),
    fiscal.year := "2015"]
table(dad$fiscal.year, useNA = "ifany")
smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
msh.adm <- readg(msh, adm)
thp.adm <- readg(thp, adm)
hcn <- unique(rbind(smh.adm[, .(Hash, EncID.new)],
             sbk.adm[, .(Hash, EncID.new)],
             uhn.adm[, .(Hash, EncID.new)],
             msh.adm[,.(Hash = newHash, EncID.new)],
             thp.adm[,.(Hash, EncID.new)]))
hcn[EncID.new%in%hcn[duplicated(EncID.new), EncID.new]]
dad$EncID.new <- as.character(dad$EncID.new)
dad <- merge(dad, hcn, by = "EncID.new")

dad[,":="(ct = EncID.new%in%ct.enc,
           mri = EncID.new%in%mri.enc,
           us = EncID.new%in%us.enc,
          endo = EncID.new%in%endo.enc)]
dad[, ctmrius := ct|mri|us]

rad <- c("ct", "mri", "us")
dad[str_sub(EncID.new, 1, 2)=="15", ':='(ct = NA,
                                         mri = NA,
                                         us = NA,
                                         ctmrius = NA)]

dad <- dad %>% arrange(Hash, ymd_hm(paste(Discharge.Date, Discharge.Time)))
dad <- data.table(dad)
time.since.last.admission<- c(NA, as.numeric(dad[2:138485, ymd_hm(paste(Admit.Date, Admit.Time))]-
  dad[1:138484, ymd_hm(paste(Discharge.Date, Discharge.Time))])/(3600*24))
dad$time.since.last.admission <- time.since.last.admission

dad[!duplicated(Hash), time.since.last.admission :=NA]

names(dad)

apply(dad, MARGIN = 2, FUN = function(x) sum(is.na(x)))
ddply(dad, ~fiscal.year, summarize,
      readmission.within.30.days = paste(sum(time.since.last.admission<=30, na.rm = T), ", ", 
                                         round(sum(time.since.last.admission<=30, na.rm = T)/
                                                 length(time.since.last.admission<=30)*100, 1),
                                         sep = ""))
ddply(dad, ~fiscal.year, summarize,
      number.of.hospitalization = length(unique(EncID.new)),
      length.of.stay = paste(round(quantile(LoS)[2:4], 1), collapse = ", "),
      age = paste(round(quantile(Age)[2:4], 1), collapse = ", "),
      number.of.comorbidity = paste(round(quantile(n.comorb)[2:4]), collapse = ", "),
      chars2 = paste(sum(Charlson.Comorbidity.Index%in%c("2", "3+")),
                     round(sum(Charlson.Comorbidity.Index%in%c("2", "3+"))/
                       length(Charlson.Comorbidity.Index)*100, 1), sep = ", "),
      transfer.to.ICU = paste(sum(SCU.adm==1), ", ", 
                  round(sum(SCU.adm==1)/length(SCU.adm)*100, 1),
                  sep = ""),
      death.in.hospital = paste(sum(Discharge.Disposition==7), ", ", 
                                round(sum(Discharge.Disposition==1)/
                                        length(Discharge.Disposition)*100, 1),
                                sep = ""),
      readmission.within.30.days = paste(sum(time.since.last.admission<=30, na.rm = T), ", ", 
                                         round(sum(time.since.last.admission<=30, na.rm = T)/
                                                 length(time.since.last.admission[
                                                   ymd(Admit.Date)>=ymd("2010-05-01")]<=30)*100, 1),
                                         sep = ""),
      ALC = paste(sum(Number.of.ALC.Days>0), ", ", 
                  round(sum(Number.of.ALC.Days>0)/length(Number.of.ALC.Days)*100, 1),
                  sep = ""),
      Cost = paste(round(quantile(Cost, na.rm = T)[2:4], 1), collapse = ", "),
      US.CT.MRI = paste(sum(ctmrius, na.rm = T), ", ", 
                        round(sum(ctmrius, na.rm = T)/sum(!is.na(ctmrius))*100, 1),
                        sep = ""),
      US = paste(sum(us, na.rm = T), ", ", 
                 round(sum(us, na.rm = T)/sum(!is.na(us))*100, 1),
                 sep = ""),
      CT = paste(sum(ct, na.rm = T), ", ", 
                 round(sum(ct, na.rm = T)/sum(!is.na(ct))*100, 1),
                 sep = ""),
      MRI = paste(sum(mri, na.rm = T), ", ", 
                 round(sum(mri, na.rm = T)/sum(!is.na(mri))*100, 1),
                 sep = ""),
      endo.bron = paste(sum(endo), ", ", 
                        round(sum(endo)/length(endo)*100, 1),
                        sep = "")
      ) -> table2
table2 <- data.frame(t(table2))
fwrite(table2, "H:/GEMINI/Results/DesignPaper/table2.new.march9.csv", row.names = T)




dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
diag.freq <- table(dad$Diag.Code) %>% data.table %>% arrange(desc(N))
diag.names <- readxl::read_excel("H:/GEMINI/Results/Diabetes/MRD.freqtable.xlsx") %>% data.table
dad <- merge(dad, diag.names[,.(Diagnosis.Code, Diagnosis)],by.x = "Diag.Code",
             by.y = "Diagnosis.Code")
dad[, top10.diag := ifelse(Diag.Code%in%diag.freq$V1[1:10], Diagnosis, "Other")]
dad[, top20.diag := ifelse(Diag.Code%in%diag.freq$V1[1:20], Diagnosis, "Other")]


library(devtools)
library(treemapify)
top20diag  <- ddply(dad, ~top20.diag, summarize,
      N = length(EncID.new),
      Cost = median(Cost, na.rm = T)) %>% arrange(desc(N))
names(top20diag)[1] <- "Diagnosis"
top20diag$ID <-c(21, 1:20)
top10diag <- ddply(dad, ~top10.diag, summarize,
      N = length(EncID.new),
      Cost = median(Cost, na.rm = T)) %>% arrange(desc(N))
names(top10diag)[1] <- "Diagnosis"
top10diag$ID <- c(11,1:10)



library(treemap)
RColorBrewer::display.brewer.all()
reds <- c(rgb(255/255, 255/255, 255/255, 1),
          rgb(255/255, 204/255, 204/255, 1),
          rgb(255/255, 153/255, 153/255, 1),
          rgb(255/255, 102/255, 102/255, 1),
          rgb(255/255, 51/255, 51/255, 1),
          rgb(255/255, 0/255, 0/255, 1),
          rgb(204/255, 0/255, 0/255, 1),
          rgb(153/255, 0/255, 0/255, 1))
treemap(top20diag,
        algorithm = "pivotSize",
        index = c("Diagnosis"),
        vSize = "N",
        vColor = "Cost",
        sortID = "ID",
        type = "value",
        palette = reds,
        title = "",
        title.legend = "Median Cost per Hospitalization",
        force.print.labels = T,
        mapping = c(2000, 6000, 10000)
)
treemap(top10diag,
        index = c("Diagnosis"),
        vSize = "N",
        vColor = "Cost",
        type = "value",
        sortID = "ID",
        palette = reds,
        title = "Top 10 Diagnosis",
        force.print.labels = T,
        mapping = c(2000, 6000, 10000)
)

ggplot(top10diag, aes(Diagnosis, y = Cost, 
                      fill = Diagnosis)) + 
  geom_bar(stat = "identity", width = top10diag$N/sum(top10diag$N)) + 
  coord_polar("x")


ggplot(top10diag, aes(x = "", y =  ,fill = Diagnosis)) +
  geom_bar(stat = "identity") + 
  coord_polar("x")



library(plotrix)
radial.pie(top10diag$Cost, labels = top10diag$Diagnosis,radlab = TRUE)

pie1<-c(3,6,5,4,7,8,9,1,4)
pie2<-list(0:3,1:6,2:5,1:4,0:7,4:8,2:9,0:1,0:4)
pie3<-sample(10:60,36)
pie4<-list(sort(sample(1:60,8)))
for(sector in 2:36) pie4[[sector]]<-sort(sample(1:60,8))
oldpar<-radial.pie(pie1,labels=LETTERS[1:9])
radial.pie(pie2,labels=letters[2:10])
radial.pie(pie3,labels=1:36)
radial.pie(pie4,labels=1:36)
# restore the par values
par(oldpar)
