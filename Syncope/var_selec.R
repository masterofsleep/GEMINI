#===================== syncope study variable selection ========================


inc.ex2 <- fread("H:/GEMINI/Results/Syncope/inc.ex2.csv")
cohort <- inc.ex2[,.(EncID.new, Gender, Age)]

smh.ip <- readg(smh, ip_diag)
sbk.ip <- readg(sbk, ip_diag)
uhn.ip <- readg(uhn, ip_diag)
ip.diag <- rbind(smh.ip, sbk.ip[,c(1:4), with = F], uhn.ip)
ip.diag <- ip.diag[EncID.new%in%cohort$EncID.new]

prev.pe <- ip.diag[str_sub(Diagnosis.Code,1,3) =="I26"&Diagnosis.Type=="1"]
prev.dvt <- ip.diag[(str_sub(Diagnosis.Code,1,3) =="I80"|
                    str_sub(Diagnosis.Code,1,4)%in%c("I821", "I828", "I829"))&
                    Diagnosis.Type=="1"]
atrial.fib.or.flutter <- ip.diag[str_sub(Diagnosis.Code,1,3) =="I48"]
#calculate cci
library(icd)
cmd <- icd10_comorbid_quan_deyo(ip.diag, visit_name = "EncID.new",
                                icd_name = "Diagnosis.Code")
cci <- data.frame(icd_charlson_from_comorbid(cmd, visit_name = "EncID.new", 
                                             scoring_system = "charlson"))
colnames(cci)[1] <- "Charlson.Comorbidity.Index"
cci$EncID.new <- row.names(cci)
cohort$EncID.new <- as.character(cohort$EncID.new)

cohort <- merge(cohort, cci, by = "EncID.new", all.x = T, all.y = F)
cohort[as.numeric(Charlson.Comorbidity.Index)>=3, Charlson.Comorbidity.Index:="3+"]
cohort$Charlson.Comorbidity.Index[is.na(cohort$Charlson.Comorbidity.Index)] <- "3+"

cohort$previous.PE <- cohort$EncID.new%in%prev.pe$EncID.new
cohort$previous.DVT <- cohort$EncID.new%in%prev.dvt$EncID.new
cohort$atrial.fib.or.flutter <- cohort$EncID.new%in%atrial.fib.or.flutter$EncID.new

dvt.dad <- ip.diag[str_sub(Diagnosis.Code,1,3) =="I26"&
                     Diagnosis.Type%in%c("M","2","3")]
pe.dad <- ip.diag[(str_sub(Diagnosis.Code,1,3) =="I80"|
                     str_sub(Diagnosis.Code,1,4)%in%c("I821", "I828", "I829"))&
                    Diagnosis.Type%in%c("M","2","3")]

cohort$dad.DVT <- cohort$EncID.new%in%dvt.dad$EncID.new
cohort$dad.PE <- cohort$EncID.new%in%pe.dad$EncID.new



# DAD diag with type M only
dvt.dad <- ip.diag[str_sub(Diagnosis.Code,1,3) =="I26"&
                     Diagnosis.Type=="M"]
pe.dad <- ip.diag[(str_sub(Diagnosis.Code,1,3) =="I80"|
                     str_sub(Diagnosis.Code,1,4)%in%c("I821", "I828", "I829"))&
                    Diagnosis.Type=="M"]

cohort$dad.DVT <- cohort$EncID.new%in%dvt.dad$EncID.new
cohort$dad.PE <- cohort$EncID.new%in%pe.dad$EncID.new



# DAD diag with type M and 2 only
dvt.dad <- ip.diag[str_sub(Diagnosis.Code,1,3) =="I26"&
                     Diagnosis.Type%in%c("M","2")]
pe.dad <- ip.diag[(str_sub(Diagnosis.Code,1,3) =="I80"|
                     str_sub(Diagnosis.Code,1,4)%in%c("I821", "I828", "I829"))&
                    Diagnosis.Type%in%c("M","2")]

cohort$dad.DVT <- cohort$EncID.new%in%dvt.dad$EncID.new
cohort$dad.PE <- cohort$EncID.new%in%pe.dad$EncID.new








fwrite(cohort, "H:/GEMINI/Results/Syncope/cohort.with.var.csv")
