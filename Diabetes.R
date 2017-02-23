library(gemini)
lib.pa()
#============================ Diabetes =========================================
#========================= Jan 10 2017 =========================================

rm(list = ls())
smh.diag <- readg(smh, ip_diag)
sbk.diag <- readg(sbk, ip_diag)
uhn.diag <- readg(uhn, ip_diag)
msh.diag <- readg(msh, ip_diag)
thp.diag <- readg(thp, ip_diag)
ip.diag <- rbind(smh.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)],
                 sbk.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)],
                 uhn.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)],
                 msh.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)],
                 thp.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)])

# Top Diagnosis Codes
ip.diag[Diagnosis.Type=="M", str_sub(Diagnosis.Code, 1, 3)] %>% table %>% 
  data.table %>% arrange(desc(N)) %>% data.table -> mrd.freq
names(mrd.freq) <- c("Diagnosis.Code", "Frequency")
mrd.freq$Prevalence <- paste(round(mrd.freq$Frequency/139151 * 100, 3), "%", sep = "")
fwrite(mrd.freq, "H:/GEMINI/Results/MRD.freqtable.csv")

mrd.freq[Diagnosis.Code=="E12"]

ip.diag3 <- ip.diag[, .(str_sub(Diagnosis.Code, 1, 3), EncID.new)]
names(ip.diag3) <- c("Diag.Code3", "EncID.new")
diab.enc <- ip.diag3[Diag.Code3%in%c("E08", "E09","E10", "E11", "E13")]
diab.enc2 <- ip.diag3[Diag.Code3%in%c("E10", "E11", "E13", "E12", "E14")]
table(diab.enc$Diag.Code3)
table(diab.enc2$Diag.Code3)
length(unique(diab.enc$EncID.new)); length(unique(diab.enc$EncID.new))/139151
length(unique(diab.enc2$EncID.new)); length(unique(diab.enc2$EncID.new))/139151



icd10 <- fread("R:/GEMINI/Coding/CIHI/ICD10-2015-PARSED-CATEGORIES.txt",header = F)
names(icd10) <- c("Code", "Diagnosis")
sum(mrd.freq$Diagnosis.Code%in%icd10$Code)

mrd.freq <- merge(mrd.freq, icd10, by.x = "Diagnosis.Code", by.y = "Code",
                  all.x = T, all.y = F)
