library(gemini)
lib.pa()
#===================   GIM_ER_diag   ===========================================
#------------available for SMH, SBK, UHN, MSH, THP -----------------------------
rm(list = ls())
smh <- readg(smh, er_diag)
sbk <- readg(sbk, er_diag,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn <- readg(uhn, er_diag,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh <- readg(msh, er_diag)
thp <- readg(thp, er_diag)




names(smh) <- c("NACRSRegistrationNumber", "ER.Diagnosis.Code", 
                "ER.Diagnosis.Type", "EncID.new")
smh <- smh[!duplicated(smh)]
write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.er_diag.nophi.csv", 
          row.names = F, na = "")
names(sbk)
table(sbk$ERDiagOccurrence)
table(smh$ER.Diagnosis.Type)
table(uhn$ER_Diagnosis_Type)
table(msh$DiagnosisType)


names(sbk) <- c("NACRSRegistrationNumber", "ER.Diagnosis.Occurrence",
                "ER.Diagnosis.Code", "EncID.new")
sbk <- sbk[!duplicated(sbk)]
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.er_diag.nophi.csv", 
          row.names = F, na = "")

names(uhn) <- names(smh)
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.er_diag.nophi.csv", 
          row.names = F, na = "")
names(msh) <- names(smh)
msh <- msh[!duplicated(msh)]
write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.er_diag.nophi.csv", 
          row.names = F, na = "")
thp <- thp[!duplicated(thp)]
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.er_diag.nophi.csv", 
          row.names = F, na = "")
names(smh)
names(sbk)
names(uhn)
names(msh)
names(thp)


apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(thp, MARGIN = 2, FUN = function(x)sum(is.na(x)))





table(smh$ER.Diagnosis.Type)
table(sbk$ER.Diagnosis.Occurrence)
table(uhn$ER.Diagnosis.Type)


length(unique(smh$EncID.new))
length(unique(sbk$EncID.new))
length(unique(uhn$EncID.new))



uhn.no.mrd <- uhn[!uhn$EncID.new %in% uhn$EncID.new[uhn$ER.Diagnosis.Type=="M"]]


sbk$ER.Diagnosis.Type[sbk$ER.Diagnosis.Occurrence>1] <- 3
sbk$ER.Diagnosis.Type[sbk$ER.Diagnosis.Occurrence==1] <- "M"
names(sbk)[2] <- "ER.Diagnosis.Type"
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.er_diag.nophi.csv", 
          row.names = F, na = "")

#----------------Dec 1 2016 ----------------------------------------------------
#--------------- data format ---------------------------------------------------

rm(list = ls())
smh <- readg(smh, er_diag)
sbk <- readg(sbk, er_diag,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn <- readg(uhn, er_diag,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh <- readg(msh, er_diag)
thp <- readg(thp, er_diag)



head(smh)
head(sbk)
head(uhn)
head(msh)
head(thp)
names(thp) <- names(smh)
thp$ER.Diagnosis.Code <- str_replace(thp$ER.Diagnosis.Code, "\\.", "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.er_diag.nophi.csv", 
          row.names = F, na = "")
#------------------- Jan 13 2017 -----------------------------------------------
# ------------------------ create merged file ----------------------------------

er.diag <- rbind(smh,
                 sbk,
                 uhn,
                 msh,
                 thp)
fwrite(er.diag, "H:/GEMINI/Data/GEMINI/gim.er_diag.csv")


