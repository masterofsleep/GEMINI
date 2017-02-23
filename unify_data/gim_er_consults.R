library(gemini)
lib.pa()

#===================   GIM_ER_CONSULTS  ========================================
#------------available for SMH, SBK, UHN, MSH, THP -----------------------------
rm(list = ls())
smh <- readg(smh, er_consults, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
sbk <- readg(sbk, er_consults, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn <- readg(uhn, er_consults,  
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh <- readg(msh, er_consults,  
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
thp <- readg(thp, er_consults, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))

names(smh)
names(sbk)
names(uhn)
names(msh)
names(thp)
name.cons <- c("NACRSRegistrationNumber", "ER.Consult.Occurrence",
               "ER.Consult.Service", "ER.Consult.Date",
               "EncID.new")
names(smh) <- name.cons
names(sbk) <- name.cons
names(uhn) <- name.cons
names(msh)[1:5] <- name.cons
names(thp) <- name.cons


msh <- msh[!duplicated(msh)]

write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.er_consults.nophi.csv",
          row.names = F, na = "")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.er_consults.nophi.csv",
          row.names = F, na = "")
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.er_consults.nophi.csv",
          row.names = F, na = "")
write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.er_consults.nophi.csv",
          row.names = F, na = "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.er_consults.nophi.csv",
          row.names = F, na = "")


apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(thp, MARGIN = 2, FUN = function(x)sum(is.na(x)))
