library(gemini)
lib.pa()
#===================GIM_IP_CMG_regrouped ========================================
#------------available for SMH, SBK, UHN, THP, MSh -----------------------------
rm(list = ls())
smh <- readg(smh, ip_cmg)
sbk <- readg(sbk, ip_cmg)
uhn <- readg(uhn, ip_cmg)
msh <- readg(msh, ip_cmg)
thp <- readg(thp, ip_cmg)
cmg.names <- c("Methodology.Year", "CMG", "Diagnosis.For.CMG.Assignment",
               "CMG.Intervention", "Comorbidity.Level", 
               "RIW.Inpatient.Atypical.Indicator", "RIW.15", "EncID.new")
names(smh) <- cmg.names
names(sbk) <- cmg.names
names(uhn) <- cmg.names
names(msh)[1:8] <- cmg.names
names(thp) <- cmg.names



write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.ip_cmg.nophi.csv",
          row.names = F, na = "")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_cmg.nophi.csv",
          row.names = F, na = "")
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.ip_cmg.nophi.csv",
          row.names = F, na = "")
write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.ip_cmg.nophi.csv",
          row.names = F, na = "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.ip_cmg.nophi.csv",
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

smh.check <- smh[is.na(Methodology.Year)]
check.sbk <- sbk[is.na(EncID.new)]
