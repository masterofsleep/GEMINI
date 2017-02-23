library(gemini)
lib.pa()
#===================   GIM_IP_SCU   ===========================================
#------------available for SMH, SBK, UHN, MSH, THP -----------------------------

rm(list = ls())
smh <- readg(smh, ip_scu)
sbk <- readg(sbk, ip_scu)
uhn <- readg(uhn, ip_scu)
msh <- readg(msh, ip_scu)
thp <- readg(thp, ip_scu)


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


sbk.check.noenc <- sbk[is.na(EncID.new)]
sbk <- sbk[!is.na(EncID.new)]
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_scu.nophi.csv",
          row.names = F, na = "")
msh <- msh[!duplicated(msh)]
write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.ip_scu.nophi.csv",
          row.names = F, na = "")
sbk.check.unit <- sbk[SCU.Unit.Number!=99]

thp.check.unit <- thp[SCU.Unit.Number!=99]


