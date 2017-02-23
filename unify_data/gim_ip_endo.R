library(gemini)
lib.pa()
#===================GIM_IP_Endo=================================================
#------------available for SMH, SBK --------------------------------------------
rm(list = ls())

smh <- readg(smh, endo)
sbk <- readg(sbk, endo)

names(smh)
names(sbk)
smh <- smh[!duplicated(smh)]
sbk <- sbk[!duplicated(sbk)]
write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.ip_endo.nophi.csv",
          row.names = F, na = "")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_endo.nophi.csv",
          row.names = F, na = "")
apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))

