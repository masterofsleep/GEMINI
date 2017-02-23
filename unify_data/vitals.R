#============================ Vitals =============================================
#------------------  available for SMH ---------- ------------------------------
library(gemini)
lib.pa()
rm(list = ls())
smh <- readg(smh, vitals)


names(smh)
sum(duplicated(smh))
smh <- smh[!duplicated(smh)]
apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))

write.csv(smh, "H:/GEMINI/Data/SMH/Vitals/smh.vitals.csv", row.names = F, na = "")
