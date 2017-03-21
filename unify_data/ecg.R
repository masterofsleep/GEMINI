#============================ ECG =============================================
#------------------  available for SMH, MSH ------------------------------
library(gemini)
lib.pa()
rm(list = ls())
smh <- readg(smh, ecg)
msh <- readg(msh, ecg)

names(smh)
names(msh)

sum(duplicated(smh))
sum(duplicated(msh))
msh <- msh[!duplicated(msh)]
write.csv(msh, "H:/GEMINI/Data/MSH/ECG/msh.ecg.csv",row.names = F,
          na = "")
apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x)sum(is.na(x)))


msh <- readg(msh, ecg)
apply(msh, 2, function(x)sum(is.na(x)))