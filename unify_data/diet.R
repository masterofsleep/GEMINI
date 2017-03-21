#============================ Diet =============================================
#------------------  available for SMH ---------- ------------------------------
library(gemini)
lib.pa()
rm(list = ls())
smh <- readg(smh, diet)


names(smh)
sum(duplicated(smh))
smh <- smh[!duplicated(smh)]
apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))

write.csv(smh, "H:/GEMINI/Data/SMH/Diet/smh.diet.csv", row.names = F, na = "")

msh <- readg(msh, diet)
apply(msh, 2, function(x)sum(is.na(x)))
