#============================ Echo =============================================
#------------------  available for SMH, SBK, UHN  ------------------------------
library(gemini)
lib.pa()
rm(list = ls())
smh <- readg(smh, echo)
sbk <- readg(sbk, echo)
uhn <- readg(uhn, echo)

names(smh)
names(sbk)
names(uhn)

sum(duplicated(smh))
sum(duplicated(sbk))
sum(duplicated(uhn))
uhn <- uhn[!duplicated(uhn)]
write.csv(uhn, "H:/GEMINI/Data/UHN/Echo/uhn.echo.csv", row.names = F, na = "")
apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))
