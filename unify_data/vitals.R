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




# -------------------------- MSH -----------------------------------------------
msh.vitals <- fread("R:/GEMINI/_RESTORE/MSH/Clinical/msh.clinical.nophi.csv")
msh.dad <- readg(msh, dad)
msh.vitals[, EncID.new:=paste("14", EncID.new, sep = "")]
sum(msh.vitals$EncID.new%in%msh.dad$EncID.new)

msh.dad[, vitals := EncID.new%in%msh.vitals$EncID.new]
ggplot(msh.dad, aes(ymd(Admit.Date), fill = vitals)) + geom_histogram(binwidth = 10)
ggplot(msh.dad, aes(ymd(Discharge.Date), fill = vitals)) + geom_histogram(binwidth = 10)



msh.vitals[is.na(ymd(DOCUMENTATION_DATE)), DOCUMENTATION_DATE:=
             paste(str_sub(DOCUMENTATION_DATE, -4, -1), 
                   str_sub(DOCUMENTATION_DATE, 1, -6), sep = "/")]
msh.vitals[is.na(ymd(DOCUMENTATION_DATE))]
ggplot(msh.vitals, aes(ymd(DOCUMENTATION_DATE))) + geom_histogram(binwidth = 10)

range(ymd(msh.vitals$DOCUMENTATION_DATE), na.rm = T)
range(msh.dad[vitals==T, ymd(Discharge.Date)])


ggplot(msh.dad[vitals==F], aes(ymd(Discharge.Date))) + geom_histogram(binwidth = 10)
