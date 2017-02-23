library(gemini)
lib.pa()

rm(list = ls())
smh.diag <- readg(smh, ip_diag)
sbk.diag <- readg(sbk, ip_diag)
uhn.diag <- readg(uhn, ip_diag)
msh.diag <- readg(msh, ip_diag)
thp.diag <- readg(thp, ip_diag)
ip.diag <- rbind(smh.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)],
                 sbk.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)],
                 uhn.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)],
                 msh.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)],
                 thp.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)])

smh.er.diag <- readg(smh, er_diag)
sbk.er.diag <- readg(sbk, er_diag,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn.er.diag <- readg(uhn, er_diag,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh.er.diag <- readg(msh, er_diag)
thp.er.diag <- readg(thp, er_diag)
er.diag <- rbind(smh.er.diag[,.(EncID.new, ER.Diagnosis.Code, ER.Diagnosis.Type)],
                 sbk.er.diag[,.(EncID.new, ER.Diagnosis.Code, ER.Diagnosis.Type)],
                 uhn.er.diag[,.(EncID.new, ER.Diagnosis.Code, ER.Diagnosis.Type)],
                 msh.er.diag[,.(EncID.new, ER.Diagnosis.Code, ER.Diagnosis.Type)],
                 thp.er.diag[,.(EncID.new, ER.Diagnosis.Code, ER.Diagnosis.Type)])

133641/139151
compare.diag <- merge(ip.diag[Diagnosis.Type=="M"],
                      er.diag[ER.Diagnosis.Type=="M"],
                      by = "EncID.new")
head(compare.diag)
#discrepency of diagnosis with full diag code
sum(compare.diag$Diagnosis.Code!=compare.diag$ER.Diagnosis.Code)
dim(compare.diag)[1]
sum(compare.diag$Diagnosis.Code!=compare.diag$ER.Diagnosis.Code)/
dim(compare.diag)[1]



#discrepency of diagnosis with full diag code
sum(str_sub(compare.diag$Diagnosis.Code,1,3)!=
      str_sub(compare.diag$ER.Diagnosis.Code,1,3))
sum(str_sub(compare.diag$Diagnosis.Code,1,3)!=
      str_sub(compare.diag$ER.Diagnosis.Code,1,3))/
  dim(compare.diag)[1]
