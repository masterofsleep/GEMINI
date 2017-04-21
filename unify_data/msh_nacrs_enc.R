# ==============================================================================
# ======== check msh incorrectly matched NACRS number and EncID ================
# ==============================================================================

rm(list = ls())
library(gemini)
lib.pa()

msh.adm <- readg(msh, adm) %>% unique
msh.adm[EncID.new%in%msh.adm[duplicated(EncID.new), EncID.new]]
msh.adm[newHash%in%msh.adm[EncID.new%in%msh.adm[duplicated(EncID.new), EncID.new], newHash]]
msh.dad <- readg(msh, dad)
msh.dad[EncID.new%in%msh.adm[duplicated(EncID.new), EncID.new]]
msh.er <- readg(msh, er.nophi)
compare <- merge(msh.adm[,.(Admit_Date, Admit_Time, EncID.new, newHash)],
                 msh.er[,.(NACRSRegistrationNumber, Triage.Date, Triage.Time, EncID.new)],
                 by = "EncID.new")
check <- compare[EncID.new%in%compare[duplicated(EncID.new), EncID.new]]
furthercheck <- check %>% arrange(EncID.new, ymd(Admit_Date), ymd_hm(paste(Triage.Date, Triage.Time)))
# NACRS and EncID.new combinations that should be removed
furthercheck[c(1, 3, 6, 9, 11, 15, 17, 19, 21, 27), ] %>%data.table -> danacrs.ex
fwrite(nacrs.ex, "H:/GEMINI/Results/DataSummary/to.exclude/msh.extra.nacrs.csv")
msh.er <- msh.er[!paste(EncID.new, NACRSRegistrationNumber)%in%
                   paste(nacrs.ex$EncID.new, nacrs.ex$NACRSRegistrationNumber)]

fwrite(msh.er, "H:/GEMINI/Data/MSH/CIHI/msh.er.nophi.csv")

nacrs.ex <- fread("H:/GEMINI/Results/DataSummary/to.exclude/msh.extra.nacrs.csv")
swdh("MSH/CIHI")
files <- list.files();files
for(i in files[c(3, 4, 5, 6)]){
  dat <- fread(i)
  dat <- dat[!paste(EncID.new, NACRSRegistrationNumber)%in%
               paste(nacrs.ex$EncID.new, nacrs.ex$NACRSRegistrationNumber)]
  fwrite(dat, i)
}
