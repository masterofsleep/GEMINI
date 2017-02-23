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
compare <- merge(msh.adm[,.(ADMIT_DATE, ADMIT_TIME, EncID.new, newHash)],
                 msh.er[,.(NACRSRegistrationNumber, Triage.Date, Triage.Time, EncID.new)],
                 by = "EncID.new")
check <- compare[ymd_hm(paste(ADMIT_DATE, ADMIT_TIME)) <= ymd_hm(paste(Triage.Date, Triage.Time))] 
furthercheck <- compare[newHash%in%check$newHash] %>% arrange(newHash, ymd_hm(paste(ADMIT_DATE, ADMIT_TIME))) 

msh.adm[EncID.new%in%check3 <- msh.er[EncID.new %in% msh.er[duplicated(EncID.new), EncID.new], EncID.new]]

length(unique(msh.er$EncID.new))
length(unique(msh.er$NACRSRegistrationNumber))


nacrs <- msh.er[!duplicated(NACRSRegistrationNumber), .(NACRSRegistrationNumber, newHash)]
enc <- msh.er[!duplicated(EncID.new), .(EncID.new, newHash)]


