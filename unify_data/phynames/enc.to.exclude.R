# ------------ list of admissions to exclude base on physician -----------------
# ------------------------------ 2017-03-20 ------------------------------------
library(gemini)
lib.pa()
# smh geriatrics and not gim
smh.marked <- readxl::read_excel("R:/GEMINI/_RESTORE/SMH/Physicians/smh.notgim.mrn.xlsx") %>%
  data.table
geriatrics.smh <- smh.marked[Geriatrics==1, EncID.new]
not.gim.smh <- smh.marked[is.na(Geriatrics), EncID.new]

# sbk not gim
sbk.adm <- readg(sbk, adm)
sbk.adm[Admitting.Service=="GMS", EncID.new]
names.marked <- fread("C:/Users/guoyi/Desktop/marked_names/names_code_link_ASW.csv")
adm.names <- fread("R:/GEMINI/_RESTORE/SBK/Physicians/adm.physician.hashes.csv")
dad.names <- fread("R:/GEMINI/_RESTORE/SBK/Physicians/dad.mrp.hashes.csv")

intersect(dad.names$mrpCode, adm.names$admitCode)
intersect(dad.names$mrpCode, adm.names$disCode)

adm.names <- merge(adm.names, names.marked[,.(hashed, Admiting.Name = Name, `Was GIM Attending`)],
                   by.x = "admitCode", by.y = "hashed",
                   all.x = T, all.y = F)

adm.names <- merge(adm.names, names.marked[,.(hashed, Discharging.Name = Name, `Was GIM Attending`)],
                   by.x = "disCode", by.y = "hashed",
                   all.x = T, all.y = F)
names(adm.names)[c(5,7)] <- c("gim.adm", "gim.dis")
sum(is.na(adm.names$gim.dis))
sum(is.na(adm.names$gim.adm))

adm.names[,gim:= ifelse(gim.adm=="Y"|gim.dis=="Y", "Y", 
                        ifelse(gim.adm=="N"&gim.dis=="N", "N", "U"))]
adm.names[gim%in%c("N", "U")] -> check
not.gim.sbk <- check$EncID.new

# msh not gim in overlap
not.gim.msh <- c(fread("R:/GEMINI/_RESTORE/MSH/Physician Names/march15/overlap.notgim.csv", select = "EncID.new"),
                 fread("R:/GEMINI/_RESTORE/MSH/Physician Names/march15/overlap.unknown.csv", select = "EncID.new"))%>%unlist






exclude <- c(not.gim.smh, not.gim.sbk, not.gim.msh)
