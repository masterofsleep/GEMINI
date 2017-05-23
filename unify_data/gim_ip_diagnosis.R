library(gemini)
lib.pa()
#===================   GIM_IP_diag   ===========================================
#------------available for SMH, SBK, UHN, MSH, THP -----------------------------
rm(list = ls())
smh <- readg(smh, ip_diag)
sbk <- readg(sbk, ip_diag)
uhn <- readg(uhn, ip_diag)
msh <- readg(msh, ip_diag)
thp <- readg(thp, ip_diag)



names(smh)
names(sbk)
names(uhn) <- names(smh)
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.ip_diag.nophi.csv", 
          row.names = F, na = "")
names(msh)
names(thp) <- names(smh)
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.ip_diag.nophi.csv", 
          row.names = F, na = "")

names(smh)
names(sbk)
names(uhn)
names(msh)
names(thp)


apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(thp, MARGIN = 2, FUN = function(x)sum(is.na(x)))









length(unique(smh$EncID.new))
table(smh$Diagnosis.Type)
length(unique(sbk$EncID.new))
table(sbk$Diagnosis.Type)
length(unique(uhn$EncID.new))
table(uhn$Diagnosis.Type)
#all good



smh <- smh[!duplicated(smh)]
sbk <- sbk[!duplicated(sbk)]
msh <- msh[!duplicated(msh)]
thp <- thp[!duplicated(thp)]


write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.ip_diag.nophi.csv", 
          row.names = F, na = "")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_diag.nophi.csv", 
          row.names = F, na = "")
write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.ip_diag.nophi.csv", 
          row.names = F, na = "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.ip_diag.nophi.csv", 
          row.names = F, na = "")


apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(thp, MARGIN = 2, FUN = function(x)sum(is.na(x)))




#----------------Dec 1 2016 ----------------------------------------------------
#--------------- data format ---------------------------------------------------
rm(list = ls())
smh <- readg(smh, ip_diag)
sbk <- readg(sbk, ip_diag)
uhn <- readg(uhn, ip_diag)
msh <- readg(msh, ip_diag)
thp <- readg(thp, ip_diag)

head(smh)
head(sbk)
head(uhn)
head(msh)
head(thp)

thp$Diagnosis.Code <- str_replace(thp$Diagnosis.Code, "\\.", "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.ip_diag.nophi.csv", 
          row.names = F, na = "")


#-------------- create cci file for all ----------------------------------------
ip.diag <- readg(gim, ip_diag)
library(icd)
cmd <- icd10_comorbid_quan_deyo(ip.diag, visit_name = "EncID.new",
                                icd_name = "Diagnosis.Code")
# updated to "quan" on 2017-05-16
cci <- data.frame(icd_charlson_from_comorbid(cmd, visit_name = "EncID.new", 
                                             scoring_system = "quan"))
colnames(cci)[1] <- "Charlson.Comorbidity.Index"
cci$EncID.new <- row.names(cci)
fwrite(cci, "H:/GEMINI/Data/GEMINI/gim.ip_cci.csv")


ip.diag[duplicated(ip.diag)|duplicated(ip.diag, fromLast = T)] -> check
ip.diag <- unique(ip.diag)
fwrite(ip.diag, "H:/GEMINI/Data/GEMINI/gim.ip_diag.csv")


# ------------------- ip_diag --------------------------------------------------
sbk[, Site:=NULL]
msh[, Site:= NULL]
ip_diag <- rbind(smh, sbk, uhn, thp, msh) %>% unique
#ex <- readg(gim, notgim)
#ip_diag <- ip_diag[!EncID.new%in%ex$EncID.new]
length(unique(ip_diag$EncID.new))
fwrite(ip_diag, "H:/GEMINI/Data/GEMINI/gim.ip_diag.csv")

library(DBI)
setwd("C:/Users/guoyi/sqlite")
con = dbConnect(RSQLite::SQLite(), dbname = "gemini.db")
dbWriteTable(con, "ip_diag", ip_diag)
dbDisconnect(con)


# --------------------- compare two methods for CCI ----------------------------
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")
ip.diag <- readg(gim, ip_diag)[EncID.new%in%dad$EncID.new]
library(icd)
cmd <- icd10_comorbid_quan_deyo(ip.diag, visit_name = "EncID.new",
                                icd_name = "Diagnosis.Code")
# updated to "quan" on 2017-05-16
cci_quan <- data.frame(icd_charlson_from_comorbid(cmd, visit_name = "EncID.new", 
                                             scoring_system = "quan"))
cci_charlson <- data.frame(icd_charlson_from_comorbid(cmd, visit_name = "EncID.new", 
                                                      scoring_system = "charlson"))

table(cci_quan)
table(cci_charlson)
