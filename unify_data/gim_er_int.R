library(gemini)
lib.pa()

#===================   GIM_ER_Intervetnion   ===========================================
#------------available for SMH, SBK, UHN, MSH, THP -----------------------------
rm(list = ls())
smh <- readg(smh, er_int, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
sbk <- readg(sbk, er_int, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn <- readg(uhn, er_int, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                                        "EncID.new")))
msh <- readg(msh, er_int, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
thp <- readg(thp, er_int, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))

names(smh)
names(sbk)
names(uhn)
names(msh)
names(thp)

table(smh$Occurrence.Code)
table(sbk$IntervOccurrence)
table(uhn$OccurrenceCode)
table(msh$OccurrenceCode)
table(thp$Occurrence.Type)

table(uhn$Occurrencetype)

names(smh) <- c("NACRSRegistrationNumber", "Occurrence.Code",
                "Occurrence.Type", "Interv.loc.attrib",
                "Interv.status.attrib", "Interv.extent.attrib",
                "EncID.new")



write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.er_int.nophi.csv", 
          row.names = F, na = "", quote = F)
names(sbk) <- c("NACRSRegistrationNumber", "Occurrence.Code",
                "Occurrence.Type", "Interv.loc.attrib",
                "Interv.status.attrib", "Interv.extent.attrib",
                "EncID.new")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.er_int.nophi.csv", 
          row.names = F, na = "", quote = F)

names(uhn) <- c("NACRSRegistrationNumber", "Occurrence.Code",
                "ER.Intervention.Type", "Interv.loc.attrib",
                "Interv.status.attrib", "Occurrence.Type",
                "Interv.extent.attrib","EncID.new")
uhn <- uhn[!duplicated(uhn)]
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.er_int.nophi.csv", 
          row.names = F, na = "")

names(msh)[1:7] <- c("NACRSRegistrationNumber", "Occurrence.Type",
                "Occurrence.Code", "Interv.loc.attrib",
                "Interv.status.attrib", "Interv.extent.attrib",
                "EncID.new")
msh <- msh[,c(1,3,2,4,5,6,7), with = F]
msh <- msh[!duplicated(msh)]
write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.er_int.nophi.csv", 
          row.names = F, na = "", quote = F)

names(thp) <- c("NACRSRegistrationNumber", "Occurrence.Type",
               "Occurrence.Code", "Interv.loc.attrib",
               "Interv.status.attrib", "Interv.extent.attrib",
               "EncID.new")
thp <- thp[, c(1,3,2,4,5,6,7), with = F]
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.er_int.nophi.csv", 
          row.names = F, na = "", quote = F)

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


names(uhn) <- names(uhn)[c(1:5,7,6,8)]
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.er_int.nophi.csv", 
          row.names = F, na = "")



#----------------Dec 1 2016 ----------------------------------------------------
#--------------- data format ---------------------------------------------------
rm(list = ls())
rm(list = ls())
smh <- readg(smh, er_int, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
sbk <- readg(sbk, er_int, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn <- readg(uhn, er_int, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh <- readg(msh, er_int, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
thp <- readg(thp, er_int, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))

head(smh)
head(sbk)
head(uhn)
head(msh)
head(thp)

thp$Occurrence.Type <- str_replace_all(thp$Occurrence.Type, "[:punct:]", "")
uhn$Occurrence.Type <- str_replace_all(uhn$Occurrence.Type, "[:punct:]", "")
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.er_int.nophi.csv", 
          row.names = F, na = "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.er_int.nophi.csv",
          row.names = F, na = "")



#------------------- Jan 13 2017 -----------------------------------------------
# ------------------------ create merged file ----------------------------------
rm(list = ls())
smh.inter <- readg(smh, er_int, 
                   colClasses = list(character = c("NACRSRegistrationNumber",
                                                   "EncID.new")))
sbk.inter <- readg(sbk, er_int, 
                   colClasses = list(character = c("NACRSRegistrationNumber",
                                                   "EncID.new")))
uhn.inter <- readg(uhn, er_int, 
                   colClasses = list(character = c("NACRSRegistrationNumber",
                                                   "EncID.new")))
msh.inter <- readg(msh, er_int, 
                   colClasses = list(character = c("NACRSRegistrationNumber",
                                                   "EncID.new")))
thp.inter <- readg(thp, er_int, 
                   colClasses = list(character = c("NACRSRegistrationNumber",
                                                   "EncID.new")))
uhn.inter[,ER.Intervention.Type:=NULL]
int.er <- rbind(smh.inter,
                sbk.inter,
                uhn.inter,
                msh.inter,
                thp.inter) %>% unique
int.er[is.na(Occurrence.Type)&is.na(Occurrence.Code)] -> check
apply(check, 2, function(x)sum(is.na(x)))
int.er <- int.er[!(is.na(Occurrence.Type)&is.na(Occurrence.Code))]
apply(int.er, 2, function(x)sum(is.na(x))) 
# ex <- readg(gim, notgim)
# int.er <- int.er[!EncID.new%in%ex$EncID.new]
int.er[EncID.new%in%int.er[is.na(Occurrence.Type), EncID.new]] -> check
fwrite(int.er, "H:/GEMINI/Data/GEMINI/gim.er_int.csv")

library(DBI)
setwd("C:/Users/guoyi/sqlite")
con = dbConnect(RSQLite::SQLite(), dbname = "gemini.db")
dbWriteTable(con, "er_int", int.er)
dbDisconnect(con)
