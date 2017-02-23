library(gemini)
lib.pa()

#===================   GIM_ER_CACS_REGROUPED   ===========================================
#------------available for SMH, SBK, UHN, MSH, THP -----------------------------
rm(list = ls())
smh <- readg(smh, er_cacs, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
sbk <- readg(sbk, er_cacs, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn <- readg(uhn, er_cacs,  
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh <- readg(msh, er_cacs,  
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
thp <- readg(thp, er_cacs, 
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))

names(smh)
names(sbk)
names(uhn)
names(msh)
names(thp)


smh <- smh[, 1:7, with = F]
names(smh) <- c("NACRSRegistrationNumber", "Methodology.Year",
                "CACS", "CACS.WT","CACS.WT.ON", "MACCODE",
                "EncID.new")
write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.er_cacs.nophi.csv", 
          row.names = F, na = "", quote = F)

sbk <- sbk[,1:7, with = F]
names(sbk) <- c("NACRSRegistrationNumber", "Methodology.Year",
                "CACS", "CACS.WT","CACS.WT.ON", "MACCODE",
                "EncID.new")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.er_cacs.nophi.csv", 
          row.names = F, na = "")

uhn <- uhn[,1:7, with = F]
names(uhn) <- c("NACRSRegistrationNumber", "Methodology.Year",
                "CACS", "CACS.WT","CACS.WT.ON", "MACCODE",
                "EncID.new")
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.er_cacs.nophi.csv", 
          row.names = F, na = "")

msh <- msh[,c(1,2,4,6,8,10,11), with = F]
msh <- msh[1:16996]
msh <- msh[!duplicated(msh)]
names(msh)<- c("NACRSRegistrationNumber", "Methodology.Year",
               "CACS", "ACW","CACSHBAM", "MACCODE", "EncID.new")
msh$Methodology.Year <- as.numeric(str_replace(msh$Methodology.Year, ",", ""))


write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.er_cacs.nophi.csv", 
          row.names = F, na = "")

names(thp) <- c("NACRSRegistrationNumber", "Methodology.Year",
                "CACS", "CACS.WT","CACS.WT.ON", "MACCODE",
                "EncID.new")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.er_cacs.nophi.csv", 
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
