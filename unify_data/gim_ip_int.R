library(gemini)
lib.pa()
#===================GIM_IP_Intervention ========================================
#------------available for SMH, SBK, UHN, THP, MSh -----------------------------
rm(list = ls())
smh <- readg(smh, ip_int)
sbk <- readg(sbk, ip_int)
uhn <- readg(uhn, ip_int)
msh <- readg(msh, ip_int)
thp <- readg(thp, ip_int)

names(smh)[5:8] <- c("Procedure.Location", "Intervention.Location.Attribute",
                     "Intervention.Status.Attribute", 
                     "Intervention.Extent.Attribute")

names(sbk)[1:8] <- names(smh)[1:8]
sbk <- sbk[!(is.na(EncID.new)&is.na(Intervention.Occurrence))]
names(uhn) <- names(smh)
names(msh)[1:9] <- names(smh)
names(thp) <- names(smh)

write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.ip_int.nophi.csv",
          row.names = F, na = "")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_int.nophi.csv",
          row.names = F, na = "")
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.ip_int.nophi.csv",
          row.names = F, na = "")
msh <- msh[!duplicated(msh)]
write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.ip_int.nophi.csv",
          row.names = F, na = "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.ip_int.nophi.csv",
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


sbk.check <- sbk[is.na(Intervention.Occurrence)]
apply(sbk.check, MARGIN = 2, FUN = function(x)sum(is.na(x)))

sbk.check.noenc <- sbk[is.na(EncID.new)]


uhn.check <- uhn[!is.na(Intervention.Occurrence)&is.na(Intervention.Code)]


msh.check <- msh[!is.na(Intervention.Occurrence)&is.na(Intervention.Code)]





#----------------Dec 1 2016 ----------------------------------------------------
#--------------- data format ---------------------------------------------------
rm(list = ls())
smh <- readg(smh, ip_int)
sbk <- readg(sbk, ip_int)
uhn <- readg(uhn, ip_int)
msh <- readg(msh, ip_int)
thp <- readg(thp, ip_int)

head(smh)
head(sbk)
head(uhn)
head(msh)
head(thp)

thp$Intervention.Code <- str_replace_all(thp$Intervention.Code, "[:punct:]", "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.ip_int.nophi.csv",
          row.names = F, na = "")


#------------------- Jan 13 2017 -----------------------------------------------
# ------------------------ create merged file ----------------------------------
rm(list = ls())

smh.intip <- readg(smh, ip_int)
sbk.intip <- readg(sbk, ip_int)
uhn.intip <- readg(uhn, ip_int)
msh.intip <- readg(msh, ip_int)
thp.intip <- readg(thp, ip_int)
msh.intip[,Site:=NULL]
int.ip <- rbind(smh.intip,
                sbk.intip,
                uhn.intip,
                msh.intip,
                thp.intip)
int.ip <- int.ip[!is.na(Intervention.Code)]
fwrite(int.ip, "H:/GEMINI/Data/GEMINI/gim.ip_int.csv")
msh.intip[is.na(Intervention.Code)&!is.na(Intervention.Occurrence)]
228/34245




# --------------- missingness of intervention date time ------------------------
msh.intip <- msh.intip %>% arrange(EncID.new, Intervention.Occurrence) %>% 
  select(EncID.new, Intervention.Occurrence, Intervention.Code,
         Intervention.Episode.Start.Date, Intervention.Episode.Start.Time)
