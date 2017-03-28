
#================================ RAD ==========================================
#-----------------------  available for SMH, SBK  ------------------------------

library(gemini)
lib.pa()
rm(list = ls())

smh.ango <- readg(smh, ango)
smh.ct <- readg(smh, ct)
smh.mri <- readg(smh, mri)
smh.nuc <- readg(smh, nuc)
smh.us <- readg(smh.us, us)
smh.xray <- readg(smh, xray)
names(smh.ango)
names(smh.ct)
names(smh.mri)
names(smh.nuc)
names(smh.us)
names(smh.xray)
apply(smh.ango, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(smh.ct, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(smh.mri, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(smh.nuc, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(smh.us, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(smh.xray, MARGIN = 2, FUN = function(x)sum(is.na(x)))
sum(duplicated(smh.ango))
sum(duplicated(smh.ct))
sum(duplicated(smh.mri))
sum(duplicated(smh.nuc))
sum(duplicated(smh.us))
sum(duplicated(smh.xray))

sbkip <- readg(SBK, rad_ip)
names(sbkip)
apply(sbkip, MARGIN = 2, FUN = function(x)sum(is.na(x)))

sbker <- readg(SBK, rad_er)
names(sbker)
apply(sbker, MARGIN = 2, FUN = function(x)sum(is.na(x)))

sbkres <- readg(sbk, results)
sbkgeminires <- readg(SBK, GEMINIRadResult)

sum(sbkres$V1 %in% sbkgeminires$V1)
check <- filter(sbkgeminires, V1 %in% sbkres$V1)

sbk.gem.res.nona <- na.omit(sbkgeminires[,.(V1, V3)])

temp <- by(sbk.gem.res.nona$V3, sbk.gem.res.nona$V1, 
           FUN = function(x)paste(x, collapse = " "))

sbk.rad.results <- data.frame(cbind(names(temp), as.vector(temp)))
names(sbk.rad.results) <- c("SID", "Results")

write.csv(sbk.rad.results, "H:/GEMINI/Data/SBK/Radiology/sbk.rad_results.csv",
          row.names = F)




sbkres <- readg(sbk, rad.results)

sum(sbkres$SID%in%sbkip$SID)
sum(sbkres$SID%in%sbker$SID)


sum(!sbkip$SID%in%sbkres$SID)
sum(!sbker$SID%in%sbkres$SID)
length(unique(sbkip$SID))
length(unique(sbker$SID))
length(intersect(sbker$SID, sbkip$SID))

sum(duplicated(sbkip$SID))
sum(duplicated(sbker))

sum(duplicated(sbkip))
sum(duplicated(sbker))

sbker <- sbker[!duplicated(sbker)]
names(sbker)[1] <- "NACRSRegistrationNumber"
write.csv(sbker, "H:/GEMINI/Data/SBK/Radiology/sbk.rad_er.csv",
          row.names = F)

check <- sbkip[!SID%in%sbkres$SID]

#----Dec 5 2016 check whether there are clusters in missing results-------------
sbkip <- readg(SBK, rad_ip)
sbker <- readg(SBK, rad_er)
sbkres <- readg(sbk, rad.results)

resip <- sbkip[SID%in%sbkres$SID, EncID.new]
noresip <- unique(sbkip$EncID.new)[!unique(sbkip$EncID.new)%in%resip]
reser <- sbker[SID%in%sbkres$SID, EncID.new]
noreser <- unique(sbker$EncID.new)[!unique(sbker$EncID.new)%in%reser]

sbkip$result <- sbkip$SID%in%sbkres$SID
ggplot(sbkip, aes(x = mdy(str_sub(`Date/Time Test Performed`, 1, 11)),
                  fill = result)) +
  geom_histogram(binwidth = 30, alpha = 0.5) +
  ggtitle("Sunny Brook IP Radiology Count by Performed Date")

ggplot(sbkip, aes(x = mdy(str_sub(`Date/Time Test Ordered`, 1, 11)),
                  fill = result)) +
  geom_histogram(binwidth = 30, alpha = 0.5) +
  ggtitle("Sunny Brook IP Radiology Count by Performed Date")

range(mdy(str_sub(sbkip$`Date/Time Test Performed`, 1, 11)), na.rm = T)

sbker$result <- sbker$SID%in%sbkres$SID
ggplot(sbker, aes(x = mdy(str_sub(TestPerformed, 1, 11)),
                  fill = result)) +
  geom_histogram(binwidth = 30, alpha = 0.5) +
  ggtitle("Sunny Brook ER Radiology Count by Performed Date")
#----------------- UHN ---------------------------------------------------------

uhnip <- readg(UHN, radip_jdri)
uhner <- readg(UHN, rader_jdri)


names(uhner)
names(uhnip)
apply(uhner, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhnip, MARGIN = 2, FUN = function(x)sum(is.na(x)))
sum(duplicated(uhnip))
sum(duplicated(uhner))

uhnip <- uhnip[!duplicated(uhnip)]
uhner <- uhner[!duplicated(uhner)]
write.csv(uhner, "H:/GEMINI/Data/UHN/Radiology/uhn.rad_er.csv",
          row.names = F)
write.csv(uhnip, "H:/GEMINI/Data/UHN/Radiology/uhn.rad_ip.csv",
          row.names = F)
#----------------- MSH ---------------------------------------------------------

mship <- readg(msh, rad_ip)
msher <- readg(msh, rad_er)

names(msher)
names(mship)
apply(msher, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(mship, MARGIN = 2, FUN = function(x)sum(is.na(x)))

sum(duplicated(mship))
sum(duplicated(msher))



mship <- mship[!duplicated(mship)]
msher <- msher[!duplicated(msher)]

write.csv(msher, "H:/GEMINI/Data/MSH/Radiology/msh.rad_er.csv",
          row.names = F)
write.csv(mship, "H:/GEMINI/Data/MSH/Radiology/msh.rad_ip.csv",
          row.names = F)






#-----------Dec 08 2016 Check new radresult data brought from sbk --------------
res <- fread("C:/Users/guoyi/Downloads/GEMINIRadResult")
sum(sbker$SID%in%res$V1)
sum(sbkip$SID%in%res$V1)




#-----------Dec 15 2016 removed sbk records without text summary ---------------
sbkip <- readg(SBK, rad_ip)
sbker <- readg(SBK, rad_er)
sbkres <- readg(sbk, rad.results)

names(sbkip) <- c("SID","PID","Test.Name","Test.Code", "Order.DtTm",
                  "Perform.DtTm", "V9", "EncID.new")
names(sbker) <- c("NACRSRegistrationNumber","EncID.new",
                  "SID","PID","Test.Name","Test.Code", 
                  "Order.DtTm",
                  "Perform.DtTm", "V9")
sbk <- rbind(sbker, sbkip, fill = T)
sbk <- sbk[SID%in% sbkres$SID]

sum(is.na(sbk$NACRSRegistrationNumber))
sbk[str_sub(Order.DtTm, -2, -1)=="PM", 
      Ordered.DtTm:= (mdy_hms(str_sub(Order.DtTm, 1, 20)) + hours(12))]
sbk[str_sub(Order.DtTm, -2, -1)=="AM", 
    Ordered.DtTm:= mdy_hms(str_sub(Order.DtTm, 1, 20))]

check <- sbk[is.na(Ordered.DtTm)]

sbk[str_sub(Perform.DtTm, -2, -1)=="PM", 
    Performed.DtTm:= (mdy_hms(str_sub(Perform.DtTm, 1, 20)) + hours(12))]
sbk[str_sub(Perform.DtTm, -2, -1)=="AM", 
    Performed.DtTm:= mdy_hms(str_sub(Perform.DtTm, 1, 20))]
check <- sbk[is.na(Performed.DtTm)]
sbk$SID <- as.character(sbk$SID)

sbk <- merge(sbk, sbkres, by = "SID", all.x = T)
sbk[,V9:=NULL]
write.csv(sbk, "H:/GEMINI/Data/SBK/Radiology/sbk.rad.csv",
          row.names = F)


sbk.freq <- data.frame(table(sbk$Test.Name))
sbk <- readg(sbk, rad.csv)

# UHN 
uhnip <- readg(UHN, rad_ip)
uhner <- readg(UHN, rad_er)



#-------------- Frequency table of smh radiology test names --------------------

library(gemini)
lib.pa()
rm(list = ls())

smh.ango <- readg(smh, ango)
smh.ct <- readg(smh, ct)
smh.mri <- readg(smh, mri)
smh.nuc <- readg(smh, nuc)
smh.us <- readg(smh.us, us)
smh.xray <- readg(smh, xray)

smh.rad <- rbind(smh.ango, smh.ct, smh.mri, 
                 smh.nuc, smh.us, smh.xray)

smh.rad.freq <- table(smh.rad$proc_desc_long) %>% data.table
names(smh.rad.freq) <- c("Test.Name", "Freq")
fwrite(smh.rad.freq, "H:/GEMINI/Results/Ad Hoc/smh.rad.freq.csv")

fwrite(smh.rad, "H:/GEMINI/Data/SMH/Radiology/smh.rad.csv")




# -------------- frequency tables for further use ------------------------------
smh.ct <- readg(smh, ct)
sbk.rad <-readg(sbk, rad.csv)
uhn.rad <- rbind(readg(UHN, rad_ip),
                 readg(UHN, rad_er))
msh.rad <- rbind(readg(msh, rad_er),
                 readg(msh, rad_ip))
## CT
data.table(table(smh.ct[,.(proc_desc_long,body_part_mne)]))[N!=0] %>%
  fwrite("H:/GEMINI/Results/DataSummary/clinical freq tables/ct.smh.csv")
data.table(table(sbk.rad[,.(Test.Name, Test.Code)]))[N!=0] %>%
  fwrite("H:/GEMINI/Results/DataSummary/clinical freq tables/rad.sbk.csv")
data.table(table(uhn.rad[str_sub(ProcedureName, 1, 2)=="CT", ProcedureName])) %>%
  rename(ProcedureName = V1) %>%
  fwrite("H:/GEMINI/Results/DataSummary/clinical freq tables/ct.uhn.csv")
data.table(table(msh.rad[str_sub(ProcedureName, 1, 2)=="CT", ProcedureName])) %>%
  rename(ProcedureName = V1) %>%
  fwrite("H:/GEMINI/Results/DataSummary/clinical freq tables/ct.msh.csv")

## XRAY
smh.xray <- readg(smh, xray)
data.table(table(smh.xray[,.(proc_desc_long,body_part_mne)]))[N!=0] %>%
  fwrite("H:/GEMINI/Results/DataSummary/clinical freq tables/xray.smh.csv")
data.table(table(uhn.rad[str_sub(ProcedureName, 1, 2)=="XR", ProcedureName])) %>%
  rename(ProcedureName = V1) %>%
  fwrite("H:/GEMINI/Results/DataSummary/clinical freq tables/xr.uhn.csv")
data.table(table(msh.rad[str_sub(ProcedureName, 1, 2)=="XR", ProcedureName])) %>%
  rename(ProcedureName = V1) %>%
  fwrite("H:/GEMINI/Results/DataSummary/clinical freq tables/xr.msh.csv")

