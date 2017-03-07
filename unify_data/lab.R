
#================================ LAB ==========================================
#-----------------------  available for SMH, SBK  ------------------------------

library(gemini)
lib.pa()
rm(list = ls())

smh <- readg(smh, corelabs)
names(smh)
apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))



sbkip <- readg(sbk, labs_ip)
names(sbkip)
apply(sbkip, MARGIN = 2, FUN = function(x)sum(is.na(x)))

sbker <- readg(sbk, labs_er)
names(sbker)
apply(sbker, MARGIN = 2, FUN = function(x)sum(is.na(x)))

check <- smh[is.na(CollectedDtTm)]



uhnip <- readg(uhn, labs_ip)
uhner <- readg(uhn, labs_er)


table(c(uhnip$Test.Name, uhner$Test.Name))




#formatting all data
smh <- readg(smh, corelabs)
sbkip <- readg(sbk, labs_ip)
sbker <- readg(sbk, labs_er)


names(smh)
names(smh)[4] <- "CollectedDtTm"

smh[, Collection.DtTm:=mdy_hm(CollectedDtTm)]
smh[,Collection.DtTm:=NULL]
smh[str_sub(CollectedDtTm, -2, -1)=="PM"&str_sub(CollectedDtTm, -8, -7)<"12",
    Collection.DtTm:=(mdy_hm(str_sub(CollectedDtTm, 1, 13)) + hours(12))]
smh[!(str_sub(CollectedDtTm, -2, -1)=="PM"&str_sub(CollectedDtTm, -8, -7)=="12"), 
    Collection.DtTm:=mdy_hm(str_sub(CollectedDtTm, 1, 13))]
smh[str_sub(CollectedDtTm, -2, -1)=="AM"&str_sub(CollectedDtTm, -8, -7)=="12"], 
    Collection.DtTm:=mdy_hm(str_sub(CollectedDtTm, 1, 13))-hours(12)]
sum(is.na(smh$Collection.DtTm))
check <- smh[is.na(Collection.DtTm)]
smh[is.na(Collection.DtTm), Collection.DtTm := mdy_hm(str_sub(CollectedDtTm, 1, 15))]
range(smh$Collection.DtTm, na.rm = T)
smh[Collection.DtTm>=ymd_hm("2015-04-01 0:00")]
smh[Collection.DtTm<=ymd_hm("2009-04-01 0:00")]
smh[is.na(Collection.DtTm)] ->check
names(smh)[6] <- "Result.Unit"
fwrite(smh, "H:/GEMINI/Data/SMH/Lab/smh.corelabs-linked.csv", row.names = F,
          na = "")


sbkip[,Collection.DtTm:=NULL]
sbkip[str_sub(Collection.Date, -2, -1)=="PM"&str_sub(Collection.Date, -14, -13)<12, 
      Collection.DtTm:= (mdy_hms(str_sub(Collection.Date, 1, 20)) + hours(12))]
sbkip[str_sub(Collection.Date, -2, -1)=="PM"&str_sub(Collection.Date, -14, -13)==12, 
      Collection.DtTm:= (mdy_hms(str_sub(Collection.Date, 1, 20)))]
sbkip[str_sub(Collection.Date, -2, -1)=="AM"&str_sub(Collection.Date, -14, -13)<12, 
      Collection.DtTm:= mdy_hms(str_sub(Collection.Date, 1, 20))]
sbkip[str_sub(Collection.Date, -2, -1)=="AM"&str_sub(Collection.Date, -14, -13)==12, 
      Collection.DtTm:= mdy_hms(str_sub(Collection.Date, 1, 20))-hours(12)]
sbkip[,X:=NULL]
sbkip[,newHash:=NULL]
names(sbkip) <- c("SID","PID","Test.Name", "Test.ID","Collection.Date",
                  "Result.Value", "Result.Unit","Reference.Range", 
                  "EncID.new", "Collection.DtTm")
range(sbkip$Collection.DtTm)
sbkip$Collection.DtTm <- as.character(sbkip$Collection.DtTm)
write.csv(sbkip, "H:/GEMINI/Data/SBK/Lab/sbk.labs_ip.csv", row.names = F, na = "")



sbkip[,Collection.DtTm:=NULL]
sbkip[str_sub(Collection.Date, -2, -1)=="PM"&str_sub(Collection.Date, -14, -13)<12, 
      Collection.DtTm:= (mdy_hms(str_sub(Collection.Date, 1, 20)) + hours(12))]
sbkip[str_sub(Collection.Date, -2, -1)=="PM"&str_sub(Collection.Date, -14, -13)==12, 
      Collection.DtTm:= (mdy_hms(str_sub(Collection.Date, 1, 20)))]
sbkip[str_sub(Collection.Date, -2, -1)=="AM"&str_sub(Collection.Date, -14, -13)<12, 
      Collection.DtTm:= mdy_hms(str_sub(Collection.Date, 1, 20))]
sbkip[str_sub(Collection.Date, -2, -1)=="AM"&str_sub(Collection.Date, -14, -13)==12, 
      Collection.DtTm:= mdy_hms(str_sub(Collection.Date, 1, 20))-hours(12)]
sbker[,newHash:=NULL]
range(sbker$Collection.DtTm)
names(sbker) <- c("PID","SID","Test.Name", "Test.ID","Collection.Date",
                  "Result.Value", "Result.Unit","Reference.Range", 
                  "EncID.new", "Collection.DtTm")
sbker$Collection.DtTm <- as.character(sbker$Collection.DtTm)
write.csv(sbker, "H:/GEMINI/Data/SBK/Lab/sbk.labs_er.csv", row.names = F, na = "")



uhnip <- readg(uhn, labs_ip)
uhner <- readg(uhn, labs_er)

apply(uhnip, MARGIN = 2, FUN = function(x)sum(is.na(x)))
uhnip[str_sub(Test.Date, -5,-5)=="/", Test.Date:= as.character(mdy(Test.Date))]
uhnip[str_sub(Test.Date, -5,-5)!="/", Test.Date:= as.character(ymd(Test.Date))]
range(ymd(uhnip$Test.Date))

uhner[str_sub(Test.Date, -5,-5)=="/", Test.Date:= as.character(mdy(Test.Date))]
uhner[str_sub(Test.Date, -5,-5)!="/", Test.Date:= as.character(ymd(Test.Date))]
range(ymd(uhner$Test.Date), na.rm = T)

apply(uhner, MARGIN = 2, FUN = function(x)sum(is.na(x)))
uhner <- uhner[!is.na(Test.Name)]
uhner[is.na(Test.Date)]

sum(duplicated(uhner))
sum(duplicated(uhnip))

sum(duplicated(rbind(uhnip, uhner)))
uhn <- rbind(uhnip, uhner)
uhn <- uhn[!duplicated(uhn)]

uhn[str_sub(Specimen.Collected.Date, -5,-5)=="/", 
    Specimen.Collected.Date:= as.character(mdy(Specimen.Collected.Date))]
uhn[str_sub(Specimen.Collected.Date, -5,-5)!="/", 
    Specimen.Collected.Date:= as.character(ymd(Specimen.Collected.Date))]
range(ymd(uhn$Specimen.Collected.Date), na.rm = T)
uhn[is.na(Specimen.Collected.Date)]

names(uhn)
names(uhn)[9] <- "Result.Unit"

write.csv(uhn, "H:/GEMINI/Data/UHN/Lab/uhn.labs.csv", row.names = F,
          na = "")



#------------------ Dec 15 2016 ------------------------------------------------
uhn <- readg(uhn, labs)
uhn$Test.Name <- trimws(uhn$Test.Name)
uhn$Test.Item <- trimws(uhn$Test.Item)

write.csv(uhn, "H:/GEMINI/Data/UHN/Lab/uhn.labs.csv", row.names = F,
          na = "")


#one week sample of uhn lab for amol
# Jan 5 2017
uhn <- readg(uhn.labs, labs.csv)
range(ymd(uhn$Specimen.Collected.Date), na.rm = T)
uhn.lab.sample <- 
  uhn[ymd(Specimen.Collected.Date) < ymd("2011-01-08")&ymd(Specimen.Collected.Date)>=ymd("2011-01-01")]

uhn.lab.sample[,EncID.new:=NULL]
fwrite(uhn.lab.sample, "H:/GEMINI/Results/Check/uhn.lab.sample.csv")




#------------------------- Feb 2 2017 ------------------------------------------
# new uhn lab data brought back by Chris
library(gemini)
lib.pa()
uhn.ip <- fread("R:/GEMINI/_RESTORE/UHN/Lab/uhn.labs.ip.csv")
uhn.er <- fread("R:/GEMINI/_RESTORE/UHN/Lab/uhn.labs.er.csv")
uhn.ip$EncID.new <- paste("13", uhn.ip$EncID.new, sep = "")
uhn.er$EncID.new <- paste("13", uhn.er$EncID.new, sep = "")

uhn.dad <- readg(uhn, dad)
uhn.dad$lab <- uhn.dad$EncID.new %in% c(uhn.ip$EncID.new, uhn.er$EncID.new)

ggplot(uhn.dad, aes(x = ymd(Discharge.Date), fill = lab)) + 
  geom_histogram(binwidth = 5)

sum(duplicated(uhn.er))
sum(duplicated(uhn.ip))
sum(duplicated(rbind(uhn.er, uhn.ip)))

uhn.lab <- unique(rbind(uhn.er, uhn.ip))
length(unique(uhn.lab$EncID.new))


fwrite(uhn.lab, "H:/GEMINI/Data/UHN/Lab/uhn.labs.csv", 
          na = "")



#-------------------- feb 22 convert 12 h to 24 h  redo ------------------------
sbkip <- readg(sbk, labs_ip)
sbker <- readg(sbk, labs_er)
sbkip[,Collection.DtTm:=NULL]
sbkip[str_sub(Collection.Date, -2, -1)=="PM"&str_sub(Collection.Date, -14, -13)<12, 
      Collection.DtTm:= (mdy_hms(str_sub(Collection.Date, 1, 20)) + hours(12))]
sbkip[str_sub(Collection.Date, -2, -1)=="PM"&str_sub(Collection.Date, -14, -13)==12, 
      Collection.DtTm:= (mdy_hms(str_sub(Collection.Date, 1, 20)))]
sbkip[str_sub(Collection.Date, -2, -1)=="AM"&str_sub(Collection.Date, -14, -13)<12, 
      Collection.DtTm:= mdy_hms(str_sub(Collection.Date, 1, 20))]
sbkip[str_sub(Collection.Date, -2, -1)=="AM"&str_sub(Collection.Date, -14, -13)==12, 
      Collection.DtTm:= mdy_hms(str_sub(Collection.Date, 1, 20))-hours(12)]
sbkip$Collection.DtTm <- as.character(sbkip$Collection.DtTm)
fwrite(sbkip, "H:/GEMINI/Data/SBK/Lab/sbk.labs_ip.csv", row.names = F, na = "")
range(sbkip$Collection.DtTm)

sbker[,Collection.DtTm:=NULL]
sbker[str_sub(Collection.Date, -2, -1)=="PM"&str_sub(Collection.Date, -14, -13)<12, 
      Collection.DtTm:= (mdy_hms(str_sub(Collection.Date, 1, 20)) + hours(12))]
sbker[str_sub(Collection.Date, -2, -1)=="PM"&str_sub(Collection.Date, -14, -13)==12, 
      Collection.DtTm:= (mdy_hms(str_sub(Collection.Date, 1, 20)))]
sbker[str_sub(Collection.Date, -2, -1)=="AM"&str_sub(Collection.Date, -14, -13)<12, 
      Collection.DtTm:= mdy_hms(str_sub(Collection.Date, 1, 20))]
sbker[str_sub(Collection.Date, -2, -1)=="AM"&str_sub(Collection.Date, -14, -13)==12, 
      Collection.DtTm:= mdy_hms(str_sub(Collection.Date, 1, 20))-hours(12)]
sbker$Collection.DtTm <- as.character(sbker$Collection.DtTm)
fwrite(sbker, "H:/GEMINI/Data/SBK/Lab/sbk.labs_er.csv")
range(sbker$Collection.DtTm)





#formatting all data
smh <- readg(smh, corelabs)
smh[,Collection.DtTm:=NULL]
smh[str_sub(CollectedDtTm, -2, -1)=="PM"&str_sub(CollectedDtTm, -8, -7)<"12",
    Collection.DtTm:=(mdy_hm(str_sub(CollectedDtTm, 1, 13)) + hours(12))]
smh[!(str_sub(CollectedDtTm, -2, -1)=="PM"&str_sub(CollectedDtTm, -8, -7)=="12"), 
    Collection.DtTm:=mdy_hm(str_sub(CollectedDtTm, 1, 13))]
smh[str_sub(CollectedDtTm, -2, -1)=="AM"&str_sub(CollectedDtTm, -8, -7)=="12", 
Collection.DtTm:=mdy_hm(str_sub(CollectedDtTm, 1, 13))-hours(12)]
sum(is.na(smh$Collection.DtTm))
check <- smh[is.na(Collection.DtTm)]
smh[is.na(Collection.DtTm), Collection.DtTm := mdy_hm(str_sub(CollectedDtTm, 1, 15))]
smh[is.na(Collection.DtTm)] ->check
smh$Collection.DtTm <- as.character(smh$Collection.DtTm)
fwrite(smh, "H:/GEMINI/Data/SMH/Lab/smh.corelabs-linked.csv", row.names = F,
       na = "")



# ------------------------- move sinai lab data to local H ---------------------
setwd("R:/GEMINI/_RESTORE/MSH/Labs")
msh.lab <- fread("msh.labs_dad.nophi.csv")
msh.lab$EncID.new <- paste("14", msh.lab$EncID.new, sep = "")
msh.lab[, V1:=NULL]
msh.lab[, X:= NULL]
head(msh.lab, 100)
sum(is.na(msh.lab$REFERANCE_LAB))
fwrite(msh.lab, "H:/GEMINI/Data/MSH/Lab/msh.lab.nophi.csv")



# ----------- march 7 new sbk lab er -------------------------------------------
sbk.lab.er1 <- fread("R:/GEMINI/_RESTORE/SBK/Lab/sbk.labs_er1.csv")
sbk.lab.er2 <- fread("R:/GEMINI/_RESTORE/SBK/Lab/sbk.labs_er2.csv")
sbk.lab.er1$EncID.new <- paste("12", sbk.lab.er1$EncID.new, sep = "")
sbk.lab.er2$EncID.new <- paste("12", sbk.lab.er2$EncID.new, sep = "")

sbk.lab.er <- rbind(sbk.lab.er1, sbk.lab.er2, fill = T)
names(sbk.lab.er) <- c("PID","SID","Test.Name", "Test.ID","Collection.Date",
                  "Result.Value", "Result.Unit","Reference.Range", 
                  "EncID.new")

sbk.lab.er[str_sub(Collection.Date, -2, -1)=="PM"&str_sub(Collection.Date, -14, -13)<12, 
      Collection.DtTm:= (mdy_hms(str_sub(Collection.Date, 1, 20)) + hours(12))]
sbk.lab.er[str_sub(Collection.Date, -2, -1)=="PM"&str_sub(Collection.Date, -14, -13)==12, 
      Collection.DtTm:= (mdy_hms(str_sub(Collection.Date, 1, 20)))]
sbk.lab.er[str_sub(Collection.Date, -2, -1)=="AM"&str_sub(Collection.Date, -14, -13)<12, 
      Collection.DtTm:= mdy_hms(str_sub(Collection.Date, 1, 20))]
sbk.lab.er[str_sub(Collection.Date, -2, -1)=="AM"&str_sub(Collection.Date, -14, -13)==12, 
      Collection.DtTm:= mdy_hms(str_sub(Collection.Date, 1, 20))-hours(12)]
fwrite(sbk.lab.er, "H:/GEMINI/Data/SBK/Lab/sbk.labs_er.csv")
