################################################################################
##################Unify data format and variable names##########################
################################################################################

rm(list = ls())
library(gemini)
lib.pa()
#===================GIM_IP_2010_2015 ===========================================
#------------available for SMH, SBK, UHN, THP ----------------------------------
# create full sbk.ip data by merging sbk.adm and sbk.Cost
sbk.cost <- readg(sbk, Cost)
sbk.adm <- readg(sbk, adm)
sbk.adm.new <- merge(sbk.adm, sbk.cost, by = "EncID.new", all.x = T, all.y = T)
write.csv(sbk.adm.new, "H:/GEMINI/Data/SBK/CIHI/sbk.adm.nophi.csv", 
          row.names = F)



#remove extra columns from smh
smh.adm <- readg(smh, adm)
smh.adm.new <- smh.adm[,c(1:16), with = F]
write.csv(smh.adm.new, "H:/GEMINI/Data/SMH/CIHI/smh.adm.nophi.csv", 
          row.names = F)

#remove extra encid from thp
thp.adm <- readg(thp, adm)
thp.adm.new <- thp.adm[, 1:18, with = F]
write.csv(thp.adm.new, "H:/GEMINI/Data/THP/CIHI/thp.adm.nophi.csv", 
          row.names = F)


#Variable names
c("EncID.new", "NACRSRegistrationNumber", "Institution", "City", "Province", 
  "Country", "Admitting.Service", "Discharging.Service","Postal.Code",
  "Language", "Admitting.Code", "Discharging.Code", "Total.Direct.Cost",
  "Total.Indirect.Cost", "Total.Cost", "Special.Care.Unit.Death.Indicator",
  "Glasgow.Coma.Scale", "Hash")
#-------------------- Compare variable names -----------------------------------

rm(list = ls())
smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
thp.adm <- readg(thp, adm)

names(smh.adm)
names(smh.adm) <- c("NACRSRegistrationNumber", "City", "Province", 
                    "Country", "Admitting.Service", "Discharging.Service",
                    "Language","Total.Direct.Cost","Total.Indirect.Cost", 
                    "Total.Cost","Special.Care.Unit.Death.Indicator",
                    "Glasgow.Coma.Scale","Hash", "Discharging.Code",
                    "Admitting.Code", "EncID.new")
write.csv(smh.adm, "H:/GEMINI/Data/SMH/CIHI/smh.adm.nophi.csv", 
          row.names = F)

names(sbk.adm)
names(sbk.adm)[6:10] <- c("Admitting.Service", "Discharging.Service", 
                          "Language", "Hash", "Admitting.Code")
names(sbk.adm)[2] <- "NACRSRegistrationNumber"
sbk.adm[, Total.Direct.Cost:= as.numeric(str_replace_all(Total.Direct.Cost, 
                                                       ",", ""))]
sbk.adm[, Total.Indirect.Cost:= as.numeric(str_replace_all(Total.Indirect.Cost, 
                                                         ",", ""))]
sbk.adm[, Total.Cost:= as.numeric(str_replace_all(Total.Cost, 
                                                         ",", ""))]

write.csv(sbk.adm, "H:/GEMINI/Data/SBK/CIHI/sbk.adm.nophi.csv", 
          row.names = F)
  
  
names(uhn.adm)
names(uhn.adm) <- c("NACRSRegistrationNumber", "City", "Province", 
                    "Country", "Admitting.Service", "Discharging.Service",
                    "Language","Total.Direct.Cost","Total.Indirect.Cost", 
                    "Total.Cost","Special.Care.Unit.Death.Indicator",
                    "Glasgow.Coma.Scale","Hash", "Discharging.Code",
                    "Admitting.Code", "EncID.new")
write.csv(uhn.adm, "H:/GEMINI/Data/UHN/CIHI/uhn.adm.nophi.csv", 
          row.names = F)


names(thp.adm)[2:18] <- c("NACRSRegistrationNumber", "City", "Province", 
                    "Country", "Admitting.Service", "Discharging.Service",
                    "Postal.Code",
                    "Language","Total.Direct.Cost","Total.Indirect.Cost", 
                    "Total.Cost","Special.Care.Unit.Death.Indicator",
                    "Glasgow.Coma.Scale","Hash", "Discharging.Code",
                    "Admitting.Code", "EncID.new")
thp.adm <- thp.adm[!duplicated(thp.adm)]
write.csv(thp.adm, "H:/GEMINI/Data/THP/CIHI/thp.adm.nophi.csv", 
          row.names = F, na = "")

apply(smh.adm, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk.adm, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn.adm, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(thp.adm, MARGIN = 2, FUN = function(x)sum(is.na(x)))


rm(list = ls())
smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
thp.adm <- readg(thp, adm)

names(smh.adm)
names(sbk.adm)
names(uhn.adm)
names(thp.adm)






#=======================GIM_IP_DAD =============================================
rm(list = ls())
smh.dad <- readg(smh, dad)
sbk.dad <- readg(sbk, dad)
uhn.dad <- readg(uhn, dad)
msh.dad <- readg(msh, dad)
thp.dad <- readg(thp, dad)

#check dupliates in msh.dad
sort(table(msh.dad$EncID.new), decreasing = T)[1:5]
#remove duplicates in msh.dad
msh.dad <- unique(msh.dad)


#merge sbk blood with dad
sbk.bld <- readg(sbk, blood)
sbk.dad.new <- merge(sbk.dad, sbk.bld[,9:16, with = F], by = "EncID.new")
sbk.dad.new <- sbk.dad.new[,c(2:28, 1), with = F]
write.csv(sbk.dad.new, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_dad.nophi.csv", 
          row.names = F)


#remove two extra discharge date in thp
thp.dad <- thp.dad[, -c(26, 27), with = F]
write.csv(thp.dad, "H:/GEMINI/Data/THP/CIHI/thp.ip_dad.nophi.csv", 
          row.names = F)

c("Admit.Date","Admit.Time", "Discharge.Date","Discharge.Time",
  "Admission.Category","Discharge.Disposition", "Responsibility.for.Payment",
  "Province.Territory.Issuing.Health.Care.Number", "Number.of.ALC.Days",
  "InstitutionFrom","InstitutionFrom.Type", "InstitutionTo", "InstitutionTo.Type",
  "Readmission", "Residence.Code", "Gender","Age","MostResponsible.DocterCode",
  "MostResponsible.DocterService","Entry.Code", "Blood.Transfusion.Indicator_DAD",
  "RBC", "Plt", "Plasma", "Albumin", "Other","Auto.Transfusion", "EncID.new")
rm(list = ls())
smh.dad <- readg(smh, dad)
sbk.dad <- readg(sbk, dad)
uhn.dad <- readg(uhn, dad)
msh.dad <- readg(msh, dad)
thp.dad <- readg(thp, dad)
names(smh.dad) <- c("Admit.Date","Admit.Time", "Discharge.Date","Discharge.Time",
                    "Admission.Category","Discharge.Disposition", "Responsibility.for.Payment",
                    "Province.Territory.Issuing.Health.Care.Number", "Number.of.ALC.Days",
                    "InstitutionFrom","InstitutionFrom.Type", "InstitutionTo", "InstitutionTo.Type",
                    "Readmission", "Residence.Code", "Gender","Age","MostResponsible.DocterCode",
                    "MostResponsible.DocterService","Entry.Code", "Blood.Transfusion.Indicator_DAD",
                    "RBC", "Plt", "Plasma", "Albumin", "Other","Auto.Transfusion", "EncID.new")
write.csv(smh.dad, "H:/GEMINI/Data/SMH/CIHI/smh.ip_dad.nophi.csv", 
          row.names = F)


names(sbk.dad) <- c("Admit.Date","Admit.Time", "Discharge.Date","Discharge.Time",
                    "Admission.Category","Discharge.Disposition", "Responsibility.for.Payment",
                    "Province.Territory.Issuing.Health.Care.Number", "Number.of.ALC.Days",
                    "InstitutionFrom","InstitutionFrom.Type", "InstitutionTo", "InstitutionTo.Type",
                    "Readmission", "Residence.Code", "Gender","Age","MostResponsible.DocterCode",
                    "MostResponsible.DocterService","Entry.Code", "Blood.Transfusion.Indicator_DAD",
                    "RBC", "Plt", "Plasma", "Albumin", "Other","Auto.Transfusion", "EncID.new")
write.csv(sbk.dad, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_dad.nophi.csv", 
          row.names = F)

names(uhn.dad) <- c("Admit.Date","Admit.Time", "Discharge.Date","Discharge.Time",
                    "Admission.Category","Discharge.Disposition", "Responsibility.for.Payment",
                    "Province.Territory.Issuing.Health.Care.Number", "Number.of.ALC.Days",
                    "InstitutionFrom","InstitutionFrom.Type", "InstitutionTo", "InstitutionTo.Type",
                    "Readmission", "Residence.Code", "Gender","Age","MostResponsible.DocterCode",
                    "MostResponsible.DocterService","Entry.Code", 
                    "RBC", "Plt", "Plasma", "Albumin", "Other","Auto.Transfusion", "EncID.new")
write.csv(uhn.dad, "H:/GEMINI/Data/UHN/CIHI/uhn.ip_dad.nophi.csv", 
          row.names = F)

names(msh.dad) <- c("Admit.Date","Admit.Time", "Discharge.Date","Discharge.Time",
                    "Admission.Category","Discharge.Disposition", "Responsibility.for.Payment",
                    "Province.Territory.Issuing.Health.Care.Number", "Number.of.ALC.Days",
                    "InstitutionFrom","InstitutionFrom.Type", "InstitutionTo", "InstitutionTo.Type",
                    "Readmission", "Residence.Code", "Gender","Age","MostResponsible.DocterCode",
                    "MostResponsible.DocterService","Entry.Code", "Blood.Transfusion.Indicator_DAD",
                    "RBC", "Plt", "Plasma", "Albumin", "Other","Auto.Transfusion", "EncID.new", "Site")
msh.dad <- msh.dad[!duplicated(msh.dad),]
write.csv(msh.dad, "H:/GEMINI/Data/MSH/CIHI/msh.ip_dad.nophi.csv", 
          row.names = F)


names(thp.dad) <- c("Admit.Date","Admit.Time", "Discharge.Date","Discharge.Time",
                    "Admission.Category","Discharge.Disposition", "Responsibility.for.Payment",
                    "Number.of.ALC.Days",
                    "InstitutionFrom","InstitutionFrom.Type", "InstitutionTo", "InstitutionTo.Type",
                    "Readmission", "Residence.Code", "Gender","Age",
                    "MostResponsible.DocterService","Entry.Code", "Blood.Transfusion.Indicator_DAD",
                    "RBC", "Plt", "Plasma", "Albumin", "Other","Auto.Transfusion", 
                    "MostResponsible.DocterCode","EncID.new")
write.csv(thp.dad, "H:/GEMINI/Data/THP/CIHI/thp.ip_dad.nophi.csv", 
          row.names = F)


names(smh.dad)
names(sbk.dad)
names(uhn.dad)
names(msh.dad)
names(thp.dad)

apply(smh.dad, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk.dad, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn.dad, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(msh.dad, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(thp.dad, MARGIN = 2, FUN = function(x)sum(is.na(x)))


















#-----------------Dec 02 2016 --------------------------------------------------
#--------------- Further formatting --------------------------------------------
rm(list = ls())
smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
thp.adm <- readg(thp, adm)

smh.ch <- smh.adm[is.na(Country)]

sbk.ch <- sbk.adm[is.na(Country)]
table(sbk.ch$City)
table(sbk.ch$Province)
table(sbk.adm$Country)




#------------ Dec 05 2016 ------------------------------------------------------
#------------ Replace old hash with new ones -----------------------------------
rm(list = ls())
smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
thp.adm <- readg(thp, adm)

swdr("SMH/CIHI/")
link <- fread("SMH.LINKLIST_NEWHASH.csv")
link$EncID.new <- paste("11", link$EncID.new, sep = "")
smh.adm <-merge(smh.adm, link[,.(EncID.new, newHash)], by = "EncID.new", 
                all.x = T, all.y = F)

smh.adm[,Hash:= newHash]
smh.adm[, newHash:=NULL]
write.csv(smh.adm, "H:/GEMINI/Data/SMH/CIHI/smh.adm.nophi.csv", 
          row.names = F)

smh.adm[Hash == "c3ed0844860fb77e4fcacbc5124ad71bede04a0579a862a5301a8dd132957692"]





swdr("SBK/CIHI/")
link <- readxl::read_excel("SBK.NewHASH.xlsx")
link$EncID.new <- paste("12", link$EncID.new, sep = "")
sbk.adm <- merge(sbk.adm, link[,c("EncID.new", "newHash")], by = "EncID.new", 
                 all.x = T, all.y = F)
sbk.adm[,Hash:= newHash]
sbk.adm[, newHash:=NULL]
sbk.adm[Hash == "c3ed0844860fb77e4fcacbc5124ad71bede04a0579a862a5301a8dd132957692"]
write.csv(sbk.adm, "H:/GEMINI/Data/SBK/CIHI/sbk.adm.nophi.csv", 
          row.names = F)



swdr("UHN/CIHI/")
link <- fread("uhn.newHash.csv")
link$EncID.new <- paste("13", link$EncID.new, sep = "")
uhn.adm <- merge(uhn.adm, link[,.(EncID.new, newHash)], by = "EncID.new", 
                 all.x = T, all.y = F)
uhn.adm[,Hash:= newHash]
uhn.adm[, newHash:=NULL]
uhn.adm[Hash == "c3ed0844860fb77e4fcacbc5124ad71bede04a0579a862a5301a8dd132957692"]
write.csv(uhn.adm, "H:/GEMINI/Data/UHN/CIHI/uhn.adm.nophi.csv", 
          row.names = F)



#----------------- Dec 09 2016 -------------------------------------------------
#date and time formatting of dad files
smh.dad <- readg(smh, dad)
sbk.dad <- readg(sbk, dad)
uhn.dad <- readg(uhn, dad)
msh.dad <- readg(msh, dad)
thp.dad <- readg(thp, dad)
smh.dad[,`:=`(Admit.Date = mdy(Admit.Date), Discharge.Date = mdy(Discharge.Date))]
sbk.dad[,`:=`(Admit.Date = mdy(Admit.Date), Discharge.Date = mdy(Discharge.Date),
              Admit.Time = paste(Admit.Time, ":00", sep = ""),
              Discharge.Time = paste(Discharge.Time, ":00", sep = ""))]
msh.dad[,`:=`(Admit.Date = mdy(Admit.Date), Discharge.Date = mdy(Discharge.Date),
              Admit.Time = paste(Admit.Time, ":00", sep = ""),
              Discharge.Time = paste(Discharge.Time, ":00", sep = ""))]
thp.dad$Admit.Time <- paste("000", thp.dad$Admit.Time, sep = "")
thp.dad$Admit.Time <- paste(str_sub(thp.dad$Admit.Time, -4, -3), 
                            ":", str_sub(thp.dad$Admit.Time, -2, -1), sep = "")


thp.dad$Discharge.Time <- paste("000", thp.dad$Discharge.Time, sep = "")
thp.dad$Discharge.Time <- paste(str_sub(thp.dad$Discharge.Time, -4, -3), 
                            ":", str_sub(thp.dad$Discharge.Time, -2, -1), sep = "")

thp.dad[,`:=`(Admit.Date = ymd(Admit.Date), Discharge.Date = ymd(Discharge.Date))]

write.csv(smh.dad, "H:/GEMINI/Data/SMH/CIHI/smh.ip_dad.nophi.csv", 
          row.names = F)
write.csv(sbk.dad, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_dad.nophi.csv", 
          row.names = F)
write.csv(msh.dad, "H:/GEMINI/Data/MSH/CIHI/msh.ip_dad.nophi.csv", 
          row.names = F)
write.csv(thp.dad, "H:/GEMINI/Data/THP/CIHI/thp.ip_dad.nophi.csv", 
          row.names = F)






## adding institution number to uhn.adm.nophi
uhn.adm <- readg(uhn, adm)
instnum.uhn <- fread("H:/GEMINI/Data/UHN/CIHI/missing ED info with Visit Facility_processed.csv",
                     colClasses = list(character = c("Visit Number","EncID.new")))
uhn.er <- readg(uhn.er, .er.nophi,
                colClasses = list(character = c("NACRSRegistrationNumber",
                                                "EncID.new")))
instnum.uhn$Institution.Number <- ifelse(instnum.uhn$`Visit Facility`==
                                           "TORONTO GENERAL & PRINCESS MARGARET HOSPITALS PROD",
                                         "54265", "54266")
instnum.uhn$EncID.new <- paste("13", instnum.uhn$EncID.new, sep = "")
uhn.all.inst.num <- rbind(uhn.er[,.(EncID.new, Institution.Number)],
                          instnum.uhn[,.(EncID.new, Institution.Number)])

uhn.all.inst.num <- uhn.all.inst.num[!duplicated(uhn.all.inst.num)]
uhn.adm <- merge(uhn.adm, uhn.all.inst.num, by = "EncID.new")
write.csv(uhn.adm, "H:/GEMINI/Data/UHN/CIHI/uhn.adm.nophi.csv", 
          row.names = F)



## --------- correct the wrong admitting physician code in smh adm -------------
## --------------------- Jan 23 2017 -------------------------------------------

smh.adm <- readg(smh, adm)
smh.adm2 <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/smh.adm.nophi_2.csv")
smh.adm2$EncID.new <- paste("11", smh.adm2$EncID.new, sep = "")
smh.adm <- merge(smh.adm, smh.adm2[,.(ADM.CODE, EncID.new)])

smh.adm[,Admitting.Code:=ADM.CODE]
smh.adm[,ADM.CODE:=NULL]

write.csv(smh.adm, "H:/GEMINI/Data/SMH/CIHI/smh.adm.nophi.csv", 
          row.names = F)







# -------------------- Feb 3 ------ MSH DATA -----------------------------------
library(gemini)
lib.pa()
msh <- fread("R:/GEMINI/_RESTORE/MSH/CIHI/msh.adm.nophi.csv")
# compare with dad
dad <- readg(msh, dad)
sum(is.na(msh$EncID.new))
sum(duplicated(msh$EncID.new))
msh$EncID.new <- paste("14", msh$EncID.new, sep = "")

sum(!msh$EncID.new%in%dad$EncID.new)



# --------------------- feb 14 fix uhn dad date formate ------------------------
uhn <- readg(uhn, dad)
uhn[,':='(Admit.Date = mdy(Admit.Date),
          Discharge.Date = mdy(Discharge.Date))]
fwrite(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.ip_dad.nophi.csv")




# --------------------- feb 24 new varaible map --------------------------------
msh.adm <- readg(msh, adm)
drop <- c("phar", "diet", "trans")
msh.adm[,c("phar", "diet", "trans"):=NULL]
names(msh.adm) <- c("City", "Province", "Country","Language", "Admit_Date", 
                    "Admit_Time", "EncID.new", "newHash")
fwrite(msh.adm, "H:/GEMINI/Data/MSH/CIHI/msh.adm.nophi.csv")
msh.adm[EncID.new%in%msh.adm$EncID.new[duplicated(msh.adm$EncID.new)]] -> check
check[!duplicated(paste(check$EncID.new, check$newHash))] %>% fwrite("R:/GEMINI/Check/msh.discre.enc.hash.csv")
library(gemini)
lib.pa()
smh <- readg(smh, adm)
sbk <- readg(sbk, adm)
uhn <- readg(uhn, adm)
msh <- readg(msh, adm)
thp<- readg(thp, adm)
apply(smh, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(thp, MARGIN = 2, FUN = function(x) sum(is.na(x)))

smh <- readg(smh, dad)
sbk <- readg(sbk, dad)
uhn <- readg(uhn, dad)
msh <- readg(msh, dad)
thp<- readg(thp, dad)
apply(smh, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x) sum(is.na(x)))
apply(thp, MARGIN = 2, FUN = function(x) sum(is.na(x)))


# ------------------------- fix dup encounters in msh adm ----------------------
msh <- readg(msh, adm) %>% unique
sum(duplicated(msh$EncID.new)) 

msh.adm[duplicated(EncID.new)] -> msh.hcn.ex
fwrite(msh.hcn.ex, "H:/GEMINI/Results/DataSummary/to.exclude/msh.extra.hcn.csv")


msh.adm <- msh.adm[!duplicated(EncID.new)]
names(msh.adm)[5:8] <- c("Admit.Date", "Admit.Time", "EncID.new", "Hash")
fwrite(msh.adm, "H:/GEMINI/Data/MSH/CIHI/msh.adm.nophi.csv")

thp.adm <- readg(thp, adm)
smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
hcn <- rbind(smh.adm[, .(Hash, EncID.new)],
             sbk.adm[, .(Hash, EncID.new)],
             uhn.adm[, .(Hash, EncID.new)],
             thp.adm[, .(Hash, EncID.new)])
msh.hcn.ex$newHash%in%hcn$Hash


# ------------------------ create merged file for db ---------------------------
smh.dad <- readg(smh, dad)
sbk.dad <- readg(sbk, dad)
uhn.dad <- readg(uhn, dad)
msh.dad <- readg(msh, dad)
thp.dad <- readg(thp, dad)

uhn.dad[, Blood.Transfusion.Indicator_DAD:= 
          ifelse(RBC=="Y"|
                 Plt=="Y"|
                 Plasma=="Y"|
                 Albumin=="Y"|
                 Other=="Y"|
                 Auto.Transfusion=="Y", "Y", "N")]

vars <- c("EncID.new", "Gender", "Age",  
          "Admit.Date", "Admit.Time", "Discharge.Date", 
          "Discharge.Time", "Admission.Category", "Discharge.Disposition", 
          "Responsibility.for.Payment", "Number.of.ALC.Days",
          "InstitutionFrom", "InstitutionFrom.Type", 
          "InstitutionTo","InstitutionTo.Type", "Readmission", 
          "Residence.Code", "MostResponsible.DocterService",
          "Blood.Transfusion.Indicator_DAD")
dad <- rbind(smh.dad[,vars, with = F],
             sbk.dad[,vars, with = F],
             uhn.dad[,vars, with = F],
             msh.dad[,vars, with = F],
             thp.dad[,vars, with = F])
ex <- readg(gim, notgim)
dad <- dad[!EncID.new%in%ex$EncID.new]

con = dbConnect(SQLite(), dbname = "gemini.db")
dbWriteTable(con, "dad", dad)

dbListTables(con)
test <- dbReadTable(con, "dad")





thp.adm <- readg(thp, adm)
smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
msh.adm <- readg(msh, adm)



adm <- rbind(smh.adm[,.(EncID.new, NACRSRegistrationNumber,
                        Total.Direct.Cost, Total.Indirect.Cost, Total.Cost,
                        Hash)],
             sbk.adm[,.(EncID.new, NACRSRegistrationNumber,
                        Total.Direct.Cost, Total.Indirect.Cost, Total.Cost,
                        Hash)],
             uhn.adm[,.(EncID.new, NACRSRegistrationNumber,
                        Total.Direct.Cost, Total.Indirect.Cost, Total.Cost,
                        Hash)],
             msh.adm[,.(EncID.new,
                        Hash)],
             thp.adm[,.(EncID.new, NACRSRegistrationNumber,
                        Total.Direct.Cost, Total.Indirect.Cost, Total.Cost,
                        Hash)], fill = T)
adm <- adm[!EncID.new%in%ex$EncID.new]
library(RSQLite)
library(DBI)
setwd("C:/Users/guoyi/sqlite")
con = dbConnect(RSQLite::SQLite(), dbname = "gemini.db")
dbListTables(con)
dbDisconnect(con)
adm <- dbReadTable(con, "adm")
fwrite(adm, "H:/GEMINI/Data/GEMINI/gim.adm.csv")
dad <- dbReadTable(con, "dad") %>% data.table
dad[, Length.of.Stay := as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time)) - 
      ymd_hm(paste(Admit.Date, Admit.Time)))/(3600*24)]
dad[Number.of.ALC.Days=="1,105", Number.of.ALC.Days:=1105]
dbWriteTable(con, "dad", dad, overwrite = T)
fwrite(dad, "H:/GEMINI/Data/GEMINI/gim.dad.csv")


apply(dad, 2, function(x)sum(is.na(x)))


# ----------------------- check hash for missing values ------------------------
adm <- readg(gim, adm)
nvisit = adm[,.N, by = Hash]
nvisit <- nvisit[order(N, decreasing = T)]

fwrite(nvisit[N>10], "H:/GEMINI/Results/Check/hash.freq.csv")

nvisit <- fread("H:/GEMINI/Results/Check/hash.freq.csv")
dad <- readg(gim, dad)
demo<- merge(adm, dad, by = "EncID.new")

demo[Hash%in%nvisit$Hash[3]] -> check
for(i in nvisit$Hash){
  print(max(demo[Hash==i, Age])-min(demo[Hash==i, Age]))
  if(length(unique(demo[Hash==i, Gender]))>1)
    print(i)
}
adm[Hash%in%nvisit$Hash[c(1,2,4)], Hash := NA]
setwd("C:/Users/guoyi/sqlite")
con = dbConnect(RSQLite::SQLite(), dbname = "gemini.db")
dbListTables(con)
dbDisconnect(con)
fwrite(adm, "H:/GEMINI/Data/GEMINI/gim.adm.csv")
