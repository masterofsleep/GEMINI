################################################################################
# check the relationship between the blood transfusion and blood in ip dad  ####
################################################################################

library(gemini)
lib.pa()


#-------------------------------SMH---------------------------------------------
swdh("SMH")
files <- list.files(recursive = T);list.files(recursive = T)
dad <- fread(files[10])
bb <- fread(files[1])

length(unique(bb$EncID.new)) #2014
length(unique(dad$EncID.new)) #18972
sum(unique(bb$EncID.new)%in%dad$EncID.new) #2014



#-------------------------------SBK---------------------------------------------
swdh("SBK")
files <- list.files(recursive = T);list.files(recursive = T)
dad <- fread(files[11])
bb <- fread(files[2])
#

#-------------------------------UHN---------------------------------------------
swdh("UHN")
files <- list.files(recursive = T);list.files(recursive = T)
dad <- fread(files[9], na.strings = c(NA, "", NULL))
bb <- fread(files[2], na.strings = c(NA, "", NULL))

length(unique(bb$EncID.new))
length(unique(dad$EncID.new))
sum(is.na(bb$NACRSRegistrationNumber))


sum(unique(bb$EncID.new)%in%dad$EncID.new)
sum(bb$EncID.new[is.na(bb$NACRSRegistrationNumber)] %in% 
  bb$EncID.new[!is.na(bb$NACRSRegistrationNumber)])

df <- data.frame(apply(dad[,c(21:26), with = F], MARGIN = 2, FUN = function(x)sum(x=="Y")))
sum(!((dad[,21,with = F]=="Y") +(dad[,22,with = F]=="Y") +(dad[,23,with = F]=="Y") 
    +(dad[,24,with = F]=="Y") + (dad[,25,with = F]=="Y") +(dad[,26,with = F]=="Y"))==0)

sort(table(bb$EncID.new), decreasing = T)[1:5]

bb <- unique(bb)
write.csv(bb, "CIHI/uhn.blood.nophi.csv", row.names = F)


#-------------------------------THP---------------------------------------------
swdh("THP")
files <- list.files(recursive = T);list.files(recursive = T)
dad <- fread(files[3], na.strings = c(NA, "", NULL))
bb <- fread(files[2], na.strings = c(NA, "", NULL))
er <- fread(files[4], na.strings = c(NA, "", NULL))

length(unique(bb$EncID.new))
length(unique(dad$EncID.new))
sum(is.na(bb$NACRSRegistrationNumber))
sum(is.na(bb$EncID.new))
sum(unique(bb$EncID.new)%in%dad$EncID.new)


sum(bb$EncID.new %in% dad$EncID.new[dad$BloodTransfusionIndicator_DAD=="Y"])
sum(bb$EncID.new %in% er$EncID.new[er$Blood_Transfusion_in_ED == "Y"])

sum((bb$EncID.new %in% dad$EncID.new[dad$BloodTransfusionIndicator_DAD=="Y"])&
      (bb$EncID.new %in% er$EncID.new[er$Blood_Transfusion_in_ED == "Y"]))

#see if the missing encids have NACRS# in er
bb$NACRSRegistrationNumber[is.na(bb$EncID.new)][bb$NACRSRegistrationNumber[is.na(bb$EncID.new)]%in% er$NACRSRegistrationNumber]
#one of the rows with missing ENCid in bb has a corresponding encid in er by 
#comparing the NACRSRegistration#
er[NACRSRegistrationNumber =="ER086180/14"]

#union of blood transfusion in both er and ip
enc_bb <- unique(c(dad$EncID.new[dad$BloodTransfusionIndicator_DAD=="Y"], 
         er$EncID.new[er$Blood_Transfusion_in_ED == "Y"] ))

