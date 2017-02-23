################################################################################
##################  check patients without lab values   ########################
################################################################################

rm(list=ls())
library(gemini)
lib.pa()




swdh("SMH")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[2])
lab <- read.csv(files[16])
length(unique(adm$EncID.new))
sum(!adm$EncID.new %in% lab$EncID.new)
sum(!adm$EncID.new %in% lab$EncID.new)/length(unique(adm$EncID.new))
enc_nolab <- adm$EncID.new[!adm$EncID.new%in%lab$EncID.new]
write.csv(enc_nolab, "H:/GEMINI/Results/NoLabMedRad/smh_no_lab_enc.csv", 
          row.names = F)


# a random sample of 20 with no lab values of SMH
setwd("H:/GEMINI/Results/NoLabMedRad")
nolab_smh <- scan("smh_no_lab_enc.csv", sep=",", skip = 1)
swdh("SMH/CIHI")
dad <- fread("smh.ip_dad.nophi.csv")

dad_nolab <- dad[EncID.new%in%nolab_smh, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
dad_nolab_2010 <- dad_nolab[mdy(dad_nolab$Admit.Date)<mdy("1/1/2011")]
set.seed(1)
sample_smh_nolab_2010_enc <- sample(dad_nolab_2010$EncID.new, size = 20, replace = F)
sample_smh_nolab_2010 <- dad_nolab[dad_nolab$EncID.new %in% sample_smh_nolab_2010_enc]
swdr("SMH/CIHI/Archive")
link <- fread("smh.GIM_IP_LINKING_LIST.csv")
sample_smh_nolab_2010 <- merge(sample_smh_nolab_2010, 
                               link[,.(MRN, IPEncounterID, EncID.new)],
                               by = "EncID.new", all.x = T,all.y = F)

write.csv(sample_smh_nolab_2010, "H:/GEMINI/Results/NoLabMedRad/smh_nolab_2010_sample20.csv", 
          row.names = F)


swdh("SBK")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[1])
laber <- read.csv(files[18])
labip <- read.csv(files[19])
enc_lab <- unique(c(laber$EncID.new, labip$EncID.new))
length(unique(adm$EncID.new)) #26834 unique enc id
sum(!adm$EncID.new %in% enc_lab) #244 without lab values
sum(!adm$EncID.new %in% enc_lab)/length(unique(adm$EncID.new)) #0.2060446
enc_nolab <- adm$EncID.new[!adm$EncID.new%in%enc_lab]
write.csv(enc_nolab, "H:/GEMINI/Results/NoLabMedRad/sbk_no_lab_enc.csv", 
          row.names = F)


#detailed summary for patients without lab results
#SMH

setwd("H:/GEMINI/Results/NoLabMedRad")
nolab_smh <- scan("smh_no_lab_enc.csv", sep=",", skip = 1)
swdh("SMH/CIHI")
dad <- fread("smh.ip_dad.nophi.csv")

dad_nolab <- dad[EncID.new%in%nolab_smh, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
dad$Lab <- !dad$EncID.new %in% nolab_smh
ggplot(dad_nolab, aes(Age)) + geom_histogram(binwidth = 1) + ggtitle("Age - No Lab - SMH)")

dad_nolab$LOS <- as.numeric(mdy(dad_nolab$Discharge.Date) - 
                              mdy(dad_nolab$Admit.Date))
ggplot(dad_nolab, aes(LOS)) + geom_histogram(binwidth = 1) + 
  ggtitle("Length of Stay - No Lab - SMH")
ggplot(dad, aes(mdy(Admit.Date), fill = Lab)) + 
  geom_histogram(binwidth = 10,alpha = 0.5) +
  ggtitle("ALL Admission Date - SMH")


#SBK
setwd("H:/GEMINI/Results/NoLabMedRad")
nolab_sbk <- scan("sbk_no_lab_enc.csv", sep=",", skip = 1)
swdh("SBK/CIHI")
dad <- fread("sbk.ip_dad.nophi.csv")
dad$Lab <- !dad$EncID.new%in% as.character(nolab_sbk)
dad_nolab <- dad[EncID.new%in%nolab_sbk, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
ggplot(dad_nolab, aes(Age)) + geom_histogram(binwidth = 1) + ggtitle("Age - No Lab - SBK")
dad_nolab$LOS <- as.numeric(mdy(dad_nolab$Discharge.Date) - 
                              mdy(dad_nolab$Admit.Date))
ggplot(dad_nolab, aes(LOS, fill = Gender)) + 
  geom_histogram(binwidth = 1, alpha=0.5) + 
  ggtitle("Length of Stay - No Lab - SBK")

ggplot(dad_nolab, aes(mdy(Discharge.Date))) + geom_histogram(binwidth = 20) +
  ggtitle("Discharge Date - No Lab - SBK")

ggplot(dad, aes(mdy(Discharge.Date), fill = Lab)) + 
  geom_histogram(binwidth = 20,alpha = 0.5) +
  ggtitle("ALL Discharge Date - SBK")




#a small sample in 2010 to check
sbk.nolab.2010 <- dad_nolab[mdy(dad_nolab$Discharge.Date)<mdy("01/01/2011"), EncID.new]
sbk.nolab.2010.sample20 <- sample(sbk.nolab.2010, size = 20, replace = F)
write.csv(sbk.nolab.2010.sample20, "H:/GEMINI/Results/NoLabMedRad/sbk_nolab_2010_sample20.csv", 
          row.names = F)






####################check no medication results ################################
#SMH, UHN, SBK have phar data available
#-------------------------------  SMH  -----------------------------------------
swdh("SMH")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[2])
med <- fread(files[24], na.strings = c("", NA, NULL))
length(unique(adm$EncID.new))
sum(!adm$EncID.new %in% med$EncID.new)
sum(!adm$EncID.new %in% med$EncID.new)/length(unique(adm$EncID.new))
enc_nomed <- adm$EncID.new[!adm$EncID.new%in%med$EncID.new]
write.csv(enc_nomed, "H:/GEMINI/Results/NoLabMedRad/smh_no_med_enc.csv", 
          row.names = F)


#---------------------------------  SBK  ---------------------------------------
swdh("SBK")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[1])
med <- fread(files[22], na.strings = c("", NA, NULL))
length(unique(adm$EncID.new))
sum(!adm$EncID.new %in% med$EncID.new)
sum(!adm$EncID.new %in% med$EncID.new)/length(unique(adm$EncID.new))
enc_nomed <- adm$EncID.new[!adm$EncID.new%in%med$EncID.new]
write.csv(enc_nomed, "H:/GEMINI/Results/NoLabMedRad/sbk_no_med_enc.csv", 
          row.names = F)


#--------------------------------  UHN  ----------------------------------------
swdh("UHN")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[1])
meder <- fread(files[17], na.strings = c("", NA, NULL))
medip <- fread(files[18], na.strings = c("", NA, NULL))
med <- unique(c(meder$EncID.new, medip$EncID.new))

length(unique(adm$EncID.new))
sum(!adm$EncID.new %in% med)
sum(!adm$EncID.new %in% med)/length(unique(adm$EncID.new))
enc_nomed <- adm$EncID.new[!adm$EncID.new%in%med]
write.csv(enc_nomed, "H:/GEMINI/Results/NoLabMedRad/uhn_no_med_enc.csv", 
          row.names = F)




#---------------summary statistics and visualization----------------------------
#------------------------- --smh- ----------------------------------------------

setwd("H:/GEMINI/Results/NoLabMedRad")
nomed_smh <- scan("smh_no_med_enc.csv", sep=",", skip = 1)
swdh("SMH/CIHI")
dad <- fread("smh.ip_dad.nophi.csv")
dad_nomed <- dad[EncID.new%in%nomed_smh, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
ggplot(dad_nomed, aes(Age)) + geom_histogram(binwidth = 2) + 
  ggtitle("Age - No Med - SMH")

dad_nomed$LOS <- as.numeric(mdy(dad_nomed$Discharge.Date) - 
                              mdy(dad_nomed$Admit.Date))
ggplot(dad_nomed, aes(LOS)) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Length of Stay - No Med - SMH")

ggplot(dad_nomed, aes(mdy(Discharge.Date))) + geom_histogram(binwidth = 20) +
  ggtitle("Discharge Date - No Med - SMH")



#a random sample of 20 with LOS > 0
dad_nomed_los20 <- dad_nomed[dad_nomed$LOS>=0, ]
swdr("SMH/CIHI/Archive")
link <- fread("smh.GIM_IP_LINKING_LIST.csv")
sample_smh_nomed_bylos <- merge(dad_nomed_los20, 
                                link[,.(EncID.new, IPEncounterID, MRN)],
                                by = "EncID.new", all.x = T,
                                all.y = F)

write.csv(sample_smh_nomed_bylos, 
          "H:/GEMINI/Results/NoLabMedRad/smh_nomed_bylos.csv", 
          row.names = F)

#----------------------------------  sbk  --------------------------------------
setwd("H:/GEMINI/Results/NoLabMedRad")
nomed_sbk <- scan("sbk_no_med_enc.csv", sep=",", skip = 1)
swdh("SBK/CIHI")
dad <- fread("sbk.ip_dad.nophi.csv")
dad_nomed <- dad[EncID.new%in%nomed_sbk, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
ggplot(dad_nomed, aes(Age)) + geom_histogram() + ggtitle("Age - No Lab - SBK")

dad_nomed$LOS <- as.numeric(mdy(dad_nomed$Discharge.Date) - 
                              mdy(dad_nomed$Admit.Date))
ggplot(dad_nomed, aes(LOS)) + geom_histogram(binwidth = 1) + 
  ggtitle("Length of Stay - No Med - SBK")

ggplot(dad_nomed, aes(mdy(Admit.Date))) + geom_histogram(binwidth = 20) +
  ggtitle("Admit Date - No Med - SBK")


#--------------------------------  UHN  ----------------------------------------


setwd("H:/GEMINI/Results/NoLabMedRad")
nomed_uhn <- scan("uhn_no_med_enc.csv", sep=",", skip = 1)
swdh("UHN/CIHI")
dad <- fread("uhn.ip_dad.nophi.csv")
dad_nomed <- dad[EncID.new%in%nomed_uhn, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
ggplot(dad_nomed, aes(Age)) + geom_histogram() + ggtitle("Age - No Med - UHN")

dad_nomed$LOS <- as.numeric(ymd(dad_nomed$Discharge.Date) - 
                              ymd(dad_nomed$Admit.Date))
ggplot(dad_nomed, aes(LOS)) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Length of Stay - No Med - UHN")

ggplot(dad_nomed, aes(ymd(Discharge.Date))) + geom_histogram(binwidth = 40) +
  ggtitle("Discharge Date - No Med - UHN")



####################check no radiology results #################################
#--------------------------------SMH -------------------------------------------
swdh("SMH")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[2])
rad1 <- fread(files[25])
rad2 <- fread(files[26])
rad3 <- fread(files[27])
rad4 <- fread(files[28])
rad5 <- fread(files[29])
rad6 <- fread(files[30])
enc_rad <- unique(c(rad1$EncID.new, rad2$EncID.new, rad3$EncID.new, rad4$EncID.new,
                 rad5$EncID.new, rad6$EncID.new))
sum(enc_rad%in%adm$EncID.new) # all 17407 in adm data
sum(!adm$EncID.new %in% enc_rad)
sum(!adm$EncID.new %in% enc_rad)/length(adm$EncID.new)

enc_norad <- adm$EncID.new[!adm$EncID.new %in% enc_rad]
write.csv(enc_norad, "H:/GEMINI/Results/NoLabMedRad/smh_no_rad_enc.csv", 
          row.names = F)

#-------------------------------SBK---------------------------------------------
rm(list = ls())
swdh("SBK")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[1])
#SBK files need to be cleaned first
#----------------clean SBK radiology data --------------------------------------
rader <- fread(files[25])
length(unique(rader$SID))

radip <- fread(files[26])
length(unique(radip$SID))

rad_gemi_result <- fread(files[23], header = F)
length(unique(rad_gemi_result$V1))
sum(unique(rad_gemi_result$V1)%in% rader$SID)
sum(unique(rad_gemi_result$V1)%in% radip$SID)


rad_result <- fread(files[24])
length(unique(rad_result$V1))
sum(unique(rad_result$V1)%in% rader$SID)
sum(unique(rad_result$V1)%in% radip$SID)
sum(unique(rad_result$V1)%in% rad_gemi_result$V1)





#-------find those without rad records for SBK----------------------------------

enc_rad <- unique(c(rader$EncID.new, radip$EncID.new))

sum(enc_rad%in%adm$EncID.new) # all 25386 in adm data
sum(!adm$EncID.new %in% enc_rad)
sum(!adm$EncID.new %in% enc_rad)/length(adm$EncID.new)

enc_norad <- adm$EncID.new[!adm$EncID.new %in% enc_rad]
write.csv(enc_norad, "H:/GEMINI/Results/NoLabMedRad/sbk_no_rad_enc.csv", 
          row.names = F)



#------------------------- UHN -------------------------------------------------
rm(list=ls())
swdh("UHN")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[1])
rader <- fread(files[19])
radip <- fread(files[20])
enc_rad <- unique(c(rader$EncID.new, radip$EncID.new))

sum(enc_rad%in%adm$EncID.new) # all 38024 in adm data
sum(!adm$EncID.new %in% enc_rad)
sum(!adm$EncID.new %in% enc_rad)/length(adm$EncID.new)

enc_norad <- adm$EncID.new[!adm$EncID.new %in% enc_rad]
write.csv(enc_norad, "H:/GEMINI/Results/NoLabMedRad/uhn_no_rad_enc.csv", 
          row.names = F)

#------------------------- MSH -------------------------------------------------
rm(list=ls())
swdh("MSH")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[8])
length(unique(adm$EncID.new))
rader <- fread(files[14])
radip <- fread(files[15])
enc_rad <- unique(c(rader$EncID.new, radip$EncID.new))

sum(enc_rad%in%adm$EncID.new) # all 17312 in adm data
sum(!adm$EncID.new %in% enc_rad)
sum(!adm$EncID.new %in% enc_rad)/length(adm$EncID.new)

enc_norad <- adm$EncID.new[!adm$EncID.new %in% enc_rad]
write.csv(enc_norad, "H:/GEMINI/Results/NoLabMedRad/msh_no_rad_enc.csv", 
          row.names = F)


#-------------Visualization for No Rad -----------------------------------------
#------------------------------SMH----------------------------------------------
setwd("H:/GEMINI/Results/NoLabMedRad")
norad_smh <- scan("smh_no_rad_enc.csv", sep=",", skip = 1)
swdh("SMH/CIHI")
dad <- fread("smh.ip_dad.nophi.csv")
dad_norad <- dad[EncID.new%in%norad_smh, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
ggplot(dad_norad, aes(Age)) + geom_histogram() + ggtitle("Age - No Rad - SMH")

dad_norad$LOS <- as.numeric(mdy(dad_norad$Discharge.Date) - 
                              mdy(dad_norad$Admit.Date))
ggplot(dad_norad, aes(LOS)) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Length of Stay - No Rad - SMH")

ggplot(dad_norad, aes(mdy(Discharge.Date))) + geom_histogram(binwidth = 20) +
  ggtitle("Discharge Date - No Rad - SMH")




#a random sample of 20 with LOS > 0
dad_norad_los <- dad_norad[dad_norad$LOS>0, ]
swdr("SMH/CIHI/Archive")
link <- fread("smh.GIM_IP_LINKING_LIST.csv")
sample_smh_norad_bylos <- merge(dad_norad_los, 
                                link[,.(EncID.new, IPEncounterID, MRN)],
                                by = "EncID.new", all.x = T,
                                all.y = F)

write.csv(sample_smh_norad_bylos, 
          "H:/GEMINI/Results/NoLabMedRad/smh_norad_bylos.csv", 
          row.names = F)

#--------------------------------SBK -------------------------------------------
setwd("H:/GEMINI/Results/NoLabMedRad")
norad_sbk <- scan("sbk_no_rad_enc.csv", sep=",", skip = 1)
swdh("SBK/CIHI")
dad <- fread("sbk.ip_dad.nophi.csv")
dad_norad <- dad[EncID.new%in%norad_sbk, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
ggplot(dad_norad, aes(Age)) + geom_histogram() + ggtitle("Age - No Rad - SBK")

dad_norad$LOS <- as.numeric(mdy(dad_norad$Discharge.Date) - 
                              mdy(dad_norad$Admit.Date))
ggplot(dad_norad, aes(LOS)) + geom_histogram(binwidth =0.5) + 
  ggtitle("Length of Stay - No Rad - SBK")

ggplot(dad_norad, aes(mdy(Admit.Date), fill = Gender)) + 
  geom_histogram(binwidth = 30, alpha = 0.5) +
  ggtitle("Admit Date - No Rad - SBK")


#-------------------------------- UHN ------------------------------------------
setwd("H:/GEMINI/Results/NoLabMedRad")
norad_uhn <- scan("uhn_no_rad_enc.csv", sep=",", skip = 1)
swdh("UHN/CIHI")
dad <- fread("uhn.ip_dad.nophi.csv")
dad_norad <- dad[EncID.new%in%norad_uhn, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
dad_norad$LOS <- as.numeric(ymd(dad_norad$Discharge.Date) - 
                              ymd(dad_norad$Admit.Date))
ggplot(dad_norad, aes(LOS)) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Length of Stay - No Rad - UHN")

ggplot(dad_norad, aes(ymd(Discharge.Date), fill = Gender)) + 
  geom_histogram(binwidth = 40, alpha = 0.5) +
  ggtitle("Discharge Date - No Rad - UHN")


#-------------------------------- MSH ------------------------------------------
setwd("H:/GEMINI/Results/NoLabMedRad")
norad_msh <- scan("msh_no_rad_enc.csv", sep=",", skip = 1)
swdh("MSH/CIHI")
dad <- fread("msh.ip_dad.nophi.csv")
dad_norad <- dad[EncID.new%in%norad_msh, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
dad_norad$LOS <- as.numeric(mdy(dad_norad$Discharge.Date) - 
                              mdy(dad_norad$Admit.Date))
ggplot(dad_norad, aes(LOS)) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Length of Stay - No rad - MSH")

ggplot(dad_norad, aes(mdy(Discharge.Date))) + geom_histogram(binwidth = 20) +
  ggtitle("Discharge Date - No rad - MSH")



#************************ No vitals for SMH ************************************
rm(list=ls())
swdh("SMH")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[2])
vitals <- fread(files[32])

sum(!adm$EncID.new %in% vitals$EncID.new)/18972
enc_vit <- unique(vitals$EncID.new)
write.csv(enc_vit, "H:/GEMINI/Results/NoLabMedRad/smh_vit_enc.csv", 
          row.names = F)
enc_novit <- adm$EncID.new[!adm$EncID.new %in% vitals$EncID.new]
write.csv(enc_novit, "H:/GEMINI/Results/NoLabMedRad/smh_no_vit_enc.csv", 
          row.names = F)

#---------------------summary and visualizations -------------------------------
setwd("H:/GEMINI/Results/NoLabMedRad")
novit_smh <- scan("smh_no_vit_enc.csv", sep=",", skip = 1)
vit_smh <- scan("smh_vit_enc.csv", sep=",", skip = 1)
swdh("SMH/CIHI")
dad <- fread("smh.ip_dad.nophi.csv")
dad_novit <- dad[EncID.new%in%novit_smh, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]

dad_novit$LOS <- as.numeric(mdy(dad_novit$Discharge.Date) - 
                              mdy(dad_novit$Admit.Date))
dad_vit <- dad[EncID.new%in%vit_smh, .(Admit.Date, Discharge.Date, Gender, 
                                       Age, EncID.new)]
ggplot(dad_novit, aes(LOS)) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Length of Stay - No Vital- SMH")
ggplot(dad_novit, aes(mdy(Discharge.Date))) + geom_histogram(binwidth = 10) +
  ggtitle("Discharge Date - No Vital - SMH")
dad$vit <- dad$EncID.new%in% vit_smh
ggplot(dad, aes(x = mdy(Admit.Date), fill = vit)) +
  geom_histogram(binwidth = 30, alpha = 0.5) +
  ggtitle("All Admission Date")



#check old files
#done with code check_smh_vitals.R

# a random sample from those clusters for further check
dad_novit_juntodec <- dad_novit[between(month(mdy(dad_novit$Admit.Date)), 6, 12)]
set.seed(1)
dad_novit_juntodec_sample20 <- sample(dad_novit_juntodec$EncID.new, size = 20, 
                                      replace = F)
write.csv(dad_novit_juntodec_sample20, 
          "H:/GEMINI/Results/NoLabMedRad/smh_novit_juntodec_sample20.csv", 
          row.names = F)




#********************* No Microbiology *****************************************
#**************************SMH SBK**********************************************

#------------------------- SMH -------------------------------------------------
swdh("SMH")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[2])
mic <- fread(files[23])
length(unique(adm$EncID.new))
sum(!adm$EncID.new %in% mic$EncID.new)
sum(!adm$EncID.new %in% mic$EncID.new)/length(unique(adm$EncID.new))
enc_nomic <- adm$EncID.new[!adm$EncID.new%in%mic$EncID.new]
write.csv(enc_nomic, "H:/GEMINI/Results/NoLabMedRad/smh_no_mic_enc.csv", 
          row.names = F)




#check smh without MRSA/VRE with PERI
rm(list = ls())
swdh("SMH")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[2])
mic <- fread(files[23])
mic_nomsra <- mic[mic$Test_ID != "MSRA" &mic$Test_ID!="VRE"]
sum(!adm$EncID.new %in% mic_nomsra$EncID.new)
sum(!adm$EncID.new %in% mic_nomsra$EncID.new)/length(unique(adm$EncID.new))
enc_nomic <- adm$EncID.new[!adm$EncID.new%in%mic$EncID.new]

##------------------------ SMH with newly merged micro data --------------------
swdh("SMH")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[2])
mic <- fread(files[24])
length(unique(adm$EncID.new))
sum(!adm$EncID.new %in% mic$EncID.new)
sum(!adm$EncID.new %in% mic$EncID.new)/length(unique(adm$EncID.new))
enc_nomic <- adm$EncID.new[!adm$EncID.new%in%mic$EncID.new]
write.csv(enc_nomic, "H:/GEMINI/Results/NoLabMedRad/smh_no_mic_enc_new.csv", 
          row.names = F)



#--------------------------SBK--------------------------------------------------
swdh("SBK")
list.files(recursive = T); files <- list.files(recursive = T)
adm <- fread(files[1])
micneg <- fread(files[20])
micpos <- fread(files[21])
enc_mic <- unique(c(micneg$EncID.new, micpos$EncID.new))
length(unique(adm$EncID.new))
sum(!adm$EncID.new %in% enc_mic)
sum(!adm$EncID.new %in% enc_mic)/length(unique(adm$EncID.new))
enc_nomic <- adm$EncID.new[!adm$EncID.new%in%enc_mic]
write.csv(enc_nomic, "H:/GEMINI/Results/NoLabMedRad/sbk_no_mic_enc.csv", 
          row.names = F)


#=======================Summary and Visualizations =============================
#----------------------------- SMH ---------------------------------------------
rm(list = ls())
setwd("H:/GEMINI/Results/NoLabMedRad")
nomic_smh <- scan("smh_no_mic_enc.csv", sep=",", skip = 1)
swdh("SMH/CIHI")
dad <- fread("smh.ip_dad.nophi.csv")

dad_nomic <- dad[EncID.new%in%nomic_smh, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
ggplot(dad_nomic, aes(Age)) + geom_histogram() + ggtitle("Age - No MIC - SMH)")

dad_nomic$LOS <- as.numeric(mdy(dad_nomic$Discharge.Date) - 
                              mdy(dad_nomic$Admit.Date))


ggplot(dad_nomic, aes(LOS)) + geom_histogram() + ggtitle("Length of Stay - No MIC - SMH")

ggplot(dad_nomic, aes(mdy(Admit.Date))) + geom_histogram(binwidth = 10) +
  ggtitle("Admit Date - No MIC - SMH")
ggplot(dad, aes(mdy(Admit.Date))) + geom_histogram(binwidth = 10) +
  ggtitle("all admission by date - SMH")
qplot(Age, LOS, data = dad_nomic, colour = Gender, 
      main = "Length of Stay vs Age - No MIC - SMH")



#check whether there are any micro data for admission in 2014
swdh("SMH")
dad_mic <- dad[!EncID.new%in%nomic_smh, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
ggplot(dad_mic, aes(mdy(Admit.Date))) + geom_histogram(binwidth = 10) +
  ggtitle("Admit Date - with MIC - SMH")



# visualization with new merged data
setwd("H:/GEMINI/Results/NoLabMedRad")
nomic_smh <- scan("smh_no_mic_enc_new.csv", sep=",", skip = 1)
swdh("SMH/CIHI")
dad <- fread("smh.ip_dad.nophi.csv")
ggplot(dad, aes(mdy(Admit.Date))) + geom_histogram(binwidth = 10)
dad_nomic <- dad[EncID.new%in%nomic_smh, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
ggplot(dad_nomic, aes(Age)) + geom_histogram() + ggtitle("Age - No MIC - SMH)")
dad_nomic$LOS <- as.numeric(mdy(dad_nomic$Discharge.Date) - 
                              mdy(dad_nomic$Admit.Date))
ggplot(dad_nomic, aes(LOS)) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Length of Stay - No Micro - SMH")
ggplot(dad_nomic, aes(mdy(Admit.Date))) + geom_histogram(binwidth = 10) +
  ggtitle("Admit Date - No Micro - SMH")
dad_mic <- dad[!EncID.new%in%nomic_smh, .(Admit.Date, Discharge.Date, Gender, 
                                          Age, EncID.new)]
dad$micro <- !dad$EncID.new%in%nomic_smh
ggplot(dad, aes(mdy(Discharge.Date), fill = micro)) + 
  geom_histogram(binwidth = 20, alpha = 0.5) +
  ggtitle("SMH - All discharge vs micro")




#a sample of 20 from 2010
dad_nomic_2010 <- dad_nomic[mdy(dad_nomic$Discharge.Date)<mdy(01012011),]
set.seed(1)
sample_encid_20 <- sample(dad_nomic_2010$EncID.new, 20, replace = F)
sample_smh_nomic_20 <- dad_nomic_2010[dad_nomic_2010$EncID.new %in% sample_encid_20]
swdr("SMH/CIHI/Archive")
link <- fread("smh.GIM_IP_LINKING_LIST.csv")
sample_smh_nomic_2010 <- merge(sample_smh_nomic_20, 
                               link[,.(MRN, IPEncounterID, EncID.new)],
                               by = "EncID.new", all.x = T,all.y = F)

write.csv(sample_smh_nomic_2010, "H:/GEMINI/Results/NoLabMedRad/smh_nomic_2010_sample20.csv", 
          row.names = F)


#---------------------------------SBK-------------------------------------------
rm(list = ls())
setwd("H:/GEMINI/Results/NoLabMedRad")
nomic_sbk <- scan("sbk_no_mic_enc.csv", sep=",", skip = 1)
swdh("sbk/CIHI")
dad <- fread("sbk.ip_dad.nophi.csv")

dad_nomic <- dad[EncID.new%in%nomic_sbk, .(Admit.Date, Discharge.Date, Gender, 
                                           Age, EncID.new)]
ggplot(dad_nomic, aes(Age)) + geom_histogram(binwidth = 2) + 
  ggtitle("Age - No MIC - SBK")

dad_nomic$LOS <- as.numeric(mdy(dad_nomic$Discharge.Date) - 
                              mdy(dad_nomic$Admit.Date))


ggplot(dad_nomic, aes(LOS)) + geom_histogram(binwidth = 0.5) + 
  ggtitle("Length of Stay - No MIC - SBK")

ggplot(dad_nomic, aes(mdy(Discharge.Date))) + geom_histogram(binwidth = 10) +
  ggtitle("Discharge Date - No MIC - SBK")
dad$micro <- !dad$EncID.new %in% nomic_sbk
ggplot(dad, aes(mdy(Discharge.Date), fill = micro)) + 
  geom_histogram(binwidth = 10, alpha = 0.5) +
  ggtitle("all discharge by date - SBK")

