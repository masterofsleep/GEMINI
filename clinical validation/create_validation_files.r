# ----------------------- Clinical Validation ----------------------------------
# ------------------------------ 2017-05-15 ------------------------------------
library(gemini)
lib.pa()
find_100 <- function(){
  phy.all <- readg(gim, all.phy)
  link <- readg(SMH, LINKLIST_NEWHASH)[,.(MRN, EncID.new = paste("11", EncID.new, sep = ""))]
  smh.dad <- readg(smh, dad)
  set.seed(100)
  sample_enc <- sample(phy.all[str_sub(EncID.new,1,2)=="11"&(
                               adm.GIM%in%c("y", "GP-GIM")|
                                    adm.GIM%in%c("y", "GP-GIM")),
                                  EncID.new], 100, replace = F)
  smh.dad <- merge(smh.dad, link[,.(EncID.new, MRN)], all.x = T, all.y = F)
  smh.dad[EncID.new%in%sample_enc, .(MRN, EncID.new,
                                     Admit.Date, Admit.Time,
                                     Discharge.Date, Discharge.Time)]
}  
smh_vali <- find_100()  
Result.Category = c("Lab","Lab", "Lab", "Lab",
                    "Lab","Lab","Lab","Radiology","Radiology",
                    "Culture","Culture")
Test = c("Hgb", "Sodium", "Creatinine", "INR", "Calcium", "AST",
         "Troponin", "CT Head", "Xray Chest", "Blood", "Urine")
cbind(smh_vali[1],Result.Category, Test ,
      Collection.Performed.Date = "",
      Collection.Performed.Time = "",
      Result.Value = "")
for(i in 1:100){
  fwrite(cbind(smh_vali[i],Result.Category, Test ,
        Collection.Performed.Date = "",
        Collection.Performed.Time = "",
        Result.Value = ""),
        paste("H:/GEMINI/Results/Clinical Validation/smh_patient_", i,".csv",
              sep = ""))
}



# ---------------------------- Validate Result ---------------------------------

smh.xray <- readg(smh, xray, dt = T)[EncID.new%in%smh_vali$EncID.new]
smh.ct <- readg(smh, ct, dt = T)[EncID.new%in%smh_vali$EncID.new]
smh.lab <- readg(smh, labs, dt = T)[EncID.new%in%smh_vali$EncID.new]
smh.micro <- readg(smh, micro, dt = T)[EncID.new%in%smh_vali$EncID.new]

smh.xr.chest <- smh.xray[body_part_mne=="CHEST"]
smh.ct.head <- smh.ct[body_part_mne=="HEAD"]

# Labs within 2 day window
test.names <- c("HGB", "Sodium", "Creatinine", "INR", "Calcium", "AST", "Troponin I")
smh.lab <- smh.lab[Test.Name%in%test.names] %>% 
  arrange(EncID.new, ymd_hms(Collection.DtTm)) %>%
  filter((ymd(str_sub(Collection.DtTm, 1, 10))<= ymd(Admit.Date) + 1)&
           (ymd_hms(Collection.DtTm)>= ymd_hms(paste(Admit.Date, Admit.Time, ":00", sep = "")))) %>% data.table
smh.lab <- smh.lab[!duplicated(smh.lab[, .(EncID.new, Test.Name)])]

# Rad within 2 day
smh.xr.chest <- smh.xr.chest %>% arrange(EncID.new, ymd_h(proc_dtime)) %>%
  filter((ymd(str_sub(proc_dtime,1,10))<=ymd(Admit.Date) + 1)&
           (ymd_h(proc_dtime)>=ymd_hm(paste(Admit.Date, Admit.Time)))) %>%
  filter(!duplicated(EncID.new)) %>% data.table

smh.ct.head <- smh.ct.head %>% arrange(EncID.new, ymd_h(proc_dtime)) %>%
  filter((ymd(str_sub(proc_dtime,1,10))<=ymd(Admit.Date) + 1)&
           (ymd_h(proc_dtime)>=ymd_hm(paste(Admit.Date, Admit.Time)))) %>%
  filter(!duplicated(EncID.new)) %>% data.table

# culture within 2 days
smh.pos <- smh.micro[ResultIsFinal=="Y"]
smh.neg <- smh.micro[is.na(ResultIsFinal)]
smh.pos[, Collection.DtTm := mdy_hm(`Specimen_Collection_Date\\Time`)]
smh.neg[, Collection.DtTm := mdy(`Specimen_Collection_Date\\Time`)]

smh.pos.2d <- smh.pos[(Collection.DtTm<=ymd(Admit.Date) + 1)&
                        Collection.DtTm>=ymd_hm(paste(Admit.Date, Admit.Time))] %>%
  arrange(EncID.new, Collection.DtTm) %>% data.table
smh.neg.2d <- smh.neg[(Collection.DtTm<=ymd(Admit.Date) + 1)&
                        Collection.DtTm>=ymd(Admit.Date)] %>%
  arrange(EncID.new, Collection.DtTm) %>% data.table
smh.pos.2d[, Collection.DtTm := as.character(Collection.DtTm)]
smh.neg.2d[, Collection.DtTm := as.character(Collection.DtTm)]


smh.blood <- rbind(smh.pos.2d, smh.neg.2d) %>% filter(Source%in%"BLOOD") %>%
  filter(!duplicated(EncID.new)) %>% data.table
smh.urine <- rbind(smh.pos.2d, smh.neg.2d) %>% filter(Source%in%c("MSU", "URINE")|Test_ID=="URNC") %>%
  filter(!duplicated(EncID.new)) %>% data.table








setwd("R:/GEMINI-DRM-TEAM/Clinical Validation")
files <- list.files(recursive = T)



dat <- fread(files[102], colClasses = list(character = "EncID.new"))
find_vali_values <- function(dat){
  dat$EncID.new <- as.character(dat$EncID.new)
  # find lab values
  patient.lab <- smh.lab[EncID.new==dat$EncID.new[1]]
  patient.lab$Test <- patient.lab$Test.Name
  dat$Test[1:7] <- test.names
  dat <- merge(dat, patient.lab[, .(EncID.new, Test, Result.Value, Collection.DtTm)],
               by = c("EncID.new", "Test"), all.x = T, all.y = F, sort = F)
  dat$Collection.DtTm <- as.character(dat$Collection.DtTm)
  # find ct head 
  dat$Collection.DtTm[8] <- ifelse(dat$EncID.new[1]%in%smh.ct.head$EncID.new,
                                   as.character(smh.ct.head[EncID.new==dat$EncID.new[1],proc_dtime]), NA)
  dat$Collection.DtTm[9] <- ifelse(dat$EncID.new[1]%in%smh.xr.chest$EncID.new,
                                   as.character(smh.xr.chest[EncID.new==dat$EncID.new[1],proc_dtime]), NA)
  
  # find culture
  dat$Collection.DtTm[10] <- ifelse(dat$EncID.new[1]%in%smh.blood$EncID.new,
                                    as.character(smh.blood[EncID.new==dat$EncID.new[1], Collection.DtTm]),
                                    NA)
  dat$Collection.DtTm[11] <- ifelse(dat$EncID.new[1]%in%smh.urine$EncID.new,
                                    as.character(smh.urine[EncID.new==dat$EncID.new[1], Collection.DtTm]),
                                    NA)
  return(dat)
}


index <- c(seq(2,40,2),
           seq(42, 80, 2),
           seq(81, 100, 1),
           seq(101, 139, 2),
           seq(142, 180, 2))

all.dat <- NULL

for(i in index){
  dat <- fread(files[i])
  print(files[i])
  all.dat <- rbind(all.dat, find_vali_values(dat), fill = T)
}
ra <- c(rep("az", 220),
        rep("ak", 220),
        rep("an", 220),
        rep("mm", 220),
        rep("rk", 220))
all.dat <- cbind(all.dat, ra)
all.dat <- all.dat %>% arrange(Result.Category, Test) %>% data.table

all.dat[str_sub(Collection.Performed.Date, 1, 1)%in%c("N", "n")|
          Collection.Performed.Date=="", Collection.Performed.Date:= NA]
all.dat[str_sub(Collection.Performed.Time, 1, 1)%in%c("N", "n")|
          Collection.Performed.Time=="", Collection.Performed.Time:= NA]
all.dat[str_sub(Result.Value.x, 1, 1)%in%c("N", "n")|
          Result.Value.x=="", Result.Value.x:= NA]

fwrite(all.dat, "H:/GEMINI/Results/Clinical Validation/validate/all.validate.csv")

rad.vali <- all.dat[Result.Category=="Radiology"]
fwrite(rad.vali, "H:/GEMINI/Results/Clinical Validation/validate/rad.validate.csv")
lab.vali <- all.dat[Result.Category=="Lab"]
fwrite(lab.vali, "H:/GEMINI/Results/Clinical Validation/validate/lab.validate.csv")
culture.vali <- all.dat[Result.Category=="Culture"]
fwrite(culture.vali, "H:/GEMINI/Results/Clinical Validation/validate/culture.validate.csv")

dat <- fread(files[43])



checklab <- function(x){
  print(smh.lab[EncID.new==as.character(x)])
}
checklab(11204790)

smh.micro[EncID.new=="11889020"]

#  check how many instance when final results happen after discharge date





# -------------------------- 100 more ------------------------------------------
library(gemini)
lib.pa()
find_100_more <- function(){
  phy.all <- readg(gim, all.phy)
  link <- readg(SMH, LINKLIST_NEWHASH)[,.(MRN, EncID.new = paste("11", EncID.new, sep = ""))]
  smh.dad <- readg(smh, dad)
  set.seed(100)
  sample_enc_old <- sample(phy.all[str_sub(EncID.new,1,2)=="11"&(
    adm.GIM%in%c("y", "GP-GIM")|
      adm.GIM%in%c("y", "GP-GIM")),
    EncID.new], 100, replace = F)
  set.seed(500)
  sample_enc <- sample(phy.all[str_sub(EncID.new,1,2)=="11"&(
    adm.GIM%in%c("y", "GP-GIM")|
      adm.GIM%in%c("y", "GP-GIM"))&!EncID.new%in%sample_enc_old,
    EncID.new], 100, replace = F)
  smh.dad <- merge(smh.dad, link[,.(EncID.new, MRN)], all.x = T, all.y = F)
  smh.dad[EncID.new%in%sample_enc, .(MRN, EncID.new,
                                     Admit.Date, Admit.Time,
                                     Discharge.Date, Discharge.Time)]
}  


smh_vali <- find_100_more()  
smh_vali[,':='(Admit.Year = str_sub(Admit.Date, 1, 4),
               Admit.Month = str_sub(Admit.Date, 6, 7),
               Admit.Day = str_sub(Admit.Date, 9, 10))]
smh_vali <- smh_vali[,. (MRN, EncID.new, Admit.Year, Admit.Month, Admit.Day,
                         Admit.Time)]
Result.Category = c("Lab","Lab", "Lab", "Lab",
                    "Lab","Lab","Lab",
                    "Culture","Culture")
Test = c("Hgb", "Sodium", "Creatinine", "INR", "Calcium", "AST",
         "Troponin", "Blood", "Urine")
cbind(smh_vali[1],Result.Category, Test ,
      Collection.Performed.Date = "",
      Collection.Performed.Time = "",
      Result.Value = "")
for(i in 1:100){
  fwrite(cbind(smh_vali[i],Result.Category, Test ,
               Collection.Performed.Date = "",
               Collection.Performed.Time = "",
               Result.Value = ""),
         paste("H:/GEMINI/Results/Clinical Validation/smh_patient_new", i,".csv",
               sep = ""))
}
