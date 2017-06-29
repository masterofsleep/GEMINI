# ------------------------ clinical validation new -----------------------------
library(gemini)
lib.pa()

dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")
icu_before_adm <- fread("C:/Users/guoyi/Desktop/to.adm/icu.before.adm.enc.csv")

# function to add mrn to smh data
add_smh_mrn <- function(df){
  link <- readg(SMH, LINKLIST_NEWHASH)[,.(MRN, EncID.new = paste("11", EncID.new, sep = ""))]
  df$EncID.new <- as.character(df$EncID.new)
  df <- merge(link, df, by = "EncID.new", all.x = F, all.y = T)
  return(data.table(df))
}

sample_n_each_site <- function(all_enc, n=100){
  all_enc <- as.character(all_enc)
  sites <- unique(str_sub(all_enc, 1, 2))
  sample_enc <- NULL
  for(i in sites){
    set.seed(100)
    sample_enc <- c(sample_enc, sample(all_enc[startsWith(all_enc, i)], n))
  }
  return(sample_enc)
}

find_enc_hos_dt <- function(enc_list){
  dad[EncID.new%in%enc_list, .(EncID.new = as.character(EncID.new), Institution.Number,
                                 Admit.Year = str_sub(Admit.Date, 1, 4),
                                 Admit.Month = str_sub(Admit.Date, 6, 7),
                                 Admit.Day = str_sub(Admit.Date, 9, 10),
                                 Admit.Time,
                                 Discharge.Year = str_sub(Discharge.Date, 1, 4),
                                 Discharge.Month = str_sub(Discharge.Date, 6, 7),
                                 Discharge.Day = str_sub(Discharge.Date, 9, 10),
                                 Discharge.Time)]
}

# --------------------------------- Death --------------------------------------
sample_death_enc <- function(){
  death_enc <- dad[Discharge.Disposition==7, EncID.new]
  sample_enc <- sample_n_each_site(death_enc, 100)
  return(find_enc_hos_dt(sample_enc))
}


death_sample <- sample_death_enc()
death_sample[, `Death in Chart? (y/n)` := ""]

smh.death <- add_smh_mrn(death_sample[str_sub(EncID.new,1,2)=="11"])
fwrite(smh.death, "H:/GEMINI/Results/Clinical Validation/new_june23/smh_death.csv")

# no death
sample_no_death_enc <- function(){
  death_enc <- dad[Discharge.Disposition!=7, EncID.new]
  sample_enc <- sample_n_each_site(death_enc, 100)
  return(find_enc_hos_dt(sample_enc))
}

no_death_sample <- sample_no_death_enc()
no_death_sample[, `Death in Chart? (y/n)` := ""]

smh.no.death <- add_smh_mrn(no_death_sample[str_sub(EncID.new,1,2)=="11"])
fwrite(smh.no.death, "H:/GEMINI/Results/Clinical Validation/new_june23/smh_no_death.csv")


# -------------------------------- ICU -----------------------------------------

# all transfer to ICU
find_all_icu <- function(){
  smh.xf <- readg(smh, ip_xfer)[Unit.Code =="1"]
  sbk.xf <- readg(sbk, ip_xfer)[Unit.Code %in%c("1")]
  uhn.xf <- readg(uhn, ip_xfer)[Unit.Code %in%c("1")]
  msh.xf <- readg(msh, xfer)[NURSE_UNIT_DISP=="ICU"]
  smh.scu <- readg(smh, ip_scu)
  sbk.scu <- readg(sbk, ip_scu)
  uhn.scu <- readg(uhn, ip_scu)
  msh.scu <- readg(msh, ip_scu)
  
  sbk.scu <- sbk.scu[SCU.Unit.Number!="99"]
  
  all.scu <- rbind(smh.scu)
  scu.admit <- unique(c(smh.xf$EncID.new, smh.scu$EncID.new,
                        sbk.xf$EncID.new, sbk.scu$EncID.new,
                        uhn.xf$EncID.new, uhn.scu$EncID.new,
                        msh.xf$EncID.new, msh.scu$EncID.new))
  
  icu.all <- rbind(smh.scu[, .(ICU.DtTm = mdy_hm(paste(SCU.Admit.Date, SCU.Admit.Time)), EncID.new)],
                      sbk.scu[, .(ICU.DtTm =mdy_h(paste(SCU.Admit.Date, SCU.Admit.Time)), EncID.new)],
                      uhn.scu[, .(ICU.DtTm =ymd_hm(paste(SCU.Admit.Date, SCU.Admit.Time)), EncID.new)],
                      msh.scu[, .(ICU.DtTm =ymd_hm(paste(SCU.Admit.Date, SCU.Admit.Time)), EncID.new)],
                      smh.xf[, .(ICU.DtTm =ymd_hm(paste(Date.Check.in, Time.Check.in)), EncID.new)],
                      sbk.xf[, .(ICU.DtTm =ymd_hm(paste(Date.Check.in, Time.Check.in)), EncID.new)],
                      uhn.xf[, .(ICU.DtTm =ymd_hm(paste(Date.Check.in, Time.Check.in)), EncID.new)],
                      msh.xf[, .(ICU.DtTm =ymd_hm(paste(TRANSACTION_DT, TRANSACTION_TM)), EncID.new)]
  )
  return(icu.all)
}

icu_all <- find_all_icu()
first_icu <- icu_all %>% arrange(EncID.new, ICU.DtTm) %>% filter(!duplicated(EncID.new))

sample_icu_enc <- function(){
  icu_enc <- as.character(dad[SCU.adm==T&!EncID.new%in%icu_before_adm$EncID.new
                              &LoS<=14, EncID.new])
  sample_enc <- sample_n_each_site(icu_enc)
  find_enc_hos_dt(sample_enc)
}

icu_samples <- sample_icu_enc()
icu_samples <- merge(icu_samples, first_icu, by = "EncID.new")
icu_samples[, ICU.DtTm:=as.character(ICU.DtTm)]
icu_samples[, ':='(ICU.year = str_sub(ICU.DtTm, 1, 4),
                   ICU.month = str_sub(ICU.DtTm, 6, 7),
                   ICU.day = str_sub(ICU.DtTm, 9, 10),
                   ICU.time = str_sub(ICU.DtTm, 12, 16))]
icu_samples[, ICU.DtTm:=NULL]
icu_samples[, ':='(`ICU in chart?(y/n)` = "",
                   `ICU Date in chart(yyyy-mm-dd)`= "",
                   `ICU Time in Chart(hh:mm)`= "",
                   `Admitting Physician First Name` = "",
                   `Admitting Physician Last Name` = "",
                   `Discharging Physician First Name` = "",
                   `Discharging Physician Last Name` = "")]
smh.icu <- add_smh_mrn(icu_samples[str_sub(EncID.new,1,2)=="11"])
fwrite(smh.icu, "H:/GEMINI/Results/Clinical Validation/new_june23/smh_icu.csv")



# --------------------------- RBC Transfusion ----------------------------------
find_rbc_trans <- function(){
  smh.bb <- readg(smh, bb)
  sbk.bb <- readg(sbk, bb)
  uhn.bb <- rbind(readg(uhn, txm_er),
                  readg(uhn, txm_ip))
  msh.bb <- readg(msh, bb)
  uhn.bb[nchar(Time_Component_Issued_from_Lab)==8, Time_Component_Issued_from_Lab:=str_sub(
    Time_Component_Issued_from_Lab, 1,5
  )]
  rbc.trans <- 
    rbind(smh.bb[Selected_product_code=="RCB", .(Trans.Dt = mdy_hm(UseDtTm),
                                                 EncID.new)],
          sbk.bb[Product.Group.Code=="RBC", .(Trans.Dt = ymd_hms(paste(Issue.Date, Issue.Time)),
                                              EncID.new)],
          uhn.bb[Blood_Component == "RBC", .(Trans.Dt = mdy_hm(paste(Date_Component_Issued_from_Lab, 
                                                                     Time_Component_Issued_from_Lab)),
                                             EncID.new)],
          msh.bb[POPROD=="Red Blood Cells Concentrate", 
                 .(Trans.Dt = ymd_hm(paste(DATE, TIME.new)),EncID.new)])
  return(rbc.trans)
}

rbc.trans <- find_rbc_trans() 
first.trans <- rbc.trans %>% arrange(EncID.new, Trans.Dt) %>% filter(!duplicated(EncID.new))

sample_rbc_enc <- function(){
  rbc_enc <- as.character(dad[EncID.new%in%rbc.trans$EncID.new&
                                LoS<=14, EncID.new])
  sample_enc <- sample_n_each_site(rbc_enc, 100)
  return(find_enc_hos_dt(sample_enc))
}

trans_samples <- sample_rbc_enc()
trans_samples <- merge(trans_samples, first.trans, by = "EncID.new")
trans_samples[, Trans.Dt:=as.character(Trans.Dt)]
trans_samples[, ':='(trans.year = str_sub(Trans.Dt, 1, 4),
                   trans.month = str_sub(Trans.Dt, 6, 7),
                   trans.day = str_sub(Trans.Dt, 9, 10),
                   trans.time = str_sub(Trans.Dt, 12, 16))]
trans_samples[, Trans.Dt:=NULL]
trans_samples[, ':='(`trans in chart?` = "",
                   `trans Date in chart(yyyy-mm-dd)`= "",
                   `trans Time in Chart(hh:mm)`= "")]
smh.trans <- add_smh_mrn(trans_samples[str_sub(EncID.new,1,2)=="11"])
fwrite(smh.trans, "H:/GEMINI/Results/Clinical Validation/new_june23/smh_trans.csv")


# ---------------------------- No ICU and NO Trans -----------------------------
noicu_notrans <- function(){
  sample_enc <- sample_n_each_site(
    dad[SCU.adm==F&!EncID.new%in%rbc.trans$EncID.new&LoS<=14, EncID.new])
  find_enc_hos_dt(sample_enc)
}
no_icu_trans <- noicu_notrans() %>% filter(Institution.Number=="SMH") %>% data.table
no_icu_trans[, ':='(`ICU in Chart? (y/n` = "",
                    `RBC Trans in Chart? (y/n)`= "")]
smh.no_icu_trans <- add_smh_mrn(no_icu_trans[Institution.Number=="SMH"])
fwrite(smh.no_icu_trans, "H:/GEMINI/Results/Clinical Validation/new_june23/smh_no_icu_no_trans.csv")

# --------------------------------- Radiology ----------------------------------
sample_rad <- function(){
  sample_enc <- sample_n_each_site(dad$EncID.new)
  find_enc_hos_dt(sample_enc)
}
smh_rad_sample <- sample_rad()[Institution.Number=="SMH"]
smh_rad_in_2 <- function(dat){
  dat %>% arrange(EncID.new, ymd_h(proc_dtime)) %>%
  filter((ymd(str_sub(proc_dtime,1,10))<=ymd(Admit.Date) + 1)&
           (ymd_h(proc_dtime)>=ymd_hm(paste(Admit.Date, Admit.Time)))) %>%
  filter(!duplicated(EncID.new)) %>% data.table %>% return
}
smh.xray <- readg(smh, xray, dt = T) %>% smh_rad_in_2
smh.ct <- readg(smh, ct, dt = T) %>% smh_rad_in_2
smh.mri <- readg(smh, mri, dt = T) %>% smh_rad_in_2
smh.us <- readg(smh, us, dt = T) %>% smh_rad_in_2
smh.echo <- readg(smh, echo, dt = T) %>% 
  filter(dmy(StudyStartDateTime)>=ymd(Admit.Date)) %>%
  filter(dmy(StudyStartDateTime)<=ymd(Admit.Date)+1) %>%
  arrange(EncID.new, dmy(StudyStartDateTime)) %>% 
  filter(!duplicated(EncID.new)) %>% data.table
  
fillna <- function(x){
  ifelse(is.null(x), NA, x)
}

smh_rad_time <- function(x){
  ifelse(is.null(x)|is.na(x), NA,
  paste(str_sub(x, -2, -1),":00", sep = ""))
}
find_test <- function(encid){
  res <- data.frame(EncID.new=encid,
                    Test.Category = c("XRay", "CT", "US", "MRI", "Echo"))
  res$Test.Name = c(fillna(smh.xray[EncID.new==encid, proc_desc_long]),
                    fillna(smh.ct[EncID.new==encid, proc_desc_long]),
                    fillna(smh.us[EncID.new==encid, proc_desc_long]),
                    fillna(smh.mri[EncID.new==encid, proc_desc_long]),
                    fillna(smh.echo[EncID.new==encid, ProcedureName]))
  res$Performed.Date = c(fillna(smh.xray[EncID.new==encid, str_sub(proc_dtime, 1,10)]),
                       fillna(smh.ct[EncID.new==encid, str_sub(proc_dtime, 1,10)]),
                       fillna(smh.us[EncID.new==encid, str_sub(proc_dtime, 1,10)]),
                       fillna(smh.mri[EncID.new==encid, str_sub(proc_dtime, 1,10)]),
                       fillna(smh.echo[EncID.new==encid, as.character(dmy(StudyStartDateTime))]))
  res$Performed.Time = c(fillna(smh.xray[EncID.new==encid, smh_rad_time(proc_dtime)]),
                         fillna(smh.ct[EncID.new==encid, smh_rad_time(proc_dtime)]),
                         fillna(smh.us[EncID.new==encid, smh_rad_time(proc_dtime)]),
                         fillna(smh.mri[EncID.new==encid, smh_rad_time(proc_dtime)]),
                         NA)
  res
}

find_test(smh_rad_sample$EncID.new[1])

smh_rad_vali_file <- function(){S
  rad_vali <- NULL
  for(i in smh_rad_sample$EncID.new){
    rad_vali <- rbind(rad_vali, find_test(i))
  }
  return(rad_vali)
}

smh_rad_vali <- smh_rad_vali_file()

smh_rad_vali <- merge(smh_rad_sample, smh_rad_vali, by = "EncID.new")
smh_rad_vali[, ':='(
  `Test in Chart? (y/n)` = "",
  `Other Test in Chart? (y/n)` = "",
  `Other Test Date (yyyy-mm-dd)` = "",
  `Other Test Time (hh:mm)` = ""
)]
smh_rad_vali <- add_smh_mrn(smh_rad_vali)
fwrite(smh_rad_vali, "H:/GEMINI/Results/Clinical Validation/new_june23/smh_rad.csv")




# add mrn
setwd("R:/GEMINI-DRM-TEAM/Clinical Validation/New_June23")
files <- list.files();files

addtofile <- function(x){
  dat <- readxl::read_excel(x)
  dat <- add_smh_mrn(dat)
  setwd("R:/GEMINI-DRM-TEAM/Clinical Validation/New_June23")
  fwrite(dat, str_replace(x, ".xlsx", ".csv"))
}

addtofile(files[8])
addtofile(files[10])
