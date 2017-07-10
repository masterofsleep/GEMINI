# ------------------------ clinical validation SBK -----------------------------
library(gemini)
lib.pa()
phy.all <- readg(gim, all.phy)
gemini.inc <- phy.all[(adm.GIM%in%c("y", "GP-GIM")|dis.GIM%in%c("y", "GP-GIM"))]
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")[EncID.new%in%gemini.inc$EncID.new]
icu_before_adm <- fread("C:/Users/guoyi/Desktop/to.adm/icu.before.adm.enc.csv")



sample_n_each_site <- function(all_enc, n=100, seed= 100){
  all_enc <- as.character(all_enc)
  sites <- unique(str_sub(all_enc, 1, 2))
  sample_enc <- NULL
  for(i in sites){
    set.seed(seed)
    sample_enc <- c(sample_enc, sample(all_enc[startsWith(all_enc, i)], n))
  }
  return(sample_enc)
}

find_enc_host_dt <- function(enc_list){
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




# ---------------------------  Create files  -----------------------------------

dad[, ':='(death = Discharge.Disposition==7,
           rbc = EncID.new%in%first.trans$EncID.new,
           icu = EncID.new%in%first_icu$EncID.new)]


vali_death_trans_icu <- rbind(
  find_enc_host_dt(sample_n_each_site(dad[death==T&rbc==F&icu==F, EncID.new], 100)),
  find_enc_host_dt(sample_n_each_site(dad[death==F&rbc==T&icu==F, EncID.new], 100)),
  find_enc_host_dt(sample_n_each_site(dad[death==F&rbc==F&icu==T, EncID.new], 100)),
  find_enc_host_dt(sample_n_each_site(dad[death==F&rbc==F&icu==F, EncID.new], 500)))

table(str_sub(vali_death_trans_icu$EncID.new, 1, 2))

vali_death_trans_icu[, ':='(`Death in Chart? (y/n)` = "",
                            `ICU in chart?(y/n)` = "",
                            `ICU Date in chart(yyyy-mm-dd)`= "",
                            `ICU Time in Chart(hh:mm)`= "",
                            `trans in chart?` = "",
                            `trans Date in chart(yyyy-mm-dd)`= "",
                            `trans Time in Chart(hh:mm)`= "")]


fwrite(vali_death_trans_icu[startsWith(EncID.new, "12")][sample(1:800, 800, replace = F)], 
       "H:/GEMINI/Results/Clinical Validation/sbk/sbk_vali_death_trans_icu.csv")
fwrite(vali_death_trans_icu[startsWith(EncID.new, "13")][sample(1:800, 800, replace = F)], 
       "H:/GEMINI/Results/Clinical Validation/uhn/uhn_vali_death_trans_icu.csv")
fwrite(vali_death_trans_icu[startsWith(EncID.new, "14")][sample(1:800, 800, replace = F)], 
       "H:/GEMINI/Results/Clinical Validation/msh/msh_vali_death_trans_icu.csv")







# -------------------------------- Physician Name ------------------------------
sample_phy_vali <- function(){
  sample_enc <- sample_n_each_site(dad$EncID.new, seed = 5)
  find_enc_hos_dt(sample_enc)
}
phy_vali <-sample_phy_vali()
phy_vali[, ':='(Admitting.Physician.Last.Name = "",
                Discharging.Physician.Last.Name = "")]
fwrite(phy_vali[startsWith(EncID.new, "12")], 
       "H:/GEMINI/Results/Clinical Validation/sbk/sbk_phy_vali.csv")
fwrite(phy_vali[startsWith(EncID.new, "13")], 
       "H:/GEMINI/Results/Clinical Validation/uhn/uhn_phy_vali.csv")
fwrite(phy_vali[startsWith(EncID.new, "14")], 
       "H:/GEMINI/Results/Clinical Validation/msh/msh_phy_vali.csv")



# --------------------------------- radiology ----------------------------------
sample_rad <- function(){
  sample_enc <- sample_n_each_site(dad$EncID.new)
  find_enc_hos_dt(sample_enc)
}

rad_vali <- sample_rad()


create_rad_vali_dat <- function(df){
  res <- NULL
  for(i in 1:nrow(df)){
    res <- rbind(res, 
                 cbind(df[i], Test.Category = c("XRay", "CT", "US", "MRI", "Echo")))
  }
  return(res)
}
rad_vali <- create_rad_vali_dat(rad_vali)
rad_vali[,':='(`Test in Chart?(y/n)` = "",
               `Test Name` = "",
               `Test.Date (yyyy-mm-dd)`= "",
               `Test.Time (hh:mm)` = "")]


fwrite(rad_vali[startsWith(EncID.new, "12")], 
       "H:/GEMINI/Results/Clinical Validation/sbk/sbk_rad_vali.csv")
fwrite(rad_vali[startsWith(EncID.new, "13")], 
       "H:/GEMINI/Results/Clinical Validation/uhn/uhn_rad_vali.csv")
fwrite(rad_vali[startsWith(EncID.new, "14")], 
       "H:/GEMINI/Results/Clinical Validation/msh/msh_rad_vali.csv")






# ---------------------------------- Lab ---------------------------------------
# ----------------------- Clinical Validation ----------------------------------
# ------------------------------ 2017-05-15 ------------------------------------
sample_lab <- function(){
  sample_enc <- sample_n_each_site(dad$EncID.new, seed = 200)
  find_enc_hos_dt(sample_enc)
}

lab_vali <- sample_lab()

create_lab_vali_dat <- function(df){
  res <- NULL
  Test = c("Hgb", "Sodium", "Creatinine", "INR", "Calcium", "AST",
           "Troponin")
  for(i in 1:nrow(df)){
    res <- rbind(res, 
                 cbind(df[i], Test))
  }
  return(res)
}

lab_vali <- create_lab_vali_dat(lab_vali)
lab_vali[, ':='(Result.Value = "",
                Collection.Date = "",
                Collection.Time = "")]

fwrite(lab_vali[startsWith(EncID.new, "12")], 
       "H:/GEMINI/Results/Clinical Validation/sbk/sbk_lab_vali.csv")
fwrite(lab_vali[startsWith(EncID.new, "13")], 
       "H:/GEMINI/Results/Clinical Validation/uhn/uhn_lab_vali.csv")
fwrite(lab_vali[startsWith(EncID.new, "14")], 
       "H:/GEMINI/Results/Clinical Validation/msh/msh_lab_vali.csv")


