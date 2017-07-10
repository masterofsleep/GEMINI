# ---------------------------- Design Paper ------------------------------------
# ---------------------------- Tables V4.2 -------------------------------------
# ----------------------------- 2015-05-16 -------------------------------------
library(gemini)
lib.pa()


phy.all <- readg(gim, all.phy)
gemini.inc <- phy.all[(adm.GIM%in%c("y", "GP-GIM")|dis.GIM%in%c("y", "GP-GIM"))
         |str_sub(EncID.new,1, 2)=="15", EncID.new]



# Adm Info
gemini_adm <- function(){
  smh.adm <- readg(smh, adm)
  sbk.adm <- readg(sbk, adm)
  uhn.adm <- readg(uhn, adm)
  thp.adm <- readg(thp, adm)
  msh.adm <- readg(msh, adm)
  smh.adm$Institution.Number <- "SMH"
  sbk.adm$Institution.Number <- "SHSC"
  msh.adm$Institution.Number <- "SHS"
  thp.adm$Institution.Number <- paste("THP-", thp.adm$Institution, sep = "")
  uhn.adm$Institution.Number <- as.character(uhn.adm$Institution.Number)
  uhn.adm[Institution.Number=="54265", Institution.Number:="UHN-TG"]
  uhn.adm[Institution.Number=="54266", Institution.Number:="UHN-TW"]
  adm <- rbind(smh.adm[,.(EncID.new, Hash, Institution.Number)],
               sbk.adm[,.(EncID.new, Hash, Institution.Number)],
               uhn.adm[,.(EncID.new, Hash, Institution.Number)],
               msh.adm[,.(EncID.new, Hash, Institution.Number)],
               thp.adm[,.(EncID.new, Hash, Institution.Number)])[EncID.new%in%gemini.inc]
  nvisit <- fread("H:/GEMINI/Results/Check/hash.freq.csv")
  adm[Hash%in%nvisit$Hash[c(1,2,4)], Hash := NA]
  adm
}
adm <- gemini_adm() 

# dad Info
gemini_dad <- function(){
  vars <-  c("EncID.new", "Age", "Gender", "InstitutionFrom.Type",
         "Discharge.Disposition", 
         "Admit.Date", "Admit.Time",
         "Discharge.Date", "Discharge.Time", 
         "Number.of.ALC.Days")
  smh.dad <- readg(smh, dad)
  sbk.dad <- readg(sbk, dad)
  uhn.dad <- readg(uhn, dad)
  msh.dad <- readg(msh, dad)
  thp.dad <- readg(thp, dad)
  dad <- rbind(smh.dad[,vars, with = F],
               sbk.dad[,vars, with = F], 
               uhn.dad[,vars, with = F], 
               msh.dad[,vars, with = F], 
               thp.dad[,vars, with = F])[EncID.new%in%gemini.inc]
  dad[Number.of.ALC.Days=="1,105", Number.of.ALC.Days:= 1105]
  dad
}

dad <- gemini_dad()
dad <- merge(adm, dad, by = "EncID.new")

# calcualte Length-of-Stay and Acute Length-of-Stay
calcu_los <- function(dad){
  dad[, LoS := as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time)) - 
                         ymd_hm(paste(Admit.Date, Admit.Time)))/(3600*24)]
  dad[, Acute.LoS := LoS - as.numeric(Number.of.ALC.Days)]
  # remove palliative care from los
  xfer.smh <- readg(smh, xfer)
  xfer.smh$EncID.new <- as.character(xfer.smh$EncID.new)
  palli <- xfer.smh[Unit.Code == 7]
  palli <- palli %>% arrange(EncID.new, ymd_hm(paste(Date.Check.in, Time.Check.in))) %>% data.table
  palli <- palli[!duplicated(EncID.new)]
  dad <- merge(dad, palli[,.(EncID.new, Date.Check.in, Time.Check.in)],
               by = "EncID.new", all.x = T)
  # ALC days - days in PCU 
  # Ignore ALC, just use admitting date to transfer to PCU
  dad[!is.na(Date.Check.in), ":="(Acute.LoS = as.numeric( 
    ymd_hm(paste(Date.Check.in, Time.Check.in)) - 
      ymd_hm(paste(Admit.Date, Admit.Time)))/(3600*24)
  )]
  dad[,':='(Date.Check.in = NULL,
          Time.Check.in = NULL)]
  dad
}

# SCU admission
scu_adm <- function(dad){
  smh.xf <- readg(smh, ip_xfer)[Unit.Code =="1"]
  sbk.xf <- readg(sbk, ip_xfer)[Unit.Code %in%c("1")]
  uhn.xf <- readg(uhn, ip_xfer)[Unit.Code %in%c("1")]
  msh.xf <- readg(msh, xfer)[NURSE_UNIT_DISP=="ICU"]
  smh.scu <- readg(smh, ip_scu)
  sbk.scu <- readg(sbk, ip_scu)
  uhn.scu <- readg(uhn, ip_scu)
  msh.scu <- readg(msh, ip_scu)
  thp.scu <- readg(thp, ip_scu)
  
  sbk.scu <- sbk.scu[SCU.Unit.Number!="99"]
  thp.scu <- thp.scu[SCU.Unit.Number!="99"]
  
  scu.admit <- unique(c(smh.xf$EncID.new, smh.scu$EncID.new,
                        sbk.xf$EncID.new, sbk.scu$EncID.new,
                        uhn.xf$EncID.new, uhn.scu$EncID.new,
                        msh.xf$EncID.new, msh.scu$EncID.new, 
                        thp.scu$EncID.new))
  dad$SCU.adm <- dad$EncID.new%in%scu.admit
  dad[str_sub(EncID.new, 1, 2)=="15", SCU.adm:=NA]
  dad
}

# Number of Comorbidity and CCI
diag_var <- function(dad){
  ip.diag <- readg(gim, ip_diag, 
                   colClasses = list(character = c("EncID.new")))
  n.comorb <- ip.diag[,.N, by = EncID.new]
  names(n.comorb)[2] <- "Number.of.Comorbidity"
  dad <- merge(dad, n.comorb, by = "EncID.new", all.x = T, all.y = F)
  cci <- readg(gim, cci, 
               colClasses = list(character = c("EncID.new")))
  dad <- merge(dad, cci, by = "EncID.new", all.x = T, all.y = F)
  dad
}

# Readmission in 30 days
find_read <- function(dad){
  dad <- dad %>% arrange(Hash, ymd_hm(paste(Discharge.Date, Discharge.Time)))
  dad <- data.table(dad)
  time.to.next.admission<- 
    c(as.numeric(dad[2:nrow(dad), ymd_hm(paste(Admit.Date, Admit.Time))]-
                   dad[1:(nrow(dad)-1), 
                       ymd_hm(paste(Discharge.Date, Discharge.Time))])/(3600*24))
  dad$time.to.next.admission <- c(time.to.next.admission, NA)
  dad[(!duplicated(Hash, fromLast = T))|is.na(Hash), time.to.next.admission :=NA]
  dad[time.to.next.admission<=30, read.in.30 := TRUE]
  dad[is.na(read.in.30), read.in.30 := FALSE]
  dad[(ymd(Discharge.Date)>=(ymd("2015-04-01")-days(30)))|is.na(Hash), read.in.30:=NA]
  dad[is.na(read.in.30)] -> check
  dad
}

# fiscal year and cost
find_cost <- function(dad){
  dad[ymd(Discharge.Date)>=ymd("2010-04-01")&ymd(Discharge.Date)<ymd("2011-04-01"),
      fiscal.year := "2010"]
  dad[ymd(Discharge.Date)>=ymd("2011-04-01")&ymd(Discharge.Date)<ymd("2012-04-01"),
      fiscal.year := "2011"]
  dad[ymd(Discharge.Date)>=ymd("2012-04-01")&ymd(Discharge.Date)<ymd("2013-04-01"),
      fiscal.year := "2012"]
  dad[ymd(Discharge.Date)>=ymd("2013-04-01")&ymd(Discharge.Date)<ymd("2014-04-01"),
      fiscal.year := "2013"]
  dad[ymd(Discharge.Date)>=ymd("2014-04-01")&ymd(Discharge.Date)<ymd("2015-04-01"),
      fiscal.year := "2014"]
  dad[ymd(Discharge.Date)>=ymd("2015-04-01")&ymd(Discharge.Date)<ymd("2016-04-01"),
      fiscal.year := "2015"]
  CPWC <- readxl::read_excel("H:/GEMINI/Coding/CPWC_OCDM_Per Hospital.xlsx")
  names(CPWC) <- c("Institution.Number", "2010", "2011", "2012", "2013", "2014")
  CPWC.long <- melt(CPWC, id.vars = "Institution.Number", 
                    measure.vars = c("2010", "2011", "2012", "2013", "2014"), 
                    variable.name = "fiscal.year",
                    value.name = "CPWC", 
                    variable.factor = FALSE, value.factor = F)
  CPWC.long$fiscal.year <- as.character(CPWC.long$fiscal.year)
  dad <- merge(dad, CPWC.long, by = c("Institution.Number", "fiscal.year"), 
               all.x = T, all.y = F)
  smh.cmg <- readg(smh, ip_cmg)
  sbk.cmg <- readg(sbk, ip_cmg)
  uhn.cmg <- readg(uhn, ip_cmg)
  msh.cmg <- readg(msh, ip_cmg)
  thp.cmg <- readg(thp, ip_cmg)
  ip_cmg <- rbind(smh.cmg[,.(EncID.new, RIW.15)],
                  sbk.cmg[,.(EncID.new, RIW.15)],
                  uhn.cmg[,.(EncID.new, RIW.15)],
                  msh.cmg[,.(EncID.new, RIW.15)],
                  thp.cmg[,.(EncID.new, RIW.15)])
  dad <- merge(dad, ip_cmg, by = "EncID.new",all.x = T, all.y = F)
  dad[, Cost := as.numeric(RIW.15) * as.numeric(CPWC)]
  dad
}


# find CMG group

find_cmg <- function(dad){
  smh <- readg(smh, ip_cmg)
  sbk <- readg(sbk, ip_cmg)
  uhn <- readg(uhn, ip_cmg)
  msh <- readg(msh, ip_cmg)
  thp <- readg(thp, ip_cmg)
  cmg <- rbind(smh[, .(EncID.new, CMG)],
               sbk[, .(EncID.new, CMG)],
               uhn[, .(EncID.new, CMG)],
               msh[, .(EncID.new, CMG)],
               thp[, .(EncID.new, CMG)])
  cmg.list <- readxl::read_excel("H:/GEMINI/Coding/CMG Listing.xlsx", skip = 4)
  cmg.list$CMG <- as.character(cmg.list$CMG)
  cmg.list$CMG.Diagnosis <- cmg.list$`CMG Description`
  cmg <- merge(cmg, cmg.list[, c("CMG", "CMG.Diagnosis")], by = "CMG", all.x = T)
  dad <- merge(dad, unique(cmg), by = "EncID.new", all.x = T, all.y = F)
  dad
}

# find radiology tests
find_rad_var <- function(df){
  smh.rad <- readg(smh, rad)
  smh.ct <- readg(smh, ct)
  smh.mri <- readg(smh, mri)
  smh.us <- readg(smh.us, us)
  smh.xray <- readg(smh, xray)
  smh.ir <- fread("H:/GEMINI/Results/DesignPaper/smh.rad.freq.csv")
  smh.xray <- smh.xray[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
  smh.ct<- smh.ct[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
  smh.mri <- smh.mri[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
  smh.us <- smh.us[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
  smh.ir <- smh.rad[proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
  map.sbk <- readxl::read_excel("H:/GEMINI/Results/DesignPaper/rad.freq.table.new_AV.xlsx", sheet = 1)
  sbk.rad <- readg(sbk.rad, rad.csv)
  sum(sbk.rad$Test.Name%in%map.sbk$Test.Name)
  sbk.rad <- merge(sbk.rad, 
                   map.sbk[,c("Test.Name", "Test.Type", 
                              "Interventional Procedure")], 
                   by = "Test.Name",
                   all.x = T, all.y = F)
  sbk.us <- sbk.rad[Test.Type==2&is.na(`Interventional Procedure`), EncID.new]
  sbk.xray <- sbk.rad[Test.Type==1&is.na(`Interventional Procedure`), EncID.new]
  sbk.ct <- sbk.rad[Test.Type==3&is.na(`Interventional Procedure`), EncID.new]
  sbk.mri <- sbk.rad[Test.Type==4&is.na(`Interventional Procedure`), EncID.new]
  sbk.ir <- sbk.rad[`Interventional Procedure`==1, EncID.new]
  uhn.radip <- readg(uhn, rad_ip)
  uhn.rader <- readg(uhn, rad_er)
  uhn.rad <- rbind(uhn.radip, uhn.rader)
  map.uhn <- 
    readxl::read_excel("H:/GEMINI/Results/DesignPaper/rad.freq.table.new_AV.xlsx", sheet = 2)%>%
    data.table
  uhn.ir.names <- map.uhn[Interventional==1,Test.Name]
  uhn.ct <- uhn.rad[str_sub(ProcedureName,1,2) =="CT"&
                      !ProcedureName%in%uhn.ir.names, EncID.new]
  uhn.us <- uhn.rad[str_sub(ProcedureName,1,2) =="US"&
                      !ProcedureName%in%uhn.ir.names, EncID.new]
  uhn.xray <- uhn.rad[str_sub(ProcedureName,1,2) =="XR"&
                        !ProcedureName%in%uhn.ir.names, EncID.new]
  uhn.mri <- uhn.rad[str_sub(ProcedureName,1,3) =="MRI"&
                       !ProcedureName%in%uhn.ir.names, EncID.new]
  uhn.ir <- uhn.rad[ProcedureName%in%uhn.ir.names, EncID.new]
  msh.rader <- readg(msh, rad_er)
  msh.radip <- readg(msh, rad_ip)
  msh.rad <- rbind(msh.rader, msh.radip)
  msh.ct <- msh.rad[str_sub(ProcedureName,1,2) =="CT"&
                      !ProcedureName%in%uhn.ir.names, EncID.new]
  msh.us <- msh.rad[str_sub(ProcedureName,1,2) =="US"&
                      !ProcedureName%in%uhn.ir.names, EncID.new]
  msh.xray <- msh.rad[str_sub(ProcedureName,1,2) =="XR"&
                        !ProcedureName%in%uhn.ir.names, EncID.new]
  msh.mri <- msh.rad[str_sub(ProcedureName,1,3) =="MRI"&
                       !ProcedureName%in%uhn.ir.names, EncID.new]
  msh.ir <- msh.rad[ProcedureName%in%uhn.ir.names, EncID.new]
  ct.enc <- c(smh.ct, sbk.ct, uhn.ct, msh.ct)
  us.enc <- c(smh.us, sbk.us, uhn.us, msh.us)
  xray.enc <- c(smh.xray, sbk.xray, uhn.xray, msh.xray)
  mri.enc <- c(smh.mri, sbk.mri, uhn.mri, msh.mri)
  ir.enc <- c(smh.ir, sbk.ir, uhn.ir, msh.ir)
  # endo and dialysis
  int.map <- readxl::read_excel("H:/GEMINI/Results/DesignPaper/int.freq_AV.xlsx", 
                                sheet = 2, col_names = F)
  dia.code <- int.map$X2[1:3]
  endo.code <- int.map$X2[4:171]
  int.ip <- readg(gim, ip_int)
  int.er <- readg(gim, er_int)
  names(int.er)[3] <- "Intervention.Code"
  interv <- rbind(int.er[,.(EncID.new, Intervention.Code)],
                  int.ip[,.(EncID.new, Intervention.Code)])
  dia.enc <- interv[Intervention.Code%in%dia.code, EncID.new]
  endo.enc <- interv[Intervention.Code%in%endo.code, EncID.new]
  
  df <- data.table(df)
  df[,":="(xray = EncID.new%in%xray.enc,
           ct = EncID.new%in%ct.enc,
            mri = EncID.new%in%mri.enc,
            us = EncID.new%in%us.enc,
           ir = EncID.new%in%ir.enc,
            endo = EncID.new%in%endo.enc,
            dialysis = EncID.new%in%dia.enc)]
  df[, ctmrius := ct|mri|us]
  df[str_sub(EncID.new, 1, 2)=="15", ':='(xray = NA,
                                          ct = NA,
                                           mri = NA,
                                           us = NA,
                                          ir = NA,
                                           ctmrius = NA)]
  return(df)
}


find_transfusion <- function(df){
  smh.bb <- readg(smh, bb)
  sbk.bb <- readg(sbk, bb)
  uhn.bb <- rbind(readg(uhn, txm_er),
                  readg(uhn, txm_ip))
  msh.bb <- readg(msh, bb)
  rbc.trans.enc <- 
    c(smh.bb[Selected_product_code=="RCB",EncID.new],
          sbk.bb[Product.Group.Code=="RBC", EncID.new],
          uhn.bb[Blood_Component == "RBC", EncID.new],
          msh.bb[POPROD=="Red Blood Cells Concentrate", EncID.new])
  df[str_sub(EncID.new, 1, 2)!="15", RBC.trans := EncID.new%in%rbc.trans.enc]
  return(df)
}
# ---------------------- construct data set ------------------------------------
phy.all <- readg(gim, all.phy)
gemini.inc <- phy.all[(adm.GIM%in%c("y", "GP-GIM")|dis.GIM%in%c("y", "GP-GIM"))
                      |str_sub(EncID.new,1, 2)=="15", EncID.new]
dad <- merge(gemini_adm(), gemini_dad(), by = "EncID.new")
dad <- calcu_los(dad)
dad <- scu_adm(dad)
dad <- diag_var(dad)
dad <- find_read(dad)
dad <- find_cost(dad)
dad <- find_cmg(dad)
dad <- find_rad_var(dad)
dad <- find_transfusion(dad)
dad[is.na(CMG.Diagnosis)&!is.na(CMG), CMG] %>% table
apply(dad, 2, function(x)sum(is.na(x)))

dad[Discharge.Disposition==1, Discharge.Disposition1:= "Acute Care Hospital"]
dad[Discharge.Disposition==2, Discharge.Disposition1:= "Continuing Care"]
dad[Discharge.Disposition%in%c(3, 12), Discharge.Disposition1:= "Other"]
dad[Discharge.Disposition%in%c(4,5), Discharge.Disposition1:= "Home"]
dad[Discharge.Disposition==6, Discharge.Disposition1:= "Against Medical Advice"]
dad[Discharge.Disposition==7, Discharge.Disposition1:= "Death"]
dad[, age.cat:= ifelse(Age<60, "<60", ifelse(Age>80, ">80", "60-80"))]
table(dad$Discharge.Disposition1, useNA = "ifany")
fwrite(dad, "H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")

