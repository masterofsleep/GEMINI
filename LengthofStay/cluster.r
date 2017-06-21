# ---------------------------- Cluster Study -----------------------------------
# -------------------------------- Zahra ---------------------------------------
library(gemini)
lib.pa()
phy <- readg(gim, all.phy)
cohort <- phy[adm.GIM%in%c("y", "GP-GIM")|dis.GIM%in%c("y", "GP-GIM")]
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")

cluster.cohort <- dad[EncID.new%in%cohort$EncID.new&
                        Hash!=""]
find_g1 <- function(){
  return(intersect(cluster.cohort[, .N, by = Hash][N>=3, Hash],
                   cluster.cohort[, .(total.LoS = sum(Acute.LoS)), 
                                  by = Hash][total.LoS>=30, Hash]))
}

g1 <- find_g1()

find_g2 <- function(){
  cohort.after2011 <- cluster.cohort[ymd(Admit.Date)>=ymd("2011-04-01")]
  cohort.before2011 <- cluster.cohort[ymd(Admit.Date)<ymd("2011-04-01")]
  g2 <- intersect(cohort.after2011[, .N, by = Hash][N>=3, Hash],
            cohort.after2011[, .(total.LoS = sum(Acute.LoS)), 
                           by = Hash][total.LoS>=30, Hash])
  return(g2[!g2%in%cohort.before2011$Hash])
}
g2 <- find_g2()

find_g3 <- function(){
  cohort.after2011 <- cluster.cohort[ymd(Admit.Date)>=ymd("2012-04-01")]
  cohort.before2011 <- cluster.cohort[ymd(Admit.Date)<ymd("2012-04-01")]
  g3 <- intersect(cohort.after2011[, .N, by = Hash][N>=3, Hash],
                  cohort.after2011[, .(total.LoS = sum(Acute.LoS)), 
                                   by = Hash][total.LoS>=30, Hash])
  return(g3[!g3%in%cohort.before2011$Hash])
}

g3 <- find_g3()

cluster.cohort <- cluster.cohort[Hash%in%c(g1, g2, g3)]
cluster.cohort <- cluster.cohort%>% arrange(Hash, ymd(Admit.Date)) %>% data.table

time.to.next.visit <- as.numeric(ymd_hm(cluster.cohort[2:nrow(cluster.cohort), 
                                            paste(Admit.Date, Admit.Time)]) - 
  ymd_hm(cluster.cohort[1:(nrow(cluster.cohort)-1), 
                        paste(Discharge.Date, Discharge.Time)]) )/3600/24

cluster.cohort$time.to.next.visit <- c(time.to.next.visit, NA)
cluster.cohort[!duplicated(Hash, fromLast = T), time.to.next.visit:=NA]
# check <- cluster.cohort[, .(Hash, time.to.next.admission, time.to.next.visit)]
# 
# check[is.na(time.to.next.admission)&!is.na(time.to.next.visit)]
# check[!is.na(time.to.next.admission)&
#         is.na(time.to.next.visit)]

cluster.cohort[, time.to.first.adm := as.numeric(ymd_hm(paste(Admit.Date, Admit.Time))-
                 ymd_hm("2010-04-01 00:00"))/3600/24]

find_summary <- function(data){
  uni_patient <- data[!duplicated(Hash)]
  data.frame(sample_size = nrow(uni_patient),
             mean_number_of_admission = mean(table(data$Hash)),
             max_number_of_admission = max(table(data$Hash)),
             mean_age = mean(uni_patient$Age),
             percent_male = mean(uni_patient$Gender=="M")*100,
             mean_number_of_comorbidity = mean(uni_patient$Number.of.Comorbidity),
             mean_length_of_stay = mean(data$Acute.LoS),
             mean_time_between_admissions = mean(data$time.to.next.visit, na.rm = T),
             mean_time_to_first_admission = mean(uni_patient$time.to.first.adm))
}
  
cbind(Group = c("Group1", "Group2", "Group3"),
  rbind(find_summary(cluster.cohort[Hash%in%g1]),
      find_summary(cluster.cohort[Hash%in%g2]),
      find_summary(cluster.cohort[Hash%in%g3]))) %>% 
  fwrite("H:/GEMINI/Results/LengthofStay/cluster/group_summary.csv")



sum_var <- ddply(cluster.cohort, ~Hash, summarize,
                 number_of_admissions = length(EncID.new),
                 average_time_between_admissions = mean(time.to.next.visit, na.rm = T))
uni_patient <- cluster.cohort[!duplicated(Hash)]
ip.diag <- readg(gim, ip_diag)
diag.names <- fread("R:/GEMINI/Coding/CIHI/ICD_header.csv")
ip.diag.withname <- merge(ip.diag[Diagnosis.Type=="M"], 
                          diag.names[, .(Diagnosis.Code = Code,
                                         Diagnosis = Desc2)],
                 by = "Diagnosis.Code", all.x = T)
hypertension.enc <- ip.diag[Diag3=="I10", EncID.new]
type2diabetes.enc <- ip.diag[Diag3=="E11", EncID.new]
afib.enc <- ip.diag[Diag3=="I48", EncID.new]
dyslipidemia.enc <- ip.diag[Diag3=="E78", EncID.new]
electrolyteabn.enc <- ip.diag[Diag3=="E87", EncID.new]
chf.enc <- ip.diag[Diag3=="I50", EncID.new]

input_var <- 
  cluster.cohort[!duplicated(Hash), .(Hash, EncID.new, Age, Gender, Number.of.Comorbidity,
                     Charlson.Comorbidity.Index1 = 
                       Charlson.Comorbidity.Index==1,
                     Charlson.Comorbidity.Index2 = 
                       Charlson.Comorbidity.Index==2,
                     Charlson.Comorbidity.Index3 = 
                       Charlson.Comorbidity.Index==3,
                     Charlson.Comorbidity.Index4 =
                       Charlson.Comorbidity.Index==4,
                     Charlson.Comorbidity.Index5 = 
                       Charlson.Comorbidity.Index==5,
                     Charlson.Comorbidity.Index6 = 
                       Charlson.Comorbidity.Index==6,
                     Charlson.Comorbidity.Index7 = 
                       Charlson.Comorbidity.Index==7,
                     Charlson.Comorbidity.Index8 = 
                       Charlson.Comorbidity.Index==8,
                     Charlson.Comorbidity.Index9 = 
                       Charlson.Comorbidity.Index==9,
                     Charlson.Comorbidity.Index10 = 
                       Charlson.Comorbidity.Index==10,
                     Charlson.Comorbidity.Index11 = 
                       Charlson.Comorbidity.Index==11,
                     Charlson.Comorbidity.Index12 = 
                       Charlson.Comorbidity.Index==12,
                     Charlson.Comorbidity.Index13 = 
                       Charlson.Comorbidity.Index==13,
                     hypertension = EncID.new%in%hypertension.enc,
                     type2diabetes = EncID.new%in%type2diabetes.enc,
                     afib_and_flutter = EncID.new%in%afib.enc,
                     dyslipidemia = EncID.new%in%dyslipidemia.enc,
                     electrolyte_abnormality = EncID.new%in%electrolyteabn.enc,
                     heart_failure = EncID.new%in%chf.enc,
                     Discharge.Disposition1)]

input_var <- merge(input_var, ip.diag.withname[,.(Diagnosis.Code,
                                                  Diagnosis, EncID.new)], by = "EncID.new")

# find radiology tests
find_clinical_var <- function(df){
  smh.rad <- readg(smh, rad)[EncID.new%in%df$EncID.new]
  smh.ct <- readg(smh, ct)[EncID.new%in%df$EncID.new]
  smh.mri <- readg(smh, mri)[EncID.new%in%df$EncID.new]
  smh.us <- readg(smh.us, us)[EncID.new%in%df$EncID.new]
  smh.xray <- readg(smh, xray)[EncID.new%in%df$EncID.new]
  smh.ir <- fread("H:/GEMINI/Results/DesignPaper/smh.rad.freq.csv")
  smh.xray <- smh.xray[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name],.N, by = EncID.new]
  smh.ct<- smh.ct[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name],.N, by = EncID.new]
  smh.mri <- smh.mri[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name],.N, by = EncID.new]
  smh.us <- smh.us[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], .N, by = EncID.new]
  smh.ir <- smh.rad[proc_desc_long%in%smh.ir[Interventional==1, Test.Name], .N, by = EncID.new]
  map.sbk <- readxl::read_excel("H:/GEMINI/Results/DesignPaper/rad.freq.table.new_AV.xlsx", sheet = 1)
  sbk.rad <- readg(sbk.rad, rad.csv)[EncID.new%in%df$EncID.new]
  sum(sbk.rad$Test.Name%in%map.sbk$Test.Name)
  sbk.rad <- merge(sbk.rad, 
                   map.sbk[,c("Test.Name", "Test.Type", 
                              "Interventional Procedure")], 
                   by = "Test.Name",
                   all.x = T, all.y = F)
  sbk.us <- sbk.rad[Test.Type==2&is.na(`Interventional Procedure`),.N, by = EncID.new]
  sbk.xray <- sbk.rad[Test.Type==1&is.na(`Interventional Procedure`),.N, by = EncID.new]
  sbk.ct <- sbk.rad[Test.Type==3&is.na(`Interventional Procedure`),.N, by = EncID.new]
  sbk.mri <- sbk.rad[Test.Type==4&is.na(`Interventional Procedure`),.N, by = EncID.new]
  sbk.ir <- sbk.rad[`Interventional Procedure`==1,.N, by = EncID.new]
  uhn.radip <- readg(uhn, rad_ip)[EncID.new%in%df$EncID.new]
  uhn.rader <- readg(uhn, rad_er)[EncID.new%in%df$EncID.new]
  uhn.rad <- rbind(uhn.radip, uhn.rader)
  map.uhn <- 
    readxl::read_excel("H:/GEMINI/Results/DesignPaper/rad.freq.table.new_AV.xlsx", sheet = 2)%>%
    data.table
  uhn.ir.names <- map.uhn[Interventional==1,Test.Name]
  uhn.ct <- uhn.rad[str_sub(ProcedureName,1,2) =="CT"&
                      !ProcedureName%in%uhn.ir.names,.N, by = EncID.new]
  uhn.us <- uhn.rad[str_sub(ProcedureName,1,2) =="US"&
                      !ProcedureName%in%uhn.ir.names,.N, by = EncID.new]
  uhn.xray <- uhn.rad[str_sub(ProcedureName,1,2) =="XR"&
                        !ProcedureName%in%uhn.ir.names,.N, by = EncID.new]
  uhn.mri <- uhn.rad[str_sub(ProcedureName,1,3) =="MRI"&
                       !ProcedureName%in%uhn.ir.names,.N, by = EncID.new]
  uhn.ir <- uhn.rad[ProcedureName%in%uhn.ir.names,.N, by = EncID.new]
  msh.rader <- readg(msh, rad_er)[EncID.new%in%df$EncID.new]
  msh.radip <- readg(msh, rad_ip)[EncID.new%in%df$EncID.new]
  msh.rad <- rbind(msh.rader, msh.radip)
  msh.ct <- msh.rad[str_sub(ProcedureName,1,2) =="CT"&
                      !ProcedureName%in%uhn.ir.names,.N, by = EncID.new]
  msh.us <- msh.rad[str_sub(ProcedureName,1,2) =="US"&
                      !ProcedureName%in%uhn.ir.names,.N, by = EncID.new]
  msh.xray <- msh.rad[str_sub(ProcedureName,1,2) =="XR"&
                        !ProcedureName%in%uhn.ir.names,.N, by = EncID.new]
  msh.mri <- msh.rad[str_sub(ProcedureName,1,3) =="MRI"&
                       !ProcedureName%in%uhn.ir.names,.N, by = EncID.new]
  msh.ir <- msh.rad[ProcedureName%in%uhn.ir.names,.N, by = EncID.new]
  ct.enc <- rbind(smh.ct, sbk.ct, uhn.ct, msh.ct) %>% rename(number_of_ct = N)
  us.enc <- rbind(smh.us, sbk.us, uhn.us, msh.us) %>% rename(number_of_us = N)
  xray.enc <- rbind(smh.xray, sbk.xray, uhn.xray, msh.xray) %>% rename(number_of_xray = N)
  mri.enc <- rbind(smh.mri, sbk.mri, uhn.mri, msh.mri) %>% rename(number_of_mri = N)
  ir.enc <- rbind(smh.ir, sbk.ir, uhn.ir, msh.ir) %>% rename(number_of_ir = N)
  
  # blood transfusion
  smh.bb <- readg(smh, bb)[EncID.new%in%df$EncID.new]
  sbk.bb <- readg(sbk, bb)[EncID.new%in%df$EncID.new]
  uhn.bb <- rbind(readg(uhn, txm_er),
                  readg(uhn, txm_ip))[EncID.new%in%df$EncID.new]
  msh.bb <- readg(msh, bb)[EncID.new%in%df$EncID.new]
  rbc.trans.enc <- 
    rbind(smh.bb[Selected_product_code=="RCB",.N, by = EncID.new],
      sbk.bb[Product.Group.Code=="RBC", .N, by = EncID.new],
      uhn.bb[Blood_Component == "RBC",.N, by = EncID.new],
      msh.bb[POPROD=="Red Blood Cells Concentrate",.N, by = EncID.new]) %>%
    rename(number_of_transfusion = N)
  # endo and dialysis
  int.map <- readxl::read_excel("H:/GEMINI/Results/DesignPaper/int.freq_AV.xlsx", 
                                sheet = 2, col_names = F)
  dia.code <- int.map$X2[1:3]
  endo.code <- int.map$X2[4:171]
  int.ip <- readg(gim, ip_int)[EncID.new%in%df$EncID.new]
  int.er <- readg(gim, er_int)[EncID.new%in%df$EncID.new]
  names(int.er)[3] <- "Intervention.Code"
  interv <- rbind(int.er[,.(EncID.new, Intervention.Code)],
                  int.ip[,.(EncID.new, Intervention.Code)])
  interv$EncID.new <- as.character(interv$EncID.new)
  dia.enc <- interv[Intervention.Code%in%dia.code,.N, by = EncID.new]%>% rename(number_of_dialysis = N)
  endo.enc <- interv[Intervention.Code%in%endo.code,.N, by = EncID.new]%>% rename(number_of_endoscopy = N)
  
  df <- data.table(df)
  df[,":="(xray = EncID.new%in%xray.enc$EncID.new,
           ct = EncID.new%in%ct.enc$EncID.new,
           mri = EncID.new%in%mri.enc$EncID.new,
           us = EncID.new%in%us.enc$EncID.new,
           ir = EncID.new%in%ir.enc$EncID.new,
           endoscopy = EncID.new%in%endo.enc$EncID.new,
           dialysis = EncID.new%in%dia.enc$EncID.new,
           rbc_trans = EncID.new%in%rbc.trans.enc$EncID.new)]
  all.clinical <- Reduce(function(x, y) merge(x, y, all = T), 
                         list(xray.enc, ct.enc, us.enc, mri.enc, 
                              rbc.trans.enc, ir.enc, dia.enc, endo.enc))
  df$EncID.new <- as.character(df$EncID.new)
  df <- merge(df, all.clinical, by = "EncID.new", all.x = T, all.y = F)
  df <- data.frame(df)
  set_zero <- function(varname){
    df[is.na(df[, varname]), varname] <<- 0
  }
  varnames <- names(all.clinical)
  for(i in varnames[2:9]){
    set_zero(i)
  }
  
  set_na <- function(varname){
    df[str_sub(df$EncID.new, 1, 2)=="15", varname] <<- NA
  }
  
  for(i in c("xray", "ct", "mri", "us", "ir", "rbc_trans",
             "number_of_xray", "number_of_ct",
             "number_of_us", "number_of_mri",
             "number_of_ir", "number_of_transfusion")){
    set_na(i)
  }
  return(df)
}

input_var <- find_clinical_var(input_var)


input_var_final <- merge(sum_var, input_var, by = "Hash")


fwrite(input_var_final, "H:/GEMINI/Results/LengthofStay/cluster/input_var.csv")




# response var 
res_var <- cluster.cohort[, .(EncID.new, Hash,
                              Acute.LoS, 
                              Number.of.ALC.Days,
                              ICU.utilization = SCU.adm,
                              Death = Discharge.Disposition==7)]
fwrite(res_var, "H:/GEMINI/Results/LengthofStay/cluster/reponse_var.csv")
