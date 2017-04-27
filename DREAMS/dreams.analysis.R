# ==============================================================================
# =========================  DREAMS ANALYSIS  ==================================
# =========================    March 1 2017   ==================================
library(gemini)
lib.pa()


#task1 
sbkechocombined <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SBK ECHO COMBINED.xlsx")%>%
  data.table %>% unique
names(sbkechocombined)

sbk.deid <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/Combined SBK Chart Pulls Deidentified.xlsx")%>%
  data.table %>% unique

setdiff(sbkechocombined$EncID.new, sbk.deid$`encoutner ID`)
setdiff(sbk.deid$`encoutner ID`, sbkechocombined$EncID.new)

sum(sbkechocombined$EncID.new%in%sbk.deid$`encoutner ID`)
sum(sbk.deid$`encoutner ID`%in%sbkechocombined$EncID.new)
sbkechocombined[EncID.new%in%sbkechocombined$EncID.new[duplicated(sbkechocombined$EncID.new)], 
                duplicated:= TRUE]
sbk.deid[`encoutner ID`%in%sbk.deid$`encoutner ID`[duplicated(sbk.deid$`encoutner ID`)],
         duplicated:= TRUE]
int.ip <- readg(sbk, ip_int)
int.er <- readg(sbk, er_int)
tpa <- c("1KG35HH1C",
         "1KV35HA1C",
         "1JW35HA1C",
         "1JW35HH1C",
         "1AA35HH1C",
         "1ZZ35HA1C",
         "1ZZ35YA1C")
unique(c(int.ip[Intervention.Code%in%tpa, EncID.new],
         int.er[Occurrence.Type%in%tpa, EncID.new])) %>% str_sub(3, 8) -> tpa.ex

sum(sbkechocombined$EncID.new%in%tpa.ex)
sum(sbk.deid$`encoutner ID`%in%tpa.ex)

sbkechocombined[EncID.new%in%sbk.deid$`encoutner ID`&!EncID.new%in%tpa.ex]%>%
  unique() %>% 
  fwrite("H:/GEMINI/Results/DREAM/201703/SBK ECHO COMBINED_processed.csv")
sbk.deid[!`encoutner ID`%in%tpa.ex] %>%
  fwrite("H:/GEMINI/Results/DREAM/201703/Combined SBK Chart Pulls Deidentified_processed.csv")



# task 2
process.dup <- function(dat){
  print(c(nrow(dat), nrow(unique(dat))))
  dat <- unique(dat)
  dat[EncID.new%in%dat$EncID.new[duplicated(dat$EncID.new)], duplicated := TRUE]
  return(dat)
}

smh.chart.combined <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SMH chart pulls COMBINED.xlsx")%>%
  filter(!is.na(EncID.new)) %>% data.table
process.dup(smh.chart.combined) %>%
  fwrite("H:/GEMINI/Results/DREAM/201703/SMH chart pulls COMBINED_processed.csv")

smh.echo.combined <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SMH ECHO COMBINED.xlsx")%>%
  filter(!is.na(EncID.new)) %>% data.table
process.dup(smh.echo.combined) %>%
  fwrite("H:/GEMINI/Results/DREAM/201703/SMH ECHO COMBINED_processed.csv")


# variable create
# charts part
smh.chart <- fread("H:/GEMINI/Results/DREAM/201703/files_NG/SMH chart pulls COMBINED_processed.csv")
#fix one row with EncID.new == 1
smh.chart$EncID.new[smh.chart$EncID.new==1] <- 911729
smh.chart <- smh.chart[!afib+ prevstroke + antipltprior + antipltDC + Acprior + ACDC + initAC >=100]
smh.chart[, ACNEW := ifelse(Acprior==10&ACDC%in%c(1:9),
                                     1, 2)]
fwrite(smh.chart, "H:/GEMINI/Results/DREAM/201703/variable_created/SMH chart pulls COMBINED_processed_newvar.csv")

sbk.chart <- fread("H:/GEMINI/Results/DREAM/201703/files_NG/Combined SBK Chart Pulls Deidentified_processed NG.csv")
names(sbk.chart) <- str_replace_all(names(sbk.chart), " ", "")
sbk.chart <- sbk.chart[!afib+ prevstroke + antipltprior + antipltDC + ACprior + ACDC + initAC >=100]
sbk.chart[, ACNEW := ifelse(ACprior==10&ACDC%in%c(1:9),
                            1, 2)]
fwrite(sbk.chart, "H:/GEMINI/Results/DREAM/201703/variable_created/Combined SBK Chart Pulls Deidentified_processed NG_newvar.csv")

# echo part
smh.echo <- fread("H:/GEMINI/Results/DREAM/201703/files_NG/SMH ECHO COMBINED_processed.csv")%>%
  filter(!is.na(EncID.new)) %>% data.table
smh.echo[,':='(LALVTHROMBY = ifelse(LALVthromb==1, 1,
                                    ifelse(LALVthromb%in%c(2, 3, 4), 2, 100)),
               VegY = ifelse(VEG==1, 1, ifelse(VEG==100, 100, 2)),
               PFOy = ifelse(PFO==1, 1, ifelse(PFO==100, 100, 2))
               )]
fwrite(smh.echo, "H:/GEMINI/Results/DREAM/201703/variable_created/SMH ECHO COMBINED_processed_newvar.csv")

sbk.echo <- fread("H:/GEMINI/Results/DREAM/201703/files_NG/SBK ECHO COMBINED_processed NG.csv")%>%
  filter(!is.na(EncID.new)) %>% data.table
sbk.echo[,':='(LALVTHROMBY = ifelse(LALVthromb==1, 1,
                                    ifelse(LALVthromb%in%c(2, 3, 4), 2, 100)),
               VegY = ifelse(VEG==1, 1, ifelse(VEG==100, 100, 2)),
               PFOy = ifelse(PFO==1, 1, ifelse(PFO==100, 100, 2)))]
fwrite(sbk.echo, "H:/GEMINI/Results/DREAM/201703/variable_created//SBK ECHO COMBINED_processed NG_newvar.csv")

# subset part 
smh.sub.thrombus <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SMH SUBSET March 3.xlsx", sheet = 1)
smh.sub.pfo <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SMH SUBSET March 3.xlsx", sheet = 2)
smh.sub.veg <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SMH SUBSET March 3.xlsx", sheet = 3)
smh.echo <- fread("H:/GEMINI/Results/DREAM/201703/variable_created/SMH ECHO COMBINED_processed_newvar NG.csv")
intersect(smh.echo$EncID.new, smh.sub.thrombus$EncID.new)
intersect(smh.echo$EncID.new, smh.sub.pfo$EncID.new)
intersect(smh.echo$EncID.new, smh.sub.veg$EncID.new)

intersect(smh.sub.thrombus$EncID.new, smh.sub.pfo$EncID.new)
intersect(smh.sub.thrombus$EncID.new, smh.sub.veg$EncID.new)
intersect(smh.sub.pfo$EncID.new, smh.sub.veg$EncID.new)









# ----------------------- variables to pull ------------------------------------

er.diag <- readg(gim, er_diag)[str_sub(EncID.new, 1, 2)%in%c("11", "12")]
ip.diag <- readg(gim, ip_diag)[str_sub(EncID.new, 1, 2)%in%c("11", "12")]
er.diag$Diagnosis.Code <- er.diag$ER.Diagnosis.Code
diag<- rbind(er.diag[,.(Diagnosis.Code, EncID.new)],
             ip.diag[,.(Diagnosis.Code, EncID.new)])
atrial.fibrillation <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("I48"))]
hypertension <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("I10", "I11", "I12", 
                                                               "I13", "I15"))]
hyperlipidemia <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("E78"))]
hemorrhagic.stroke <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("I60", "I61", "I62"))]
diabetes <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("E10", "E11", "E13", "E14"))]
coronary.artery.disease <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("I21", "I22", 
                                                                          "I25", "I23", 
                                                                          "I24"))]
congestive.heart.failure <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("I50", "I110", 
                                                                           "I130", "I132"))]
# ----------------------- los and time to TTE ----------------------------------
smh.echo <- readg(smh, echo, dt = T)
smh.timeTTE0 <- smh.echo[dmy(ADMITDATE) == dmy(StudyStartDateTime)]
smh.timeTTE1 <- smh.echo[dmy(ADMITDATE) < dmy(StudyStartDateTime)&
                           !EncID.new%in%smh.timeTTE0$EncID.new]
sbk.echo <- readg(sbk, echo, dt = T)
sbk.timeTTE0 <- sbk.echo[mdy(str_sub(`Test Performed Date/time`, 1, 11))==
                           ymd(Admit.Date)]
sbk.timeTTE1 <- sbk.echo[mdy(str_sub(`Test Performed Date/time`, 1, 11))>
                           ymd(Admit.Date)&!EncID.new%in%sbk.timeTTE0$EncID.new]
timeTTE0 <- c(smh.timeTTE0$EncID.new, sbk.timeTTE0$EncID.new)
timeTTE1 <- c(smh.timeTTE1$EncID.new, sbk.timeTTE1$EncID.new)
timeTTE <- rbind(data.table(EncID.new = timeTTE0, timeTTE = 0),
                 data.table(EncID.new = timeTTE1, timeTTE = 1))
demo.var <-  fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv", 
              select = c("EncID.new", "LoS", "Age", "Gender"))[str_sub(EncID.new, 1, 2)%in%c("11", "12")]

demo.var[,':='(afib = as.numeric(EncID.new%in%atrial.fibrillation),
               hypertension = as.numeric(EncID.new%in%hypertension),
               hyperlipidemia = as.numeric(EncID.new%in%hyperlipidemia),
               hemorrhagic.stroke = as.numeric(EncID.new%in%hemorrhagic.stroke),
               diabetes = as.numeric(EncID.new%in%diabetes),
               coronary.artery.disease = as.numeric(EncID.new%in%coronary.artery.disease),
               congestive.heart.failure = as.numeric(EncID.new%in%congestive.heart.failure))]
demo.var$EncID.new <- as.character(demo.var$EncID.new)

dream.vars <- merge(demo.var, timeTTE, by = "EncID.new", all.x = T)

fwrite(dream.vars, "H:/GEMINI/Results/DREAM/201703/variable_created/dream.vars.csv")




# merge subset to chart pull 
sbk.sub.thrombus <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SBK Subset Deidentified March 3.xlsx", sheet = 2)%>% data.table
sbk.sub.pfo <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SBK Subset Deidentified March 3.xlsx", sheet = 1)%>%data.table
sbk.sub.veg <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SBK Subset Deidentified March 3.xlsx", sheet = 3)%>%data.table
sbk.echo.combined <- fread("H:/GEMINI/Results/DREAM/201703/variable_created/SBK ECHO COMBINED_processed NG_newvar NG.csv")%>%data.table

apply(sbk.echo.combined, MARGIN = 2, FUN = function(x) sum(is.na(x)))

## names to combine
# thrombus: ACchange, 
# pfo: NewAC, repair, doppdone, DVTdop, 
# veg: IVAB

# merge thrombus
names(sbk.echo.combined)
names(sbk.sub.thrombus)
names(sbk.sub.pfo)
names(sbk.sub.veg)

sbk.echo.combined[, ":="(IVAB = NULL,
                         repair = NULL,
                         doppdone = NULL,
                         DVTdop = NULL)]
sbk.echo.combined <- merge(sbk.echo.combined, sbk.sub.thrombus[,.(EncID.new, Acchange)], 
                           by = "EncID.new", all.x = T)
sbk.echo.combined <- merge(sbk.echo.combined, 
                           sbk.sub.pfo[,.(EncID.new, NewAC, repair, 
                                          doppdone, DVTdop)], 
                           by = "EncID.new", all.x = T)
sbk.echo.combined <- merge(sbk.echo.combined, sbk.sub.veg[,.(EncID.new, IVAB)], 
                           by = "EncID.new", all.x = T)

sbk.echo.combined[,':='(LALVTHROMBY = ifelse(LALVthromb==1, 1,
                                    ifelse(LALVthromb%in%c(2, 3, 4), 2, 100)),
               VegY = ifelse(VEG==1, 1, ifelse(VEG==100, 100, 2)),
               PFOy = ifelse(PFO==1, 1, ifelse(PFO==100, 100, 2)))]

fwrite(sbk.echo.combined, "H:/GEMINI/Results/DREAM/201703/variable_created/SBK ECHO COMBINED_YG_march14.csv")





smh.sub.thrombus <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SMH SUBSET.xlsx", sheet = 1) %>% data.table
smh.sub.pfo <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SMH SUBSET.xlsx", sheet = 2) %>% data.table
smh.sub.veg <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SMH SUBSET March 3.xlsx", sheet = 3) %>% data.table
smh.echo <- fread("H:/GEMINI/Results/DREAM/201703/variable_created/SMH ECHO COMBINED_processed_newvar NG.csv") %>% data.table
apply(smh.echo, MARGIN = 2, FUN = function(x) sum(is.na(x)))
sum(duplicated(smh.sub.thrombus$EncID.new))
sum(duplicated(smh.sub.pfo$EncID.new))
sum(duplicated(smh.sub.veg$EncID.new))

smh.echo[, ':='(repair = NULL,
                doppdone = NULL,
                DVTdop = NULL)]
## names to combine
# thrombus: ACchange, 
# pfo: NewAC, repair, doppdone, DVTdop, 
# veg: IVAB

smh.echo <- merge(smh.echo, unique(smh.sub.thrombus[3:7, .(EncID.new, Acchange)]), 
                  by = "EncID.new", all.x = T)
smh.echo <- merge(smh.echo, unique(smh.sub.pfo[,.(EncID.new, repair, doppdone, DVTdop, newAC)]),
                  by = "EncID.new", all.x = T)
smh.echo <- merge(smh.echo, smh.sub.veg[,.(EncID.new, IVAB = DCIVAB)],
                  by = "EncID.new", all.x = T)
fwrite(smh.echo, "H:/GEMINI/Results/DREAM/201703/variable_created/SMH ECHO COMBINED_YG_march14.csv")

table(smh.echo$LALVthromb)
table(sbk.echo.combined$LALVthromb)
table(c(smh.echo$LALVthromb, sbk.echo.combined$LALVthromb))

table(smh.echo$PFO)
table(sbk.echo.combined$PFO)
table(c(smh.echo$PFO, sbk.echo.combined$PFO))

table(smh.echo$VEG)
table(sbk.echo.combined$VEG)
table(c(smh.echo$VEG, sbk.echo.combined$VEG))

table(smh.echo$Mvsten)
table(sbk.echo.combined$Mvsten)
table(c(smh.echo$Mvsten, sbk.echo.combined$Mvsten))



fread("H:/GEMINI/Results/DREAM/201703/variable_created/dream.vars.csv") -> check

sum(check$coronary.artery.disease)





# ---------------------------- table 1 -----------------------------------------
setwd("R:/GEMINI-DREAM/DATA FINAL")
smh.chart <- fread("SMH chart pulls COMBINED_processed_newvar March 25.csv")
sbk.chart <- fread("Combined SBK Chart Pulls Deidentified_processed NG_newvar.csv")

# track death
smh.death <- fread("R:/GEMINI-DREAM/processed_030117/SMH chart pulls COMBINED_processed.csv")[afib == 500]
sbk.death <- fread("R:/GEMINI-DREAM/processed_030117/Combined SBK Chart Pulls Deidentified_processed NG.csv")[afib==500]
names(sbk.death) <- names(sbk.chart)[1:12]
smh.chart <- rbind(smh.chart, smh.death, fill = T)
sbk.chart <- rbind(sbk.chart, sbk.death, fill = T)
#check number of dups
sum(duplicated(smh.chart$EncID.new))
sum(duplicated(sbk.chart$encoutnerID))
smh.chart[, ACNEW := ifelse(Acprior==10&ACDC%in%c(1:9),
                            1, 2)]
sbk.chart[, ACNEW := ifelse(ACprior==10&ACDC%in%c(1:9),
                            1, 2)]


unique(smh.chart[,c(1:11,15), with = F])[duplicated(EncID.new)|duplicated(EncID.new, fromLast = T)]
unique(sbk.chart[,c(1:10,13), with = F])[duplicated(encoutnerID)|duplicated(encoutnerID, fromLast = T)]



smh.echo <- fread("SMH ECHO COMBINED_YG_march14_NG march 15.csv")
sum(duplicated(smh.echo$EncID.new))
df<- unique(smh.echo[,c(1:19, 24:34), with = F])
df[duplicated(df[,.(EncID.new, Conclusions)])|duplicated(df[,.(EncID.new, Conclusions)], fromLast = T)]

sbk.echo <- readxl::read_excel("SBK ECHO COMBINED_YG_march14_NG March 15.xlsx")%>%data.table
df<- unique(sbk.echo[,-5, with = F])
df[duplicated(df[,.(EncID.new, Report)])|duplicated(df[,.(EncID.new, Report)], fromLast = T)]


tpa.exclude <- fread("H:/GEMINI/Results/DREAM/tpa.exclude.csv")
sum(sbk.chart$encoutnerID%in%tpa.exclude$tpa.exclude)
sum(sbk.echo$EncID.new%in%tpa.exclude$tpa.exclude)

chartpull <- rbind(unique(smh.chart[,.(EncID.new = paste("11", EncID.new, sep = ""), 
                                afib, prevstroke, antipltprior, antipltDC,
                                ACprior = Acprior, ACDC, ACNEW)]),
                   unique(sbk.chart[,.(EncID.new = paste("12", encoutnerID, sep = ""),
                                afib, prevstroke, antipltprior, antipltDC,
                                ACprior, ACDC, ACNEW)]))
chartpull <- chartpull[!duplicated(EncID.new)]


dreams.cohort <- c(paste("12", sbk.chart$encoutnerID, sep = ""), paste("11", smh.chart$EncID.new, sep = "")) %>% unique
demo.all <- fread("H:/GEMINI/Results/DREAM/201703/variable_created/dream.vars.csv") %>% unique
demo.all[EncID.new%in%demo.all[duplicated(EncID.new), EncID.new]] -> check
table(demo.all$timeTTE, str_sub(demo.all$EncID.new, 1,2), useNA = "ifany")

sum(!dreams.cohort%in%cohort$EncID.new)
dreams.cohort[!dreams.cohort%in%cohort$EncID.new]

cohort <- demo.all[EncID.new%in%dreams.cohort]
table(str_sub(cohort$EncID.new,1,2))
length(unique(cohort$EncID.new))
table(cohort$timeTTE, str_sub(cohort$EncID.new, 1,2), useNA = "ifany")

cohort$EncID.new <- as.character(cohort$EncID.new)
cohort <- merge(cohort, chartpull, by = "EncID.new")



hcn <- rbind(fread("H:/GEMINI/DataBackup/Data170214/SMH/CIHI/smh.adm.nophi.csv")[,.(Hash, EncID.new)],
             fread("H:/GEMINI/DataBackup/Data170214/SBK/CIHI/sbk.adm.nophi.csv")[,.(Hash, EncID.new)])

dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")

cohort <- merge(cohort, hcn, by = "EncID.new", all.x = T, all.y = F)
dad$EncID.new <- as.character(dad$EncID.new)
cohort <- merge(cohort, dad[,.(Admit.Date, EncID.new, Discharge.Disposition)],by = "EncID.new", all.x = T, all.y = F)
cohort <- cohort %>% arrange(Hash, ymd(Admit.Date)) %>% data.table
cohort[,.N, by = Hash][N>5]

cohort[(!duplicated(Hash))|Hash=="c3ed0844860fb77e4fcacbc5124ad71bede04a0579a862a5301a8dd132957692"]

table(cohort[,.(ACDC, Discharge.Disposition)])

fwrite(cohort, "H:/GEMINI/Results/DREAM/201704/dreams.cohort.csv")


cohort <- cohort[(!duplicated(Hash))|Hash=="c3ed0844860fb77e4fcacbc5124ad71bede04a0579a862a5301a8dd132957692"]

library(tableone)
names(cohort)
cat.var <- names(cohort)[c(4,6:11,13,14:19)]
vars <- names(cohort)[c(2:4,6:11,13,14:19, 23)]
cohort$site <- str_sub(cohort$EncID.new, 1, 2)

cohort[,':='(antipltprior = ifelse(antipltprior<10, 1, antipltprior),
             antipltDC = ifelse(antipltDC<10, 1, antipltDC),
             ACprior = ifelse(ACprior<10, 1, ACprior),
             ACDC = ifelse(ACDC<10, 1, ACDC),
             ACNEW = factor(ACNEW, levels = c("2", "1")))]
CreateTableOne(vars = vars, factorVars = cat.var, data = cohort, strata = 'site')


# ---------------------------- 04-21 -------------------------------------------
# ------------------- new vars required by Nikki -------------------------------
table(str_sub(cohort$EncID.new, 1, 2))

table(cohort$EncID.new) %>% table
smh.echo <- readg(smh, echo, dt = T)[EncID.new%in%cohort$EncID.new]
sbk.echo <- readg(sbk, echo, dt = T)[EncID.new%in%cohort$EncID.new]
sbk.echo[str_sub(`Test Performed Date/time`, -2, -1)=="AM"&
           str_sub(`Test Performed Date/time`, 13, 14)<12, 
         Performed.DtTm := mdy_hms(str_sub(`Test Performed Date/time`, 1, 20))]
sbk.echo[str_sub(`Test Performed Date/time`, -2, -1)=="AM"&
           str_sub(`Test Performed Date/time`, 13, 14)==12, 
         Performed.DtTm := mdy_hms(str_sub(`Test Performed Date/time`, 1, 20)) - hours(12)]
sbk.echo[str_sub(`Test Performed Date/time`, -2, -1)=="PM"&
           str_sub(`Test Performed Date/time`, 13, 14)<12, 
         Performed.DtTm := mdy_hms(str_sub(`Test Performed Date/time`, 1, 20)) + hours(12)]
sbk.echo[str_sub(`Test Performed Date/time`, -2, -1)=="PM"&
           str_sub(`Test Performed Date/time`, 13, 14)==12, 
         Performed.DtTm := mdy_hms(str_sub(`Test Performed Date/time`, 1, 20))]
table(smh.echo$ProcedureName)
table(sbk.echo$TestName)

# number of people got TEE
smh.echo[ProcedureName=="Transesophageal Echo", EncID.new] %>% unique %>% length
sbk.echo[TestName=="Echo (Transesophageal)", EncID.new] %>% unique %>% length

# counts of how many people got 0, 1, 2, 3, 4, echos
table(smh.echo$EncID.new) %>% table
table(sbk.echo$EncID.new) %>% table

smh.echo[ProcedureName=="UNK"]



# calculate time to first echo
smh.echo[, time.to.first.echo :=  as.numeric(dmy(StudyStartDateTime)- dmy(ADMITDATE))]
sbk.echo[, time.to.first.echo := as.numeric(Performed.DtTm-
                           ymd_hm(paste(Admit.Date, Admit.Time)))/(24*60)]
smh.firstecho <- smh.echo %>% arrange(EncID.new, dmy(StudyStartDateTime)) %>% 
  filter(!duplicated(EncID.new))
sbk.firstecho <- sbk.echo %>% arrange(EncID.new, 
                                      mdy(str_sub(`Test Performed Date/time`, 1, 11))) %>%
  filter(!duplicated(EncID.new))
summary(smh.firstecho$time.to.first.echo)                                        
summary(sbk.firstecho$time.to.first.echo)                                        


# Track readmission
hcn <- rbind(readg(smh, adm)[,.(Hash, EncID.new)],
             readg(sbk, adm)[,.(Hash, EncID.new)])
cohort <- merge(cohort, hcn, by = "EncID.new")
multi.adm <- cohort[, .N, by = Hash]

cohort[Hash%in%multi.adm$Hash] %>% arrange(Hash)-> check
table(multi.adm$N)

table(str_sub(cohort$EncID.new, 1, 2), cohort$afib.y)





# ------------------ new variables required by Mike ----------------------------
cohort <- fread("H:/GEMINI/Results/DREAM/201704/dreams.cohort.csv")
setwd("R:/GEMINI-DREAM/DATA FINAL")
smh.echo <- fread("SMH ECHO COMBINED_YG_march14_NG march 15.csv")
sum(duplicated(smh.echo$EncID.new))
df1<- unique(smh.echo[,c(1:19, 24:34), with = F])
df1[duplicated(df1[,.(EncID.new, Conclusions)])|duplicated(df1[,.(EncID.new, Conclusions)], fromLast = T)]

sbk.echo <- readxl::read_excel("SBK ECHO COMBINED_YG_march14_NG March 15.xlsx")%>%data.table
df2<- unique(sbk.echo[,-5, with = F])
df[duplicated(df[,.(EncID.new, Report)])|duplicated(df[,.(EncID.new, Report)], fromLast = T)]
df1[, EncID.new := paste("11", EncID.new, sep = "")]
df2[, EncID.new := paste("12", EncID.new, sep = "")]
names(df1)
names(df2)
# numbers with PFO
name <- "number with PFO"
smh <- sum(cohort$EncID.new%in%
      c(df1[PFOy=="1", EncID.new]))
sbk <- sum(cohort$EncID.new%in%
      c(df2[PFO=="1", EncID.new]))

# PFO clos?
name <- c(name, "number with PFO closure")
smh <- c(smh, sum(cohort$EncID.new%in%
             df1[repair=="1", EncID.new]))
sbk <- c(sbk, sum(cohort$EncID.new%in%
             df2[repair=="1", EncID.new]))

# number of DVT
name <- c(name, "number with DVT diagnosed")
smh <- c(smh, sum(cohort$EncID.new%in%
      c(df1[DVTdop=="1", EncID.new])))
sbk <- c(sbk, sum(cohort$EncID.new%in%
      c(df2[DVTdop=="1", EncID.new])))

# number of thrombus
name <- c(name, "number with thrombus")
smh <- c(smh, sum(cohort$EncID.new%in%
      c(df1[LALVTHROMBY=="1", EncID.new])))
sbk <- c(sbk, sum(cohort$EncID.new%in%
      c(df2[LALVTHROMBY=="1", EncID.new])))

# number of anticoagulation initiated
table(cohort$ACNEW)
name <- c(name, "number with anticoagulation initiated")
smh <- c(smh, sum(cohort$site=="11"&cohort$ACNEW==1))
sbk <- c(sbk, sum(cohort$site=="12"&cohort$ACNEW==1))

# veg
name <- c(name, "number with vegetation")
smh <- c(smh, sum(cohort$EncID.new%in%df1[VegY=="1", EncID.new]))
sbk <- c(sbk, sum(cohort$EncID.new%in%df2[VegY=="1", EncID.new]))

# antibiotic started
name <- c(name, "number with antibiotics started")
smh <- c(smh, sum(cohort$EncID.new%in%df1[IVAB=="1", EncID.new]))
sbk <- c(sbk, sum(cohort$EncID.new%in%df2[IVAB=="1", EncID.new]))

smh <- paste(smh, " (", sprintf("%.1f", smh/649*100), ")")
sbk <- paste(sbk, " (", sprintf("%.1f", sbk/845*100), ")")

df <- data.table(name, smh, sbk)
names(df) <- c("variable", "smh (%)", "sbk (%)")
df
df %>%
  fwrite("H:/GEMINI/Results/DREAM/201704/numbers.for.mike.csv")

table(df1$IVAB)
table(df2$IVAB)


cohort[afib.y=="500", EncID.new]
all.dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")
compare.sets(cohort[afib.y=="500", EncID.new], all.dad[Discharge.Disposition=="7"&EncID.new%in%cohort$EncID.new, EncID.new])

sum(cohort$EncID.new%in%all.dad$EncID.new)
