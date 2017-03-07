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
hypertension <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("I10", "I11", "I12", 
                                                               "I13", "I15"))]
hyperlipidemia <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("E78"))]
hemorrhagic.stroke <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("I60", "I61", "I62"))]
diabetes <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("E10", "E11", "E13", "E14"))]
coronary.artery.disease <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("I121", "I122", 
                                                                          "I125", "I123", 
                                                                          "I124"))]
congestive.heart.failure <- diag$EncID.new[startwith.any(diag$Diagnosis.Code, c("L50", "L110", 
                                                                           "L130", "I132"))]
# ----------------------- los and time to TTE ----------------------------------
smh.echo <- readg(smh, echo, dt = T)
smh.timeTTE0 <- smh.echo[dmy(ADMITDATE) == dmy(StudyStartDateTime)]
smh.timeTTE1 <- smh.echo[dmy(ADMITDATE) < dmy(StudyStartDateTime)&
                           !EncID.new%in%timeTTE0$EncID.new]
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

demo.var[,':='(hypertension = as.numeric(EncID.new%in%hypertension),
               hyperlipidemia = as.numeric(EncID.new%in%hyperlipidemia),
               hemorrhagic.stroke = as.numeric(EncID.new%in%hemorrhagic.stroke),
               coronary.artery.disease = as.numeric(EncID.new%in%coronary.artery.disease),
               congestive.heart.failure = as.numeric(EncID.new%in%congestive.heart.failure))]
demo.var$EncID.new <- as.character(demo.var$EncID.new)

dream.vars <- merge(demo.var, timeTTE, by = "EncID.new")

fwrite(dream.vars, "H:/GEMINI/Results/DREAM/201703/variable_created/dream.vars.csv")
