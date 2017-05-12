# ---- preparing data for people missed in original inclusion for DREAMS -------
# ----------------------------  2017-05-12  ------------------------------------
rm(list = ls())
library(gemini)
lib.pa()

# add MRN to a file
add_smh_mrn <- function(df){
  link <- readg(SMH, LINKLIST_NEWHASH)[,.(MRN, EncID.new = paste("11", EncID.new, sep = ""))]
  df$EncID.new <- as.character(df$EncID.new)
  df <- merge(df, link, by = "EncID.new", all.x = T, all.y = F)
  return(data.table(df))
}


# find unique patients at two sites
unique_patient <- function(stroke_enc){
  adm <- readg(gim, adm)[str_sub(EncID.new, 1, 2)%in%c("11", "12")]
  dad <- readg(gim, dad)[str_sub(EncID.new, 1, 2)%in%c("11", "12")]
  patient <- merge(adm[,.(Hash, EncID.new)],
                   dad[,.(EncID.new, Gender, Age, Admit.Date, Admit.Time, 
                          Discharge.Date, Discharge.Time,
                          Discharge.Disposition)]) %>% 
    arrange(Hash, ymd_hm(paste(Admit.Date, Admit.Time))) %>% data.table
  patient <- patient[patient$EncID.new%in%stroke_enc]
  return(data.table(patient[(!duplicated(Hash))]))
}

# find those to be included in DREAMS study
dreams_inc <- function(){
  inc_diag <- c("G450","G451", "G452", "G453", "G458", "G459", "H341", "I63", "I64")
  ip.diag_2site <- readg(gim, ip_diag)[str_sub(EncID.new, 1, 2)%in%c("11", "12")]
  d1 <- ip.diag_2site[startwith.any(Diagnosis.Code, inc_diag)&Diagnosis.Type=="M"]
  data.table(d1)
}

is_stroke <- dreams_inc() %>% data.table
uni_patient <- unique_patient(is_stroke$EncID.new) %>% data.table

dreams_cohort <- uni_patient[uni_patient$EncID.new%in%is_stroke$EncID.new]

# table of diagnosis in the cohort
is_stroke[EncID.new%in%dreams_cohort$EncID.new, .(Diagnosis.Code, str_sub(EncID.new, 1, 2))] %>% table

# check if G452 shows in all diag data
ip_diag <- readg(gim, ip_diag)
ip_diag[Diagnosis.Code=="G452"]

# --------------------------- merge ECHO to file -------------------------------

# calculate number of echo and add to echo data
n_echo <- function(df){
  necho <- data.table(table(df$EncID.new))
  names(necho) <- c("EncID.new", "number_of_echo_in_14days")
  df <- merge(df, necho, by = "EncID.new", all.x = T, all.y = F)
  return(data.table(df))
}

smh_echo_dreams <- function(){
  smh.echo <- readg(smh, echo, dt = T)[EncID.new%in%dreams_cohort$EncID.new]
  # within 14 days
  smh.echo.in14 <- smh.echo[((dmy(StudyStartDateTime)-ymd(Admit.Date))<=14)]
  # add MRN
  smh.echo.in14 <- add_smh_mrn(smh.echo.in14)
  # add a column for number of echo for each MRN
  smh.echo.in14 <- n_echo(smh.echo.in14)
  # formatting
  smh.echo.dreams <- smh.echo.in14[,.(MRN, EncID.new, Admit.Date, Admit.Time,
                                      Discharge.Date, Discharge.Time, 
                                      number_of_echo_in_14days,
                                      Study.ID = StudyId,
                                      Test.Name = ProcedureName,
                                      Test.Date = dmy(StudyStartDateTime),
                                      Time.from.Admission = 
                                        as.numeric(dmy(StudyStartDateTime) - 
                                                     ymd(Admit.Date)),
                                      Report = Conclusions)]
  return(data.table(smh.echo.dreams))
}
smh_echo <- smh_echo_dreams()
sbk_echo_dreams <- function(){
  sbk.echo <- readg(sbk, echo, dt = T)[EncID.new%in%dreams_cohort$EncID.new]
  # fix time 
  sbk_echo_fix_time <- function(){
    sbk.echo[, Test.Date := mdy(str_sub(`Test Performed Date/time`, 1, 11))]
    sbk.echo[str_sub(`Test Performed Date/time`, -2, -1)=="AM"&
               str_sub(`Test Performed Date/time`, -14, -13)=="12", 
             Test.Time := 
               paste(as.numeric(str_sub(`Test Performed Date/time`, -14, -13)) -12,
                                     str_sub(`Test Performed Date/time`, -12, -7),
                                     sep = "")]
    sbk.echo[str_sub(`Test Performed Date/time`, -2, -1)=="AM"&
               str_sub(`Test Performed Date/time`, -14, -13)!="12", 
             Test.Time := str_sub(`Test Performed Date/time`, -14, -7)]
    sbk.echo[str_sub(`Test Performed Date/time`, -2, -1)=="PM"&
               str_sub(`Test Performed Date/time`, -14, -13)=="12", 
             Test.Time := str_sub(`Test Performed Date/time`, -14, -7)]
    sbk.echo[str_sub(`Test Performed Date/time`, -2, -1)=="PM"&
               str_sub(`Test Performed Date/time`, -14, -13)!="12", 
             Test.Time := 
               paste(as.numeric(str_sub(`Test Performed Date/time`, -14, -13)) + 12,
                                     str_sub(`Test Performed Date/time`, -12, -7),
                                     sep = "")]
  }
  sbk_echo_fix_time()
  # within 14 days
  sbk.echo.in14 <- sbk.echo[Test.Date - ymd(Admit.Date) <= 14]
  # add number of test
  sbk.echo.in14 <- n_echo(sbk.echo.in14)
  # formatting
  sbk.echo.dreams <- 
    sbk.echo.in14[, .(EncID.new, Admit.Date, Admit.Time,
                      Discharge.Date, Discharge.Time,
                      number_of_echo_in_14days,
                      Study.ID = SID,
                      Test.Name = TestName,
                      Test.Date, Test.Time,
                      Time.from.Admission = as.numeric(ymd_hms(paste(Test.Date, Test.Time)) - 
                        ymd_hm(paste(Admit.Date, Admit.Time)))/60/24,
                      Report)]
}

# validate with old data 
# compare.sets(cohort$EncID.new, dreams_cohort$EncID.new)
# old.cohort.enc <- cohort[!EncID.new%in%dreams_cohort$EncID.new, EncID.new]
# ip_diag <- readg(gim, ip_diag)
# ip_diag[EncID.new%in%old.cohort.enc&Diagnosis.Type=="M"]
# sum(old.cohort.enc%in%uni_patient$EncID.new)
