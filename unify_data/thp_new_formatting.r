# ------------------------ new THP data cleaning -------------------------------
library(gemini)
lib.pa()
n_miss <- function(df){
  apply(df, 2, function(x)sum(is.na(x)))
}

add_pr <- function(df){
  df$EncID.new <- paste("15", df$EncID.new, sep = "")
  return(df)
}
n_mi
# -------------------------------- adm -----------------------------------------
adm.old <- readg(thp, adm)
adm.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.adm.nophi.csv")
sum(duplicated(adm.new$EncID.new))
names(adm.old)
names(adm.new)
names(adm.new) <- c(names(adm.old)[1:14], "Discharging.Code", "Admitting.Code",
                    "Hash", "EncID.new")
adm.new <- add_pr(adm.new)
fwrite(adm.new, "H:/GEMINI/Data/THP/CIHI/thp.adm.nophi.csv")

# -------------------------------- dad -----------------------------------------

dad.old <- readg(thp, dad)
dad.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.dad.nophi.csv")
n_miss(dad.new)
dad.new[, ':='(DischargeDate_s=NULL,
               DischargeDate_d=NULL)]
names(dad.new)
names(dad.old)
names(dad.new)[1:25] <- names(dad.old)[1:25]
dad.new[, ':='(Admit.Date = as.character(ymd(Admit.Date)),
               Discharge.Date = as.character(ymd(Discharge.Date))
               )]
dad.new$Admit.Time <- paste("000", dad.new$Admit.Time, sep = "")
dad.new$Admit.Time <- paste(str_sub(dad.new$Admit.Time, -4, -3), 
                            ":", str_sub(dad.new$Admit.Time, -2, -1), sep = "")
dad.new$Discharge.Time <- paste("000", dad.new$Discharge.Time, sep = "")
dad.new$Discharge.Time <- paste(str_sub(dad.new$Discharge.Time, -4, -3), 
                                ":", str_sub(dad.new$Discharge.Time, -2, -1), sep = "")
dad.new <- add_pr(dad.new)
fwrite(dad.new, "H:/GEMINI/Data/THP/CIHI/thp.ip_dad.nophi.csv")

# -------------------------------- ip cmg --------------------------------------
cmg.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.ip_cmg.nophi.csv")
cmg.old <- readg(thp, ip_cmg)
names(cmg.new) <- names(cmg.old)
cmg.new <- add_pr(cmg.new)
cmg.new[, Diagnosis.For.CMG.Assignment := str_replace_all(Diagnosis.For.CMG.Assignment, "\\.|-", "")]
cmg.new[, CMG.Intervention := str_replace_all(CMG.Intervention, "\\.|-", "")]
fwrite(cmg.new, "H:/GEMINI/Data/THP/CIHI/thp.ip_cmg.nophi.csv")

# -------------------------------- ip diag -------------------------------------
diag.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.ip_diag.nophi.csv")
diag.old <- readg(thp, ip_diag)
names(diag.new) <- names(diag.old)
diag.new[, Diagnosis.Code := str_replace_all(Diagnosis.Code, "\\.", "")]
add_pr(diag.new)
diag.new<- add_pr(diag.new)
fwrite(diag.new, "H:/GEMINI/Data/THP/CIHI/thp.ip_diag.nophi.csv")

# -------------------------------- ip hig --------------------------------------
hig.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.ip_hig.nophi.csv")
hig.old <- readg(thp, ip_hig)
names(hig.new) <- names(hig.old)
hig.new <- add_pr(hig.new)
fwrite(hig.new, "H:/GEMINI/Data/THP/CIHI/thp.ip_hig.nophi.csv")

# -------------------------------- ip int --------------------------------------
int.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.ip_int.nophi.csv")
int.old <- readg(thp, ip_int)
names(int.new) <- names(int.old)
int.new[, Intervention.Code := str_replace_all(Intervention.Code, "\\.|-", "")]
int.new<- add_pr(int.new)
fwrite(int.new, "H:/GEMINI/Data/THP/CIHI/thp.ip_int.nophi.csv")




# -------------------------------- ip scu --------------------------------------
scu.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.ip_scu.nophi.csv")
scu.old <- readg(thp, ip_scu)
names(scu.new) <- names(scu.old)
scu.new <- add_pr(scu.new)
fwrite(scu.new, "H:/GEMINI/Data/THP/CIHI/thp.ip_scu.nophi.csv")


# --------------------------------  er -----------------------------------------
er.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.er.nophi.csv")
er.old <- readg(thp, .er.nophi)
names(er.new) <- names(er.old)
thptime <- function(x){
  dat <- x
  dat[!is.na(x)] <- paste("000", x[!is.na(x)],sep = "")
  dat[!is.na(x)] <- paste(str_sub(dat[!is.na(x)], -4,-3),":",
                          str_sub(dat[!is.na(x)], -2,-1),sep = "")
  return(dat)
}
thptime(er.new$Ambulance.Arrival.Time)


er.new[,`:=`(Ambulance.Arrival.Date = ymd(Ambulance.Arrival.Date),
          Triage.Date = ymd(Triage.Date),
          Registration.Date = ymd(Registration.Date),
          Date.of.Physician.Initial.Assessment = ymd(Date.of.Physician.Initial.Assessment),
          Disposition.Date = ymd(Disposition.Date),
          Ambulance.Arrival.Time = thptime(Ambulance.Arrival.Time),
          Triage.Time = thptime(Triage.Time),
          Registration.Time = thptime(Registration.Time),
          Time.of.Physician.Initial.Assessment = thptime(Time.of.Physician.Initial.Assessment),
          Disposition.Time = thptime(Disposition.Time))]
er.new[,`:=`(Date.Left.ER = ymd(Date.Left.ER),
          Time.Left.ER = thptime(Time.Left.ER))]
er.new <- add_pr(er.new)

fwrite(er.new, "H:/GEMINI/Data/THP/CIHI/thp.er.nophi.csv")

# -------------------------------- er cacs -------------------------------------
cacs.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.er_cacs.nophi.csv")
cacs.old <- readg(thp, cacs)
names(cacs.new) <- names(cacs.old)
cacs.new <- add_pr(cacs.new)
fwrite(cacs.new, "H:/GEMINI/Data/THP/CIHI/thp.er_cacs.nophi.csv")

# -------------------------------- er consult ----------------------------------
consults.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.er_consults.nophi.csv")
consults.old <- readg(thp, consults)
names(consults.new) <- names(consults.old)
consults.new[, ER.Consult.Date := as.character(ymd(ER.Consult.Date))]
consults.new <- add_pr(consults.new)
fwrite(consults.new, "H:/GEMINI/Data/THP/CIHI/thp.er_consults.nophi.csv")


# -------------------------------- er diag -------------------------------------
er.diag.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.er_diag.nophi.csv")
er.diag.old <- readg(thp, er.diag)
names(er.diag.new) <- names(er.diag.old)
er.diag.new[, ER.Diagnosis.Code := str_replace_all( ER.Diagnosis.Code, "\\.|-", "")]
er.diag.new <- add_pr(er.diag.new)
fwrite(er.diag.new, "H:/GEMINI/Data/THP/CIHI/thp.er_diag.nophi.csv")

# -------------------------------- er int --------------------------------------
er.int.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.er_int.nophi.csv")
er.int.old <- readg(thp, er.int)
names(er.int.new) <- names(er.int.old)[c(1,3,2,4:7)]
er.int.new[, Occurrence.Type := str_replace_all(Occurrence.Type, "\\.|-", "")]
er.int.new <- add_pr(er.int.new)
fwrite(er.int.new, "H:/GEMINI/Data/THP/CIHI/thp.er_int.nophi.csv")



# ---check old -------
adm.old <- fread("H:/GEMINI/DataBackup/Data170214/THP/CIHI/thp.adm.nophi.csv")
dad.old <- fread("H:/GEMINI/DataBackup/Data170214/THP/CIHI/thp.ip_dad.nophi.csv")

dad.old <- merge(dad.old, adm.old[, .(EncID.new, Institution)])
ggplot(dad.old, aes(x = ymd(Discharge.Date), fill = Institution)) + 
  geom_histogram(binwidth = 10)

adm.new <- readg(thp, adm)
compare.sets(adm.new$EncID.new, adm.old$EncID.new)
dad.old[, in.new:= EncID.new%in%adm.new$EncID.new]
ggplot(dad.old, aes(ymd(Discharge.Date), fill = in.new)) + geom_histogram(binwidth = 10)

adm.old[!EncID.new%in%adm.new$EncID.new]
all.phy <- readg(gim, all.phy)
all.phy[EncID.new%in%adm.old[!EncID.new%in%adm.new$EncID.new, EncID.new]] -> check

apply(check, 2, function(x)sum(is.na(x)))
phy.list <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.new2.csv")


check[, .N, by = adm.code][order(N, decreasing = T)]
check[, .N, by = dis.code][order(N, decreasing = T)]
