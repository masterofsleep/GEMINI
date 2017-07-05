# ------------------------ new THP data cleaning -------------------------------
# -------------------------------- adm -----------------------------------------
adm.old <- readg(thp, adm)
adm.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.adm.nophi.csv")
sum(duplicated(adm.new$EncID.new))

n_miss <- function(df){
  apply(df, 2, function(x)sum(is.na(x)))
}

add_pr <- function(df){
  temp <- deparse(substitute(df))
  df$EncID.new <- paste("15", df$EncID.new, sep = "")
  assign(temp, df)
  print(temp)
}
n_miss(adm.new)
names(adm.old)
names(adm.new)
names(adm.new) <- c(names(adm.old)[1:14], "Discharging.Code", "Admitting.Code",
                    "Hash", "EncID.new")

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

# -------------------------------- ip cmg --------------------------------------
cmg.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.ip_cmg.nophi.csv")
cmg.old <- readg(thp, ip_cmg)
names(cmg.new) <- names(cmg.old)

# -------------------------------- ip diag -------------------------------------
diag.new <- fread("R:/GEMINI/_RESTORE/THP/CIHI/thp.ip_diag.nophi.csv")
diag.old <- readg(thp, ip_diag)
names(diag.new) <- names(diag.old)
diag.new[, Diagnosis.Code := str_replace_all(Diagnosis.Code, "\\.", "")]
add_pr(diag.new)


# -------------------------------- ip hig --------------------------------------
# -------------------------------- ip int --------------------------------------
# -------------------------------- ip scu --------------------------------------

# --------------------------------  er -----------------------------------------

# -------------------------------- er cacs -------------------------------------


# -------------------------------- er consult ----------------------------------


# -------------------------------- er diag -------------------------------------


