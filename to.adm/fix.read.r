# ------------------------ fix readmission -------------------------------------
dad <- fread("C:/Users/guoyi/Desktop/to.adm/design.paper.dad.csv")
dad <- dad %>% arrange(Hash, ymd_hm(paste(Discharge.Date, Discharge.Time)))
dad <- data.table(dad)
dad <- dad[!duplicated(EncID.new)]

time.to.next.admission<- c(as.numeric(dad[2:138481, ymd_hm(paste(Admit.Date, Admit.Time))]-
                                               dad[1:138480, ymd_hm(paste(Discharge.Date, Discharge.Time))])/(3600*24))
dad$time.to.next.admission <- c(time.to.next.admission, NA)

dad[!duplicated(Hash, fromLast = T), time.to.next.admission :=NA]
dad[,.(Hash, Admit.Date, time.to.next.admission)] -> check
dad[, time.since.last.admission := NULL]
dad[, read.in.30 := NULL]

dad[time.to.next.admission<=30, read.in.30 := TRUE]
dad[is.na(read.in.30), read.in.30 := FALSE]
dad[ymd(Discharge.Date)>=(ymd("2015-04-01")-days(30)), read.in.30:=NA]

table(dad$read.in.30, useNA = "ifany")

dad[ymd(Discharge.Date)>=(ymd("2015-04-01")-days(30)), Discharge.Date]
dad[read.in.30==T, time.to.next.admission]

fwrite(dad, "C:/Users/guoyi/Desktop/to.adm/design.paper.dad.csv")
