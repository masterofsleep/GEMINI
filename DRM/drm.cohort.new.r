# --------------------------- DRM cohort new -----------------------------------
# --------------------------- 2017-04-18 ---------------------------------------
library(gemini)
lib.pa()
rm(list = ls())
smh.phar <- readg(smh, phar, dt = T)
sbk.phar <- readg(sbk, phar, dt = T)
sbk.phar$EncID.new <- as.character(sbk.phar$EncID.new)
sbk.phar$ndc_din[!is.na(sbk.phar$ndc_din)&str_detect(sbk.phar$ndc_din, "-")] <-
  str_split(sbk.phar$ndc_din[!is.na(sbk.phar$ndc_din)&str_detect(sbk.phar$ndc_din, "-")], "-") %>% 
  unlist %>% matrix(ncol = 2, byrow = T) %>% `[`(,1)
sbk.phar$ndc_din <- gsub("(?<![0-9])0+", "", sbk.phar$ndc_din, perl = TRUE)
uhn.phar <- readg(uhn, phar.nophi, dt = T)
uhn.phar$DIN <- gsub("(?<![0-9])0+", "", uhn.phar$DIN, perl = TRUE)
drm.din <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/FINALDINLIST.xlsx")
drm.din2 <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/FINALDINLIST2.xls")
drm.din2$din <- gsub("(?<![0-9])0+", "", drm.din2$din, perl = TRUE)
drm.din2 <- drm.din2[!is.na(drm.din2$din),]
din.drm <- union(drm.din$`FINAL DINS`, drm.din2$din)
generic <- union(drm.din$Name, drm.din2$drugname)

# include only those receiving abox within 48 h
smh.abx <- smh.phar[din%in%din.drm|generic_name%in%generic]
sbk.abx <- sbk.phar[(ndc_din%in%din.drm|generic_name%in%generic)]
uhn.abx <- uhn.phar[(DIN%in%din.drm|toupper(Generic_Name)%in%generic)]

apply(smh.abx, 2, function(x)sum(is.na(x)))
apply(sbk.abx, 2, function(x)sum(is.na(x)))
apply(uhn.abx, 2, function(x)sum(is.na(x)))
# # frequency table of frequency
# data.table(table(smh.abx[,ord_frequency])) %>%
#   fwrite("H:/GEMINI/Results/DRM/smh.abx.freq.csv")
# data.table(table(sbk.abx[,frequency])) %>%
#   fwrite("H:/GEMINI/Results/DRM/sbk.abx.freq.csv")
# data.table(table(uhn.abx[,Frequency])) %>%
#   fwrite("H:/GEMINI/Results/DRM/uhn.abx.freq.csv")

# antibiotic in 48
abx.inc <- rbind(smh.abx[,.(abx.dttm = ymd_hm(paste(start_date, start_time)),
                            abx.stop.dttm = ymd_hm(paste(stop_date, stop_time)),
                           adm.dttm = ymd_hm(paste(Admit.Date, Admit.Time)),
                           dis.dttm = ymd_hm(paste(Discharge.Date, Discharge.Time)),
                           EncID.new)],
                sbk.abx[,.(abx.dttm = mdy_hms(paste(start_date, start_time)),
                           abx.stop.dttm = mdy_hms(paste(stop_date, stop_time)),
                           adm.dttm = ymd_hm(paste(Admit.Date, Admit.Time)),
                           dis.dttm = ymd_hm(paste(Discharge.Date, Discharge.Time)),
                           EncID.new)],
                uhn.abx[,.(abx.dttm = dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time)),
                           abx.stop.dttm = dmy_hm(paste(Order_Sto, Order_Stop_Time)),
                           adm.dttm = ymd_hm(paste(Admit.Date, Admit.Time)),
                           dis.dttm = ymd_hm(paste(Discharge.Date, Discharge.Time)),
                           EncID.new)])

abx.inc[is.na(abx.stop.dttm), abx.stop.dttm := dis.dttm]
apply(abx.inc, 2, function(x)sum(is.na(x)))

abx.d1 <- abx.inc[date(abx.dttm)<=date(adm.dttm)&date(abx.stop.dttm)>=date(adm.dttm)]
abx.d2 <- abx.inc[date(abx.dttm)<=date(adm.dttm)+days(1)&date(abx.stop.dttm)>=date(adm.dttm)+days(1)]
abx.d3 <- abx.inc[date(abx.dttm)<=date(adm.dttm)+days(2)&date(abx.stop.dttm)>=date(adm.dttm)+days(2)]

abx <- data.table(EncID.new = unique(abx.inc$EncID.new))
abx[, ':='(d1 = EncID.new%in%abx.d1$EncID.new,
           d2 = EncID.new%in%abx.d2$EncID.new,
           d3 = EncID.new%in%abx.d3$EncID.new)]
abx[, n.abx := d1 + d2 + d3]
table(abx$n.abx, useNA = "ifany")
