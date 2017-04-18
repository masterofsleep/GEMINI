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

# frequency table of frequency
data.table(table(smh.abx[,ord_frequency])) %>%
  fwrite("H:/GEMINI/Results/DRM/smh.abx.freq.csv")
data.table(table(sbk.abx[,frequency])) %>%
  fwrite("H:/GEMINI/Results/DRM/sbk.abx.freq.csv")
data.table(table(uhn.abx[,Frequency])) %>%
  fwrite("H:/GEMINI/Results/DRM/uhn.abx.freq.csv")

# antibiotic in 48
abx.inc <- rbind(smh.abx[,.(abx.dttm = ymd_hm(paste(start_date, start_time)),
                           adm.dttm = ymd_hm(paste(Admit.Date, Admit.Time)),
                           EncID.new)],
                sbk.abx[,.(abx.dttm = mdy_hms(paste(start_date, start_time)),
                           adm.dttm = ymd_hm(paste(Admit.Date, Admit.Time)),
                           EncID.new)],
                uhn.abx[,.(abx.dttm = dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time)),
                           adm.dttm = ymd_hm(paste(Admit.Date, Admit.Time)),
                           EncID.new)])
abx.48.d1 <- abx.48[abx.dttm >= adm.dttm&
                      abx.dttm < adm.dttm + days(1)]
abx.48.d2 <- abx.48[abx.dttm >= adm.dttm + days(1)&
                      abx.dttm < adm.dttm + days(2)]
abx.48.d3 <- abx.48[abx.dttm >= adm.dttm + days(2)&
                      abx.dttm < adm.dttm + days(3)]

