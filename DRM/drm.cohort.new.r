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

fwrite(abx, "H:/GEMINI/Results/DRM/new_170424/abx.new.csv")


# --------------------------- culture ------------------------------------------
library(gemini)
lib.pa()
rm(list = ls())
smh.mic <- readg(smh, micro, dt = T)
smh.mic <- smh.mic[!Org_Name%in%c("Coagulase negative Staphylococcus species",
                                  "Methicillin Resistant Staphylococcus aureus")]
smh.mic[, paste := paste(Test_ID, Source)]
culture.marked <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/CultureClassification_Feb9.xlsx",
                                     sheet = 1) %>% data.table
culture.marked[,paste := paste(Test_ID, Source)]
names(culture.marked)[6] <- "Urine"
names(smh.mic)[3] <- "Specimen_Collection_Date/Time"
cul.ns.smh <- smh.mic[paste%in%culture.marked[NonScreening==1, paste]&
                        !is.na(Isolate_num)]
smh.mic[is.na(Isolate_num)] %>% dim
cul.ns.smh <- cul.ns.smh[mdy_hm(`Specimen_Collection_Date/Time`)>=ymd_hm(paste(Admit.Date, Admit.Time))&
                           mdy_hm(`Specimen_Collection_Date/Time`)<=(ymd_hm(paste(Admit.Date, Admit.Time))+hours(48))]
smh.drm.cul <- merge(cul.ns.smh, 
                     culture.marked[,.(paste, Urine , Blood, Resp, Screening, 
                                       NonScreening, NonBacterial, unknown)], 
                     by = "paste", all.x = T)
smh.drm.cul <- smh.drm.cul[,.(EncID.new, Urine , Blood, Resp, Screening, 
                              NonScreening, NonBacterial)]


smh.mic[!is.na(Isolate_num)] -> pos
length(unique(pos$Order_Number))/length(unique(smh.mic$Order_Number))
length(pos$Order_Number)/length(smh.mic$Order_Number)

#sbk 
culture.marked <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/CultureClassification_Feb9.xlsx",
                                     sheet = 2)%>% data.table
names(culture.marked)[5] <- "Urine"
culture.marked[, paste := paste(culture_test_cd, description, specimen_source)]
cul.ns <- culture.marked[NonScreening==1]
sbk.mic <- readg(sbk, micro_pos.csv, dt = T)
sbk.mic <- sbk.mic[!Organism%in%c("Staphylococcus capitis",
                                  "Staphylococcus epidermidis",
                                  "Staphylococcus schleiferi",
                                  "Staphylococcus species",
                                  "Coagulase negative staphylococci")]
12315/(115887+12315
)

sbk.mic[, paste:= paste(culture_test_cd, description, specimen_source)]
cul.ns.sbk <- sbk.mic[paste%in%cul.ns$paste&
                        mdy_hm(specimen_collection_datetime)>=ymd_hm(paste(Admit.Date, Admit.Time))&
                        mdy_hm(specimen_collection_datetime)<=(ymd_hm(paste(Admit.Date, Admit.Time))+hours(48))]
sbk.drm.cul <- merge(cul.ns.sbk, 
                     culture.marked[,.(paste, Urine, Blood, Resp, Screening, 
                                       NonScreening, NonBacterial)],
                     by = "paste", all.x = T)
sbk.drm.cul <- sbk.drm.cul[,.(EncID.new, Urine, Blood, Resp, Screening, 
                              NonScreening, NonBacterial)]

# UHN
# Check the concordance bewteen the two marked files 
uhn.marked <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/UHN_Source Setup.xls",
                                 sheet = 1)%>%data.table
names(uhn.marked)[3] <- "Urine"

uhn.micro <- rbind(readg(uhn.tgh, micro),
                   readg(uhn.twh, micro), fill = T)
uhn.micro <- uhn.micro[!ORG%in%c("Coagulase negative Staphylococcus",
                                 "Coagulase negative staphylococcus (not S. lugdunensis)",
                                 "Staphylococcus saprophyticus")]
cul.ns.uhn <- uhn.micro[SRC%in%uhn.marked[NonScreening==1, SOURCE_ID]|
                          (SRC=="NONE"&TEST%in%c("BC", "UCS")), ]
cul.ns.uhn <- merge(cul.ns.uhn[,.(EncID.new, TEST, SRC)], 
                    uhn.marked[NonScreening==1, .(SOURCE_ID, Urine, Blood, Resp, Screening,
                                                  NonScreening, NonBacterial)],
                    by.x = "SRC", by.y = "SOURCE_ID", all.x = T, all.y = F)
cul.ns.uhn[SRC=="NONE"&TEST=="BC", ':='(Blood =1, NonScreening=1)]
cul.ns.uhn[SRC=="NONE"&TEST=="UCS", ':='(Urine = 1, NonScreening=1)]

drm.cul <- rbind(smh.drm.cul, sbk.drm.cul,
                 cul.ns.uhn[,.(EncID.new, Urine, Blood, Resp, Screening, NonScreening, NonBacterial)])

fwrite(drm.cul, "H:/GEMINI/Results/DRM/new_170424/drm.cul.ns.new.csv")


# ------------------------------ feasibility -----------------------------------
rm(list = ls())
antibio.inc <- fread("H:/GEMINI/Results/DRM/new_170424/abx.new.csv")
ns.cul <- fread("H:/GEMINI/Results/DRM/new_170424/drm.cul.ns.new.csv")
drm.cohort <- intersect(antibio.inc[n.abx>=3, EncID.new], ns.cul$EncID.new)
all.dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")
all.dad[EncID.new%in%antibio.inc[n.abx>=3, EncID.new], 
        Institution.Number] %>% table
all.dad[EncID.new%in%ns.cul$EncID.new, Institution.Number] %>% table
all.dad[EncID.new%in%drm.cohort, Institution.Number] %>% table
all.dad[Institution.Number=="54265", Institution.Number := "tgh"]
all.dad[Institution.Number=="54266", Institution.Number := "twh"]
drm.cohort <- drm.cohort[!drm.cohort%in%all.dad[Discharge.Disposition=="7"&LoS<=3, EncID.new]]

sepsis <- "A41"
fever <- "R50"
pneumonia <- c("J100", "J110", "J120", "J121", "J122", "J128", "J129",
               "J13", "J14", "J15", "J160", "J168", "J17", "J18")
uti <- c("N10", "N12", "N151", "N300", "N308", "N309","N410", "N412",
         "N413", "N510", "N390")



ip.diag <- readg(gim, ip_diag)
er.diag <- readg(gim, er_diag)
drm.ip.diag <- ip.diag[Diagnosis.Type=="M", .(Diagnosis.Code, EncID.new)]
drm.er.diag <- er.diag[ER.Diagnosis.Type=="M", 
                       .(ER.Diagnosis.Code, EncID.new)]
drm.cohort <- merge(drm.ip.diag[EncID.new%in%drm.cohort],
                    drm.er.diag[EncID.new%in%drm.cohort],
                    by = "EncID.new", all.x = T, all.y = T)
drm.cohort[,':='(Urine = EncID.new%in%ns.cul[Urine==1, EncID.new],
                 Blood = EncID.new%in%ns.cul[Blood==1, EncID.new],
                 Resp = EncID.new%in%ns.cul[Resp==1, EncID.new],
                 Other.sterile = EncID.new%in%ns.cul[
                   is.na(Urine)&is.na(Blood)&is.na(Resp)&NonBacterial==1, EncID.new],
                 Other.non.sterile = EncID.new%in%ns.cul[
                   is.na(Urine)&is.na(Blood)&is.na(Resp)&is.na(NonBacterial), EncID.new])]
diag.names <- readxl::read_excel("H:/GEMINI/Results/Diabetes/MRD.freqtable.xlsx")
drm.cohort <- merge(drm.cohort, diag.names[,c(1,4)], by.x = "Diagnosis.Code",
                    by.y = "Diagnosis.Code", all.x = T)
drm.cohort <- merge(drm.cohort, diag.names[,c(1,4)], by.x = "ER.Diagnosis.Code",
                    by.y = "Diagnosis.Code", all.x = T)
names(drm.cohort)[9:10] <- c("IP.Diagnosis", "ER.Diagnosis")
ip.diag.freq <- data.table(table(drm.cohort[!Diagnosis.Code%in%uti,
                                            .(Diagnosis.Code, IP.Diagnosis)]))[N!=0]
er.diag.freq <- data.table(table(drm.cohort[!ER.Diagnosis.Code%in%uti,
                                            .(ER.Diagnosis.Code, ER.Diagnosis)]))[N!=0]
ip.diag.all <- ip.diag[EncID.new%in%drm.cohort$EncID.new[!drm.cohort$Diagnosis.Code%in%uti], 
                       str_sub(Diagnosis.Code,1,3)] %>% table %>% data.table
er.diag.all <- er.diag[EncID.new%in%drm.cohort$EncID.new[!drm.cohort$ER.Diagnosis.Code%in%uti], 
                       str_sub(ER.Diagnosis.Code,1,3)] %>% table %>% data.table
ip.diag.all <- merge(ip.diag.all, diag.names[,c(1,4)], by.x = ".",
                     by.y = "Diagnosis.Code", all.x = T)

er.diag.all <- merge(er.diag.all, diag.names[,c(1,4)], by.x = ".",
                     by.y = "Diagnosis.Code", all.x = T)
fwrite(ip.diag.freq[order(N, decreasing = T)], "H:/GEMINI/Results/DRM/new_170424/ip.diag.freq.csv")
fwrite(er.diag.freq[order(N, decreasing = T)], "H:/GEMINI/Results/DRM/new_170424/er.diag.freq.csv")


fwrite(ip.diag.all[order(N, decreasing = T)], "H:/GEMINI/Results/DRM/new_170424/ip.diag.all.freq.csv")
fwrite(er.diag.all[order(N, decreasing = T)], "H:/GEMINI/Results/DRM/new_170424/er.diag.all.freq.csv")



# mrp
mrp <- readg(gim, all.phy)
drm.cohort <- merge(drm.cohort, mrp[,.(MostResponsible.DocterCode = mrp.code.new, EncID.new)],
                    by = "EncID.new",
                    all.x = T, all.y = F)
hcn <- readg(gim, adm)[,.(Hash, EncID.new)]
drm.cohort <- merge(drm.cohort, hcn, by = "EncID.new", all.x = T, all.y = F)
drm.cohort <- merge(drm.cohort, all.dad[,.(EncID.new, Age, Institution.Number, Gender)])

feasi.table <- rbind(drm.feasi(drm.cohort, er.diag = c(sepsis, fever, uti, pneumonia)), 
                     drm.feasi(drm.cohort[startwith.any(ER.Diagnosis.Code, sepsis)], er.diag = sepsis),
                     drm.feasi(drm.cohort[startwith.any(ER.Diagnosis.Code, fever)], er.diag = fever),
                     drm.feasi(drm.cohort[startwith.any(ER.Diagnosis.Code, uti)], er.diag = uti),
                     drm.feasi(drm.cohort[startwith.any(ER.Diagnosis.Code, pneumonia)], er.diag = pneumonia),
                     drm.feasi(drm.cohort, ip.diag = c(sepsis, fever, uti, pneumonia)), 
                     drm.feasi(drm.cohort[startwith.any(Diagnosis.Code, sepsis)], ip.diag = sepsis),
                     drm.feasi(drm.cohort[startwith.any(Diagnosis.Code, fever)], ip.diag = fever),
                     drm.feasi(drm.cohort[startwith.any(Diagnosis.Code, uti)], ip.diag = uti),
                     drm.feasi(drm.cohort[startwith.any(Diagnosis.Code,pneumonia)], ip.diag = pneumonia),
                     fill = T)
t(feasi.table)
write.csv(t(feasi.table), "H:/GEMINI/Results/DRM/new_170424/feasi.table.april24.csv")
