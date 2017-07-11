# --------------------------- DRM cohort new 2-----------------------------------
# --------------------------- 2017-06-28 ---------------------------------------
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
drm.din <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/medication.frequency_updated.xlsx")
drm.generic <- drm.din$generic.name %>% toupper()
din.drm <- drm.din$din
smh.generic <- readxl::read_excel("H:/GEMINI/Results/DRM/abx_generic_not_din/SMH_GENERIC_PICKUPS.xlsx")%>%
  filter(include==1) %>% select(generic_name)
uhn.generic <- readxl::read_excel("H:/GEMINI/Results/DRM/abx_generic_not_din/UHNlist_GENERICPICKUPS.xlsx")%>%
  filter(include==1) %>% select(Generic_Name)
smh.generic$generic_name%in%drm.generic


# include only those receiving abox within 48 h
smh.abx <- smh.phar[din%in%din.drm|generic_name%in%smh.generic$generic_name]
sbk.abx <- sbk.phar[(ndc_din%in%din.drm)]
uhn.abx <- uhn.phar[DIN%in%din.drm|toupper(Generic_Name)%in%toupper(uhn.generic$Generic_Name)]

# # check how many captured by din and how many be generic name
smh.abx[, ':='(bydin = din%in%din.drm, bygene = generic_name%in%drm.generic)]
sbk.abx[, ':='(bydin = ndc_din%in%din.drm, bygene = generic_name%in%drm.generic)]
uhn.abx[, ':='(bydin = DIN%in%din.drm, bygene = toupper(Generic_Name)%in%drm.generic)]

checkdin_abx <- function(x){
  table(x[, .(bydin, bygene)])
}

checkdin_abx(smh.abx)
checkdin_abx(sbk.abx)
checkdin_abx(uhn.abx)
sbk.gene<- fread("H:/GEMINI/Results/DRM/abx_generic_not_din/sbk.generic.not.din.csv")
sbk.gene$generic_name%in%drm.generic %>% sum
# smh.abx[bygene&!bydin, din] %>% table %>% data.table %>% fwrite("H:/GEMINI/Results/DRM/abx_generic_not_din/smh.csv")
# sbk.abx[bygene&!bydin, ndc_din] %>% table %>% data.table %>% fwrite("H:/GEMINI/Results/DRM/abx_generic_not_din/sbk.csv")
# uhn.abx[bygene&!bydin, DIN] %>% table %>% data.table %>% fwrite("H:/GEMINI/Results/DRM/abx_generic_not_din/uhn.csv")
# 
# 
# smh.abx[bygene&!bydin, .(generic_name, din, route, ord_frequency)] %>% unique %>%
#   fwrite("H:/GEMINI/Results/DRM/abx_generic_not_din/smh.generic.not.din.csv")
# sbk.abx[bygene&!bydin, .(generic_name, ord_description,
#                          ndc_din, route, frequency)] %>% unique %>%
#   fwrite("H:/GEMINI/Results/DRM/abx_generic_not_din/sbk.generic.not.din.csv")
# uhn.abx[bygene&!bydin, .(Generic_Name, DIN, Route_Code, Frequency)] %>% unique %>%
#   fwrite("H:/GEMINI/Results/DRM/abx_generic_not_din/uhn.generic.not.din.csv")
# apply(smh.abx, 2, function(x)sum(is.na(x)))
# apply(sbk.abx, 2, function(x)sum(is.na(x)))
# apply(uhn.abx, 2, function(x)sum(is.na(x)))
# # frequency table of frequency
# data.table(table(smh.abx[,ord_frequency])) %>%
#   fwrite("H:/GEMINI/Results/DRM/smh.abx.freq.csv")
# data.table(table(sbk.abx[,frequency])) %>%
#   fwrite("H:/GEMINI/Results/DRM/sbk.abx.freq.csv")
# data.table(table(uhn.abx[,Frequency])) %>%
#   fwrite("H:/GEMINI/Results/DRM/uhn.abx.freq.csv")


abx.inc <- rbind(smh.abx[,.(abx.dttm = ymd_hm(paste(start_date, start_time)),
                            abx.stop.dttm = ymd_hm(paste(stop_date, stop_time)),
                            adm.dttm = ymd_hm(paste(Admit.Date, Admit.Time)),
                            dis.dttm = ymd_hm(paste(Discharge.Date, Discharge.Time)),
                            din, generic.name = generic_name, 
                            EncID.new)],
                 sbk.abx[,.(abx.dttm = mdy_hms(paste(start_date, start_time)),
                            abx.stop.dttm = mdy_hms(paste(stop_date, stop_time)),
                            adm.dttm = ymd_hm(paste(Admit.Date, Admit.Time)),
                            dis.dttm = ymd_hm(paste(Discharge.Date, Discharge.Time)),
                            din = ndc_din, generic.name = generic_name,
                            EncID.new)],
                 uhn.abx[,.(abx.dttm = dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time)),
                            abx.stop.dttm = dmy_hm(paste(Order_Sto, Order_Stop_Time)),
                            adm.dttm = ymd_hm(paste(Admit.Date, Admit.Time)),
                            dis.dttm = ymd_hm(paste(Discharge.Date, Discharge.Time)),
                            din = DIN, generic.name = toupper(Generic_Name),
                            EncID.new)])
# abx.inc[, .N, by = .(din, generic.name)][order(generic.name)] %>% 
#   fwrite("H:/GEMINI/Results/DRM/cohort_new/medication.frequency.csv")
# 
 abx.inc[is.na(abx.stop.dttm), abx.stop.dttm := dis.dttm]
# apply(abx.inc, 2, function(x)sum(is.na(x)))

abx.d1 <- abx.inc[date(abx.dttm)<=date(adm.dttm)&date(abx.stop.dttm)>=date(adm.dttm)]
abx.d2 <- abx.inc[date(abx.dttm)<=date(adm.dttm)+days(1)&date(abx.stop.dttm)>=date(adm.dttm)+days(1)]
abx.d3 <- abx.inc[date(abx.dttm)<=date(adm.dttm)+days(2)&date(abx.stop.dttm)>=date(adm.dttm)+days(2)]

abx <- data.table(EncID.new = unique(abx.inc$EncID.new))
abx[, ':='(d1 = EncID.new%in%abx.d1$EncID.new,
           d2 = EncID.new%in%abx.d2$EncID.new,
           d3 = EncID.new%in%abx.d3$EncID.new)]
abx[, n.abx := d1 + d2 + d3]
table(abx$n.abx, useNA = "ifany")

fwrite(abx, "H:/GEMINI/Results/DRM/new_170424/abx.new3.csv")


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
antibio.inc <- fread("H:/GEMINI/Results/DRM/new_170424/abx.new3.csv")
antibio.inc.old <- fread("H:/GEMINI/Results/DRM/new_170424/abx.new.csv")

ns.cul <- fread("H:/GEMINI/Results/DRM/new_170424/drm.cul.ns.new.csv")
drm.cohort <- intersect(antibio.inc[n.abx>=2, EncID.new], ns.cul$EncID.new)
all.dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")

# ---------------------- CREATE LIST FOR CHART REVIEW --------------------------
phy.all <- readg(gim, all.phy)
gemini.inc <- phy.all[(adm.GIM%in%c("y", "GP-GIM")|dis.GIM%in%c("y", "GP-GIM"))
                      |str_sub(EncID.new,1, 2)=="15", EncID.new]
drm.cohort <- all.dad[EncID.new%in%drm.cohort&EncID.new%in%gemini.inc]
table(drm.cohort$Institution.Number)

# flow chart numbers
step1 <- all.dad[EncID.new%in%gemini.inc&EncID.new%in%antibio.inc[n.abx>=2, EncID.new]]
table(step1$Institution.Number)
step2 <- step1[EncID.new%in%ns.cul$EncID.new]
table(step2$Institution.Number)




for(i in unique(drm.cohort$Institution.Number)){
  dat <- drm.cohort[Institution.Number==i,
                    .(EncID.new, Admit.Date, Admit.Time,
                      Discharge.Date, Discharge.Time)]
  if(i =="SMH"){
    link <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/SMH.LINKLIST_NEWHASH.csv")
    link$EncID.new <- paste("11", link$EncID.new, sep = "")
    dat$EncID.new <- as.character(dat$EncID.new)
    dat <- merge(dat, link[,.(MRN, FIRSTNAME, LASTNAME,EncID.new)], by = "EncID.new",
                 all.x = T, all.y = F)
    dat <- dat[,.(MRN, EncID.new, 
                  First.Name =  FIRSTNAME, Last.Name = LASTNAME,
                  Admit.Date, Admit.Time,
                  Discharge.Date, Discharge.Time)]
  }
  fwrite(dat, paste("R:/GEMINI-DRM-TEAM/DRM-TEAM/july10/cohort_", i, ".csv", sep = ""))
}



# ----------------------------- compare old and new ----------------------------
smh.old <- fread("R:/GEMINI-DRM-TEAM/DRM-TEAM/cohort_SMH.csv")
sbk.old <- fread("R:/GEMINI-DRM-TEAM/DRM-TEAM/cohort_SHSC.csv")
tgh.old <- fread("R:/GEMINI-DRM-TEAM/DRM-TEAM/cohort_UHN-TG.csv")
twh.old <- fread("R:/GEMINI-DRM-TEAM/DRM-TEAM/cohort_UHN-TW.csv")


smh.new <- fread("R:/GEMINI-DRM-TEAM/DRM-TEAM/july10/cohort_SMH.csv")
sbk.new <- fread("R:/GEMINI-DRM-TEAM/DRM-TEAM/july10/cohort_SHSC.csv")
tgh.new <- fread("R:/GEMINI-DRM-TEAM/DRM-TEAM/july10/cohort_UHN-TG.csv")
twh.new <- fread("R:/GEMINI-DRM-TEAM/DRM-TEAM/july10/cohort_UHN-TW.csv")


compare.sets(smh.old$EncID.new, smh.new$EncID.new)
compare.sets(sbk.old$EncID.new, sbk.new$EncID.new)
compare.sets(tgh.old$EncID.new, tgh.new$EncID.new)
compare.sets(twh.old$EncID.new, twh.new$EncID.new)
