#============================ DRM-TEAM =========================================
library(gemini)
lib.pa()
rm(list = ls())

smh.phar <- readg(smh, phar)
sbk.phar <- readg(sbk, phar)
sbk.phar$EncID.new <- as.character(sbk.phar$EncID.new)
sbk.phar$ndc_din[!is.na(sbk.phar$ndc_din)&str_detect(sbk.phar$ndc_din, "-")] <-
  str_split(sbk.phar$ndc_din[!is.na(sbk.phar$ndc_din)&str_detect(sbk.phar$ndc_din, "-")], "-") %>% 
  unlist %>% matrix(ncol = 2, byrow = T) %>% `[`(,1) 
uhn.phar <- readg(uhn, phar.nophi)
drm.din <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/FINALDINLIST.xlsx")
drm.din2 <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/FINALDINLIST2.xls")
drm.din2$din <- gsub("(?<![0-9])0+", "", drm.din2$din, perl = TRUE)
drm.din2 <- drm.din2[!is.na(drm.din2$din),]
din.drm <- union(drm.din$`FINAL DINS`, drm.din2$din)

generic <- union(drm.din$Name, drm.din2$drugname)
sum(din.drm%in%smh.phar$din)
sum(din.drm%in%sbk.phar$ndc_din)
sum(din.drm%in%uhn.phar$DIN)

# smh.route <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/antibio route code_nd.xlsx", sheet=1)%>%data.table
# sbk.route <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/antibio route code_nd.xlsx", sheet=2)%>%data.table
# uhn.route <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/antibio route code_nd.xlsx", sheet=3)%>%data.table
# smh.route$`Route Code`[smh.route$include==1&!is.na(smh.route$include)]

smh.inc <- smh.phar[(din%in%din.drm|generic_name%in%generic) &
                      ymd_hm(paste(start_date, start_time))>=mdy_hms(paste(ADMITDATE, ADMIT.TIME))&
                      ymd_hm(paste(start_date, start_time))<=mdy_hms(paste(ADMITDATE, ADMIT.TIME)) + hours(24)]

# smh.inc <- smh.phar[din%in%din.drm|
#                       route%in%smh.route$`Route Code`[smh.route$include==1&!is.na(smh.route$include)]&
#                       ymd_hm(paste(start_date, start_time))>=mdy_hms(paste(ADMITDATE, ADMIT.TIME))&
#                       ymd_hm(paste(start_date, start_time))<=mdy_hms(paste(ADMITDATE, ADMIT.TIME)) + hours(24)]


sbk.dad <- readg(sbk, dad)
sbk.phar <- merge(sbk.phar, sbk.dad[,.(EncID.new, Admit.Date, Admit.Time)], 
                  all.x = T)
sbk.phar$ndc_din <- gsub("(?<![0-9])0+", "", sbk.phar$ndc_din, perl = TRUE)
# sbk.inc <- sbk.phar[ndc_din%in%din.drm&
#                       route%in%sbk.route$`Route Code`[sbk.route$include==1&!is.na(sbk.route$include)]&
#                       mdy_hms(paste(start_date, start_time))>=ymd_hm(paste(Admit.Date, Admit.Time))&
#                       mdy_hms(paste(start_date, start_time))<=ymd_hm(paste(Admit.Date, Admit.Time)) + hours(24)]
sbk.inc <- sbk.phar[(ndc_din%in%din.drm|generic_name%in%generic)&
                      mdy_hms(paste(start_date, start_time))>=ymd_hm(paste(Admit.Date, Admit.Time))&
                      mdy_hms(paste(start_date, start_time))<=ymd_hm(paste(Admit.Date, Admit.Time)) + hours(24)]


time.convert <- function(x){
  ifelse(str_sub(x, -2,-1)=="pm"&as.numeric(str_sub(x, 1, 2))<12, 
         paste(as.numeric(str_sub(x, 1, 2)) + 12, str_sub(x, 3, 5), sep = ""),
         ifelse((str_sub(x, -2,-1)=="am"&as.numeric(str_sub(x, 1, 2))==12),
                paste(as.numeric(str_sub(x, 1, 2)) - 12, str_sub(x, 3, 5), sep = ""),
                paste(as.numeric(str_sub(x, 1, 2)), str_sub(x, 3, 5), sep = "")))
}

time.convert("11:20 pm")
uhn.dad <- readg(uhn, dad)[ymd(Discharge.Date) <= ymd("2015-03-31")]
uhn.phar[,`:=`(Order_Start_Time = time.convert(Order_St),
               Order_Stop_Time = time.convert(Order_St.1))]
uhn.phar <- merge(uhn.phar, uhn.dad[,.(EncID.new, Admit.Date, Admit.Time)])
uhn.phar$DIN <- gsub("(?<![0-9])0+", "", uhn.phar$DIN, perl = TRUE)
# sum(uhn.phar$DIN%in%din.drm)
# sum(uhn.phar$Route_Code%in%uhn.route$`Route Code`[uhn.route$include==1&!is.na(uhn.route$include)]&uhn.phar$DIN%in%din.drm)
# dim(uhn.phar[dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))>=
#       ymd_hm(paste(Admit.Date, Admit.Time))&
#       dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))<=
#       ymd_hm(paste(Admit.Date, Admit.Time)) + hours(24)])
# sum(uhn.phar$Route_Code%in%uhn.route[include==1, 'Route Code'])
# uhn.inc <- uhn.phar[DIN%in%din.drm&
#                       Route_Code%in%uhn.route$`Route Code`[uhn.route$include==1&!is.na(uhn.route$include)]&
#                       dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))>=
#                       ymd_hm(paste(Admit.Date, Admit.Time))&
#                       dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))<=
#                       ymd_hm(paste(Admit.Date, Admit.Time)) + hours(24)]
uhn.inc <- uhn.phar[(DIN%in%din.drm|Generic_Name%in%generic)&
                      dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))>=
                      ymd_hm(paste(Admit.Date, Admit.Time))&
                      dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))<=
                      ymd_hm(paste(Admit.Date, Admit.Time)) + hours(24)]

#Route_Code%in%uhn.route$`Route Code`[uhn.route$include==1&!is.na(uhn.route$include)]&
uhn.phar[is.na(dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time)))]
table(uhn.phar$DIN, useNA = "ifany") %>% data.table -> missingdin


length(unique(smh.inc$EncID.new))
length(unique(sbk.inc$EncID.new))
length(unique(uhn.inc$EncID.new))
drm.antibio.inc <- c(smh.inc$EncID.new, sbk.inc$EncID.new, uhn.inc$EncID.new) %>% unique
fwrite(data.table(drm.antibio.inc), "H:/GEMINI/Results/DRM/drm.antibio.inc.csv")

# table to check quality
with.route <- c(round(sum(smh.phar$route%in%
                      smh.route$`Route Code`[smh.route$include==1&!is.na(smh.route$include)])/
                  nrow(smh.phar)*100, 1), 
                round(sum(sbk.phar$route%in%
                            sbk.route$`Route Code`[sbk.route$include==1&!is.na(sbk.route$include)])/
                        nrow(sbk.phar)*100, 1),
                round(sum(uhn.phar$Route_Code%in%
                            uhn.route$`Route Code`[uhn.route$include==1&!is.na(uhn.route$include)])/
                        nrow(uhn.phar)*100, 1))
with.din <- c(round(sum(smh.phar$din%in%din.drm)/
                      nrow(smh.phar)*100, 1), 
              round(sum(sbk.phar$ndc_din%in%din.drm)/
                      nrow(sbk.phar)*100, 1),
              round(sum(uhn.phar$DIN%in%din.drm)/
                      nrow(uhn.phar)*100, 1))
within.24h <- c(round(nrow(smh.phar[ymd_hm(paste(start_date, start_time))>=
                                      mdy_hms(paste(ADMITDATE, ADMIT.TIME))&
                             ymd_hm(paste(start_date, start_time))<=
                               mdy_hms(paste(ADMITDATE, ADMIT.TIME)) + hours(24)])/
                        nrow(smh.phar)*100, 1), 
                round(nrow(sbk.phar[mdy_hms(paste(start_date, start_time))>=
                                      ymd_hm(paste(Admit.Date, Admit.Time))&
                                      mdy_hms(paste(start_date, start_time))<=
                                      ymd_hm(paste(Admit.Date, Admit.Time)) + hours(24)])/
                        nrow(sbk.phar)*100, 1),
                round(nrow(uhn.phar[dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))>=
                                      ymd_hm(paste(Admit.Date, Admit.Time))&
                                      dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))<=
                                      ymd_hm(paste(Admit.Date, Admit.Time)) + hours(24)])/
                        nrow(uhn.phar)*100, 1))
drm.cohort <- c(round(nrow(smh.inc)/nrow(smh.phar)*100, 1),
                round(nrow(sbk.inc)/nrow(sbk.phar)*100, 1),
                round(nrow(uhn.inc)/nrow(uhn.phar)*100, 1))

match.din <- uhn.phar[DIN %in% din.drm]
tab <- table(match.din$DIN, match.din$Route_Code) %>% data.table %>%
  filter(N!=0) %>% arrange(V1)

prop <- data.frame(rbind(with.route, with.din, within.24h, drm.cohort))
names(prop) <- c("smh", "sbk", "uhn")
prop
library(gemini)
lib.pa()
rm(list = ls())
smh.mic <- readg(smh, micro, dt = T)
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
sbk.mic.neg <- readg(sbk, micro_neg)
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

fwrite(drm.cul, "H:/GEMINI/Results/DRM/drm.cul.ns.csv")
# tgh.marked <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/CultureClassification_Feb9_DM.xlsx",
#                                   sheet = 3)%>%data.table
# names(tgh.marked)[5] <- "Urine"
# tgh.marked[,':='(TEST = trimws(TEST),
#                  SRC = trimws(SRC),
#                  SITE = trimws(SITE))]
# tgh.marked[unknown==1, NonBacterial:=1]
# tgh.marked[, paste := paste(TEST, SRC, SITE)]
# tgh.ns <- tgh.marked[NonScreening==1]
# 
# 
# # tgh.ns[!SRC%in%uhn.marked[NonScreening==1, SOURCE_ID]] %>% 
# #   fwrite("H:/GEMINI/Results/DRM/tgh.discrepancy1.csv")
# # tgh.marked[is.na(NonScreening)&!SRC%in%uhn.marked[NonScreening==1, SOURCE_ID]] %>% 
# #   fwrite("H:/GEMINI/Results/DRM/tgh.discrepancy2.csv")
# #   
# 
# twh.marked <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/CultureClassification_Feb9_DM.xlsx",
#                                   sheet = 4)%>%data.table
# names(twh.marked)[5] <- "Urine"
# twh.marked[,':='(TEST = trimws(TEST),
#                  SRC = trimws(SRC),
#                  SITE = trimws(SITE))]
# twh.marked[unknown==1, NonBacterial:=1]
# twh.marked[, paste := paste(TEST, SRC, SITE)]
# twh.ns <- twh.marked[NonScreening==1]
# 
# # twh.ns[!SRC%in%uhn.marked[NonScreening==1, SOURCE_ID]] %>% 
# #   fwrite("H:/GEMINI/Results/DRM/twh.discrepancy1.csv")
# # twh.marked[is.na(NonScreening)&!SRC%in%uhn.marked[NonScreening==1, SOURCE_ID]] %>% 
# #   fwrite("H:/GEMINI/Results/DRM/twh.discrepancy2.csv")
# 
# uhn.dad <- readg(uhn, dad)
# setwd("H:/GEMINI/Data/UHN/Micro/TGH")
# files <- list.files()
# tgh.drm.cul <- NULL
# for(i in 1:length(files)){
#   dat <- fread(files[i])
#     if(!"SRC"%in%names(dat)){
#       dat$SRC <- dat$X.SRC..
#     }
#     if(!"SITE"%in%names(dat)){
#       dat$SITE <- dat$X..SITE..........................
#     }
#     if(!"TEST"%in%names(dat)){
#       dat$TEST <- dat$X..TEST.
#     }
#   dat[,':='(TEST = trimws(TEST),
#             SRC = trimws(SRC),
#             SITE = trimws(SITE))]
#   dat[,paste:= paste(TEST, SRC, SITE)]
#   dat.ns <- dat[(paste%in%tgh.ns$paste|
#                    SRC%in%uhn.marked[NonScreening==1, SOURCE_ID])]
#   dat.ns <- dat.ns[ymd_hm(paste(CDATE, CTIME))>=ymd_hm(paste(Admit.Date, Admit.Time))&
#                 ymd_hm(paste(CDATE, CTIME))<=(ymd_hm(paste(Admit.Date, Admit.Time))+hours(48)),
#                 .(EncID.new, TEST, SRC, SITE, paste)]
#   dat.ns1 <- merge(dat.ns, tgh.ns[,.(paste, Urine , Blood, Resp, 
#                                      Screening, NonScreening, NonBacterial)],
#                    by = "paste", all.x = T)
#   dat.ns2 <- merge(dat.ns, 
#                    uhn.marked[NonScreening==1,
#                               .(SOURCE_ID, Urine, Blood, Resp, Screening, 
#                                 NonScreening, NonBacterial)], 
#                    by.x = "SRC", by.y = "SOURCE_ID", all.x = T, all.y = F)
#   dat.ns.all <- rbind(dat.ns1, dat.ns2, fill = T) %>% filter(NonScreening==1)%>%unique
#   print(files[i])
#   print(nrow(dat.ns))
#   tgh.drm.cul <- rbind(tgh.drm.cul, dat.ns.all)
# }
# 
# 
# setwd("H:/GEMINI/Data/UHN/Micro/TW")
# files <- list.files()
# twh.drm.cul <- NULL
# for(i in 1:length(files)){
#   dat <- fread(files[i])
#   dat[,':='(TEST = trimws(TEST),
#             SRC = trimws(SRC),
#             SITE = trimws(SITE))]
#   dat[,paste:= paste(TEST, SRC, SITE)]
#   dat.ns <- dat[(paste%in%twh.ns$paste|
#                    SRC%in%uhn.marked[NonScreening==1, SOURCE_ID])]
#   dat.ns <- dat.ns[ymd_hm(paste(CDATE, CTIME))>=ymd_hm(paste(Admit.Date, Admit.Time))&
#                      ymd_hm(paste(CDATE, CTIME))<=(ymd_hm(paste(Admit.Date, Admit.Time))+hours(48)),
#                    .(EncID.new, TEST, SRC, SITE, paste)]
#   dat.ns1 <- merge(dat.ns, twh.ns[,.(paste, Urine , Blood, Resp, 
#                                      Screening, NonScreening, NonBacterial)],
#                    by = "paste", all.x = T)
#   dat.ns2 <- merge(dat.ns, 
#                    uhn.marked[NonScreening==1,
#                               .(SOURCE_ID, Urine, Blood, Resp, Screening, 
#                                 NonScreening, NonBacterial)], 
#                    by.x = "SRC", by.y = "SOURCE_ID", all.x = T, all.y = F)
#   dat.ns.all <- rbind(dat.ns1, dat.ns2, fill = T) %>% filter(NonScreening==1)%>%unique
#   print(files[i])
#   print(dim(twh.drm.cul))
#   twh.drm.cul <- rbind(twh.drm.cul, dat.ns.all)
# }
# 
# tgh.drm.cul <- data.table(tgh.drm.cul)
# twh.drm.cul <- data.table(twh.drm.cul)
# ns.cul <- rbind(cbind(smh.drm.cul, site = "smh"),
#                 cbind(sbk.drm.cul, site = "sbk"),
#                 cbind(tgh.drm.cul[,.(EncID.new, Urine, Blood,
#                                      Resp, Screening, NonScreening,
#                                      NonBacterial)], site = "tgh"),
#                 cbind(twh.drm.cul[,.(EncID.new, Urine, Blood,
#                                      Resp, Screening, NonScreening,
#                                      NonBacterial)], site = "twh"))
# fwrite(ns.cul, "H:/GEMINI/Results/DRM/drm.cul.ns.csv")                
# 
# fwrite(data.table(cul.inc), "H:/GEMINI/Results/DRM/cul.ns.inc.csv")
# 
# 
# length(unique(antibio.inc$drm.antibio.inc))
# length(unique(cul.inc$cul.inc))
# 



#--------------------- freasibility table --------------------------------------
rm(list = ls())
antibio.inc <- fread("H:/GEMINI/Results/DRM/drm.antibio.inc.csv")
ns.cul <- fread("H:/GEMINI/Results/DRM/drm.cul.ns.csv")
drm.cohort <- intersect(antibio.inc$drm.antibio.inc, ns.cul$EncID.new)
all.dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
all.dad[EncID.new%in%antibio.inc$drm.antibio.inc, Institution.Number] %>% table
all.dad[EncID.new%in%ns.cul$EncID.new, Institution.Number] %>% table
all.dad[EncID.new%in%drm.cohort, Institution.Number] %>% table

pneumonia <- "J18"
uti <- "N39"
sepsis <- "A41"
fever <- "R50"

# new
pneumonia <- c("J100", "J110", "J120", "J121", "J122", "J128", "J129",
                           "J13", "J14", "J15", "J160", "J168", "J17", "J18")
uti <- c("N10", "N12", "N151", "N300", "N308", "N309","N410", "N412",
             "N413", "N510", "N390")
ip.diag <- readg(gim, ip_diag)
er.diag <- readg(gim, er_diag)

drm.ip.diag <- ip.diag[Diagnosis.Type=="M", .(Diagnosis.Code = str_sub(Diagnosis.Code, 1, 3), EncID.new)]
drm.er.diag <- er.diag[ER.Diagnosis.Type=="M", 
                       .(ER.Diagnosis.Code = str_sub(ER.Diagnosis.Code, 1, 3), EncID.new)]
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


drm.cohort <- merge(drm.cohort, all.dad[,.(EncID.new, Age, Institution.Number, Gender)])

dad <- readg(smh, dad)
mrp <- rbind(readg(smh, dad, select = c("MostResponsible.DocterCode", "EncID.new")),
             readg(sbk, dad, select = c("MostResponsible.DocterCode", "EncID.new")),
             readg(uhn, dad, select = c("MostResponsible.DocterCode", "EncID.new")))
hcn <- rbind(readg(smh, adm, select = c("Hash", "EncID.new")),
             readg(sbk, adm, select = c("Hash", "EncID.new")),
             readg(uhn, adm, select = c("Hash", "EncID.new")))
drm.cohort$EncID.new <- as.character(drm.cohort$EncID.new)
drm.cohort <- merge(drm.cohort, mrp, by = "EncID.new")
drm.cohort <- merge(drm.cohort, hcn, by = "EncID.new")
drm.cohort[Institution.Number=="uhn-general", Institution.Number:="tgh"]
drm.cohort[Institution.Number=="uhn-western", Institution.Number:="twh"]
fwrite(drm.cohort, "H:/GEMINI/Results/DRM/drm.cohort.csv")
drm.cohort <- fread("H:/GEMINI/Results/DRM/drm.cohort.csv")
drm.feasi <- function(x, er.diag = NULL, ip.diag = NULL){
  part1 <- data.table(n.patient = length(unique(x$Hash)),
        n.admission = length(unique(x$EncID.new)),
        smh = paste(sum(x$Institution.Number=="smh"), " (",
                    round(sum(x$Institution.Number=="smh")/nrow(x)*100, 1), ")",
                    sep = ""),
        tgh = paste(sum(x$Institution.Number=="tgh"), " (",
                    round(sum(x$Institution.Number=="tgh")/nrow(x)*100, 1), ")",
                    sep = ""),
        twh = paste(sum(x$Institution.Number=="twh"), " (",
                    round(sum(x$Institution.Number=="twh")/nrow(x)*100, 1), ")",
                    sep = ""),
        sbk = paste(sum(x$Institution.Number=="sbk"), " (",
                    round(sum(x$Institution.Number=="sbk")/nrow(x)*100, 1), ")",
                    sep = ""),
        age = paste(round(mean(x$Age), 1), " (", round(sd(x$Age),1),
                    ")", sep = ""),
        sex = paste(sum(x$Gender=="F"), " (",
                  round(sum(x$Gender=="F")/nrow(x)*100, 1), ")", sep = ""),
        urine = paste(sum(x$Urine==1), " (",
                      round(sum(x$Urine==1)/nrow(x)*100, 1), ")",
                      sep = ""), 
        respiratory = paste(sum(x$Resp==1), " (",
                      round(sum(x$Resp==1)/nrow(x)*100, 1), ")",
                      sep = ""),
        blood = paste(sum(x$Blood==1), " (",
                      round(sum(x$Blood==1)/nrow(x)*100, 1), ")",
                      sep = ""),
        other.sterile = paste(sum(x$Other.sterile==1), " (",
                      round(sum(x$Other.sterile==1)/nrow(x)*100, 1), ")",
                      sep = ""),
        other.non.sterile = paste(sum(x$Other.non.sterile==1), " (",
                      round(sum(x$Other.non.sterile==1)/nrow(x)*100, 1), ")",
                      sep = "")
        )
  if(is.null(er.diag)&is.null(ip.diag)) return(part1)
  if(!is.null(er.diag)){
    smh.phy.n <- x[ER.Diagnosis.Code%in%er.diag&Institution.Number=="smh", 
                   .N, by = MostResponsible.DocterCode]
    sbk.phy.n <- x[ER.Diagnosis.Code%in%er.diag&Institution.Number=="sbk", 
                   .N, by = MostResponsible.DocterCode]
    tgh.phy.n <- x[ER.Diagnosis.Code%in%er.diag&Institution.Number=="tgh", 
                   .N, by = MostResponsible.DocterCode]
    twh.phy.n <- x[ER.Diagnosis.Code%in%er.diag&Institution.Number=="twh", 
                   .N, by = MostResponsible.DocterCode]
    provider <- data.table(
      provider.5 = " ",
      smh.5 = paste(sum(smh.phy.n$N>5), " (", 
                    round(sum(smh.phy.n$N>5)/nrow(smh.phy.n)*100, 1), ")", 
                    sep = ""),
      tgh.5 = paste(sum(tgh.phy.n$N>5), " (", 
                    round(sum(tgh.phy.n$N>5)/nrow(tgh.phy.n)*100, 1), ")", 
                    sep = ""),
      twh.5 = paste(sum(twh.phy.n$N>5), " (", 
                    round(sum(twh.phy.n$N>5)/nrow(twh.phy.n)*100, 1), ")", 
                    sep = ""),
      sbk.5 = paste(sum(sbk.phy.n$N>5), " (", 
                    round(sum(sbk.phy.n$N>5)/nrow(sbk.phy.n)*100, 1), ")", 
                    sep = ""),
      
      provider.10 = " ",
      smh.10 = paste(sum(smh.phy.n$N>10), " (", 
                     round(sum(smh.phy.n$N>10)/nrow(smh.phy.n)*100, 1), ")", 
                     sep = ""),
      tgh.10 = paste(sum(tgh.phy.n$N>10), " (", 
                     round(sum(tgh.phy.n$N>10)/nrow(tgh.phy.n)*100, 1), ")", 
                     sep = ""),
      twh.10 = paste(sum(twh.phy.n$N>10), " (", 
                     round(sum(twh.phy.n$N>10)/nrow(twh.phy.n)*100, 1), ")", 
                     sep = ""),
      sbk.10 = paste(sum(sbk.phy.n$N>10), " (", 
                     round(sum(sbk.phy.n$N>10)/nrow(sbk.phy.n)*100, 1), ")", 
                     sep = ""),
      provider.15 = " ",
      smh.15 = paste(sum(smh.phy.n$N>15), " (", 
                     round(sum(smh.phy.n$N>15)/nrow(smh.phy.n)*100, 1), ")", 
                     sep = ""),
      tgh.15 = paste(sum(tgh.phy.n$N>15), " (", 
                     round(sum(tgh.phy.n$N>15)/nrow(tgh.phy.n)*100, 1), ")", 
                     sep = ""),
      twh.15 = paste(sum(twh.phy.n$N>15), " (", 
                     round(sum(twh.phy.n$N>15)/nrow(twh.phy.n)*100, 1), ")", 
                     sep = ""),
      sbk.15 = paste(sum(sbk.phy.n$N>15), " (", 
                     round(sum(sbk.phy.n$N>15)/nrow(sbk.phy.n)*100, 1), ")", 
                     sep = "")
    )
    return(cbind(part1, provider))
  }
  if(!is.null(ip.diag)){
    smh.phy.n <- x[Diagnosis.Code%in%ip.diag&Institution.Number=="smh", 
                   .N, by = MostResponsible.DocterCode]
    sbk.phy.n <- x[Diagnosis.Code%in%ip.diag&Institution.Number=="sbk", 
                   .N, by = MostResponsible.DocterCode]
    tgh.phy.n <- x[Diagnosis.Code%in%ip.diag&Institution.Number=="tgh", 
                   .N, by = MostResponsible.DocterCode]
    twh.phy.n <- x[Diagnosis.Code%in%ip.diag&Institution.Number=="twh", 
                   .N, by = MostResponsible.DocterCode]
    provider <- data.table(
      provider.5 = " ",
      smh.5 = paste(sum(smh.phy.n$N>5), " (", 
                    round(sum(smh.phy.n$N>5)/nrow(smh.phy.n)*100, 1), ")", 
                    sep = ""),
      tgh.5 = paste(sum(tgh.phy.n$N>5), " (", 
                    round(sum(tgh.phy.n$N>5)/nrow(tgh.phy.n)*100, 1), ")", 
                    sep = ""),
      twh.5 = paste(sum(twh.phy.n$N>5), " (", 
                    round(sum(twh.phy.n$N>5)/nrow(twh.phy.n)*100, 1), ")", 
                    sep = ""),
      sbk.5 = paste(sum(sbk.phy.n$N>5), " (", 
                    round(sum(sbk.phy.n$N>5)/nrow(sbk.phy.n)*100, 1), ")", 
                    sep = ""),
      
      provider.10 = " ",
      smh.10 = paste(sum(smh.phy.n$N>10), " (", 
                     round(sum(smh.phy.n$N>10)/nrow(smh.phy.n)*100, 1), ")", 
                     sep = ""),
      tgh.10 = paste(sum(tgh.phy.n$N>10), " (", 
                     round(sum(tgh.phy.n$N>10)/nrow(tgh.phy.n)*100, 1), ")", 
                     sep = ""),
      twh.10 = paste(sum(twh.phy.n$N>10), " (", 
                     round(sum(twh.phy.n$N>10)/nrow(twh.phy.n)*100, 1), ")", 
                     sep = ""),
      sbk.10 = paste(sum(sbk.phy.n$N>10), " (", 
                     round(sum(sbk.phy.n$N>10)/nrow(sbk.phy.n)*100, 1), ")", 
                     sep = ""),
      provider.15 = " ",
      smh.15 = paste(sum(smh.phy.n$N>15), " (", 
                     round(sum(smh.phy.n$N>15)/nrow(smh.phy.n)*100, 1), ")", 
                     sep = ""),
      tgh.15 = paste(sum(tgh.phy.n$N>15), " (", 
                     round(sum(tgh.phy.n$N>15)/nrow(tgh.phy.n)*100, 1), ")", 
                     sep = ""),
      twh.15 = paste(sum(twh.phy.n$N>15), " (", 
                     round(sum(twh.phy.n$N>15)/nrow(twh.phy.n)*100, 1), ")", 
                     sep = ""),
      sbk.15 = paste(sum(sbk.phy.n$N>15), " (", 
                     round(sum(sbk.phy.n$N>15)/nrow(sbk.phy.n)*100, 1), ")", 
                     sep = "")
    )
    return(cbind(part1, provider))
  }
}


feasi.table <- rbind(drm.feasi(drm.cohort, er.diag = c(sepsis, fever, uti, pneumonia)), 
                     drm.feasi(drm.cohort[ER.Diagnosis.Code==sepsis], er.diag = sepsis),
                     drm.feasi(drm.cohort[ER.Diagnosis.Code==fever], er.diag = fever),
                     drm.feasi(drm.cohort[ER.Diagnosis.Codeuti], er.diag = uti),
                     drm.feasi(drm.cohort[ER.Diagnosis.Code==pneumonia], er.diag = pneumonia),
                     drm.feasi(drm.cohort, ip.diag = c(sepsis, fever, uti, pneumonia)), 
                     drm.feasi(drm.cohort[Diagnosis.Code==sepsis], ip.diag = sepsis),
                     drm.feasi(drm.cohort[Diagnosis.Code==fever], ip.diag = fever),
                     drm.feasi(drm.cohort[Diagnosis.Code==uti], ip.diag = uti),
                     drm.feasi(drm.cohort[Diagnosis.Code==pneumonia], ip.diag = pneumonia),
                     fill = T)

write.csv(t(feasi.table), "H:/GEMINI/Results/DRM/feasi.table.march15.csv")


# ------------ new tables with codes in design paper for cap and uti -----------
rm(list = ls())
antibio.inc <- fread("H:/GEMINI/Results/DRM/drm.antibio.inc.csv")
ns.cul <- fread("H:/GEMINI/Results/DRM/drm.cul.ns.csv")
drm.cohort <- intersect(antibio.inc$drm.antibio.inc, ns.cul$EncID.new)
all.dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
all.dad[EncID.new%in%antibio.inc$drm.antibio.inc, Institution.Number] %>% table
all.dad[EncID.new%in%ns.cul$EncID.new, Institution.Number] %>% table
all.dad[EncID.new%in%drm.cohort, Institution.Number] %>% table

sepsis <- "A41"
fever <- "R50"

# new
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


drm.cohort <- merge(drm.cohort, all.dad[,.(EncID.new, Age, Institution.Number, Gender)])

dad <- readg(smh, dad)
mrp <- rbind(readg(smh, dad, select = c("MostResponsible.DocterCode", "EncID.new")),
             readg(sbk, dad, select = c("MostResponsible.DocterCode", "EncID.new")),
             readg(uhn, dad, select = c("MostResponsible.DocterCode", "EncID.new")))
hcn <- rbind(readg(smh, adm, select = c("Hash", "EncID.new")),
             readg(sbk, adm, select = c("Hash", "EncID.new")),
             readg(uhn, adm, select = c("Hash", "EncID.new")))
drm.cohort$EncID.new <- as.character(drm.cohort$EncID.new)
drm.cohort <- merge(drm.cohort, mrp, by = "EncID.new")
drm.cohort <- merge(drm.cohort, hcn, by = "EncID.new")
drm.cohort[Institution.Number=="uhn-general", Institution.Number:="tgh"]
drm.cohort[Institution.Number=="uhn-western", Institution.Number:="twh"]
fwrite(drm.cohort, "H:/GEMINI/Results/DRM/drm.cohort.csv")
drm.cohort <- fread("H:/GEMINI/Results/DRM/drm.cohort.csv")
drm.feasi <- function(x, er.diag = NULL, ip.diag = NULL){
  part1 <- data.table(n.patient = length(unique(x$Hash)),
                      n.admission = length(unique(x$EncID.new)),
                      smh = paste(sum(x$Institution.Number=="smh"), " (",
                                  round(sum(x$Institution.Number=="smh")/nrow(x)*100, 1), ")",
                                  sep = ""),
                      tgh = paste(sum(x$Institution.Number=="tgh"), " (",
                                  round(sum(x$Institution.Number=="tgh")/nrow(x)*100, 1), ")",
                                  sep = ""),
                      twh = paste(sum(x$Institution.Number=="twh"), " (",
                                  round(sum(x$Institution.Number=="twh")/nrow(x)*100, 1), ")",
                                  sep = ""),
                      sbk = paste(sum(x$Institution.Number=="sbk"), " (",
                                  round(sum(x$Institution.Number=="sbk")/nrow(x)*100, 1), ")",
                                  sep = ""),
                      age = paste(round(mean(x$Age), 1), " (", round(sd(x$Age),1),
                                  ")", sep = ""),
                      sex = paste(sum(x$Gender=="F"), " (",
                                  round(sum(x$Gender=="F")/nrow(x)*100, 1), ")", sep = ""),
                      urine = paste(sum(x$Urine==1), " (",
                                    round(sum(x$Urine==1)/nrow(x)*100, 1), ")",
                                    sep = ""), 
                      respiratory = paste(sum(x$Resp==1), " (",
                                          round(sum(x$Resp==1)/nrow(x)*100, 1), ")",
                                          sep = ""),
                      blood = paste(sum(x$Blood==1), " (",
                                    round(sum(x$Blood==1)/nrow(x)*100, 1), ")",
                                    sep = ""),
                      other.sterile = paste(sum(x$Other.sterile==1), " (",
                                            round(sum(x$Other.sterile==1)/nrow(x)*100, 1), ")",
                                            sep = ""),
                      other.non.sterile = paste(sum(x$Other.non.sterile==1), " (",
                                                round(sum(x$Other.non.sterile==1)/nrow(x)*100, 1), ")",
                                                sep = "")
  )
  if(is.null(er.diag)&is.null(ip.diag)) return(part1)
  if(!is.null(er.diag)){
    smh.phy.n <- x[startwith.any(ER.Diagnosis.Code, er.diag)&Institution.Number=="smh", 
                   .N, by = MostResponsible.DocterCode]
    sbk.phy.n <- x[startwith.any(ER.Diagnosis.Code, er.diag)&Institution.Number=="sbk", 
                   .N, by = MostResponsible.DocterCode]
    tgh.phy.n <- x[startwith.any(ER.Diagnosis.Code, er.diag)&Institution.Number=="tgh", 
                   .N, by = MostResponsible.DocterCode]
    twh.phy.n <- x[startwith.any(ER.Diagnosis.Code, er.diag)&Institution.Number=="twh", 
                   .N, by = MostResponsible.DocterCode]
    provider <- data.table(
      provider.5 = " ",
      smh.5 = paste(sum(smh.phy.n$N>5), " (", 
                    round(sum(smh.phy.n$N>5)/nrow(smh.phy.n)*100, 1), ")", 
                    sep = ""),
      tgh.5 = paste(sum(tgh.phy.n$N>5), " (", 
                    round(sum(tgh.phy.n$N>5)/nrow(tgh.phy.n)*100, 1), ")", 
                    sep = ""),
      twh.5 = paste(sum(twh.phy.n$N>5), " (", 
                    round(sum(twh.phy.n$N>5)/nrow(twh.phy.n)*100, 1), ")", 
                    sep = ""),
      sbk.5 = paste(sum(sbk.phy.n$N>5), " (", 
                    round(sum(sbk.phy.n$N>5)/nrow(sbk.phy.n)*100, 1), ")", 
                    sep = ""),
      
      provider.10 = " ",
      smh.10 = paste(sum(smh.phy.n$N>10), " (", 
                     round(sum(smh.phy.n$N>10)/nrow(smh.phy.n)*100, 1), ")", 
                     sep = ""),
      tgh.10 = paste(sum(tgh.phy.n$N>10), " (", 
                     round(sum(tgh.phy.n$N>10)/nrow(tgh.phy.n)*100, 1), ")", 
                     sep = ""),
      twh.10 = paste(sum(twh.phy.n$N>10), " (", 
                     round(sum(twh.phy.n$N>10)/nrow(twh.phy.n)*100, 1), ")", 
                     sep = ""),
      sbk.10 = paste(sum(sbk.phy.n$N>10), " (", 
                     round(sum(sbk.phy.n$N>10)/nrow(sbk.phy.n)*100, 1), ")", 
                     sep = ""),
      provider.15 = " ",
      smh.15 = paste(sum(smh.phy.n$N>15), " (", 
                     round(sum(smh.phy.n$N>15)/nrow(smh.phy.n)*100, 1), ")", 
                     sep = ""),
      tgh.15 = paste(sum(tgh.phy.n$N>15), " (", 
                     round(sum(tgh.phy.n$N>15)/nrow(tgh.phy.n)*100, 1), ")", 
                     sep = ""),
      twh.15 = paste(sum(twh.phy.n$N>15), " (", 
                     round(sum(twh.phy.n$N>15)/nrow(twh.phy.n)*100, 1), ")", 
                     sep = ""),
      sbk.15 = paste(sum(sbk.phy.n$N>15), " (", 
                     round(sum(sbk.phy.n$N>15)/nrow(sbk.phy.n)*100, 1), ")", 
                     sep = "")
    )
    return(cbind(part1, provider))
  }
  if(!is.null(ip.diag)){
    smh.phy.n <- x[startwith.any(Diagnosis.Code, ip.diag)&Institution.Number=="smh", 
                   .N, by = MostResponsible.DocterCode]
    sbk.phy.n <- x[startwith.any(Diagnosis.Code, ip.diag)&Institution.Number=="sbk", 
                   .N, by = MostResponsible.DocterCode]
    tgh.phy.n <- x[startwith.any(Diagnosis.Code, ip.diag)&Institution.Number=="tgh", 
                   .N, by = MostResponsible.DocterCode]
    twh.phy.n <- x[startwith.any(Diagnosis.Code, ip.diag)&Institution.Number=="twh", 
                   .N, by = MostResponsible.DocterCode]
    provider <- data.table(
      provider.5 = " ",
      smh.5 = paste(sum(smh.phy.n$N>5), " (", 
                    round(sum(smh.phy.n$N>5)/nrow(smh.phy.n)*100, 1), ")", 
                    sep = ""),
      tgh.5 = paste(sum(tgh.phy.n$N>5), " (", 
                    round(sum(tgh.phy.n$N>5)/nrow(tgh.phy.n)*100, 1), ")", 
                    sep = ""),
      twh.5 = paste(sum(twh.phy.n$N>5), " (", 
                    round(sum(twh.phy.n$N>5)/nrow(twh.phy.n)*100, 1), ")", 
                    sep = ""),
      sbk.5 = paste(sum(sbk.phy.n$N>5), " (", 
                    round(sum(sbk.phy.n$N>5)/nrow(sbk.phy.n)*100, 1), ")", 
                    sep = ""),
      
      provider.10 = " ",
      smh.10 = paste(sum(smh.phy.n$N>10), " (", 
                     round(sum(smh.phy.n$N>10)/nrow(smh.phy.n)*100, 1), ")", 
                     sep = ""),
      tgh.10 = paste(sum(tgh.phy.n$N>10), " (", 
                     round(sum(tgh.phy.n$N>10)/nrow(tgh.phy.n)*100, 1), ")", 
                     sep = ""),
      twh.10 = paste(sum(twh.phy.n$N>10), " (", 
                     round(sum(twh.phy.n$N>10)/nrow(twh.phy.n)*100, 1), ")", 
                     sep = ""),
      sbk.10 = paste(sum(sbk.phy.n$N>10), " (", 
                     round(sum(sbk.phy.n$N>10)/nrow(sbk.phy.n)*100, 1), ")", 
                     sep = ""),
      provider.15 = " ",
      smh.15 = paste(sum(smh.phy.n$N>15), " (", 
                     round(sum(smh.phy.n$N>15)/nrow(smh.phy.n)*100, 1), ")", 
                     sep = ""),
      tgh.15 = paste(sum(tgh.phy.n$N>15), " (", 
                     round(sum(tgh.phy.n$N>15)/nrow(tgh.phy.n)*100, 1), ")", 
                     sep = ""),
      twh.15 = paste(sum(twh.phy.n$N>15), " (", 
                     round(sum(twh.phy.n$N>15)/nrow(twh.phy.n)*100, 1), ")", 
                     sep = ""),
      sbk.15 = paste(sum(sbk.phy.n$N>15), " (", 
                     round(sum(sbk.phy.n$N>15)/nrow(sbk.phy.n)*100, 1), ")", 
                     sep = "")
    )
    return(cbind(part1, provider))
  }
}


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
write.csv(t(feasi.table), "H:/GEMINI/Results/DRM/feasi.table.march21.csv")

