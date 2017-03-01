#============================ DRM-TEAM =========================================
library(gemini)
lib.pa()
rm(list = ls())

smh.phar <- readg(smh, phar)
sbk.phar <- readg(sbk, phar)
sbk.phar$EncID.new <- as.character(sbk.phar$EncID.new)
uhn.phar <- readg(uhn, phar.nophi)
drm.din <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/FINALDINLIST.xlsx")
drm.din2 <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/FINALDINLIST2.xls")
drm.din2$din <- gsub("(?<![0-9])0+", "", drm.din2$din, perl = TRUE)
drm.din2 <- drm.din2[!is.na(drm.din2$din),]
din.drm <- union(drm.din$`FINAL DINS`, drm.din2$din)

smh.route <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/antibio route code_nd.xlsx", sheet=1) %>% data.table
sbk.route <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/antibio route code_nd.xlsx", sheet=2) %>% data.table
uhn.route <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/antibio route code_nd.xlsx", sheet=3) %>% data.table

smh.inc <- smh.phar[din%in%din.drm&
                      route%in%smh.route$`Route Code`[smh.route$include==1]&
                      ymd_hm(paste(start_date, start_time))>=mdy_hms(paste(ADMITDATE, ADMIT.TIME))&
                      ymd_hm(paste(start_date, start_time))<=mdy_hms(paste(ADMITDATE, ADMIT.TIME)) + hours(24)]


sbk.dad <- readg(sbk, dad)
sbk.phar <- merge(sbk.phar, sbk.dad[,.(EncID.new, Admit.Date, Admit.Time)], 
                  all.x = T)
sbk.phar$ndc_din <- gsub("(?<![0-9])0+", "", sbk.phar$ndc_din, perl = TRUE)
sbk.inc <- sbk.phar[ndc_din%in%din.drm&
                      route%in%sbk.route$`Route Code`[sbk.route$include==1]&
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
uhn.inc <- uhn.phar[DIN%in%din.drm&
                      Route_Code%in%uhn.route$`Route Code`[uhn.route$include==1]&
                      dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))>=
                      ymd_hm(paste(Admit.Date, Admit.Time))&
                      dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time))<=
                      ymd_hm(paste(Admit.Date, Admit.Time)) + hours(24)]

uhn.phar[is.na(dmy_hm(paste(str_sub(Order_Sta, 1, 10), Order_Start_Time)))]


drm.antibio.inc <- c(smh.inc$EncID.new, sbk.inc$EncID.new, uhn.inc$EncID.new)
fwrite(data.table(drm.antibio.inc), "H:/GEMINI/Results/DRM/drm.antibio.inc.csv")



library(gemini)
lib.pa()
rm(list = ls())
smh.mic <- readg(smh, micro, dt = T)
culture.marked <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/CultureClassification_Feb9.xlsx")
cul.ns <- culture.marked[culture.marked$NonScreening==1, ]
cul.ns.paste <- paste(cul.ns$Test_ID, cul.ns$Source, sep = "")
names(smh.mic)[3] <- "Specimen_Collection_Date/Time"
cul.ns.smh <- smh.mic[paste(Test_ID, Source, sep = "")%in%cul.ns.paste&
                      !is.na(Isolate_num)]
cul.ns.smh <- cul.ns.smh[mdy_hm(`Specimen_Collection_Date/Time`)>=ymd_hm(paste(Admit.Date, Admit.Time))&
                      mdy_hm(`Specimen_Collection_Date/Time`)<=(ymd_hm(paste(Admit.Date, Admit.Time))+hours(48)),
                      EncID.new]

#sbk 
culture.marked <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/CultureClassification_Feb9.xlsx",
                                     sheet = 2)%>% data.table
sum(culture.marked$N[culture.marked$NonScreening==1], na.rm = T)
cul.ns <- culture.marked[NonScreening==1]
sbk.mic <- readg(sbk, micro_pos.csv, dt = T)
cul.ns.paste <- paste(cul.ns$culture_test_cd, cul.ns$description, cul.ns$specimen_source,
                      sep = "")
cul.ns.sbk <- sbk.mic[paste(culture_test_cd, description, specimen_source, sep = "")%in%
                        cul.ns.paste&
                      mdy_hm(specimen_collection_datetime)>=ymd_hm(paste(Admit.Date, Admit.Time))&
                      mdy_hm(specimen_collection_datetime)<=(ymd_hm(paste(Admit.Date, Admit.Time))+hours(48)),
                      EncID.new]



# UHN
# Check the concordance bewteen the two marked files 
uhn.marked <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/UHN_Source Setup.xls",
                                                                   sheet = 1)%>%data.table
tgh.marked <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/CultureClassification_Feb9_DM.xlsx",
                                  sheet = 3)%>%data.table
tgh.marked[,':='(TEST = trimws(TEST),
                 SRC = trimws(SRC),
                 SITE = trimws(SITE))]
tgh.ns <- tgh.marked[NonScreening==1]

# tgh.ns[!SRC%in%uhn.marked[NonScreening==1, SOURCE_ID]] %>% 
#   fwrite("H:/GEMINI/Results/DRM/tgh.discrepancy1.csv")
# tgh.marked[is.na(NonScreening)&!SRC%in%uhn.marked[NonScreening==1, SOURCE_ID]] %>% 
#   fwrite("H:/GEMINI/Results/DRM/tgh.discrepancy2.csv")
#   

twh.marked <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/CultureClassification_Feb9_DM.xlsx",
                                  sheet = 4)%>%data.table
twh.marked[,':='(TEST = trimws(TEST),
                 SRC = trimws(SRC),
                 SITE = trimws(SITE))]
twh.ns <- tgh.marked[NonScreening==1]

# twh.ns[!SRC%in%uhn.marked[NonScreening==1, SOURCE_ID]] %>% 
#   fwrite("H:/GEMINI/Results/DRM/twh.discrepancy1.csv")
# twh.marked[is.na(NonScreening)&!SRC%in%uhn.marked[NonScreening==1, SOURCE_ID]] %>% 
#   fwrite("H:/GEMINI/Results/DRM/twh.discrepancy2.csv")


twh.paste <- paste(twh.ns$TEST, twh.ns$SRC, twh.ns$SITE)
tgh.paste <- paste(tgh.ns$TEST, tgh.ns$SRC, tgh.ns$SITE)

uhn.dad <- readg(uhn, dad)
setwd("H:/GEMINI/Data/UHN/Micro/TGH")
files <- list.files()
tgh.ns.inc <- NULL
for(i in 1:length(files)){
  dat <- fread(files[i])
    if(!"SRC"%in%names(dat)){
      dat$SRC <- dat$X.SRC..
    }
    if(!"SITE"%in%names(dat)){
      dat$SITE <- dat$X..SITE..........................
    }
    if(!"TEST"%in%names(dat)){
      dat$TEST <- dat$X..TEST.
    }
  dat[,':='(TEST = trimws(TEST),
            SRC = trimws(SRC),
            SITE = trimws(SITE))]
  ns <- paste(dat$TEST, dat$SRC, dat$SITE, sep = "")
  dat.ns <- dat[(ns%in%tgh.paste|SRC%in%uhn.marked[NonScreening==1, SOURCE_ID])&
                ymd_hm(paste(CDATE, CTIME))>=ymd_hm(paste(Admit.Date, Admit.Time))&
                ymd_hm(paste(CDATE, CTIME))<=(ymd_hm(paste(Admit.Date, Admit.Time))+hours(48))]
  print(files[i])
  print(nrow(dat.ns))
  tgh.ns.inc <- c(tgh.ns.inc, dat.ns$EncID.new)
}


setwd("H:/GEMINI/Data/UHN/Micro/TW")
files <- list.files()
twh.ns.inc <- NULL
for(i in 1:length(files)){
  dat <- fread(files[i])
  dat[,':='(TEST = trimws(TEST),
            SRC = trimws(SRC),
            SITE = trimws(SITE))]
  ns <- paste(dat$TEST, dat$SRC, dat$SITE, sep = "")
  dat.ns <- dat[(ns%in%twh.paste|SRC%in%uhn.marked[NonScreening==1, SOURCE_ID])&
                  ymd_hm(paste(CDATE, CTIME))>=ymd_hm(paste(Admit.Date, Admit.Time))&
                  ymd_hm(paste(CDATE, CTIME))<=(ymd_hm(paste(Admit.Date, Admit.Time))+hours(48))]
  print(files[i])
  print(nrow(dat.ns))
  twh.ns.inc <- c(tgh.ns.inc, dat.ns$EncID.new)
}


cul.inc <- c(cul.ns.smh,
             cul.ns.sbk,
             twh.ns.inc,
             tgh.ns.inc)


fwrite(data.table(cul.inc), "H:/GEMINI/Results/DRM/cul.ns.inc.csv")

rm(list = ls())
antibio.inc <- fread("H:/GEMINI/Results/DRM/drm.antibio.inc.csv")
cul.inc <- fread("H:/GEMINI/Results/DRM/cul.ns.inc.csv")
drm.cohort <- intersect(antibio.inc$drm.antibio.inc, cul.inc$cul.inc)

length(unique(antibio.inc$drm.antibio.inc))
length(unique(cul.inc$cul.inc))



er.diag <- readg(gim, er_diag)
table(er.diag$ER.Diagnosis.Type, useNA = "ifany")
