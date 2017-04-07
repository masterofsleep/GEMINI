# -----------------------------  IV abx  ---------------------------------------
library(gemini)
lib.pa()

smh.phar <- readg(smh, phar)
sbk.phar <- readg(sbk, phar, dt = T)
uhn.phar <- readg(uhn, phar)
msh.phar <- readg(msh, phar)
apply(sbk.phar, 2, function(x)sum(is.na(x)))
apply(smh.phar, 2, function(x)sum(is.na(x)))
apply(uhn.phar, 2, function(x)sum(is.na(x)))
apply(msh.phar, 2, function(x)sum(is.na(x)))

#msh.route.freq <- data.table(table(msh.phar$ROUTE)) 
#names(msh.route.freq) <- c("ROUTE", "N")
#fwrite(msh.route.freq, "H:/GEMINI/Results/DRM/msh.route.freq.csv")

# fix sbk stop date time
sbk.phar[, ':='(start_date = mdy(start_date),
                stop_date = mdy(stop_date))]
sbk.phar[is.na(stop_date), ':='(stop_date = ymd(Discharge.Date))]
sbk.phar[is.na(stop_time), ':='(stop_time = paste(Discharge.Time, ":00", sep = ""))]


sbk.phar$EncID.new <- as.character(sbk.phar$EncID.new)
sbk.phar$ndc_din[!is.na(sbk.phar$ndc_din)&str_detect(sbk.phar$ndc_din, "-")] <-
  str_split(sbk.phar$ndc_din[!is.na(sbk.phar$ndc_din)&str_detect(sbk.phar$ndc_din, "-")], "-") %>% 
  unlist %>% matrix(ncol = 2, byrow = T) %>% `[`(,1) 



drm.din <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/FINALDINLIST.xlsx")
drm.din2 <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/FINALDINLIST2.xls")
drm.din2$din <- gsub("(?<![0-9])0+", "", drm.din2$din, perl = TRUE)
drm.din2 <- drm.din2[!is.na(drm.din2$din),]
din.drm <- union(drm.din$`FINAL DINS`, drm.din2$din)
generic <- union(drm.din$Name, drm.din2$drugname)

smh.abx <- smh.phar[din%in%din.drm|generic_name%in%generic]
sbk.abx <- sbk.phar[ndc_din%in%din.drm|generic_name%in%generic]
uhn.abx <- uhn.phar[DIN%in%din.drm|toupper(Generic_Name)%in%generic]
msh.abx <- msh.phar[DIN%in%din.drm|toupper(ORDER_DESC)%in%generic]

apply(sbk.abx, 2, function(x)sum(is.na(x)))
apply(smh.abx, 2, function(x)sum(is.na(x)))
apply(uhn.abx, 2, function(x)sum(is.na(x)))
apply(uhn.phar, 2, function(x)sum(is.na(x)))


biav1 <- readxl::read_excel("H:/GEMINI/Feasibility/ivabx/FINALDINLIST1.xlsx", sheet = 2)
biav2 <- readxl::read_excel("H:/GEMINI/Feasibility/ivabx/ID_DIN_LIST_1.xls", sheet = 2)
biav3 <- readxl::read_excel("H:/GEMINI/Feasibility/ivabx/ID_DIN_LIST_2.xls", sheet = 2)
biav.din <- c(biav1$`FINAL DINS`, biav2$din, biav3$din)
biav.din <- gsub("(?<![0-9])0+", "", biav.din, perl = TRUE)
biav.generic <- c(biav1$Name, biav2$drugname, biav3$drugname)

smh.biav <- smh.abx[din%in%biav.din|generic_name%in%biav.generic]
sbk.biav <- sbk.abx[ndc_din%in%biav.din|generic_name%in%biav.generic]
uhn.biav <- uhn.abx[DIN%in%biav.din|toupper(Generic_Name)%in%biav.generic]
msh.biav <- msh.abx[DIN%in%biav.din|toupper(ORDER_DESC)%in%biav.generic]


setwd("H:/GEMINI/Results/to.administrator")
cohort <- fread("cohort.csv")
copd <- fread("qbp.copd.csv")
cap <- fread("qbp.cap.csv")
uti <- fread("qbp.uti.csv")
cohort$copd <- cohort$EncID.new%in%copd$EncID.new
cohort$cap <- cohort$EncID.new%in%cap$EncID.new
cohort$uti <- cohort$EncID.new%in%uti$EncID.new

cohort$with.biav <- cohort$EncID.new%in%
  c(smh.biav$EncID.new, sbk.biav$EncID.new, uhn.biav$EncID.new, msh.biav$EncID.new)
cohort.biav <- cohort[with.biav==T]
exc <- readg(gim, notgim)
cohort.biav <- cohort.biav[!EncID.new%in%exc$EncID.new]

feasi1 <- ddply(cohort.biav, ~mrp.code.new, summarize,
      overall = length(EncID.new),
      n.copd = sum(copd),
      n.cap = sum(cap),
      n.uti = sum(uti))

apply(feasi1[, 2:5], 2, function(x)sum(x>10))
apply(feasi1[, 2:5], 2, function(x)sum(x>20))
apply(feasi1[, 2:5], 2, function(x)sum(x>50))


smh.biav.route <- data.table(table(smh.biav[,.(din, generic_name ,route)], useNA="ifany"))[N!=0] 
smh.biav.route$site = "smh"
names(smh.biav.route)[1:3] <- c("din", "generic.name", "route")
sbk.biav.route <- data.table(table(sbk.biav[,.(ndc_din, generic_name ,route)], useNA = "ifany"))[N!=0]
sbk.biav.route$site <- "sbk"
names(sbk.biav.route)[1:3] <- c("din", "generic.name", "route")
uhn.biav.route <- data.table(table(uhn.biav[,.(DIN, Generic_Name, Route_Code)], useNA = "ifany"))[N!=0]
uhn.biav.route$site <- "uhn"
names(uhn.biav.route)[1:3] <- c("din", "generic.name", "route")
msh.biav.route <- data.table(table(msh.biav[,.(DIN, ORDER_DESC ,ROUTE)], useNA = "ifany"))[N!=0]
msh.biav.route$site <- "msh"
names(msh.biav.route)[1:3] <- c("din", "generic.name", "route")

bioav.route <- rbind(smh.biav.route,
                    sbk.biav.route,
                    uhn.biav.route,
                    msh.biav.route)
fwrite(bioav.route, "H:/GEMINI/Results/ivabx/abx.bioav.route.freq.csv")
