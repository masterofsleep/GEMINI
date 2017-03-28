# ------------------------ THP Names -------------------------------------------
library(gemini)
lib.pa()

adm <- fread("C:/Users/guoyi/Desktop/marked_names/thp/THP MED GIM IP_physicianNames.csv")
mrp <- fread("C:/Users/guoyi/Desktop/marked_names/thp/THP MED GIM IP GENERALDAD_physicianNames.csv")
apply(adm, 2, function(x)sum(x==""))

thp <- readg(thp, dad)
length(unique(thp$EncID.new))

intersect(adm$AdmittingPhysicianCode, mrp$MostResponsibleDoctorCode)
adm[AdmittingPhysicianCode=="1050"]
mrp[MostResponsibleDoctorCode=="1050"]
# all names are in the same coding system


all.names <- rbind(data.table(Code = adm$AdmittingPhysicianCode,
                              first.name = adm$AdmittingPhysicianFirstName,
                              last.name = adm$AdmittingPhysicianLastName),
                   data.table(Code = adm$DischargingPhysicianCode,
                              first.name = adm$DischargingPhysicianFirstName,
                              last.name = adm$DischargingPhysicianLastName),
                   data.table(Code = mrp$MostResponsibleDoctorCode,
                              first.name = mrp$MostResponsiblePhysicianFirstName,
                              last.name = mrp$MostResponsiblePhysicianLastName))

all.names <- merge(unique(all.names), data.table(table(all.names$Code)),
                   by.x = "Code", by.y= "V1", all.x = T)

all.names$code.type <- "thp"
all.names$GIM <- "y"

fwrite(all.names, "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/thp.names.csv")



# ----------------------------- create link file -------------------------------
dad.link <- fread("R:/GEMINI/_RESTORE/THP/phynames/thp.dad.LINKINGLIST_physicians.csv")
adm.link <- fread("R:/GEMINI/_RESTORE/THP/phynames/thp.GIM_IP_LINKING_LIST_physicians.csv")
thp.dad <- readg(thp, dad)

apply(adm.link, 2, function(x) sum(x ==""))
apply(dad.link, 2, function(x) sum(x ==""))

adm.link[AdmittingPhysicianCode=="", EncID.new] -> check
adm.link[AdmittingPhysicianCode==DischargingPhysicianCode] %>% dim

thp.dad[str_sub(EncID.new, 3, 8)%in%check] %>%
  ggplot(aes(ymd(Admit.Date), fill = Gender)) + geom_histogram(binwidth = 5)

thp.dad[str_sub(EncID.new, 3, 8)%in%check] %>%
  ggplot(aes(ymd(Discharge.Date), fill = Gender)) + geom_histogram(binwidth = 5)
