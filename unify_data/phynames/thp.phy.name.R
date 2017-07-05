# ------------------------ THP Names -------------------------------------------
library(gemini)
lib.pa()

adm <- fread("C:/Users/guoyi/Desktop/marked_names/thp/THP MED GIM IP_physicianNames.csv")
mrp <- fread("C:/Users/guoyi/Desktop/marked_names/thp/THP MED GIM IP GENERALDAD_physicianNames.csv")
apply(adm, 2, function(x)sum(x==""))

thp <- readg(thp, dad)
length(unique(thp$EncID.new))

intersect(adm$AdmittingPhysicianCode, mrp$MostResponsibleDoctorCode)
adm[AdmittingPhysicianCode=="1348"]
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


link <- merge(unique(adm.link[,.(EncID.new, adm.code = AdmittingPhysicianCode, 
                          dis.code = DischargingPhysicianCode)]),
              dad.link[,.(EncID.new, mrp.code = MostResponsibleDoctorCode)],
              by = "EncID.new")
link$EncID.new <- paste("15", link$EncID.new, sep = "")

fwrite(link, "H:/GEMINI/Results/DataSummary/physician_names/link/thp.link.csv")


# ---------------------- check new adm dis physician file ----------------------
# ------------------------------- 2017-06-06 -----------------------------------
library(gemini)
lib.pa()
adm.link <- fread("H:/GEMINI/Results/DataSummary/physician_names/revisedIP_deidentified.csv")
adm.link[, V1:=NULL]
adm.link <- unique(adm.link)
adm.old <- fread("R:/GEMINI/_RESTORE/THP/phynames/thp.GIM_IP_LINKING_LIST_physicians.csv")
sum(adm.link$EncID.new%in%adm.old$EncID.new)

sum(c(adm.link$AdmittingPhysicianCode, adm.link$DischargingPhysicianCode)%in%
      c(adm.old$AdmittingPhysicianCode, adm.old$DischargingPhysicianCode))
names(adm.link) <- paste(names(adm.link), "new", sep = "_")
adm.old <- adm.old[, 1:7, with = F]
adm.old <- unique(adm.old)
adm.compare <- merge(adm.old, adm.link, by.x = "EncID.new",
                     by.y = "EncID.new_new")

sum(adm.compare$DischargingPhysicianCode == adm.compare$DischargingPhysicianCode_new)
adm.compare[DischargingPhysicianCode!=DischargingPhysicianCode_new, 
            .(DischargingPhysicianCode, DischargingPhysicianCode_new,
              DischargingPhysicianFirstName, DischargingPhysicianLastName,
              DischargingPhysicianFirstName_new, DischargingPhysicianLastName_new)] -> check
adm.compare[DischargingPhysicianLastName!=toupper(DischargingPhysicianLastName_new),
            .(DischargingPhysicianCode, DischargingPhysicianCode_new,
              DischargingPhysicianFirstName, DischargingPhysicianLastName,
              DischargingPhysicianFirstName_new, DischargingPhysicianLastName_new)] -> check
##? ~3000 patients had different discharging physician

adm.compare[AdmittingPhysicianCode!=""&
  AdmittingPhysicianLastName!=toupper(AdmittingPhysicianLastName_new),
            .(EncID.new, AdmittingPhysicianCode, AdmittingPhysicianFirstName, 
              AdmittingPhysicianLastName,
              AdmittingPhysicianCode_new, AdmittingPhysicianFirstName_new, 
              AdmittingPhysicianLastName_new)] -> check_adm_p
fwrite(check_adm_p, "R:/GEMINI/Check/physician_names/thp_adm_phy_discrepancy.csv")

apply(adm.link, 2, function(x)sum(x==""))
adm.link[AdmittingPhysicianCode==""] -> check
adm.old[EncID.new%in%check$EncID.new] -> check2
apply(check2, 2, function(x)sum(x==""))
check2[AdmittingPhysicianCode!=""]
fwrite(check, "R:/GEMINI/Check/physician_names/thp_missing_adm_dis_md.csv")


adm.link <- fread("H:/GEMINI/Results/DataSummary/physician_names/revisedIP_deidentified.csv")
adm.link[, V1:=NULL]
adm.link <- unique(adm.link)
dad.link <- fread("R:/GEMINI/_RESTORE/THP/phynames/thp.dad.LINKINGLIST_physicians.csv")
link <- merge(unique(adm.link[,.(EncID.new, adm.code = AdmittingPhysicianCode, 
                                 dis.code = DischargingPhysicianCode)]),
              dad.link[,.(EncID.new, mrp.code = MostResponsibleDoctorCode)],
              by = "EncID.new")
link$EncID.new <- paste("15", link$EncID.new, sep = "")

fwrite(link, "H:/GEMINI/Results/DataSummary/physician_names/link/thp.link.new.csv")





all.names <- rbind(data.table(Code = adm.link$AdmittingPhysicianCode,
                              first.name = adm.link$AdmittingPhysicianFirstName,
                              last.name = adm.link$AdmittingPhysicianLastName),
                   data.table(Code = adm.link$DischargingPhysicianCode,
                              first.name = adm.link$DischargingPhysicianFirstName,
                              last.name = adm.link$DischargingPhysicianLastName),
                   data.table(Code = dad.link$MostResponsibleDoctorCode,
                              first.name = dad.link$MostResponsiblePhysicianFirstName,
                              last.name = dad.link$MostResponsiblePhysicianLastName))

all.names <- merge(unique(all.names), data.table(table(all.names$Code)),
                   by.x = "Code", by.y= "V1", all.x = T)

all.names$code.type <- "thp"
all.names$GIM <- "y"

fwrite(all.names, "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/thp.names.new.csv")


simpleCap <- function(x)gsub("(^|[[:space:]]|'|-)([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)
all.names$first.name <- simpleCap(tolower(all.names$first.name))
all.names$last.name <- simpleCap(tolower(all.names$last.name))


all.phy.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.new.csv")
all.phy.name.new <- rbind(all.phy.name,
                          all.names[!Code%in%all.phy.name[code.type=="thp", Code]],
                          fill = T)
# all.phy.name.new <- all.phy.name.new %>% arrange(last.name, first.name)
# fwrite(all.phy.name.new, "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.new2.csv")
# 
# 
# all.phy.name.new[duplicated(all.phy.name.new), ]



# ----------------------------- new thp data -----------------------------------
adm.names <- fread("H:/GEMINI/Results/DataSummary/physician_names/thp.physicians/thp.physicians_list.csv")
mrp.names <- fread("H:/GEMINI/Results/DataSummary/physician_names/thp.physicians/thp.dad.mrp.csv")

unique_names <- rbind(
  adm.names[, .(first.name = AdmittingPhysicianFirstName,
                last.name = AdmittingPhysicianLastName,
                code = AdmittingPhysicianCode,
                hashcode = ADM.CODE)],
  adm.names[, .(first.name = DischargingPhysicianFirstName,
                last.name = DischargingPhysicianLastName,
                code = DischargingPhysicianCode,
                hashcode = DIS.CODE)],
  mrp.names[, .(first.name = MostResponsiblePhysicianFirstName,
                last.name = MostResponsiblePhysicianLastName,
                code = MostResponsibleDoctorCode,
                hashcode = MRP.CODE)]
)
unique_names <- unique_names[,.N, by = .(first.name, last.name, code, hashcode)]

all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.new2.csv")

sum(unique_names$code%in%all.name[code.type=="thp", Code])
unique_names[!code%in%all.name$Code]

fwrite(unique_names[!code%in%all.name$Code],
       "H:/GEMINI/Results/DataSummary/physician_names/thp.physicians/thp_new_names.csv")
adm.new[Admitting.Code=="75f325be32eb7132685ff432180febc7d1f28ae0"]
adm.new[Discharging.Code=="75f325be32eb7132685ff432180febc7d1f28ae0"]
dad.new[MRP.CODE=="75f325be32eb7132685ff432180febc7d1f28ae0"]
