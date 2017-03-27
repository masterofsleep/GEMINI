# --------------------------- UHN Phynames -------------------------------------
# ---------------------------- 2017-03-17 --------------------------------------

library(gemini)
lib.pa()

mrp.all <- fread("C:/Users/guoyi/Desktop/marked_names/uhn/dad.mrp.codes-names.csv")
mrp.all$paste <- paste(mrp.all$MostResponsibleDoctorCode, 
                       mrp.all$mostresponsiblefirstname, 
                       mrp.all$mostresponsiblelastname,
                       mrp.all$mrpCode)
table(mrp.all$paste) %>%data.table -> mrp.freq
mrp.all <- mrp.all[!duplicated(paste)]
  
mrp.freq <- merge(mrp.freq, mrp.all[, .(MostResponsibleDoctorCode,
                                        mostresponsiblefirstname,
                                         mostresponsiblelastname,
                                         mrpCode, paste)],
                  by.x = "V1", by.y = "paste",
                  all.x = T, all.y = F)
mrp.freq <- mrp.freq[order(MostResponsibleDoctorCode),
                     .(MostResponsibleDoctorCode, mrpCode,
                        mostresponsiblefirstname, mostresponsiblelastname,
                        N)]
mrp.freq[MostResponsibleDoctorCode%in%mrp.freq[duplicated(MostResponsibleDoctorCode),MostResponsibleDoctorCode]]
fwrite(mrp.freq, "R:/GEMINI/Check/physician_names/uhn.mrp.freq.csv")


adm.all <- fread("C:/Users/guoyi/Desktop/marked_names/uhn/adm.dis.phys.codes-names.csv")
adm.all$paste <- paste(adm.all$admCode, adm.all$AdmittingPhysicianCode,
                       adm.all$AdmittingPhysicianfirstname,
                       adm.all$AdmittingPhysicianlastname)
adm.freq <- data.table(table(adm.all$paste))
adm.all <- adm.all[!duplicated(paste)]
adm.freq <- merge(adm.freq, adm.all[,.(admCode,
                                       AdmittingPhysicianCode,
                                       AdmittingPhysicianfirstname, 
                                        AdmittingPhysicianlastname,
                                       paste)],
                  by.x = "V1", by.y = "paste",
                  all.x = T, all.y = F)
adm.freq <- adm.freq[order(AdmittingPhysicianCode), 
                     .(AdmittingPhysicianCode, admCode,
                       AdmittingPhysicianfirstname,
                       AdmittingPhysicianlastname,
                       N)]

adm.freq[AdmittingPhysicianCode%in%adm.freq[duplicated(AdmittingPhysicianCode), AdmittingPhysicianCode]]
fwrite(adm.freq, "R:/GEMINI/Check/physician_names/uhn.adm.freq.csv")



# ------------------------ compare with list uhn provded -----------------------
uhn.list <- readxl::read_excel("C:/Users/guoyi/Desktop/marked_names/uhn/UHN Physician List for GEMINI.xlsx") %>% data.table

uhn.adm.list <- rbind(uhn.list[,.(Code = AdmittingPhysicianCode,
                                  first.name = `Admitting Provider Last Name`,
                                  last.name = `Admitting Provider First Name`)],
                      uhn.list[,.(Code = DischargingPhysicianCode,
                                  first.name = `Attending Provider First Name`,
                                  last.name = `Attending Provider Last Name`)])[!is.na(Code)]
uhn.adm.list <- uhn.adm.list[order(Code)] %>% unique 
sum(adm.freq$AdmittingPhysicianCode%in%uhn.adm.list$Code)
#adm.freq[!AdmittingPhysicianCode%in%uhn.adm.list$Code]
adm.all <- fread("C:/Users/guoyi/Desktop/marked_names/uhn/adm.dis.phys.codes-names.csv")
adm.freq.new <- data.table(table(c(adm.all$AdmittingPhysicianCode,
                                   adm.all$DischargingPhysicianCode), useNA = "ifany"))
names(adm.freq.new)[1] <- "Code"
adm.freq.new$Code <- as.integer(adm.freq.new$Code)
adm.freq.new <- merge(adm.freq.new, uhn.adm.list,
                      by = "Code", 
                      all.x = T, all.y = F)
adm.freq.new <- adm.freq.new[order(Code)]
sum(duplicated(adm.freq.new$Code))
fwrite(adm.freq.new, "R:/GEMINI/Check/physician_names/uhn.adm.freq.csv")




uhn.mrp.list <- uhn.list[, .(MostResponsibleDoctorCode,
                             `MRP First Name`, `MRP Last Name`,
                             `MRP Name`)] %>% unique
mrp.all <- fread("C:/Users/guoyi/Desktop/marked_names/uhn/dad.mrp.codes-names.csv")
sum(mrp.all$MostResponsibleDoctorCode%in%uhn.mrp.list$MostResponsibleDoctorCode)
mrp.freq <- data.table(table(mrp.all$MostResponsibleDoctorCode))
names(mrp.freq)[1] <- "MRP.Code"
mrp.freq$MRP.Code <- as.integer(mrp.freq$MRP.Code)
#there are duplicated codes in uhn.mrp.code
uhn.mrp.list[MostResponsibleDoctorCode%in%
               uhn.mrp.list[duplicated(MostResponsibleDoctorCode), MostResponsibleDoctorCode]]
mrp.freq <- merge(mrp.freq, uhn.mrp.list,
                  by.x = "MRP.Code", by.y = "MostResponsibleDoctorCode",
                  all.x = T, all.y = T)

sum(duplicated())


# ------------------- create names list to be marked ---------------------------
adm.names <- fread("R:/GEMINI/Check/physician_names/uhn.adm.freq.csv")

marked <- readxl::read_excel("R:/GEMINI/Check/physician_names/UHN Physician Names - Coded.xlsx")
marked$paste <- paste(marked$first.name, marked$last.name)
adm.names$paste <- paste(adm.names$first.name, adm.names$last.name)
adm.names <- merge(adm.names, marked[,c(4,5,7)], by = "paste", all.x = T)
adm.names <- adm.names[order(Code)]
adm.names[,':='(first.name = last.name,
                last.name = first.name,
                paste = NULL,
                code.type = "uhn.adm")]
fwrite(adm.names, "R:/GEMINI/Check/physician_names/uhn.adm.freq.csv")

mrp.freq <- fread("R:/GEMINI/Check/physician_names/uhn.mrp.freq.csv")
names(mrp.freq) <- c("Code","mrp.hash", "first.name", "last.name", "N")

simpleCap <- function(x)gsub("(^|[[:space:]]|'|-)([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)
mrp.freq <- mrp.freq[!duplicated(Code)]
mrp.freq$first.name <- simpleCap(mrp.freq$first.name)
mrp.freq$last.name <- simpleCap(mrp.freq$last.name)
mrp.freq$paste <- toupper(paste(mrp.freq$last.name, mrp.freq$first.name))
mrp.freq <- merge(mrp.freq, marked[,c(4,5,7)], by = "paste", all.x = T)
mrp.freq[,':='(paste = NULL,
               mrp.hash = NULL,
               code.type = "uhn.mrp")]
uhn.names <- rbind(adm.names, mrp.freq) %>% arrange(last.name, first.name)
fwrite(uhn.names, "R:/GEMINI/Check/physician_names/uhn.all.names.csv")



# ------------------  find the GIM patients ------------------------------------
uhn.all <- fread("C:/Users/guoyi/Desktop/marked_names/uhn//uhn.all.names.csv")
adm.link <- fread("R:/GEMINI/_RESTORE/UHN/Physicians/adm.dis.phys.codes.csv")
adm.all <- fread("C:/Users/guoyi/Desktop/marked_names/uhn/adm.dis.phys.codes-names.csv")
mrp.all <- fread("C:/Users/guoyi/Desktop/marked_names/uhn/dad.mrp.codes-names.csv")
mrp.link <- fread("R:/GEMINI/_RESTORE/UHN/Physicians/dad.mrp.codes.csv")

uhn.all[is.na(`MD Code`), `MD Code`:=4]
uhn.all$GIM <- ifelse(uhn.all$`MD Code`%in%c(1,2), "y", 
                      ifelse(uhn.all$`MD Code`%in%c(3,5), "n","u"))
fwrite(uhn.all[,c(1:5,8), with = F], "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/uhn.names.csv")


sum(adm.link$admCode==adm.all$admCode)
sum(adm.link$disCode==adm.all$disCode)
sum(adm.link$EncID.new==mrp.all$EncID.new)
sum(mrp.link$mrpCode==mrp.all$mrpCode)
sum(mrp.link$EncID.new==adm.link$EncID.new)
uhn.phy.codes <- cbind(EncID.new = adm.link$EncID.new, 
                       adm.code = adm.all$AdmittingPhysicianCode, 
                       dis.code = adm.all$DischargingPhysicianCode,
                       mrp.code = mrp.all$MostResponsibleDoctorCode)
uhn.phy.codes <- data.table(uhn.phy.codes)
uhn.phy.codes$EncID.new <- paste("13", uhn.phy.codes$EncID.new, sep = "")
#fwrite(uhn.phy.codes, "H:/GEMINI/Results/DataSummary/physician_names/link/uhn.link.csv")

uhn.phy.codes <- merge(uhn.phy.codes, uhn.all[code.type=="uhn.adm", .(Code, first.name, last.name, GIM)],
                       by.x = "adm.code", by.y = "Code", all.x = T)
uhn.phy.codes <- merge(uhn.phy.codes, uhn.all[code.type=="uhn.adm", .(Code, first.name, last.name, GIM)],
                       by.x = "dis.code", by.y = "Code", all.x = T)

uhn.phy.codes <- data.table(uhn.phy.codes)
uhn.phy.codes[, GIM:= ifelse(GIM.x=="y"|GIM.y=="y", "y",
                             ifelse(GIM.x=="n"&GIM.y=="n", "n", "u"))]
not.gim <- uhn.phy.codes[GIM=="n"]
unknown <- uhn.phy.codes[GIM=="u"]

xfer <- readg(uhn, xfer)
xfer[str_sub(EncID.new, 3, 8)%in%uhn.phy.codes[adm.code=="56118", EncID.new]|
           str_sub(EncID.new, 3, 8)%in%uhn.phy.codes[dis.code=="56118", EncID.new]]%>% 
  fwrite("R:/GEMINI/_RESTORE/UHN/Physicians/carolina.landolt.marticorena.csv")
xfer[str_sub(EncID.new, 3, 8)%in%uhn.phy.codes[adm.code=="15142", EncID.new]|
          str_sub(EncID.new, 3, 8)%in%uhn.phy.codes[dis.code=="15142", EncID.new]]%>% 
  fwrite("R:/GEMINI/_RESTORE/UHN/Physicians/malcolm.moore.csv")
xfer[str_sub(EncID.new, 3, 8)%in%uhn.phy.codes[adm.code=="110530", EncID.new]|
          str_sub(EncID.new, 3, 8)%in%uhn.phy.codes[dis.code=="110530", EncID.new]]%>% 
  fwrite("R:/GEMINI/_RESTORE/UHN/Physicians/phil.segal.csv")



fwrite(not.gim, "H:/GEMINI/Results/Check/phynames/uhn.notgim.csv")
fwrite(unknown, "H:/GEMINI/Results/Check/phynames/uhn.unknown.csv")
