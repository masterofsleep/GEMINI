# ==============================================================================
# =========================  Physician Names  ==================================
# =========================    March 2 2017   ==================================
library(gemini)
lib.pa()

smh.link <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/SMH.LINKLIST_NEWHASH.csv")
adm.names <- table(smh.link[, paste(AdmittingPhysicianFirstName, ADMITTINGPRACTITIONERLASTNAME)])%>%
  data.table %>% rename(Admit.Physician.Name = V1) %>% 
  arrange(desc(N))
fwrite(adm.names, "H:/GEMINI/Results/DataSummary/physician_names/smh.adm.phy.csv")

dis.names <- table(smh.link[,paste(DischargingPhysicianFirstName, DischargingPhysicianLastName)])%>%
  data.table %>% rename(Discharge.Physician.Name = V1) %>% arrange(desc(N)) 
fwrite(dis.names, "H:/GEMINI/Results/DataSummary/physician_names/smh.dis.phy.csv")

smh <- readg(smh, dad)
mrp.freq <- table(smh[, MostResponsible.DocterCode]) %>% data.table
mrp.names <- fread("R:/GEMINI/_RESTORE/SMH/Physicians/DAD_MRPs.csv")
mrp.names$MostResponsibleDoctorCode <- as.character(mrp.names$MostResponsibleDoctorCode)
mrp.freq <- merge(mrp.freq, mrp.names[,.(MostResponsiblePhysicianFirstName, 
                                         MostResponsiblePhysicianLastName, MostResponsibleDoctorCode)], 
                  by.x = "V1",
                  by.y = "MostResponsibleDoctorCode", all.x = T) %>% unique %>%
  arrange(desc(N))
names(mrp.freq)[1] <- "MRP.Code"
fwrite(mrp.freq, "H:/GEMINI/Results/DataSummary/physician_names/smh.mrp.csv")

smh.phy <- c(adm.names$Admit.Physician.Name, dis.names$Discharge.Physician.Name,
             paste(mrp.freq$MostResponsiblePhysicianFirstName, mrp.freq$MostResponsiblePhysicianLastName)) %>% unique
write.csv(smh.phy, "H:/GEMINI/Results/DataSummary/physician_names/smh.physician.names.csv")



# ---------------------------- march 7 -----------------------------------------
# ------------------------ check by coded names --------------------------------
names.coded <- readxl::read_excel("R:/GEMINI/Check/physician_names/smh.physician.names.coded.all.xlsx")
smh.link <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/SMH.LINKLIST_NEWHASH.csv")
table(names.coded$GIM, useNA = "ifany")
smh.link <- smh.link[,.(EncID.new, AdmittingPhysicianFirstName, ADMITTINGPRACTITIONERLASTNAME,
                        DischargingPhysicianFirstName, DischargingPhysicianLastName)]
smh.link[,':='(admit.phy.name = paste(AdmittingPhysicianFirstName, ADMITTINGPRACTITIONERLASTNAME),
               dis.phy.name = paste(DischargingPhysicianFirstName, DischargingPhysicianLastName))]

smh.link[, GIM:= admit.phy.name%in%names.coded$Name[names.coded$GIM==1]|
           dis.phy.name%in%names.coded$Name[names.coded$GIM==1]]
smh.link$GIM %>% table

smh.link[GIM==F] %>% fwrite("H:/GEMINI/Results/Check/phynames/smh.notgim.csv")


# ---------------------------- complete list -----------------------------------
smh.link <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/SMH.LINKLIST_NEWHASH.csv")
adm.names <- data.table(
  Code = c(smh.link$AdmitingPhysicianCode, smh.link$DischargingPhysicianCode),
  first.name = c(smh.link$AdmittingPhysicianFirstName, smh.link$DischargingPhysicianFirstName),
  last.name = c(smh.link$ADMITTINGPRACTITIONERLASTNAME, smh.link$DischargingPhysicianLastName)
)
adm.names$Code <- as.character(adm.names$Code)
adm.names <- merge(unique(adm.names), data.table(table(adm.names$Code)), 
                   by.x = "Code", by.y = "V1")
adm.names$code.type <- "smh.adm"

mrp.names <- fread("R:/GEMINI/_RESTORE/SMH/Physicians/DAD_MRPs.csv")
mrp.names[, UMRDOCSERVICE:=NULL]
names(mrp.names) <- c("Code", "first.name", "last.name")
mrp.names$Code <- as.character(mrp.names$Code)
mrp.names <- merge(unique(mrp.names), data.table(table(mrp.names$Code)),
                   by.x = "Code", by.y = "V1")
mrp.names$code.type <- "smh.mrp"

smh.names <- merge(adm.names, mrp.names, by = c("first.name", "last.name"),
            all.x = T, all.y = T)

smh.names[is.na(Code.x), ':='(Code.x = Code.y,
                              N.x = 0)]
smh.names[is.na(N.y), N.y := 0]
smh.names <- smh.names[,.(Code = Code.x, 
                          first.name, 
                          last.name,
                          N = N.x + N.y,
                          code.type = "smh")]
names.coded <- readxl::read_excel("R:/GEMINI/Check/physician_names/smh.physician.names.coded.all.xlsx")%>%
  data.table
names.coded[, GIM:= ifelse(GIM==1, "y", "n")]
names.coded[Geriatrics==1, GIM:= "Geriatrics"]

smh.names$Name <- paste(smh.names$first.name, smh.names$last.name)
smh.names <- merge(smh.names, names.coded[,.(Name, GIM)],
                   by = "Name", all.x = T)
smh.names[,Name:=NULL]
fwrite(smh.names[,.(Code, code.type, N, 
                    first.name, last.name,
                    GIM)], "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/smh.names.csv")

