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
