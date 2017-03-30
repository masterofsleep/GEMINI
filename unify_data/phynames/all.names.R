# ------------ To create a complete list of physician names in GEMINI ----------
# -------------------------- 2017-03-20 ----------------------------------------
smh.link <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/SMH.LINKLIST_NEWHASH.csv")
mrp.names <- fread("R:/GEMINI/_RESTORE/SMH/Physicians/DAD_MRPs.csv")

smh.all.names <- rbind(smh.link[,.(Code = AdmitingPhysicianCode,
                                   First = AdmittingPhysicianFirstName,
                                   Last = ADMITTINGPRACTITIONERLASTNAME,
                                   site = "SMH")],
                       smh.link[,.(Code = DischargingPhysicianCode,
                                   First = DischargingPhysicianFirstName,
                                   Last = DischargingPhysicianLastName,
                                   site = "SMH")],
                       mrp.names[,.(Code = MostResponsibleDoctorCode,
                                    First = MostResponsiblePhysicianFirstName,
                                    Last = MostResponsiblePhysicianLastName,
                                    site = "SMH")]) %>% unique %>% 
  arrange(Last, First)




