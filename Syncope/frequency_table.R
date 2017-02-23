#===================   Syncope =================================================

#------------------ Frequency Table --------------------------------------------
library(gemini)
lib.pa()
rm(list = ls())

#----------------------SMH------------------------------------------------------

smh.lab <- readg(smh, corelabs)
head(smh.lab)
smh.lab.table <- data.frame(table(smh.lab$Test.Name))
names(smh.lab.table) <- c("Test.Name", "Freq")
write.csv(smh.lab.table, "H:/GEMINI/Results/Syncope/freq.table.smh.csv",
          row.names = F)

smh.ct <- readg(smh, ct)
smh.ct.table <- data.frame(table(smh.ct$proc_desc_long))
names(smh.ct.table) <- c("Test.Name", "Freq")
write.csv(smh.ct.table, "H:/GEMINI/Results/Syncope/freq.table.smh.ct.csv",
          row.names = F)

smh.us <- readg(smh.us, us.csv)
smh.us.table <- data.frame(table(smh.us$proc_desc_long))
names(smh.us.table) <- c("Test.Name", "Freq")
write.csv(smh.us.table, "H:/GEMINI/Results/Syncope/freq.table.smh.us.csv",
          row.names = F)
smh.nuc <- readg(smh, nuc)
smh.nuc.table <- data.frame(table(smh.nuc$proc_desc_long))
names(smh.nuc.table) <- c("Test.Name", "Freq")
write.csv(smh.nuc.table, "H:/GEMINI/Results/Syncope/freq.table.smh.nuc.csv",
          row.names = F)

rm(smh.lab)


#----------------------SBK------------------------------------------------------
sbk.lab.er <- readg(sbk, labs_er)
head(sbk.lab.er)
sbk.lab.er.table <- data.frame(table(sbk.lab.er$TestName))
names(sbk.lab.er.table) <- c("Test.Name", "Freq")
write.csv(sbk.lab.er.table, "H:/GEMINI/Results/Syncope/freq.table.sbk.lab.er.csv",
          row.names = F)


sbk.lab.ip <- readg(sbk, labs_ip)
head(sbk.lab.ip)
sbk.lab.ip.table <- data.frame(table(sbk.lab.ip$TestName))
names(sbk.lab.ip.table) <- c("Test.Name", "Freq")
write.csv(sbk.lab.ip.table, "H:/GEMINI/Results/Syncope/freq.table.sbk.lab.ip.csv",
          row.names = F)

### Dec 15 2016 SBK freq table
sbk.lab <- rbind(sbk.laber, sbk.labip)
sbk.lab.table <- data.frame(table(sbk.lab$Test.Name))
names(sbk.lab.table) <- c("Test.Name", "Freq")
write.csv(sbk.lab.table, "H:/GEMINI/Results/Syncope/freq.table.sbk.lab.csv",
          row.names = F)

sbk.rad.er <- fread("H:/GEMINI/Data/SBK/Radiology/rad_er.csv")
head(sbk.rad.er)
sbk.rad.er.table <- data.frame(table(sbk.rad.er$TestName))
names(sbk.rad.er.table) <- c("Test.Name", "Freq")
write.csv(sbk.rad.er.table, "H:/GEMINI/Results/Syncope/freq.table.sbk.rad.er.csv",
          row.names = F)



sbk.rad.ip <- fread("H:/GEMINI/Data/SBK/Radiology/rad_ip.csv")
head(sbk.rad.ip)
sbk.rad.ip.table <- data.frame(table(sbk.rad.ip$`Test Name`))
names(sbk.rad.ip.table) <- c("Test.Name", "Freq")
write.csv(sbk.rad.ip.table, "H:/GEMINI/Results/Syncope/freq.table.sbk.rad.ip.csv",
          row.names = F)








#----------------------------- UHN ---------------------------------------------


uhn.rad.er <- readg(rad, rader_jdri)
head(uhn.rad.er)
uhn.rad.er.table <- data.frame(table(uhn.rad.er$ProcedureName))
names(uhn.rad.er.table) <- c("Test.Name", "Freq")
write.csv(uhn.rad.er.table, "H:/GEMINI/Results/Syncope/freq.table.uhn.rad.er.csv",
          row.names = F)



uhn.rad.ip <- readg(rad, radip_jdri)
head(uhn.rad.ip)
uhn.rad.ip.table <- data.frame(table(uhn.rad.ip$TestName))
names(uhn.rad.ip.table) <- c("Test.Name", "Freq")
write.csv(uhn.rad.ip.table, "H:/GEMINI/Results/Syncope/freq.table.uhn.rad.ip.csv",
          row.names = F)


uhn <- readg(uhn, labs)
uhn$Test.Name <- trimws(uhn$Test.Name)
uhn.lab.table <- data.frame(table(uhn$Test.Name))
write.csv(uhn.lab.table, "H:/GEMINI/Results/Syncope/freq.table.uhn.lab.csv",
          row.names = F)
