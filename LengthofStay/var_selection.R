#====================== Length of Stay =========================================

rm(list = ls())
library(gemini)
lib.pa()

smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
thp.adm <- readg(thp, adm)



smh.adm <- smh.adm[startsWith(Admitting.Service, "TM")]
sbk.adm <- sbk.adm[Admitting.Service == "GM"]
uhn.adm <- uhn.adm[Admitting.Service=="GIM"]
smh.adm$Institution.Number <- "53985"
sbk.adm$Institution.Number <- "54204"
msh.adm <- msh.dad[,.(EncID.new)]
msh.adm$Institution.Number <- "54110"
thp.adm$Institution.Number <- thp.adm$Institution

sbk.adm$Discharging.Code <- msh.adm$Admitting.Code <- 
  msh.adm$Discharging.Code <- NA

adm <- rbind(smh.adm[,.(EncID.new, Institution.Number, 
                        Admitting.Code, Discharging.Code)],
             sbk.adm[,.(EncID.new, Institution.Number, 
                        Admitting.Code, Discharging.Code)],
             uhn.adm[,.(EncID.new, Institution.Number, 
                        Admitting.Code, Discharging.Code)],
             msh.adm[,.(EncID.new, Institution.Number, 
                        Admitting.Code, Discharging.Code)],
             thp.adm[,.(EncID.new, Institution.Number, 
                        Admitting.Code, Discharging.Code)])

smh.dad <- readg(smh, dad)
sbk.dad <- readg(sbk, dad)
uhn.dad <- readg(uhn, dad)
msh.dad <- readg(msh, dad)
thp.dad <- readg(thp, dad)

dad <- rbind(smh.dad[,.(EncID.new, Admit.Date, Admit.Time,
                        Discharge.Date, Discharge.Time, 
                        Age, Gender, 
                        MostResponsible.DocterCode)],
             sbk.dad[,.(EncID.new, Admit.Date, Admit.Time,
                        Discharge.Date, Discharge.Time, 
                        Age, Gender, 
                        MostResponsible.DocterCode)],
             uhn.dad[,.(EncID.new, Admit.Date, Admit.Time,
                        Discharge.Date, Discharge.Time, 
                        Age, Gender, 
                        MostResponsible.DocterCode)],
             msh.dad[,.(EncID.new, Admit.Date, Admit.Time,
                        Discharge.Date, Discharge.Time, 
                        Age, Gender, 
                        MostResponsible.DocterCode)],
             thp.dad[,.(EncID.new, Admit.Date, Admit.Time,
                        Discharge.Date, Discharge.Time, 
                        Age, Gender, 
                        MostResponsible.DocterCode)])

cohort.los <- merge(adm, dad, by ="EncID.new", all.x = T, all.y = F)

cohort.los[Institution.Number=="53985", Institution.Number:= "SMH"]
cohort.los[Institution.Number=="54204", Institution.Number:= "SBK"]
cohort.los[Institution.Number=="54110", Institution.Number:= "MSH"]
cohort.los[Institution.Number=="M", Institution.Number:= "THP-M"]
cohort.los[Institution.Number=="C", Institution.Number:= "THP-CVH"]
cohort.los[Institution.Number=="54265", Institution.Number:= "TGH"]
cohort.los[Institution.Number=="54266", Institution.Number:= "TWH"]
table(cohort.los$Institution.Number)


cohort.los[,Length.of.Stay:= as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time))-
             ymd_hm(paste(Admit.Date, Admit.Time)))/3600]

names(cohort.los)[2] <- "Institution"

cohort.los <- merge(cohort.los, cci, by = "EncID.new", all.x = T, all.y = F)
cohort.los$Charlson.Comorbidity.Index[cohort.los$Charlson.Comorbidity.Index>=3] <- "3+"

smh.cmg <- readg(smh, ip_cmg)
sbk.cmg <- readg(sbk, ip_cmg)
uhn.cmg <- readg(uhn, ip_cmg)
msh.cmg <- readg(msh, ip_cmg)
thp.cmg <- readg(thp, ip_cmg)
cmg <- rbind(smh.cmg[,.(EncID.new, CMG)],
             sbk.cmg[,.(EncID.new, CMG)],
             uhn.cmg[,.(EncID.new, CMG)],
             msh.cmg[,.(EncID.new, CMG)],
             thp.cmg[,.(EncID.new, CMG)])

cohort.los <- merge(cohort.los, cmg, by = "EncID.new", all.x = T, all.y = F)

fwrite(cohort.los, "H:/GEMINI/Results/LengthofStay/cohort.los.csv")




#-------------------------- Jan 25 2017-----------------------------------------
# summary in protocol

cohort <- fread("H:/GEMINI/Results/LengthofStay/cohort.los.csv")
#132840
cohort <- cohort[Length.of.Stay<=3*sd(Length.of.Stay)]
#130630
fwrite(cohort, "H:/GEMINI/Results/LengthofStay/cohort.los.new.csv")
#2210 removed

mrd.rank <- cohort[,.N, by = c("Institution", "MostResponsible.DocterCode")] %>% 
  arrange(Institution, desc(N))%>%data.table

fwrite(mrd.rank, "H:/GEMINI/Results/LengthofStay/mrd.rank.csv")

summary(cohort$Length.of.Stay)

ggplot(cohort, aes(Length.of.Stay)) + 
  geom_histogram(fill = "white", color = "black", binwidth = 20) + 
  ggtitle("Histogram of Length of Stay")
ggsave("H:/GEMINI/Results/LengthofStay/hist.los.png")

  
