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

  





# ---------------------- NEW Cohort selection ----------------------------------
all.phy <- readg(gim, all.phy)
los.cohort <- all.phy[adm.code.new==dis.code.new&GIM=="y"]
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")

los.cohort <- merge(los.cohort[,.(EncID.new, mrp.code.new, GIM)], 
                    unique(dad[,.(EncID.new, Age, Gender, LoS)]),by = "EncID.new")

cci <- readg(gim, cci)
los.cohort <- merge(los.cohort, cci, by = "EncID.new", all.x = T, all.y = F)

site.map <- data.table(
  code = c("11", "12","13","14","15"),
  site = c("smh", "sbk", "uhn", "msh", "thp")
)
los.cohort$code <- str_sub(los.cohort$EncID.new, 1, 2)
los.cohort <- merge(los.cohort, site.map, by = "code")

los.cohort$mrp.code <- paste(los.cohort$site, los.cohort$mrp.code.new, sep = "-")
los.cohort[, LoS := LoS * 24]
los.cohort[, LOS_in_20_grps := 
             cut(LoS, breaks=quantile(LoS, probs=seq(0,1, by=0.05), na.rm=TRUE), 
                                             include.lowest=TRUE,
                 labels = 1:20)]
los.cohort[, LOS_in_10_grps := 
             cut(LoS, breaks=quantile(LoS, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                 include.lowest=TRUE,
                 labels = 1:10)]
los.cohort[, Group_by_10hrs := ceiling((LoS-min(LoS))/10)]
los.cohort[, Group_by_20hrs := ceiling((LoS-min(LoS))/20)]
cmg$EncID.new <- as.character(cmg$EncID.new)
los.cohort$EncID.new <- as.character(los.cohort$EncID.new)
los.cohort <- merge(los.cohort, cmg, by = "EncID.new", all.x = T, all.y = F)
los.cohort[, mrp.code.new:=NULL]

fwrite(los.cohort[,.(EncID.new, Age, Gender, Charlson.Comorbidity.Index, CMG,LoS, mrp.code, mrp.GIM = GIM, site, 
                     Group_by_10hrs, Group_by_20hrs, LOS_in_20_grps, LOS_in_10_grps)],
       "H:/GEMINI/Results/LengthofStay/cohort.los.april07.csv")
fwrite(data.table(table(los.cohort[,mrp.code])),
       "H:/GEMINI/Results/LengthofStay/mrp.freq.april07.csv")

table(los.cohort$site)
table(los.cohort$LOS_in_10_grps)
