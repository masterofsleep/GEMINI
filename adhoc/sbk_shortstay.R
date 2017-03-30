library(gemini)
lib.pa()

xfer <- readg(sbk, xfer)
xfer <- xfer %>% arrange(EncID.new, ymd_hm(paste(Date.Check.in, Time.Check.in)))


inc <- ddply(xfer, ~EncID.new, summarize,
              inc = Unit[1]=="EMRG"&Unit[2]%in%c("C6MU", "CGMU"))
ex <- readg(gim, notgim)


dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")

cohort <- dad[EncID.new%in%inc[inc$inc==T, "EncID.new"]&!EncID.new%in%ex$EncID.new,
    .(EncID.new, LoS, Discharge.Disposition)]

scu <- readg(sbk, scu)
icu <- c(scu[SCU.Unit.Number!=99, EncID.new], xfer[xfer$Unit.Code==1, "EncID.new"])

cohort$ICU <- cohort$EncID.new%in%icu

sum(cohort$Discharge.Disposition==7)#/3333
sum(cohort$ICU==T)/3333
sum(cohort$LoS<2)/3333
sum(cohort$LoS<3)/3333
