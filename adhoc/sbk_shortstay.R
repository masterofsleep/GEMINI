library(gemini)
lib.pa()

xfer <- readg(sbk, xfer)
xfer <- xfer %>% arrange(EncID.new, ymd_hm(paste(Date.Check.in, Time.Check.in)))


inc <- ddply(xfer, ~EncID.new, summarize,
              inc = Unit[1]=="EMRG"&Unit[2]%in%c("C6MU", "CGMU"))
ex <- readg(gim, notgim)


dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")

cohort <- dad[str_sub(EncID.new, 1, 2)=="12"&
                (!EncID.new%in%ex$EncID.new),
    .(EncID.new, LoS, Discharge.Disposition)]
cohort$short.stay.unit <- cohort$EncID.new%in%inc$EncID.new[inc$inc==T]

scu <- readg(sbk, scu)
icu <- c(scu[SCU.Unit.Number!=99, EncID.new], xfer[xfer$Unit.Code==1, "EncID.new"])

cohort$ICU <- cohort$EncID.new%in%icu
cohort$died <- cohort$Discharge.Disposition=="7"
cohort$los48 <- cohort$LoS<2.0
cohort$los72 <- cohort$LoS<3.0

library(tableone)
vars <- c("died", "ICU", "los48", "los72")
catvars <- vars
CreateTableOne(vars = vars, factorVars = catvars, strata = "short.stay.unit", data = cohort)
