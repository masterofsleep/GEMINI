#------------------------- Diabetes --------------------------------------------
#------------------------- Feb 27 2017 -----------------------------------------
rm(list = ls())
library(gemini)
lib.pa()
ip.diag <- readg(gim, ip_diag)
er.diag <- readg(gim, er_diag)
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad <- dad[, .(EncID.new, Age, Gender, Discharge.Disposition, Number.of.ALC.Days,
               Institution.Number, LoS, SCU.adm, Cost)]
dbt.code <- c("E11", "E10")
dbt <- union(ip.diag[startwith.any(Diagnosis.Code, dbt.code), EncID.new],
             er.diag[startwith.any(ER.Diagnosis.Code, dbt.code), EncID.new])
dad$diabetic <- dad$EncID.new%in%dbt

mrd <- ip.diag[Diagnosis.Type=="M", .(Diagnosis.Code = str_sub(Diagnosis.Code, 1, 3), EncID.new)]
icd10 <- fread("H:/GEMINI/Coding/icd10_3digit.csv")
names(icd10) <- c("Code", "Diagnosis")
mrd <- merge(mrd, icd10, by.x = "Diagnosis.Code", by.y = "Code",
             all.x = T, all.y = F)

dad <- merge(dad, mrd[,.(Diagnosis.Code, Diagnosis, EncID.new)], by = "EncID.new")
dad <- dad[!is.na(Cost)]


codes <- c("I50", "N39", "I63", "J18", 
           "J44", "N17", "A41", "E87", 
           "F05", "L03", "I20", "M86")

dbt <- dad[diabetic==T]
nondbt <- dad[diabetic==F]
find.p <- function(code){
  dbt.diag <- dbt[Diagnosis.Code==code]
  nondbt.diag <- nondbt[Diagnosis.Code==code]
  prop.p <- prop.test(c(nrow(dbt.diag), nrow(nondbt.diag)),
                      c(nrow(dad), nrow(dad)))$p.value
  mort.p <- prop.test(c(sum(dbt.diag$Discharge.Disposition==4),
                        sum(nondbt.diag$Discharge.Disposition==4)),
                      c(nrow(dbt.diag), 
                        nrow(nondbt.diag)))
  icu.p <- prop.test(c(sum(dbt.diag$SCU.adm==T),
                        sum(nondbt.diag$SCU.adm==T)),
                      c(nrow(dbt.diag), 
                        nrow(nondbt.diag)))$p.value
  los.p <- t.test(dbt.diag$LoS, nondbt.diag$LoS)$p.value
  cost.p <- t.test(dbt.diag$Cost, nondbt.diag$Cost)$p.value
  alc.days.p <- t.test(dbt.diag$Number.of.ALC.Days, 
                       nondbt.diag$Number.of.ALC.Days)$p.value
  return(c(prop.p, mort.p, icu.p, los.p, cost.p, alc.days.p))
}
p.value <- NULL
for(i in codes){
  p.value <- c(p.value, find.p(i))
}
p.value
p.value <- sprintf("%.3f", round(p.value, 3))
p.value[as.numeric(p.value)==0] <- "<0.001"
write.csv(p.value, "H:/GEMINI/Results/Diabetes/p.value.csv", row.names = F)

code = "I63"

dat <- matrix(c(2724,(38515-2724),3881,(99917-3881)), nrow = 2, byrow = TRUE)
rownames(dat) <- c("DF+", "DF-"); colnames(dat) <- c("FUS+", "FUS-"); dat

res <- epi.2by2(dat = as.table(dat), method = "cross.sectional", 
         conf.level = 0.95, units = 100,  homogeneity = "breslow.day", 
         outcome = "as.columns")

res$n.strata
