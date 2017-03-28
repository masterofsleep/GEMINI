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
code = "E87"

dbt <- dad[diabetic==T]
nondbt <- dad[diabetic==F]
find.p <- function(code){
  dbt.diag <- dbt[Diagnosis.Code==code]
  nondbt.diag <- nondbt[Diagnosis.Code==code]
  prop.p <- prop.test(c(nrow(dbt.diag), nrow(nondbt.diag)),
                      c(nrow(dbt), nrow(nondbt)))$p.value
  mort.p <- prop.test(c(sum(dbt.diag$Discharge.Disposition==7),
                        sum(nondbt.diag$Discharge.Disposition==7)),
                      c(nrow(dbt.diag), 
                        nrow(nondbt.diag)))$p.value
  icu.p <- prop.test(c(sum(dbt.diag$SCU.adm==T),
                        sum(nondbt.diag$SCU.adm==T)),
                      c(nrow(dbt.diag), 
                        nrow(nondbt.diag)))$p.value
  # los.p <- t.test(dbt.diag$LoS, nondbt.diag$LoS)$p.value
  # cost.p <- t.test(dbt.diag$Cost, nondbt.diag$Cost)$p.value
  los.p <- wilcox.test(dbt.diag$LoS, nondbt.diag$LoS)$p.value
  cost.p <- wilcox.test(dbt.diag$Cost, nondbt.diag$Cost)$p.value
  # alc.days.p <- t.test(dbt.diag$Number.of.ALC.Days, 
  #                      nondbt.diag$Number.of.ALC.Days)$p.value
  return(c(prop.p, mort.p, icu.p, los.p, cost.p))
}


prop.test(c(1092, 1795), c(38517, 99917))
prop.test(c(33, 15), c(38517, 99917))


p.value <- NULL
for(i in codes){
  p.value <- c(p.value, find.p(i))
}
p.value
p.value <- sprintf("%.3f", round(p.value, 3))
p.value[as.numeric(p.value)==0] <- "<0.001"
matrix(p.value, nrow = 6)
write.csv(matrix(p.value, nrow = 6), "H:/GEMINI/Results/Diabetes/p.value.csv", row.names = F)

codes <- c("I50", "N39", "I63", "J18", 
           "J44", "N17", "A41", "E87", 
           "F05")
find.p2 <- function(code){
  dbt.diag <- dbt[Diagnosis.Code==code]
  nondbt.diag <- nondbt[Diagnosis.Code==code]
  
  los.p <- wilcox.test(dbt.diag$LoS, nondbt.diag$LoS)$p.value
  cost.p <- wilcox.test(dbt.diag$Cost, nondbt.diag$Cost)$p.value
  return(c(los.p, cost.p))
}
p.value <- NULL
for(i in codes){
  p.value <- c(p.value, find.p2(i))
}
p.value <- sprintf("%.3f", round(p.value, 3))
p.value[as.numeric(p.value)==0] <- "<0.001"
p.value %>% matrix(nrow = 2)
# soft tissue and bone infections
dad <- merge(dad, ip.diag[Diagnosis.Type=="M", .(Diagnosis.Code, EncID.new)])
dad.in <- dad[startwith.any(Diagnosis.Code, c("M86", "L03", "E105", "E115"))&diabetic==T]
dad.out <- dad[startwith.any(Diagnosis.Code, c("M86", "L03", "E105", "E115"))&diabetic==F]

wilcox.test(dad.in$LoS, dad.out$LoS)

wilcox.test(dad.in$Cost, dad.out$Cost)

code = "I63"

dat <- matrix(c(2724,(38515-2724),3881,(99917-3881)), nrow = 2, byrow = TRUE)
rownames(dat) <- c("DF+", "DF-"); colnames(dat) <- c("FUS+", "FUS-"); dat

res <- epi.2by2(dat = as.table(dat), method = "cross.sectional", 
         conf.level = 0.95, units = 100,  homogeneity = "breslow.day", 
         outcome = "as.columns")

library(epiR)
dbt <- dad[diabetic==T]
nondbt <- dad[diabetic==F]
pr.ci <- function(code){
  dbt.diag <- dbt[Diagnosis.Code==code] %>% nrow
  nondbt.diag <- nondbt[Diagnosis.Code==code] %>% nrow
  dat <- matrix(c(dbt.diag,(38517-dbt.diag),nondbt.diag,(99917-nondbt.diag)), nrow = 2, byrow = TRUE)
  print(epi.2by2(dat = as.table(dat), method = "cross.sectional", 
                  conf.level = 0.95, units = 100,  homogeneity = "breslow.day", 
                  outcome = "as.columns"))
}


celebral.infarction = "I63"
copd = "J44"
uti = "N39"
hf = c("I50")
acute.renal.failure = "N17"
stroke.code = c("I63")
pneumonia  = "J18"


pr.ci(hf)
pr.ci(uti)
pr.ci(stroke.code)
pr.ci(pneumonia)
pr.ci(copd)
pr.ci(acute.renal.failure)
