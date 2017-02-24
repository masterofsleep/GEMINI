#------------------------- Diabetes --------------------------------------------
#------------------------- Feb 10 2017 -----------------------------------------
rm(list = ls())
library(gemini)
library(plyr)
lib.pa()

ip.diag <- readg(gim, ip.diag)
er.diag <- readg(gim, er.diag)
ip.diag <- ip.diag[EncID.new%in%er.diag$EncID.new]
er.diag.main <- er.diag[ER.Diagnosis.Type=="M"]
check <- er.diag.main[EncID.new%in%er.diag.main[duplicated(EncID.new), EncID.new]]
msh.adm <- readg(msh, adm)
msh.adm[EncID.new%in%check$EncID.new]
# Find Cost
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
icd.names <- fread("R:/GEMINI/Coding/CIHI/ICD_header.csv")
dad <- dad[!is.na(Cost), .(Cost, EncID.new)]
diag.tobefind <- list(all = unique(c(er.diag$ER.Diagnosis.Code, 
                                     ip.diag$Diagnosis.Code)),
                      "Diagbetes type 2" = c("E11"),
                      "COPD" = c("J40", "J41", "J42", "J43"),
                      "stroke" = c("I60"," I61", "I62", "I63", "I65", "I66",
                                 "I67", "I68", "I69"))

sum.cost <- function(cost){
  data.table("mean(sd)" = paste(round(mean(cost), 1), "(",round(sd(cost), 1),")",sep = ""),
             median = round(median(cost), 1),
             range = paste("(", round(min(cost), 1), ", ", round(max(cost),1), 
                           ")", sep = ""))
}

find.cost.by.diag <- function(ip.diag, er.diag, diag.code, diag.name){
  if(diag.name=="all"){ip <- ip.diag
  }else{ ip <- ip.diag[startwith.any(Diagnosis.Code, diag.code)]}
  if(diag.name=="all"){er <- er.diag
  }else {er <- er.diag[startwith.any(ER.Diagnosis.Code, diag.code)]}
  all.enc <- c(ip$EncID.new, er$EncID.new)
  uni.enc <- length(unique(all.enc))
  adm.rank10 <- er.diag[ER.Diagnosis.Type=="M"&EncID.new%in%all.enc, .N, 
                        by = ER.Diagnosis.Code] %>% 
    arrange(desc(N)) %>% 
    data.table %>% `[`(1:10) %>% 
    merge(icd.names[,.(Code, Desc1)], by.x = "ER.Diagnosis.Code", by.y = "Code",
          all.x = T, all.y = F, sort = F) %>%
    select(Admit.Diag.Code = ER.Diagnosis.Code, 
           Admit.Diag.Name = Desc1, N) %>% 
    mutate("Prop(%)" = round(N/uni.enc * 100, 1))
  cost.adm.freq <- NULL
  for(i in 1:10){
    cost.adm <- dad[EncID.new%in%
                      er.diag[ER.Diagnosis.Type=="M"&
                                ER.Diagnosis.Code==adm.rank10$Admit.Diag.Code[i], 
                              EncID.new], 
                    Cost]
    cost.adm.freq <- rbind(cost.adm.freq, sum.cost(cost.adm))
  }
  adm.rank10 <- cbind(adm.rank10, cost.adm.freq)
  
  dis.rank10 <- ip.diag[Diagnosis.Type=="M"&EncID.new%in%all.enc, .N, 
                        by = Diagnosis.Code] %>% 
    arrange(desc(N)) %>% 
    data.table %>% `[`(1:10) %>% 
    merge(icd.names[,.(Code, Desc1)], by.x = "Diagnosis.Code", by.y = "Code",
          all.x = T, all.y = F, sort = F) %>%
    select(Discharge.Diag.Code = Diagnosis.Code, 
           Discharge.Diag.Name = Desc1, N) %>% 
    mutate("Prop(%)" = round(N/uni.enc * 100, 1))
  cost.dis.freq <- NULL
  for(i in 1:10){
    cost.dis <- dad[EncID.new%in%
                      ip.diag[Diagnosis.Type=="M"&
                                Diagnosis.Code==dis.rank10$Discharge.Diag.Code[i],
                              EncID.new], 
                    Cost]
    cost.dis.freq <- rbind(cost.dis.freq, sum.cost(cost.dis))
  }
  dis.rank10 <- cbind(dis.rank10, cost.dis.freq)
  diag.cost <- dad[EncID.new%in%all.enc, Cost]
  cost.out <- paste(sum.cost(diag.cost), collapse = "\n")
  return(data.table(comorb = c(paste(diag.name, "\n N = ", uni.enc, sep = ""), 
                               rep("", 9)),
                    cost = c(cost.out, rep("", 9)), 
                    cbind(adm.rank10, dis.rank10)))
}

res <- NULL
for(i in 1:4){
  res <- rbind(res, find.cost.by.diag(ip.diag, er.diag, 
                                      diag.tobefind[[i]], names(diag.tobefind)[i]))
}
fwrite(res, "H:/GEMINI/Results/Diabetes/table1.by.freq.csv")

## table 1 by cost
find.cost.by.cost <- function(ip.diag, er.diag, diag.code, diag.name){
  if(diag.name=="all"){ip <- ip.diag
  }else{ ip <- ip.diag[startwith.any(Diagnosis.Code, diag.code)]}
  if(diag.name=="all"){er <- er.diag
  }else {er <- er.diag[startwith.any(ER.Diagnosis.Code, diag.code)]}
  all.enc <- c(ip$EncID.new, er$EncID.new)
  uni.enc <- length(unique(all.enc))
  dad.comorb <- dad[EncID.new%in%all.enc]
  dad.comorb <- merge(dad.comorb, ip.diag[Diagnosis.Type=="M",.(Diagnosis.Code, EncID.new)],
               by = "EncID.new")
  dad.comorb <- merge(dad.comorb, er.diag[ER.Diagnosis.Type=="M",.(ER.Diagnosis.Code, EncID.new)],
               by = "EncID.new")
  adm.rank10 <- ddply(dad.comorb, ~ER.Diagnosis.Code, summarize,
                      meancost = mean(Cost),
                      N = length(EncID.new)) %>% 
    arrange(desc(meancost)) %>% data.table %>% `[`(1:10) %>%
    merge(icd.names[,.(Code, Desc1)], by.x = "ER.Diagnosis.Code", by.y = "Code",
          all.x = T, all.y = F, sort = F)  %>% 
    mutate("Prop(%)" = round(N/uni.enc * 100, 1)) %>%
    select(Admit.Diag.Code = ER.Diagnosis.Code, Admit.Diag.Name = Desc1, N, `Prop(%)`)
  cost.by.adm <- NULL
  for(i in 1:10){
    cost.adm <- dad.comorb[EncID.new%in%
                      er.diag[ER.Diagnosis.Type=="M"&
                                ER.Diagnosis.Code==adm.rank10$Admit.Diag.Code[i], 
                              EncID.new], 
                    Cost]
    cost.by.adm <- rbind(cost.by.adm, sum.cost(cost.adm))
  }
  adm.rank10 <- cbind(adm.rank10, cost.by.adm)
  
  dis.rank10 <- ddply(dad.comorb, ~Diagnosis.Code, summarize,
                      meancost = mean(Cost),
                      N = length(EncID.new)) %>% 
    arrange(desc(meancost)) %>% data.table %>% `[`(1:10) %>%
    merge(icd.names[,.(Code, Desc1)], by.x = "Diagnosis.Code", by.y = "Code",
          all.x = T, all.y = F, sort = F)  %>% 
    mutate("Prop(%)" = round(N/uni.enc * 100, 1)) %>%
    select(Discharge.Diag.Code = Diagnosis.Code, Discharge.Diag.Name = Desc1, N, `Prop(%)`)
  cost.by.dis <- NULL
  for(i in 1:10){
    cost.dis <- dad.comorb[EncID.new%in%
                      ip.diag[Diagnosis.Type=="M"&
                                Diagnosis.Code==dis.rank10$Discharge.Diag.Code[i],
                              EncID.new], 
                    Cost]
    cost.by.dis <- rbind(cost.by.dis, sum.cost(cost.dis))
  }
  dis.rank10 <- cbind(dis.rank10, cost.by.dis)
  diag.cost <- dad[EncID.new%in%all.enc, Cost]
  cost.out <- paste(sum.cost(diag.cost), collapse = "\n")
  return(data.table(comorb = c(paste(diag.name, "\n N = ", uni.enc, sep = ""), 
                               rep("", 9)),
                    cost = c(cost.out, rep("", 9)), 
                    cbind(adm.rank10, dis.rank10)))
}
dad
res <- NULL
for(i in 1:4){
  res <- rbind(res, find.cost.by.cost(ip.diag, er.diag, 
                                      diag.tobefind[[i]], names(diag.tobefind)[i]))
}
fwrite(res, "H:/GEMINI/Results/Diabetes/table1.by.cost.csv")


# --------------------------- table 2 ------------------------------------------



conditions <- c(dka = c("E111"), 
              coma = c("E110"),
              diab.wounds = c("E115"),
              kidney.failure = c("E112"),
              hf.code = c("I50"),
              stroke.code = c("I63"))
for(i in 1:6){
  print(conditions[[i]]%in%ip.diag$Diagnosis.Code)}
ip.diag[startwith.any(Diagnosis.Code, c("E09", "E08", "E11", "E1101", "E1300", 
                                        "E1301"))]
ip.diag[startwith.any(Diagnosis.Code, c("E1162"))]


dm2.enc <- unique(c(ip.diag[startsWith(Diagnosis.Code, "E11"), EncID.new],
                    er.diag[startsWith(ER.Diagnosis.Code, "E11"), EncID.new]))
ip.diag.dm2 <- ip.diag[EncID.new%in%dm2.enc]
er.diag.dm2 <- er.diag[EncID.new%in%dm2.enc]

find.cost2 <- function(condition, condition.name){
  with.con <- c(ip.diag.dm2[startwith.any(Diagnosis.Code, condition)&
                              Diagnosis.Type=="M", EncID.new], 
                er.diag.dm2[startwith.any(ER.Diagnosis.Code, condition)&
                              ER.Diagnosis.Type=="M", EncID.new])
  cost <- dad[EncID.new%in%with.con, Cost]
  data.table("Condition" = condition.name, 
             "N" = length(cost),
             "mean (sd)" = paste(round(mean(cost), 1), " (",
                                 round(sd(cost), 1), ")", sep = ""),
             "median" = round(median(cost), 1),
             "range" = paste(" (", round(min(cost), 1) , ", ",
                             round(max(cost), 1), ")", sep = ""))
}
i = 6
find.cost2(conditions[i], names(conditions)[i])
diabetes.tab2 <- function(){
  tab2 <- NULL
  for(i in 1:6){
    tab2 <- rbind(tab2, find.cost2(conditions[i], names(conditions)[i]))
  }
  tab2
}

res2 <- diabetes.tab2()
fwrite(res2, "H:/GEMINI/Results/Diabetes/table2.csv")




# ----------------------- Feb 24 2017-------------------------------------------
# ----------------------- new tables -------------------------------------------
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
# icd10 <- fread("R:/GEMINI/Coding/CIHI/ICD10-2015-PARSED-CATEGORIES.txt",header = F)
# icd10 <- rbind(icd10, data.table(Code = c("A17", "A90", "A91", 
#                                                       "B24", "B59", "I84"),
#                                  Diagnosis = c("Tuberculosis of nervous system",
#                                                  "Dengue fever (classical dengue)",
#                                                  "Dengue haemorrhagic fever",
#                                                  "Unspecified human HIV disease",
#                                                  "Pneumocystosis",
#                                                  "Haemorrhoids")))
# fwrite(icd10, "H:/GEMINI/Coding/icd10_3digit.csv")
icd10 <- fread("H:/GEMINI/Coding/icd10_3digit.csv")
names(icd10) <- c("Code", "Diagnosis")
mrd <- merge(mrd, icd10, by.x = "Diagnosis.Code", by.y = "Code",
                  all.x = T, all.y = F)

dad <- merge(dad, mrd[,.(Diagnosis, EncID.new)], by = "EncID.new")
dad <- dad[!is.na(Cost)]

tab1 <- function(df, nr){
  rbind(df[, .(
    Diagnosis = "all",
    "N(%)" = paste(length(EncID.new), " (",
                   round(length(EncID.new)/nr*100, 1), ")", sep = ""),
    cost.mean = round(mean(Cost), 1), 
    cost.sd = round(sd(Cost), 1),
    cost.median = round(median(Cost), 1),
    cost.range = paste("(", round(min(Cost), 1),",",
                       round(max(Cost), 1), ")", sep = ""),
    cost.total = sum(Cost),
    length.of.stay.mean = round(mean(LoS), 1),
    length.of.stay.sd = round(sd(LoS), 1),
    length.of.stay.median = round(median(LoS), 1),
    "mortality(N(%))" = paste(sum(Discharge.Disposition=="7"), " (",
                              round(sum(Discharge.Disposition=="7")/length(EncID.new)*100, 1),
                              ")", sep = ""),
    "ICU(N(%))" = paste(sum(SCU.adm), " (",
                        round(sum(SCU.adm)/length(EncID.new)*100, 1),
                        ")", sep = ""),
    ALC.days.mean = round(mean(Number.of.ALC.Days, na.rm = T), 1), 
    ALC.days.sd = round(sd(Number.of.ALC.Days, na.rm = T), 1),
    ALC.days.median = round(median(Number.of.ALC.Days, na.rm = T), 1)
  )],
  ddply(df, ~Diagnosis, summarize,
        "N(%)" = paste(length(EncID.new), " (",
                       round(length(EncID.new)/nr*100, 1), ")", sep = ""),
        cost.mean = round(mean(Cost), 1), 
        cost.sd = round(sd(Cost), 1),
        cost.median = round(median(Cost), 1),
        cost.range = paste("(", round(min(Cost), 1),",",
                           round(max(Cost), 1), ")", sep = ""),
        cost.total = sum(Cost),
        length.of.stay.mean = round(mean(LoS), 1),
        length.of.stay.sd = round(sd(LoS), 1),
        length.of.stay.median = round(median(LoS), 1),
        "mortality(N(%))" = paste(sum(Discharge.Disposition=="7"), " (",
                                  round(sum(Discharge.Disposition=="7")/length(EncID.new)*100, 1),
                                  ")", sep = ""),
        "ICU(N(%))" = paste(sum(SCU.adm), " (",
                            round(sum(SCU.adm)/length(EncID.new)*100, 1),
                            ")", sep = ""),
        ALC.days.mean = round(mean(Number.of.ALC.Days, na.rm = T), 1), 
        ALC.days.sd = round(sd(Number.of.ALC.Days, na.rm = T), 1),
        ALC.days.median = round(median(Number.of.ALC.Days, na.rm = T), 1)
        ))
}
nr = 138432
all <- tab1(dad, 138432) 
fwrite(all, "H:/GEMINI/Results/Diabetes/table1.new.all.csv")

dad.diabetic <- dad[diabetic==T]
nr = 38515
dia <- tab1(dad.diabetic, 38515)
fwrite(dia, "H:/GEMINI/Results/Diabetes/table1.new.diabetic.csv")
  
  
dad.non.diabetic <- dad[diabetic==F]
nr = 99917
non.dia <- tab1(dad.non.diabetic, 99917)
fwrite(non.dia, "H:/GEMINI/Results/Diabetes/table1.new.non.diabetic.csv")
