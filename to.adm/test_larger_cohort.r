# -------------------------- test wider cohort ---------------------------------
library(gemini)
lib.pa()


find_cohort <- function(x){
  phy <- readg(gim, all.phy)
  dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")
  #cohort <- phy[(adm.code.new==dis.code.new&adm.code.new==mrp.code.new&
  #                 adm.code!=0&adm.GIM!="n")]
  cohort <- phy[mrp.GIM!="n"]
  cohort[,':='(mrp.code = NULL,
               adm.code = NULL,
               dis.code = NULL)]
  setwd("H:/GEMINI/Results/to.administrator")
  copd <- fread("qbp.copd.csv")
  cap <- fread("qbp.cap.csv")
  uti <- fread("qbp.uti.csv")
  stroke <- fread("qbp.stroke.csv")
  chf <- fread("qbp.chf.csv")
  
  cohort[,':='(copd = EncID.new%in%copd$EncID.new,
               cap = EncID.new%in%cap$EncID.new,
               chf = EncID.new%in%chf$EncID.new,
               stroke = EncID.new%in%stroke$EncID.new,
               uti = EncID.new%in%uti$EncID.new)]
  cohort <- merge(cohort, dad, by = "EncID.new")
  cohort[, physician:=paste(Institution.Number, mrp.code.new, sep = "-")]
  #cohort <- cohort[!GIM%in%c("Geriatrics", "u", "family.md")]
  n.pat <- cohort[,.N, by = physician]
  cohort <- cohort[physician%in%n.pat[N>=100, physician]]
  cohort <- cohort[Acute.LoS<=30]
  
  # 69558 hospitalizations, if select physicians first
  cohort <- cohort[Acute.LoS<=30]
  n.pat <- cohort[,.N, by = physician]
  cohort <- cohort[physician%in%n.pat[N>=100, physician]]
  
  # add number of advanced imaging
  ctmrius <- fread("C:/Users/guoyi/Desktop/to.adm/n.ctmrius.csv")
  cohort$EncID.new <- as.character(cohort$EncID.new)
  ctmrius$EncID.new <- as.character(ctmrius$EncID.new)
  cohort <- merge(cohort, ctmrius, by = "EncID.new", all.x = T, all.y = F)
  cohort[is.na(N.rad), N.rad:= 0]
  
  # CBC and Electrolyte Test
  hgb <- readg(lab, hgb)
  sod <- readg(lab, sodium)
  hgb[is.na(as.numeric(Result.Value))]
  sod[is.na(as.numeric(Result.Value))]
  n.blood.test <- data.table(table(c(hgb$EncID.new, sod$EncID.new)))
  names(n.blood.test) <- c("EncID.new", "n.bloodtest")
  cohort$EncID.new <- as.character(cohort$EncID.new)
  cohort <- merge(cohort, n.blood.test, by = "EncID.new", all.x = T)
  cohort[is.na(n.bloodtest), n.bloodtest:=0]
  
  # Transfusion with pre hgb > 80
  rbc.trans <- fread("H:/GEMINI/Results/to.administrator/rbc.trans.with.pre.hgb.csv")
  rbc.trans.70 <- rbc.trans[with.pre.hgb==T&pre.hgb>70]
  n.with.pre.trans.70 <- rbc.trans.70[,.N, by = EncID.new]
  n.with.pre.trans.70$EncID.new <- as.character(n.with.pre.trans.70$EncID.new)
  names(n.with.pre.trans.70)[2] <- "N.pre.tran.hgb.gt70"
  cohort <- merge(cohort, n.with.pre.trans.70, by = "EncID.new",
                  all.x = T, all.y = F)
  cohort[is.na(N.pre.tran.hgb.gt70), N.pre.tran.hgb.gt70 := 0]
  
  # AKI 
  inc <- fread("C:/Users/guoyi/Desktop/to.adm/kdigo.csv")
  cohort$aki <- cohort$EncID.new%in%inc[KDIGO%in%c("2", "3"), EncID.new]
  
  # mark ICU before adm as non ICU admission
  icu.before.adm.enc <- fread("C:/Users/guoyi/Desktop/to.adm/icu.before.adm.enc.csv")
  cohort[EncID.new%in%icu.before.adm.enc$EncID.new, SCU.adm := F]
  
  # find mortality (excluding palliative in diagnoses)
  ip.diag <- readg(gim, ip_diag)
  er.diag <- readg(gim, er_diag)
  palli <- c(ip.diag[startwith.any(Diagnosis.Code, "Z515"), EncID.new],
             er.diag[startwith.any(ER.Diagnosis.Code, "Z515"), EncID.new])
  cohort$death <- cohort$Discharge.Disposition==7
  cohort[EncID.new%in%palli, death:= F]
  
  # weekday
  cohort$wkd <- wday(ymd(cohort$Admit.Date))
  cohort[, weekday := ifelse(wkd%in%c(2:6), 1, 0)]
  cohort[, wkd:=NULL]
  return(cohort)
}

big <- find_cohort()

find_small <- function(big){
  small <- big[adm.code.new==dis.code.new&adm.code.new==mrp.code.new]
  n.pat <- small[,.N, by = physician]
  small <- small[physician%in%n.pat[N>=100, physician]]
  return(small)
}

small <- find_small(big)

merge(big[, .N, by = Institution.Number],
      small[, .N, by = Institution.Number], by = "Institution.Number")

phy.sum <- function(data){
  ddply(data, ~physician, function(x)
    data.frame(N = nrow(x),
             code.new = x$mrp.code.new[1],
             GIM = x$mrp.GIM[1],
             site = x$Institution.Number[1],
             n.patient = nrow(x)/length(unique(x$physician)),
             ave.acute.los = mean(x$Acute.LoS, na.rm = T),
             ave.alc = mean(x$Number.of.ALC.Days, na.rm = T),
             alc.rate = mean(x$Number.of.ALC.Days > 0),
             read.rate = sum(x$read.in.30, na.rm = T)/sum(!is.na(x$read.in.30), na.rm = T)*100,
             mortality = mean(x$death==T, na.rm = T)*100,
             short.adm = mean(x$Acute.LoS < 2, na.rm = T)*100,
             icu.rate = mean(x$SCU.adm, na.rm = T)*100,
             cbc.per.patientday = sum(x$n.bloodtest)/sum(x$Acute.LoS),
             trans.with.prehgb70.per1000patient = sum(x$N.pre.tran.hgb.gt70)/nrow(x)*1000,
             aki.rate = sum(x$aki, na.rm = T)/nrow(x)*100,
             #weekday = mean(x$weekday, na.rm = T),
             #daytime = mean(x$daytime, na.rm = T),
             ave.cost = mean(x$Cost, na.rm = T)
  ))
}
  
  
  
big.summary <-phy.sum(big)
small.summary <- phy.sum(small)
code <- ""
for(i in names(small.summary)[2:17]){
  code <- paste(code, ", \n", 
                i, " = cor(", i, ".x, ", i, ".y)", sep = "")
}
cat(code)

comparison <- merge(big.summary, small.summary, by = c("physician", "site"))
ddply(comparison, ~ site, summarize, 
      `Number of Patients` = cor(n.patient.x, n.patient.y), 
      `Average Acute LoS` = cor(ave.acute.los.x, ave.acute.los.y), 
      `Average ALC Days` = cor(ave.alc.x, ave.alc.y), 
      `ALC Rate` = cor(alc.rate.x, alc.rate.y), 
      `Readmission Rate` = cor(read.rate.x, read.rate.y), 
      `Mortality` = cor(mortality.x, mortality.y), 
      `Short Admission Rate` = cor(short.adm.x, short.adm.y), 
      `ICU Utilization` = cor(icu.rate.x, icu.rate.y), 
      `CBC per Patient Day` = cor(cbc.per.patientday.x, cbc.per.patientday.y), 
      `Inappropriate RBC Transfusion` = cor(trans.with.prehgb70.per1000patient.x, trans.with.prehgb70.per1000patient.y), 
      `Hospital-Acquired AKI` = cor(aki.rate.x, aki.rate.y), 
      `Average Cost` = cor(ave.cost.x, ave.cost.y)) %>% 
  melt(id.vars = c("site")) -> big.vs.small.cor

png("C:/Users/guoyi/Desktop/to.adm/figures.v4/compare_cohort.png",
    res = 200, width = 2000, height = 1000)
ggplot(big.vs.small.cor, aes(x = site, y = variable, fill = value)) + 
  geom_tile(colour = "white") + 
  scale_fill_gradientn(colors = c("red", "white", "#12ba00"),
                       values = rescale(c(-1, 0, 1)),
                       limits = c(-1, 1)) +
  geom_text(aes(label = sprintf("%.3f", value)))
dev.off()





# further breakdown
breakdown <- function(data){
  ddply(data, ~ Institution.Number, summarize,
        mean.Age = mean(Age),
        Gender.F = mean(Gender=="F")*100,
        Number.of.Comorbidities = mean(Number.of.Comorbidity),
        Charlson = mean(Charlson.Comorbidity.Index),
        Hospitalization.on.weekend = mean(!weekday)*100,
        Acute.LoS = mean(Acute.LoS))
}

bd_big <- breakdown(big)
bd_big$Institution.Number <- paste(bd_big$Institution.Number, "-large", sep = "")


bd_small <- breakdown(small)
bd_small$Institution.Number <- paste(bd_small$Institution.Number, "-strick", sep = "")

bd <- rbind(bd_big, bd_small) %>% arrange(Institution.Number) %>% t %>% data.frame 
fwrite(bd, "C:/Users/guoyi/Desktop/to.adm/large_vs_small_breakdown.csv",
       row.names = T)
