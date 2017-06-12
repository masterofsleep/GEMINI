# ----------------- test strick vs large cohort --------------------------------
library(gemini)
lib.pa()

find_cohort <- function(x){
  phy <- readg(gim, all.phy)
  dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")
  cohort <- phy[#(adm.code.new==dis.code.new&adm.code.new==mrp.code.new&
                   adm.code!=0&adm.GIM!="n"]
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
  rbc.trans.80 <- rbc.trans[with.pre.hgb==T&pre.hgb>80]
  n.with.pre.trans.80 <- rbc.trans.80[,.N, by = EncID.new]
  n.with.pre.trans.80$EncID.new <- as.character(n.with.pre.trans.80$EncID.new)
  names(n.with.pre.trans.80)[2] <- "N.pre.tran.hgb.gt80"
  cohort <- merge(cohort, n.with.pre.trans.80, by = "EncID.new",
                  all.x = T, all.y = F)
  cohort[is.na(N.pre.tran.hgb.gt80), N.pre.tran.hgb.gt80 := 0]
  
  # AKI 
  inc <- fread("C:/Users/guoyi/Desktop/to.adm/kdigo.csv")
  cohort$aki <- cohort$EncID.new%in%inc[KDIGO%in%c("2", "3"), EncID.new]
  return(cohort)
}

cohort.full <- find_cohort()
cohort.strick <- cohort[(adm.code.new==dis.code.new&adm.code.new==mrp.code.new)]
table(cohort.full$mrp.GIM)
