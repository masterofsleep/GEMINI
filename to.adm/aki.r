# ---------------------------- AKI ---------------------------------------------
library(gemini)
lib.pa()
crea <- readg(lab, creatinine)
cohort  <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")
# cohort <- cohort[physician!="thp-m-708"]
# cohort[Institution.Number=="sbk", Institution.Number:="SHSC"]
# cohort[Institution.Number=="msh", Institution.Number:="SHS"]
# cohort[, Institution.Number := toupper(Institution.Number)]
# crea <- crea[EncID.new%in%cohort$EncID.new]
# cohort[Institution.Number=="SHS"&EncID.new%in%crea$EncID.new]
crea <- merge(crea, cohort[,.(EncID.new, Admit.Date, Admit.Time)], by = "EncID.new")
crea.in3d <- crea[ymd_hms(Collection.DtTm)<= ymd_hm(paste(Admit.Date, Admit.Time)) + days(3)&
                  ymd_hms(Collection.DtTm)>= ymd_hm(paste(Admit.Date, Admit.Time))]
crea.after3d <- crea[ymd_hms(Collection.DtTm)>= ymd_hm(paste(Admit.Date, Admit.Time)) + days(3)]
base <- ddply(crea.in3d, ~EncID.new, summarise,
              base.crea = min(Result.Value))


# tr
crea.max <- ddply(crea.after3d, ~EncID.new, summarise,
                  crea.max = max(Result.Value))

base <- merge(base, cohort[,.(EncID.new, Gender, Age)],by = "EncID.new")
base <- data.table(base)
base[, eGFR := 186 * (base.crea/88.4)^(-1.154) * Age^(-0.203) * ifelse(Gender=="F", 0.742, 1)]

#inc <- merge(base[eGFR >= 60], crea.max, by = "EncID.new", all.x = T, all.y = F)
inc <- merge(base[eGFR >= 45], crea.max, by = "EncID.new", all.x = T, all.y = F)


inc[, ':='(times = crea.max/base.crea,
           diff = crea.max - base.crea)]
inc[, KDIGO := ifelse(times>2.9|crea.max>=354, "3",
                      ifelse(times>=2&times<=2.9, "2", "1"))]

fwrite(inc, "C:/Users/guoyi/Desktop/to.adm/kdigo.new.csv")
table(inc$KDIGO)
1473/31006
cohort$aki <- cohort$EncID.new%in%inc[KDIGO%in%c("2", "3"), EncID.new]
table(cohort$aki)

aki.rate <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = sum(x$aki, na.rm = T)/nrow(x)*100)
}
png("C:/Users/guoyi/Desktop/to.adm/figures/aki.rate.overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort[str_sub(EncID.new, 1, 2)%in%c("11","12","13", "14")], 100, "Overall", 
         ylab = "Proportion of Patients with Hospital-Acquired AKI (%) per Doctor", ave.fun = aki.rate)
dev.off()
