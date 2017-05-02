# --------------------------- To adm v3 ----------------------------------------
library(gemini)
lib.pa()
# a function to generate random labels for sites
hide_site <- function(data){
  ninst <- length(unique(data$Institution.Number))
  newcode <- data.frame(Institution.Number = unique(data$Institution.Number), 
                   new.inst = sample(c("A", "B", "C", "D", "E", "F", "G")[1:ninst]))
  data <- merge(data, newcode, by = "Institution.Number")
  data$Institution.Number <- data$new.inst
  return(data)
}

hide_site(cohort)

find_cohort <- function(x){
  phy <- readg(gim, all.phy)
  dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")
  cohort <- phy[(adm.code.new==dis.code.new&adm.code.new==mrp.code.new)]
  cohort[,':='(mrp.code = NULL,
               adm.code = NULL,
               dis.code = NULL)]
                                                                                  # keep only y or not ??
  
  dad[Institution.Number=="M", Institution.Number:="THP-M"]
  dad[Institution.Number=="C", Institution.Number:="THP-C"]
  dad[Institution.Number=="54265", Institution.Number:="UHN-TG"]
  dad[Institution.Number=="54266", Institution.Number:="UHN-TW"]
  dad[Institution.Number=="sbk", Institution.Number:="SHSC"]
  dad[Institution.Number=="msh", Institution.Number:="SHS"]
  dad[Institution.Number=="smh", Institution.Number:="SMH"]
  

    
  
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
  cohort <- cohort[LOS.without.ALC<=30]
  
  # 69558 hospitalizations, if select physicians first
  cohort <- cohort[LOS.without.ALC<=30]
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
cohort <- find_cohort()

# phy.sum <- ddply(cohort, ~physician, function(x)
#       data.frame(N = nrow(x),
#                  code.new = x$mrp.code.new[1],
#                  GIM = x$GIM[1],
#                  site = x$Institution.Number[1],
#                  n.patient = nrow(x)/length(unique(x$physician)),
#                  ave.acute.los = mean(x$LOS.without.ALC, na.rm = T),
#                  ave.alc = mean(x$Number.of.ALC.Days, na.rm = T),
#                  read.rate = sum(x$read.in.30, na.rm = T)/sum(!is.na(x$read.in.30), na.rm = T)*100,
#                  mortality = mean(x$Discharge.Disposition ==7, na.rm = T)*100,
#                  short.adm = mean(x$LOS.without.ALC < 2, na.rm = T)*100,
#                  icu.rate = mean(x$SCU.adm, na.rm = T)*100,
#                  cbc.per.patientday = sum(x$n.bloodtest)/sum(x$LOS.without.ALC),
#                  trans.with.prehgb80.per1000patient = sum(x$N.pre.tran.hgb.gt80)/nrow(x)*1000,
#                  aki.rate = sum(x$aki, na.rm = T)/nrow(x)*100
#                  ))
# all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.csv")
# phy.sum <- merge(phy.sum, all.name[!duplicated(code.new), .(code.new, first.name, last.name)], by.x = "code.new",
#                  by.y = "code.new", all.x = T, all.y = F)
# table(phy.sum$GIM)
# phy.sum$phy.name <- paste(phy.sum$first.name, phy.sum$last.name)
# fwrite(phy.sum, "C:/Users/guoyi/Desktop/to.adm/phy.summary.csv")

plot.phy <- function(data, title, xlab = "Physician", 
                     ylab, nextreme = 1,
                     ave.fun, xstart = -2, digit = 1){
  data <- hide_site(data)
  df <- ddply(data, ~physician, .fun = ave.fun) %>% data.table
  digitform <- paste("%.", digit, "f", sep = "")
  names(df)[4] <- "phy.ave"
  site.mean <- ddply(data[physician%in%df$physician], ~Institution.Number, .fun = ave.fun)
  names(site.mean)[4] <- "site.mean"
  df <- merge(df, site.mean[,c(1,4)], by.x = "site", by.y = "Institution.Number")
  for(i in unique(df$site)){
    df[site ==i, phy := as.numeric(factor(physician, levels = physician[order(phy.ave, decreasing = T)]))]
  }
  p <- ggplot(df, aes(phy, phy.ave, fill = site)) + 
    geom_bar(stat = "identity", width = 0.5) + 
    geom_line(aes(x = phy, y = site.mean), alpha = 0.5,
              linetype = 2, size = 0.5) + 
    facet_grid(.~site, scales = "free_x") + 
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    expand_limits(x = xstart) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  del <- ddply(df, ~site, summarise,
               #site.ave = sum(phy.ave * N)/sum(N),
               site.ave = mean(phy.ave), # for patient sum only
               xm = max(phy),
               ymi = quantile(phy.ave, probs = 0.25),
               yma = quantile(phy.ave, probs = 0.75),
               yav = quantile(phy.ave, probs = 0.5),
               ydiff = sprintf(digitform , yma - ymi))
  ave.shift <- max(df$phy.ave) * 0.02
  p <- p + geom_errorbar(data = del, aes(x = xstart/2, y = NULL,ymin = ymi, ymax = yma), 
                         alpha = 0.3, width = 2) + 
    # plot the 25% - 75% range
    geom_rect(data = del, aes(x = NULL, y = NULL, xmin = xstart/2 - 1, xmax =  xstart/2 +1, 
                              ymin = yav-0.5*ave.shift, ymax = yav+0.5*ave.shift), fill = "#EEEEEE") + 
    geom_text(data = del, aes(x = xstart/2, y = yav, label = ydiff), size = 3) +
    # plot the label for average
    geom_text(data = del, aes(x = xm-2, y = site.ave + ave.shift, 
                              label = sprintf(digitform , site.ave)),
              size = 3) 
  print(p)
}

setwd("C:/Users/guoyi/Desktop/to.adm/figures.v3")
N.patient <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = nrow(x)/length(unique(x$physician)))
}
png("number.of.patient.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort,  "Number of Patients", ylab = "Number of Patients", 
         ave.fun = N.patient, xstart = -4, digit = 0)
dev.off()


# ---------------------- average length-of-stay --------------------------------
ave.los <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$LOS.without.ALC, na.rm = T))
}
png("ave.los_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort,  "Average Acute Length-of-Stay (Days)", ylab = "Average Length-of-Stay (Days)", ave.fun = ave.los)
dev.off()

# ---------------------------average alc days ----------------------------------
ave.alc <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Number.of.ALC.Days, na.rm = T))
}
png("ave.alc_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, "Average ALC Days", ylab = "Average ALC Days", ave.fun = ave.alc)
dev.off()

# ----------------------------- readmission rate -------------------------------
read.rate <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = sum(x$read.in.30, na.rm = T)/sum(!is.na(x$read.in.30), na.rm = T)*100)
}
png("re.admission.rate_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, "Re-admission (within 30 days) Rate (%)", ylab = "Re-admission (within 30 days) Rate (%)", ave.fun = read.rate)
dev.off()

# ----------------------------- mortality rate ---------------------------------
mort <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Discharge.Disposition ==7, na.rm = T)*100)
}
png("inhospital.mortality_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, "In-hospital Mortality (%)", ylab = "In-hospital Mortality (%)", ave.fun = mort)
dev.off()

# ------------------------------ short admission rate --------------------------
shortadm <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$LOS.without.ALC < 2, na.rm = T)*100)
}
png("short.adm.rate_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, "Short-Admission (<48h) Rate (%)", ylab = "Short-Admission (<48h) Rate (%)", ave.fun = shortadm, xstart = -4)
dev.off()

# ----------------------------- Rate of ICU admission --------------------------
icuadm<- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$SCU.adm, na.rm = T)*100)
}
png("ICU.uti_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort[str_sub(EncID.new,1,2)!="15"], "ICU Utilization Rate(%)", 
         ylab = "ICU Utilization Rate(%)", ave.fun = icuadm, xstart = -4)
dev.off()
# ----------------------------CBC and Electrolyte Tests-------------------------
ave.bloodtest <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = sum(x$n.bloodtest)/sum(x$LOS.without.ALC))
}
png("n.bloodtest_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort[str_sub(EncID.new, 1, 2)%in%c("11","12","13", "14")],  
         "Number of CBC and Electrolyte Tests \n per Patient Day", 
         ylab = "Number of CBC and Electrolyte Tests \n per Patient Day", ave.fun = ave.bloodtest)
dev.off()

# -------------------------Transfusion with pre hgb > 80 -----------------------
num.pre.trans.hgb80 <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = sum(x$N.pre.tran.hgb.gt80)/nrow(x)*1000)
}
png("number.of.rbc.trans.with.prehbg.gt80.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort[str_sub(EncID.new, 1, 2)%in%c("11","12","13", "14")], 
         "Number of RBC Transfusions \n with pre-Transfusion Hgb > 80 \n per 1000 Patient per Doctor", 
         ylab = "Number of RBC Transfusions \n with pre-Transfusion Hgb > 80 \n per 1000 Patient per Doctor", 
         ave.fun = num.pre.trans.hgb80, xstart = -3)
dev.off()

# ----------------------------------- AKI --------------------------------------
aki.rate <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = sum(x$aki, na.rm = T)/nrow(x)*100)
}
png("aki.rate.overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort[str_sub(EncID.new, 1, 2)%in%c("11","12","13", "14")],  
         "Proportion of Patients with Hospital-Acquired AKI (%)", 
         ylab = "Proportion of Patients with Hospital-Acquired AKI (%)", ave.fun = aki.rate)
dev.off()

