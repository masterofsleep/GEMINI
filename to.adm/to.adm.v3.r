# --------------------------- To adm v3 ----------------------------------------
library(gemini)
lib.pa()
phy <- readg(gim, all.phy)
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")
cohort <- phy[(adm.code.new==dis.code.new&adm.code.new==mrp.code.new)]
cohort[,':='(mrp.code = NULL,
             adm.code = NULL,
             dis.code = NULL)]
table(cohort$GIM)
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



phy.sum <- ddply(cohort, ~physician, function(x)
      data.frame(N = nrow(x),
                 code.new = x$mrp.code.new[1],
                 GIM = x$GIM[1],
                 site = x$Institution.Number[1],
                 n.patient = nrow(x)/length(unique(x$physician)),
                 ave.acute.los = mean(x$LOS.without.ALC, na.rm = T),
                 ave.alc = mean(x$Number.of.ALC.Days, na.rm = T),
                 read.rate = sum(x$read.in.30, na.rm = T)/sum(!is.na(x$read.in.30), na.rm = T)*100,
                 mortality = mean(x$Discharge.Disposition ==7, na.rm = T)*100,
                 short.adm = mean(x$LOS.without.ALC < 2, na.rm = T)*100,
                 icu.rate = mean(x$SCU.adm, na.rm = T)*100,
                 cbc.per.patientday = sum(x$n.bloodtest)/sum(x$LOS.without.ALC),
                 trans.with.prehgb80.per1000patient = sum(x$N.pre.tran.hgb.gt80)/nrow(x)*1000,
                 aki.rate = sum(x$aki, na.rm = T)/nrow(x)*100
                 ))
all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.csv")
phy.sum <- merge(phy.sum, all.name[!duplicated(code.new), .(code.new, first.name, last.name)], by.x = "code.new",
                 by.y = "code.new", all.x = T, all.y = F)
table(phy.sum$GIM)
phy.sum$phy.name <- paste(phy.sum$first.name, phy.sum$last.name)
fwrite(phy.sum, "C:/Users/guoyi/Desktop/to.adm/phy.summary.csv")

plot.phy <- function(data, title, xlab = "Physician", 
                     ylab, nextreme = 1,
                     ave.fun, xstart = -2){
  df <- ddply(data, ~physician, .fun = ave.fun) %>% data.table
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
               site.ave = sum(phy.ave * N)/sum(N),
               xm = max(phy),
               ymi = quantile(phy.ave, probs = 0.25),
               yma = quantile(phy.ave, probs = 0.75),
               yav = quantile(phy.ave, probs = 0.5),
               ydiff = sprintf("%.1f", yma - ymi))
  ave.shift <- max(df$phy.ave) * 0.02
  p <- p + geom_errorbar(data = del, aes(x = xstart/2, y = NULL,ymin = ymi, ymax = yma), 
                         alpha = 0.3, width = 2) + 
    # plot the 25% - 75% range
    geom_rect(data = del, aes(x = NULL, y = NULL, xmin = xstart/2 - 1, xmax =  xstart/2 +1, 
                              ymin = yav-0.5*ave.shift, ymax = yav+0.5*ave.shift), fill = "#EEEEEE") + 
    geom_text(data = del, aes(x = xstart/2, y = yav, label = ydiff), size = 3) +
    # plot the label for average
    geom_text(data = del, aes(x = xm-2, y = site.ave + ave.shift, 
                              label = sprintf("%.1f", site.ave)),
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
plot.phy(cohort,  "Number of Patients", ylab = "Number of Patients", ave.fun = N.patient, xstart = -4)
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