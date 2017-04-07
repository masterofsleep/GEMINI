# ------------------------- final plots ----------------------------------------
library(gemini)
lib.pa()

cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv")
all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]
cohort[Institution.Number=="sbk", Institution.Number:="SHSC"]
cohort[Institution.Number=="msh", Institution.Number:="SHS"]
cohort[, Institution.Number := toupper(Institution.Number)]

cap.co <- cohort[cap==T]
cap.co[Institution.Number%in%c("THP-M", "THP-C"),
       ':='(Institution.Number = "THP")]
cap.co[, physician := paste(Institution.Number, mrp.code.new, sep = "-")]
plot.phy <- function(data, leastpatient, title, xlab = "physician", 
                     ylab, nextreme = 1,
                     ave.fun, xstart = -2){
  df <- ddply(data, ~physician, .fun = ave.fun) %>% data.table
  names(df)[4] <- "phy.ave"
  df <- df[N>=leastpatient]
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
               ymi = quantile(phy.ave, probs = 0.1),
               yma = quantile(phy.ave, probs = 0.9),
               yav = quantile(phy.ave, probs = 0.1)*0.5 + quantile(phy.ave, probs = 0.9)*0.5,
               ydiff = sprintf("%.1f", yma - ymi))
  ave.shift <- max(df$phy.ave) * 0.02
  p <- p + geom_errorbar(data = del, aes(x = xstart/2, y = NULL,ymin = ymi, ymax = yma), 
                         alpha = 0.3, width = 2) + 
    # plot the 10% - 90% range
    geom_rect(data = del, aes(x = NULL, y = NULL, xmin = xstart/2 - 1, xmax =  xstart/2 +1, 
                              ymin = yav-0.5*ave.shift, ymax = yav+0.5*ave.shift), fill = "#EEEEEE") + 
    geom_text(data = del, aes(x = xstart/2, y = yav, label = ydiff), size = 3) +
    # plot the label for average
    geom_text(data = del, aes(x = xm-2, y = site.ave + ave.shift, 
                              label = sprintf("%.1f", site.ave)),
              size = 3) 
  print(p)
}

# ---------------------- average length-of-stay --------------------------------
ave.los <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$LOS.without.ALC, na.rm = T))
}
png("C:/Users/guoyi/Desktop/to.adm/figures/ave.los_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, 100, "Overall", ylab = "Average Length-of-Stay (Days)", ave.fun = ave.los)
dev.off()
png("C:/Users/guoyi/Desktop/to.adm/figures/ave.los_cap.png", res = 250, width = 2000, height = 1200)
plot.phy(cap.co, 20, "Pneumonia", ylab = "Average Length-of-Stay (Days)", ave.fun = ave.los)
dev.off()

# ---------------------------average alc days ----------------------------------
ave.alc <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Number.of.ALC.Days, na.rm = T))
}
png("C:/Users/guoyi/Desktop/to.adm/figures/ave.alc_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, 100, "Overall", ylab = "Average ALC Days", ave.fun = ave.alc)
dev.off()
png("C:/Users/guoyi/Desktop/to.adm/figures/ave.alc_cap.png", res = 250, width = 2000, height = 1200)
plot.phy(cap.co, 20, "Pneumonia", ylab = "Average ALC Days", ave.fun = ave.alc)
dev.off()
# ---------------------------- average cost -------------------------------------


ave.cost <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Cost, na.rm = T))
}
png("C:/Users/guoyi/Desktop/to.adm/figures/ave.cost_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, 100, "Overall", ylab = "Average Cost ($)", ave.fun = ave.cost, xstart = -5)
dev.off()
png("C:/Users/guoyi/Desktop/to.adm/figures/ave.cost_cap.png", res = 250, width = 2000, height = 1200)
plot.phy(cap.co, 20, "Pneumonia", ylab = "Average Cost ($)", ave.fun = ave.cost, xstart = -5)
dev.off()

# ---------------------------- adjusted cost -----------------------------------
adj.cost <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$adj.cost, na.rm = T))
}
png("C:/Users/guoyi/Desktop/to.adm/figures/ave.adj.cost_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, 100, "Overall", ylab = "Adjusted Cost ($)", ave.fun = adj.cost, xstart = -5)
dev.off()
png("C:/Users/guoyi/Desktop/to.adm/figures/ave.adj.cost_cap.png", res = 250, width = 2000, height = 1200)
plot.phy(cap.co, 20, "Pneumonia", ylab = "Adjusted Cost ($)", ave.fun = adj.cost, xstart = -5)
dev.off()
# ----------------------------- readmission rate -------------------------------
read.rate <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = sum(x$read.in.30, na.rm = T)/sum(!is.na(x$read.in.30), na.rm = T)*100)
}
png("C:/Users/guoyi/Desktop/to.adm/figures/re.admission.rate_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, 100, "Overall", ylab = "Re-admission (within 30 days) Rate (%)", ave.fun = read.rate)
dev.off()
png("C:/Users/guoyi/Desktop/to.adm/figures/re.admission.rate_cap.png", res = 250, width = 2000, height = 1200)
plot.phy(cap.co, 20, "Pneumonia", ylab = "Re-admission (within 30 days) Rate (%)", ave.fun = read.rate)
dev.off()


# ----------------------------- mortality rate ---------------------------------
mort <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Discharge.Disposition ==7, na.rm = T)*100)
}
png("C:/Users/guoyi/Desktop/to.adm/figures/inhospital.mortality_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, 100, "Overall", ylab = "In-hospital Mortality (%)", ave.fun = mort)
dev.off()
png("C:/Users/guoyi/Desktop/to.adm/figures/inhospital.mortality_cap.png", res = 250, width = 2000, height = 1200)
plot.phy(cap.co, 20, "Pneumonia", ylab = "In-hospital Mortality (%)", ave.fun = mort)
dev.off()

# ------------------------------ short admission rate --------------------------
shortadm <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$LOS.without.ALC < 2, na.rm = T)*100)
}
png("C:/Users/guoyi/Desktop/to.adm/figures/short.adm.rate_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, 100, "Overall", ylab = "Short-Admission (<48h) Rate (%)", ave.fun = shortadm, xstart = -4)
dev.off()
png("C:/Users/guoyi/Desktop/to.adm/figures/short.adm.rate_cap.png", res = 250, width = 2000, height = 1200)
plot.phy(cap.co, 20, "Pneumonia", ylab = "Short-Admission (<48h) Rate (%)", ave.fun = shortadm, xstart = -3)
dev.off()

# ----------------------------- Rate of ICU admission --------------------------
icuadm<- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$SCU.adm, na.rm = T)*100)
}
png("C:/Users/guoyi/Desktop/to.adm/figures/ICU.uti_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort, 100, "Overall", ylab = "ICU Utilization Rate(%)", ave.fun = icuadm, xstart = -4)
dev.off()
png("C:/Users/guoyi/Desktop/to.adm/figures/ICU.uti_cap.png", res = 250, width = 2000, height = 1200)
plot.phy(cap.co, 20, "Pneumonia", ylab = "ICU Utilization Rate(%)", ave.fun = icuadm)
dev.off()
