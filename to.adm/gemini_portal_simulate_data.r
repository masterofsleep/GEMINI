# ----------------- Simulated data for GEMINI Portal demo ----------------------
library(gemini)
lib.pa()
simu_phy_sum <- data.frame(
  physician = letters[1:10],
  N = sample(c(100:1500), 10),
  ave.los = seq(3.5, 5.5, 0.201) + rnorm(10)/10,
  read.rate = seq(10, 16, 0.601) + rnorm(10))

simu_phy_sum$color <- 0
simu_phy_sum$color[4] <- 1


demo_plot <- function(xstart, fondsize)
setwd("C:/Users/guoyi/Desktop/to.adm/portal_demo")
png("los.png", res = 150, width = 1000, height = 600)
ggplot(simu_phy_sum, aes(physician[order(ave.los, decreasing = T)],
                         ave.los,
                         fill = factor(color))) + 
  geom_bar(stat = "identity", width = 0.5) +
  expand_limits(x = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none") +
  xlab("Physician") + 
  ylab("Average Acute Length-of-Stay") +
  geom_hline(aes(yintercept = 4.3), alpha = 0.5,
             linetype = 2, size = 1) 
  # geom_errorbar(aes(x = 0.4, y = NULL,ymin = 3.8, ymax = 4.86), 
  #                 alpha = 0.3, width = 0.5) +
  # # plot the 25% - 75% range
  # geom_rect(aes(x = NULL, y = NULL, xmin = 0, xmax =  0.5, 
  #                           ymin = 4.1, ymax = 4.5), fill = "#FFFFFF") + 
  # geom_text(aes(x = 0.4, y = 4.3, label = "0.4"), size = 3) +
  # # plot the label for average
  # geom_text(aes(x = 10, y = 4.5,label = "4.3"),size = 3) 
  dev.off()
  
  
patient_los <- rexp(simu_phy_sum$N[4], 1/simu_phy_sum$ave.los[4])
patient_los <- data.frame(patient_id = 1:length(patient_los),
                          los = patient_los)
png("my_los.png", res = 150, width = 1000, height = 600)
ggplot(patient_los, aes(los)) + 
  geom_histogram(binwidth = 0.5, fill = "#5DD0D8", color = "#555555")  + 
  theme_bw() +
  xlab("Acute Length-of-Stay") + 
  ylab("Count")
dev.off()


# --------------------- new figures, with real data ----------------------------
coh.smh <- cohort[Institution.Number=="SMH"]
ave.los <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Acute.LoS, na.rm = T))
}
library(extrafont)
loadfonts()
plot_los <- function(dat, period){
  time_limit <- c("1q" = "2015-01-01",
                  "2q" = "2014-09-30",
                  "3q" = "2014-07-01",
                  "1y" = "2014-04-01",
                  "all" = "2010-04-01")
  cohort <- dat[ymd(Discharge.Date)>=ymd(time_limit[period])]
  site.ave <- mean(cohort$Acute.LoS)
  phy.sum <- ddply(cohort, ~physician, ave.los) %>% data.table
  phy.sum$col <- ifelse(phy.sum$physician=="SMH-278", "Me", "Others")
  phy.sum[, phy := as.numeric(factor(physician, levels = physician[order(ave, decreasing = T)]))]
  ggplot(phy.sum, aes(phy,ave)) + 
    geom_bar(aes(fill = factor(col)), stat = "identity", width = 0.5) +
    expand_limits(x = 0, y = c(0, 5.5)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none",
          text = element_text(family = "Trebuchet MS")) +
    xlab("Physician \n(N Patient)") + 
    ylab("Average Acute Length-of-Stay") +
    geom_hline(aes(yintercept = site.ave), alpha = 0.5,
               linetype = 2, size = 1) +
    geom_text(aes(y = -0.1, label = N), size = 3, check_overlap = T) + 
    annotate("text", x = 0, y = site.ave+ 0.15, label = sprintf("%.1f", site.ave),
             size = 3) +
    geom_label(aes(y = ave + 0.15, label = sprintf("%.1f", ave)), size = 3,
               color = NA, label.padding = unit(0.1, "lines"))  +
    geom_text(aes(y = ave + 0.15, label = sprintf("%.1f", ave)), size = 3)
}
setwd("C:/Users/guoyi/Desktop/to.adm/portal_demo")
png("1q.png", res = 130, width = 1000, height = 600)
plot_los(coh.smh, "1q")
dev.off()
png("2q.png", res = 130, width = 1000, height = 600)
plot_los(coh.smh, "2q")
dev.off()
png("3q.png", res = 130, width = 1000, height = 600)
plot_los(coh.smh, "3q")
dev.off()
png("1y.png", res = 130, width = 1000, height = 600)
plot_los(coh.smh, "1y")
dev.off()
png("all.png", res = 130, width = 1000, height = 600)
plot_los(coh.smh, "all")
dev.off()




coh.smh[copd==T, mrd := "COPD"]
coh.smh[cap==T, mrd := "Pneumonia"]
coh.smh[stroke==T, mrd := "Stroke"]
coh.smh[chf==T, mrd := "Heart Failure"]
coh.smh[uti==T, mrd := "UTI"]
coh.smh[is.na(mrd), mrd:="Other"]

phy.sum.diag <- 
  ddply(coh.smh, ~physician + mrd, ave.los)
fwrite(phy.sum.diag, "C:/Users/guoyi/Desktop/to.adm/portal_demo/phy.sum.bydiag.csv")

phy.sum.diag.one <- merge(phy.sum.diag[phy.sum.diag$physician=="SMH-278", ],
                          ddply(coh.smh, ~mrd, function(x)
                            data.frame(site.ave = mean(x$Acute.LoS))),
                            by = "mrd")
phy.sum.diag.one$diff <- phy.sum.diag.one$ave - phy.sum.diag.one$site.ave
fwrite(phy.sum.diag.one, "C:/Users/guoyi/Desktop/to.adm/portal_demo/phy.sum.diag.one.csv")



phy.sum.diag.one <- rbind(phy.sum.diag[phy.sum.diag$physician=="SMH-278", c(2,3,4,5)],
                          ddply(coh.smh, ~mrd, ave.los))


plot_diag_los <- function(dat, period){
  time_limit <- c("1q" = "2015-01-01",
                  "2q" = "2014-09-30",
                  "3q" = "2014-07-01",
                  "1y" = "2014-04-01",
                  "all" = "2010-04-01")
  cohort <- dat[ymd(Discharge.Date)>=ymd(time_limit[period])]
  phy.sum.diag <- 
    ddply(cohort, ~physician + mrd, ave.los)
  phy.sum.diag.one <- rbind(phy.sum.diag[phy.sum.diag$physician=="SMH-278", c(2,3,4,5)],
                            ddply(cohort, ~mrd, ave.los))
  phy.sum.diag.one$phy <- factor(c(rep("Me", 6), rep("Hospital", 6)),
                                 levels = c("Me", "Hospital"))
  phy.sum.diag.one$mrd <- factor(phy.sum.diag.one$mrd,
                                 levels = c("Other", "UTI",
                                            "Stroke", "Pneumonia", 
                                            "Heart Failure", "COPD"))
  phy.sum.diag.one <- phy.sum.diag.one%>% arrange(desc(mrd))
  ggplot(phy.sum.diag.one, aes(mrd, ave, fill = phy, order = as.numeric(phy))) +
    geom_bar(stat = "identity", width = 0.5,position = "dodge") +
    theme_bw() +
    theme(legend.position = c(0.92, 0.1),
          axis.title.y = element_blank()) +
    coord_flip() +
    expand_limits(x = 0, y = c(0, 5.5)) +
    ylab("Average Acute Length-of-stay (Days)")+ 
    scale_fill_discrete(name = "") +
    geom_text(aes(y = ave + 0.1, label = sprintf("%.1f", ave)),
              position = position_dodge(width = 0.5), size = 3) +
    scale_fill_manual(guide = guide_legend(reverse = T))
}

setwd("C:/Users/guoyi/Desktop/to.adm/portal_demo")
png("diag.1q.png", res = 130, width = 1000, height = 600)
plot_diag_los(coh.smh, "1q")
dev.off()
png("diag.2q.png", res = 130, width = 1000, height = 600)
plot_diag_los(coh.smh, "2q")
dev.off()
png("diag.3q.png", res = 130, width = 1000, height = 600)
plot_diag_los(coh.smh, "3q")
dev.off()
png("diag.1y.png", res = 130, width = 1000, height = 600)
plot_diag_los(coh.smh, "1y")
dev.off()
png("diag.all.png", res = 130, width = 1000, height = 600)
plot_diag_los(coh.smh, "all")
dev.off()



pie_plot <- function(period){
  time_limit <- c("1q" = "2015-01-01",
                  "2q" = "2014-09-30",
                  "3q" = "2014-07-01",
                  "1y" = "2014-04-01",
                  "all" = "2010-04-01")
  cohort <- coh.smh[ymd(Discharge.Date)>=ymd(time_limit[period])]
  phy.sum.diag.one <- 
    ddply(cohort, ~physician + mrd, ave.los) %>% filter(physician=="SMH-278")
  phy.sum.diag.one$prop <- phy.sum.diag.one$N/sum(phy.sum.diag.one$N)
  phy.sum.diag.one$mrd <- factor(phy.sum.diag.one$mrd,
                                 levels = c("Other", "UTI",
                                            "Stroke", "Pneumonia", 
                                            "Heart Failure", "COPD"))
  phy.sum.diag.one <- phy.sum.diag.one%>% arrange(desc(mrd))
  
  
  ggplot(phy.sum.diag.one[1:6, ], aes(x = "", y = prop, fill = mrd)) + 
    geom_bar(stat = "identity",width = 1) +
    coord_polar("y", start=pi/4) +theme_bw() +
    geom_text(aes(x = 1.6, y = prop/3 + c(0, cumsum(prop)[-length(prop)]), 
                label = percent(prop), size=3)) +
    theme(axis.text.x=element_blank(),
          axis.title = element_blank(),
          title = element_blank(),
          panel.border = element_blank(), 
          legend.position = "none") +
    scale_fill_brewer(palette="Set3") 
}


setwd("C:/Users/guoyi/Desktop/to.adm/portal_demo")
png("pie.1q.png", res = 130, width = 1000, height = 600)
pie_plot("1q")
dev.off()
png("pie.2q.png", res = 130, width = 1000, height = 600)
pie_plot("2q")
dev.off()
png("pie.3q.png", res = 130, width = 1000, height = 600)
pie_plot("3q")
dev.off()
png("pie.1y.png", res = 130, width = 1000, height = 600)
pie_plot("1y")
dev.off()
png("pie.all.png", res = 130, width = 1000, height = 600)
pie_plot("all")
dev.off()




