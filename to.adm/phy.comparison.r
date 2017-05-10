# ------------------------ Physician comparison --------------------------------
library(gemini)
lib.pa()
cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv", colClasses = list(character = "EncID.new"))
all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]
n.pat <- cohort[,.N, by = physician]
cohort.100p <- cohort[physician%in%n.pat[N>=100, physician]]
cohort.100p <- cohort.100p[LOS.without.ALC<=30]
cor(cohort.100p$LOS.without.ALC, cohort.100p$adm)
fit <- glmer(read.in.30 ~ sqrt(LoS) + (1|Institution.Number), cohort.100p, family = binomial)
summary(fit)

ggplot(cohort.100p, aes(read.in.30, LOS.without.ALC)) + geom_boxplot() + facet_wrap(~Institution.Number)

phy.sum <- ddply(cohort.100p, ~physician, summarize,
                 n.patient = length(EncID.new),
                 site = Institution.Number[1],
                 ave.los.value = mean(LOS.without.ALC),
                 ave.cost.value = mean(Cost, na.rm = T),
                 re.adm.rate.value = mean(SCU.adm, na.rm = T)*100,
                 mortality.value = mean(Discharge.Disposition ==7, na.rm = T)*100) %>% data.table

fit <- lm(re.adm.rate.value ~ ave.los, data = phy.sum)
fit2 <- lm(re.adm.rate ~ ave.cost, data = phy.sum)
summary(fit)
qplot(ave.los, re.adm.rate.value, color = mortality.value, data = phy.sum)
qplot(ave.los, mortality.value, color = re.adm.rate.value, data = phy.sum) + facet_wrap(~site)


qplot(ave.cost.value, re.adm.rate.value, color = mortality.value, data = phy.sum) + facet_wrap(~site)

qplot(ave.cost, re.adm.rate, data = phy.sum, color = site)

# 2017-05-08 reproduce plot and update figure
phy.sum <- fread("C:/Users/guoyi/Desktop/to.adm/phy.summary.csv")


for(i in unique(phy.sum$site)){
  phy.sum[site==i, ':='(ave.los = rank(ave.acute.los, ties.method = "min"),
                        ave_cost = rank(ave.cost, ties.method = "min"),
                        re.adm.rate = rank(read.rate, ties.method = "min"),
                        aki = rank(aki.rate, ties.method = "min"))]
}


library(reshape2)
phy.sum.long <- melt(phy.sum[,.(physician, site, ave.los, re.adm.rate, aki, ave_cost)], 
                     id.vars = c("physician", "site", "ave.los"), 
                     measure.vars = c("ave.los", "ave_cost", "re.adm.rate", "aki"))
# setwd("C:/Users/guoyi/Desktop/to.adm/phy.comparison")
# png("physician_comparison.png", res = 200, width = 1600, height = 1000)
ggplot(phy.sum.long[site=="SMH"], aes(variable, value, group = physician, color = factor(ave.los))) +
  geom_point(size = 0.1) +
  geom_line(size = 1.5, alpha = 0.5) +
  facet_wrap(~site, nrow = 2) + ylab("Rank") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none")
# dev.off()
# getwd()

for(i in unique(phy.sum.long$site)){
  plot.name <- paste(i, ".png", sep = "")
  png(plot.name, res = 200, width = 1600, height = 1000)
  ggplot(phy.sum.long[site==i], aes(variable, value, group = physician, color = factor(ave.los))) + 
    geom_point(size = 0.1) +
    geom_line(size = 1.5, alpha = 0.5) +
    ylab("Rank") + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5)) + 
    ggtitle(toupper(i))
  dev.off()
}
i = "SMH"
i = "sbk"
i = "msh"
i = "thp-c"
i = "thp-m"
i = "uhn-western"
i = "uhn-general"


setwd("C:/Users/guoyi/Desktop/to.adm/figures.v3/phy_comparison")
for(i in unique(phy.sum.long$site)){
  plot.name <- paste(i, ".png", sep = "")
  png(plot.name, res = 200, width = 1600, height = 1000)
  p <- ggplot(phy.sum.long[site==i], aes(variable, value, group = physician, color = factor(ave.los))) + 
    geom_point(size = 0.1) +
    geom_line(size = 1.5, alpha = 0.5) +
    ylab("Rank") + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank()) + 
    scale_x_discrete(labels = c("ave.los" = "Average Acute\nLength-of-Stay", 
                                "ave_cost" = "Average Cost",
                                "re.adm.rate" = "Re-Admission Rate", 
                                "aki" = "Hospital-Aquired AKI")) +
    ggtitle(toupper(i)) 
  print(p)
dev.off()
}



# ----------------------------- check cor --------------------------------------
phy.sum <- fread("C:/Users/guoyi/Desktop/to.adm/phy.summary.csv")
cor.test(phy.sum$ave.acute.los, phy.sum$read.rate, method = "spearman", exact = FALSE)
cor(phy.sum$ave.acute.los, phy.sum$read.rate, method = "spearman")

ddply(phy.sum, ~site, function(x)
  cor(x$ave.acute.los, x$read.rate, 
           method = "spearman")
)
library(lme4)
adj_site <- function(x){
  fit <- lmer(x ~ (1|phy.sum$site))
  adj.x <- summary(fit)$coefficient[1] + summary(fit)$residuals
  adj.x
}

phy.sum[,':='(
  adj_ave.acute.los = adj_site(ave.acute.los),
  adj_read.rate = adj_site(read.rate)
)]
cor.test(adj_site(phy.sum$ave.acute.los), adj_site(phy.sum$read.rate),
         method = "pearson")


p1 <- qplot(ave.acute.los, read.rate,  color = site, data = phy.sum, size=I(2), geom = "point")
p2 <- qplot(adj_ave.acute.los, adj_read.rate, color = site, data = phy.sum, size=I(2), geom = "point")
library(cowplot)
plot_grid(p1, p2, labels = c("Unadjusted", "Adjusted by Site"))
