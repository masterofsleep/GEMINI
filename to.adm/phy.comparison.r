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



for(i in unique(phy.sum$site)){
  phy.sum[site==i, ':='(ave.los = rank(ave.los.value, ties.method = "min"),
                        re.adm.rate = rank(re.adm.rate.value, ties.method = "min"),
                        mortality = rank(mortality.value, ties.method = "min"))]
}


library(reshape2)
phy.sum.long <- melt(phy.sum, id.vars = c("physician", "site", "ave.los"), 
                     measure.vars = c("ave.los", "mortality", "re.adm.rate" ))
setwd("C:/Users/guoyi/Desktop/to.adm/phy.comparison")
png("physician_comparison.png", res = 200, width = 1600, height = 1000)
ggplot(phy.sum.long, aes(variable, value, group = physician, color = factor(ave.los))) + 
  geom_point(size = 0.1) +
  geom_line(size = 1.5, alpha = 0.5) +
  facet_wrap(~site, nrow = 2) + ylab("Rank") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none")
dev.off()
getwd()

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
i = "smh"
i = "sbk"
i = "msh"
i = "thp-c"
i = "thp-m"
i = "uhn-western"
i = "uhn-general"
