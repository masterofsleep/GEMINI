# --------------------------- test balance -------------------------------------
library(gemini)
lib.pa()

cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv", colClasses = list(character = "EncID.new"))
all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]
n.pat <- cohort[,.N, by = physician]
cohort.100p <- cohort[physician%in%n.pat[N>=100, physician]]
cohort.100p <- cohort.100p[LOS.without.ALC<=30]
library(lme4)
mod1 <- lmer(Age ~  (1|physician) + (1|Institution.Number) , cohort.100p)
check <- data.frame(summary(mod1)$varcor)
icc <- check$vcov[1] / sum(check$vcov) ; icc

i = "uhn-western"


site <- NULL
icc <- NULL
for(i in unique(cohort.100p$Institution.Number)){
  mod <- lmer(Age ~  (1|physician) , cohort.100p[Institution.Number==i])
  check <- data.frame(summary(mod)$varcor)
  icc_i <- check$vcov[1] / sum(check$vcov)
  site <- c(site, i)
  icc <- c(icc, icc_i)
}
icc.age <- data.frame(site, icc, var = "Age")

site <- NULL
icc <- NULL
for(i in unique(cohort.100p$Institution.Number)){
  mod <- glmer(factor(Gender) ~  (1|physician) , cohort.100p[Institution.Number==i], family = binomial)
  icc_i <- as.numeric(sjstats::icc(mod))
  site <- c(site, i)
  icc <- c(icc, icc_i)
}
icc.gender <- data.frame(site, icc, var = "Gender")


# charlson
site <- NULL
icc <- NULL
for(i in unique(cohort.100p$Institution.Number)){
  mod <- lmer(Charlson.Comorbidity.Index ~  (1|physician) , cohort.100p[Institution.Number==i])
  check <- data.frame(summary(mod)$varcor)
  icc_i <- check$vcov[1] / sum(check$vcov)
  site <- c(site, i)
  icc <- c(icc, icc_i)
}
icc.cci <- data.frame(site, icc, var = "Charlson Comorbidity Index")


icc <- rbind(icc.age, icc.gender, icc.cci)
fwrite(icc, "H:/GEMINI/Results/to.administrator/balance.icc.csv")


ggplot(icc, aes(x = var, y = (1-icc)*100, fill = site)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.3) +
  #facet_wrap(~site, nrow = 1) + 
  geom_hline(aes(yintercept = 100), linetype = 2) + 
  theme(axis.title.x = element_blank()) + ylab("")
       