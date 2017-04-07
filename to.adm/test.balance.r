# --------------------------- test balance -------------------------------------
library(gemini)
lib.pa()

cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv", colClasses = list(character = "EncID.new"))
all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]
n.pat <- cohort[,.N, by = physician]
cohort.100p <- cohort[physician%in%n.pat[N>=100, physician]]

library(lme4)
mod1 <- lmer(Age ~ 1 + (physician|Institution.Number) , cohort.100p)
summary(mod1)

mod1 <- lm(Age ~ physician + Institution.Number, cohort.100p)
summary(mod1)
library(lme4)
cohort.100p$Gender <- factor(cohort.100p$Gender)
mod1 <- glmer(Gender ~ (physician|Institution.Number), data = cohort.100p, family = binomial)
summary(mod1)