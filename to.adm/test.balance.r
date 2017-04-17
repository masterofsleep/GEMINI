# --------------------------- test balance -------------------------------------
library(gemini)
lib.pa()

cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv", colClasses = list(character = "EncID.new"))
all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]
n.pat <- cohort[,.N, by = physician]cohort[Institution.Number=="msh", Institution.Number:="A"]
cohort[Institution.Number=="msh", Institution.Number:="A"]
cohort[Institution.Number=="sbk", Institution.Number:="B"]
cohort[Institution.Number=="smh", Institution.Number:="C"]
cohort[Institution.Number=="thp-c", Institution.Number:="D"]
cohort[Institution.Number=="thp-m", Institution.Number:="E"]
cohort[Institution.Number=="uhn-general", Institution.Number:="F"]
cohort[Institution.Number=="uhn-western", Institution.Number:="G"]
cohort <- cohort[physician%in%n.pat[N>=100, physician]]
cohort <- cohort[LOS.without.ALC<=30]
library(lme4)
mod1 <- lmer(Age ~  (1|physician) + (1|Institution.Number) , cohort)
check <- data.frame(summary(mod1)$varcor)
icc <- check$vcov[1] / sum(check$vcov) ; icc

i = "uhn-western"

# Age
site <- NULL
icc <- NULL
for(i in unique(cohort$Institution.Number)){
  mod <- lmer(Age ~  (1|physician) , cohort[Institution.Number==i])
  check <- data.frame(summary(mod)$varcor)
  icc_i <- check$vcov[1] / sum(check$vcov)
  site <- c(site, i)
  icc <- c(icc, icc_i)
}
icc.age <- data.frame(site, icc, var = "Age")

# Gender
site <- NULL
icc <- NULL
for(i in unique(cohort$Institution.Number)){
  mod <- glmer(factor(Gender) ~  (1|physician) , cohort[Institution.Number==i], family = binomial)
  icc_i <- as.numeric(sjstats::icc(mod))
  site <- c(site, i)
  icc <- c(icc, icc_i)
}
icc.gender <- data.frame(site, icc, var = "Gender")


# charlson
site <- NULL
icc <- NULL
for(i in unique(cohort$Institution.Number)){
  mod <- lmer(Charlson.Comorbidity.Index ~  (1|physician) , cohort[Institution.Number==i])
  check <- data.frame(summary(mod)$varcor)
  icc_i <- check$vcov[1] / sum(check$vcov)
  site <- c(site, i)
  icc <- c(icc, icc_i)
}
icc.cci <- data.frame(site, icc, var = "Charlson Comorbidity Index")


icc <- rbind(icc.age, icc.gender, icc.cci)
fwrite(icc, "H:/GEMINI/Results/to.administrator/balance.icc.csv")


setwd("C:/Users/guoyi/Desktop/to.adm/to.gemini.investigators")
png("icc.png", res = 200, width = 1600, height = 1000)
ggplot(icc, aes(x = var, y = (1-icc)*100, fill = site)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.3) +
  geom_hline(aes(yintercept = 100), linetype = 2) + 
  theme(axis.title.x = element_blank()) + 
  ylab("% of Variation in Patient Characteristic \nUnrelated to Admitting Physician") +
  xlab("Baseline Characteristics of Patients at Time of Admission") + 
  geom_text(aes(x = 2, y = 102, label = "True Randomization"))
dev.off()       



# -------------------- test 5 most common diagnosis ----------------------------
# COPD
find.icc <- function(data, var, binary = T){
  site <- NULL
  icc <- NULL
  if(binary){
    fml <- paste("factor(", var, ") ~  (1|physician)")
  }else {
    fml <- paste(var, "~  (1|physician)")}
  for(i in unique(data$Institution.Number)){
    mod <- glmer(fml , cohort[Institution.Number==i], family = binomial)
    icc_i <- as.numeric(sjstats::icc(mod))
    site <- c(site, i)
    icc <- c(icc, icc_i)
  }
  data.frame(site, icc)
}


copd <- find.icc(cohort, "copd")
cap <- find.icc(cohort, "cap")
chf <- find.icc(cohort, "chf")
stroke <- find.icc(cohort, "stroke")

diag.icc <- rbind(cbind(copd, var = "COPD"),
                  cbind(cap, var = "Pneumonia"),
                  cbind(chf, var = "Heart Failure"),
                  cbind(stroke, var = "Stroke"))
diag.icc
setwd("C:/Users/guoyi/Desktop/to.adm/to.gemini.investigators")
png("icc.diag.png", res = 200, width = 1600, height = 1000)
ggplot(diag.icc, aes(x = var, y = (1-icc)*100, fill = site)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.3) +
  geom_hline(aes(yintercept = 100), linetype = 2) + 
  ylab("% of Variation in Patient Diagnosis \nUnrelated to Admitting Physician") +
  xlab("Diagnosis Defined by Quality-Based Procedures") + 
  geom_text(aes(x = 2.5, y = 102, label = "True Randomization"))
dev.off()
