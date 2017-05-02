# --------------------------- test balance -------------------------------------
library(gemini)
lib.pa()

cohort <- find_cohort()
library(lme4)
find.icc <- function(data, var, binary = T, varname = var){
  site <- NULL
  icc <- NULL
  if(binary){
    fml <- paste("factor(", var, ") ~  (1|physician)")
  }else {
    fml <- paste(var, "~  (1|physician)")}
  for(i in unique(data$Institution.Number)){
    if(binary){
      mod <- glmer(fml , cohort[Institution.Number==i], family = binomial)
    } else{
      mod <- lmer(fml , cohort[Institution.Number==i])
    }
    icc_i <- as.numeric(sjstats::icc(mod))
    site <- c(site, i)
    icc <- c(icc, icc_i)
  }
  data.frame(site, icc, var = varname)
}

cohort <- hide_site(cohort)
# Age
icc.age <- find.icc(cohort, "Age", binary = F)
# Gender
icc.gender <- data.frame(site, icc, var = "Gender")
icc.gender <- find.icc(cohort, "Gender", binary = T)

# charlson
icc.cci <- find.icc(cohort, "Charlson.Comorbidity.Index", F, 
            varname = "Charlson Comorbidity\nIndex")

# weekday vs weekend
cohort$wkd <- wday(ymd(cohort$Admit.Date))
cohort[, weekday := ifelse(wkd%in%c(2:6), 1, 0)]
icc.wkd <- find.icc(cohort, "weekday", varname = "Weekday/Weekend")
cohort[, daytime := as.numeric(str_sub(cohort$Admit.Time, -5, -4))>=7&
         as.numeric(str_sub(cohort$Admit.Time, -5, -4))<17]
icc.dtime <- find.icc(cohort, "daytime", varname = "Day/Night")

icc <- rbind(icc.age, icc.gender, icc.cci, icc.wkd, icc.dtime)

fwrite(icc, "C:/Users/guoyi/Desktop/to.adm/balence.test.csv")



library(extrafont)
loadfonts()
plot_icc <- function(data, titlex = 3){
  ggplot(data, aes(x = var, y = (1-icc)*100, fill = site)) + 
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5) +
    scale_fill_brewer(palette = "Paired") +
    geom_hline(aes(yintercept = 100), linetype = 2) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(size = 8),
          axis.line = element_line(color = "gray50", size = 0.5),
          panel.grid.minor = element_blank(), 
          #panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          text = element_text(family = "Trebuchet MS")) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 110))+
    ylab("% of Variation in Patient Characteristic \nUnrelated to Admitting Physician") +
    xlab("Baseline Characteristics of Patients at Time of Admission") + 
    annotate("text", x =  titlex, y = 102, label = "True Randomization",
                  family = "Trebuchet MS")
}
setwd("C:/Users/guoyi/Desktop/to.adm/figures.v3")
png("icc.png", res = 200, width = 1600, height = 1000)
plot_icc(icc)
dev.off()       



# -------------------- test 5 most common diagnosis ----------------------------
# COPD


copd <- find.icc(cohort, "copd", varname = "COPD")
cap <- find.icc(cohort, "cap", varname = "Pneumonia")
chf <- find.icc(cohort, "chf", varname = "Heart Failure")
stroke <- find.icc(cohort, "stroke", varname = "Stroke")

diag.icc <- rbind(copd, cap, chf, stroke)
diag.icc

fwrite(diag.icc, "C:/Users/guoyi/Desktop/to.adm/balence.test.diag.csv")


plot_icc(diag.icc, 2.5)
setwd("C:/Users/guoyi/Desktop/to.adm/figures.v3")
png("icc.diag.png", res = 200, width = 1600, height = 1000)
plot_icc(diag.icc, 2.5)
dev.off()







# lab values
# hgb
hgb <- readg(lab.hgb, hgb)
hgb <- hgb %>% arrange(EncID.new, ymd_hms(Collection.DtTm)) %>%
  filter(!duplicated(EncID.new)) %>% data.table
hgb$EncID.new <- as.character(hgb$EncID.new)
cohort <- merge(cohort, hgb[,.(HGB = Result.Value, EncID.new)],
                by = "EncID.new", all.x = T, all.y = F)
cohort[, hgb.abnormal := ifelse((Gender=="M"&(HGB>180|HGB<140))|
                                  (Gender=="F"&(HGB>160|HGB<120)),
                                T, F)]
table(cohort$hgb.abnormal, useNA = "ifany")
hgb <- find.icc(cohort[!is.na(hgb.abnormal)], "hgb.abnormal", T)
hgb

# sodium
sodium <- readg(lab, sodium)
sodium <- sodium %>% arrange(EncID.new, ymd_hms(Collection.DtTm)) %>%
  filter(!duplicated(EncID.new)) %>% data.table
sodium$EncID.new <- as.character(sodium$EncID.new)
cohort <- merge(cohort, sodium[,.(EncID.new, sodium = Result.Value)],
                by = "EncID.new", all.x = T, all.y = F)
cohort[,sodium.abnormal := sodium<135|sodium>145]
table(cohort$sodium.abnormal, useNA = "ifany")
sod <- find.icc(cohort[!is.na(sodium.abnormal)], "sodium.abnormal", T)

# lactate
lactate <- readg(lab, lactate)
range(lactate$Result.Value)
