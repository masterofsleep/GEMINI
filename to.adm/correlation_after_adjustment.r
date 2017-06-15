# --------------------- correlation after adjustment ---------------------------

cohort <- find_cohort()
cohort[, daytime := as.numeric(str_sub(cohort$Admit.Time, -5, -4))>=7&
         as.numeric(str_sub(cohort$Admit.Time, -5, -4))<17]
cohort$wkd <- wday(ymd(cohort$Admit.Date))
cohort[, weekday := ifelse(wkd%in%c(2:6), 1, 0)]


library(lme4)
adjustment <- function(data, varname, cat = F){
  data = data.frame(data)
  for(i in unique(data$Institution.Number)){
    myFormula <- paste(varname, 
                       "~ Age + Gender + Charlson.Comorbidity.Index + fiscal.year + ",
                       " + weekday + daytime + copd + cap + stroke + uti + chf", sep = "")
    newvar <- paste("adj.", varname, sep = "")
    if(cat == F){
      fit <- lm(myFormula, 
                data = data[data$Institution.Number==i,])
      data[!is.na(data[,varname])&data$Institution.Number==i, newvar] <- 
        predict(fit, newdata = data.frame(
          Age = median(data$Age),
          Gender = "F",
          Charlson.Comorbidity.Index = median(data$Charlson.Comorbidity.Index),
          fiscal.year = 2015,
          weekday = 1, daytime = T,
          copd = F,
          cap = F, chf = F, stroke = F, uti = F
        )) + fit$residuals
      } else {
      #data[, varname] = factor(data[, varname])
      fit = glm(myFormula, 
                data = data[data$Institution.Number==i,], family = binomial(link = logit))
      data[!is.na(data[,varname])&data$Institution.Number==i, newvar] <- 
        predict(fit, newdata = data.frame(
          Age = median(data$Age),
          Gender = "F",
          Charlson.Comorbidity.Index = median(data$Charlson.Comorbidity.Index),
          fiscal.year = 2015,
          weekday = 1, daytime = T,
          copd = F,
          cap = F, chf = F, stroke = F, uti = F
        ), type = "response")
    }
  }
  return(data)
}

check <- data[data$Institution.Number==i,]
fit <- glm(read.in.30 ~ Age + Gender + Charlson.Comorbidity.Index + fiscal.year, 
           data = cohort[cohort$Institution.Number=="SMH",], family = binomial)
summary(fit)
adj.vars <- c("Acute.LoS",  "Cost", "N.rad")
varcat <- c(F,  F, F)
for(i in 1:3){
  cohort <- adjustment(cohort, adj.vars[i], varcat[i])
}




# Correlation between adjusted and unadjusted variables

#cohort <- hide_site2(cohort)
phy.ave <- ddply(cohort, ~ physician, summarize, 
                 site = Institution.Number[1],
                 ave.los = mean(Acute.LoS),
                 ave.adj.los = mean(adj.Acute.LoS),
                 ave.cost = mean(Cost, na.rm = T),
                 ave.adj.cost = mean(adj.Cost, na.rm = T),
                 ave.nrad = mean(N.rad),
                 ave.adj.nrad = mean(adj.N.rad))


ddply(phy.ave, ~ site, summarise,
      los.corr = cor(ave.los, ave.adj.los),
      los.corr.p = cor.test(ave.los, ave.adj.los)$p.value,
      cost.corr = cor(ave.cost, ave.adj.cost),
      cost.corr.p = cor.test(ave.cost, ave.adj.cost)$p.value,
      nrad.coor = cor(ave.nrad, ave.adj.nrad),
      nrad.corr.p = cor.test(ave.nrad, ave.adj.nrad)$p.value) %>%
  fwrite("C:/Users/guoyi/Desktop/to.adm/correlation.after.adjustment.csv")

setwd("C:/Users/guoyi/Desktop/to.adm/figures.v4/adjustment_comparison")
png("los_adjustement_comparison.png", res = 250, width = 2000, height = 1600)
ggplot(phy.ave, aes(ave.los, ave.adj.los, color = site)) + geom_point() +
  geom_smooth(method = "lm") + xlab("Average Acute Length-of-Stay (Days)") +
  ylab("Average Adjusted Acute Length-of-Stay (Days)") + theme_bw()
dev.off()

png("cost_adjustement_comparison.png", res = 250, width = 2000, height = 1600)
ggplot(phy.ave, aes(ave.cost, ave.adj.cost, color = site)) + geom_point() +
  geom_smooth(method = "lm") + xlab("Average Cost ($)") +
  ylab("Average Adjusted Cost ($)") + theme_bw()
dev.off()

png("nrad_adjustement_comparison.png", res = 250, width = 2000, height = 1600)
ggplot(phy.ave[!startsWith(phy.ave$site, "THP"), ], aes(ave.nrad, ave.adj.nrad, color = site)) + geom_point() +
  geom_smooth(method = "lm") + xlab("Average Number of Advanced Imaging") +
  ylab("Average Adjusted Number of Advanced Imaging") + theme_bw()
dev.off()
