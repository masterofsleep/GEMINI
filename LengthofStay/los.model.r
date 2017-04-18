# ---------------------- length of stay study models ---------------------------
# ----------------------------- 2017-04-18 -------------------------------------
library(gemini)
lib.pa()


data <- fread("H:/GEMINI/Results/LengthofStay/cohort.los.april07.csv")

library(lme4)
library(multcomp)

reg1 <- lmer(LoS~ site + (1|Charlson.Comorbidity.Index), data=data)
summary(reg1)
Var_MRP = as.data.frame(VarCorr(reg1))[1, "vcov"]
Var_res = as.data.frame(VarCorr(reg1))[2, "vcov"]
ICC = Var_MRP / (Var_MRP + Var_res)
ICC



data[, CCI := ifelse(Charlson.Comorbidity.Index >=3, "3+", Charlson.Comorbidity.Index)]
reg2 <- lmer(LoS~ site + (1|CCI), data=data)
Var_MRP = as.data.frame(VarCorr(reg2))[1, "vcov"]
Var_res = as.data.frame(VarCorr(reg2))[2, "vcov"]
ICC = Var_MRP / (Var_MRP + Var_res)
ICC
summary(reg2)


ggplot(data[LoS<=30*24], aes(Charlson.Comorbidity.Index,  LoS, group = Charlson.Comorbidity.Index)) + geom_boxplot()
