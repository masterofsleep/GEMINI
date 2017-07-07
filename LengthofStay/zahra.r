rm(list=ls(all=TRUE)) 

### Set wd and read data ###
setwd("R:/GEMINI-DREAM/Length of stay/june14_update")
data = read.csv("cohort.los.june14.csv", header = T)
dim(data)#should be 76364 14

### Load packages###
library(lme4)
library(multcomp)
library(languageR)
library(sjstats)

### Subset data ###
data2 = data[which(data$LoS<=336),]
data2 = data2[which(data2$mrp.GIM==1),]
dim(data2) #70538 14

##glmer##
data2$los_24 = ifelse(data2$LoS<=24, 1, 0)
reg1 <- glmer(los_24 ~ 1 + (1 |mrp.code/Site), data=data2, family=binomial)
icc(reg1) #package sjstats wu et al ref

# function to extract paramters of lme 
mySumm <- function(.){
  c(beta=fixef(.),sigma=sigma(.), 
    sig01=sqrt(unlist(VarCorr(.))))
}

# original parameters 
t0 <- mySumm(reg1)

# parametric bootstrap (There is an way to speed this up)
system.time(boo01 <- bootMer(reg1 , mySumm, nsim = 1000, 
                             type = "parametric", .progress = "txt"))

# extract percentile CI for all parameters
alpha<-0.05 
BootCI<-apply(boo01$t, 2, function(x, alpha) {
  quantile(x, c(alpha/2, 0.5, 1-alpha/2))
}, alpha=alpha)
BootCI

