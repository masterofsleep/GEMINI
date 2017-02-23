################################################################################
################# calculating kappa values for two tables ######################
################################################################################

library(gemini)
library(psych)
library(readxl)
library(irr)
lib.pa()
setwd("R:/GEMINI-DREAM")

df1 <- read_excel("50chartsNikki.xlsx", sheet = 1)
df2 <- read_excel("50chartsNikki.xlsx", sheet = 2)
df3 <- read_excel("50chartsIsmail.xlsx", sheet = 1)
df4 <- read_excel("50chartsIsmail.xlsx", sheet = 2)
df5 <- read_excel("50chartsShahina.xlsx", sheet = 1)
df6 <- read_excel("50chartsShahina.xlsx", sheet = 2)


kappa.table <- function(df1, df2, index){
  kappa <- NULL
  varname <- names(df1)[index]
  weighted.kappa <- NULL
  for(i in index){
    co.ka <- cohen.kappa(cbind(df1[,i], df2[,i]))
    kappa <- c(kappa, co.ka$kappa)
    weighted.kappa <- c(weighted.kappa, co.ka$weighted.kappa)
  }
  ka.table <- data.frame(rbind(kappa))
  names(ka.table) <- varname
  return(t(ka.table))
}

kappa.table.narm <- function(df1, df2, index){
  kappa <- NULL
  varname <- names(df1)[index]
  weighted.kappa <- NULL
  for(i in index){
    co.ka <- cohen.kappa(cbind(df1[df1[,i]!=100,i], df2[df2[,i]!=100,i]))
    kappa <- c(kappa, co.ka$kappa)
    weighted.kappa <- c(weighted.kappa, co.ka$weighted.kappa)
  }
  ka.table <- data.frame(rbind(kappa))
  names(ka.table) <- varname
  return(t(ka.table))
}

summary.vs.full <- rbind(kappa.table(df1, df2, c(5:22)),
                         kappa.table(df3, df4, c(5:22)),
                         kappa.table(df5, df6, c(5:22)))

row.names(summary.vs.full) <- c("Nikki", "Ismail", "Shahina")
write.csv(round(t(summary.vs.full), 3), file = "H:/GEMINI/Results/DREAM/kappatable.csv")





kappa.table2 <- function(df1, df2, df3, index){
  kappa <- NULL
  varname <- names(df1)[index]
  for(i in index){
    co.ka <- kappam.fleiss(cbind(df1[,i], df2[,i], df3[,i]))
    kappa <- c(kappa, co.ka$value)
  }
  ka.table <- data.frame(rbind(kappa))
  names(ka.table) <- varname
  return(ka.table)
}

kappa.table2(df1, df3, df5, c(5:22))

kappa3 <- rbind(kappa.table2(df1, df3, df5, c(5:22)),
                kappa.table2(df2, df4, df6, c(5:22)))

write.csv(round(t(kappa3), 3), file = "H:/GEMINI/Results/DREAM/kappatable2.csv")


cohen.kappa(cbind(df1$PFO, df2$PFO))
kappa2(cbind(df1$PFO, df2$PFO))



#------------Nov 24 2016 new kappa table ---------------------------------------
library(gemini)
library(psych)
library(readxl)
library(irr)
lib.pa()
setwd("R:/GEMINI-DREAM")
rm(list = ls())
df1 <- read_excel("50practice Nikki 2 ways.xlsx", sheet = 1)[1:50,]
df2 <- read_excel("50practice Nikki 2 ways.xlsx", sheet = 2)[1:50,]

kappa.table <- function(df1, df2, index1, index2){
  kappa <- NULL
  varname <- names(df1)[index1]
  weighted.kappa <- NULL
  for(i in 1:length(index1)){
    co.ka <- cohen.kappa(cbind(df1[,index1[i]], df2[,index2[i]]))
    kappa <- c(kappa, co.ka$kappa)
    weighted.kappa <- c(weighted.kappa, co.ka$weighted.kappa)
  }
  ka.table <- data.frame(rbind(kappa))
  names(ka.table) <- varname
  return(t(ka.table))
}




names(df1)
names(df2)
sum(df2[,5]==100)
sum(df1[,8]==100)

sum(df1[,6]==100)
sum(df1[,7]==100)

sum(df2[,4]==100)
sum(df1[,4]==100)

sum(df1[,10]==100)
sum(df1[,16]==100)


sum(df1[,11]==100)
sum(df1[,17]==100)

cohen.kappa(cbind(df2[,5], df1[,8]))$kappa
cohen.kappa(cbind(df1[df1[,6]!=100, 6], df1[df1[,7]!=100,7]))$kappa
cohen.kappa(cbind(df2[4], df1[4]))$kappa
cohen.kappa(cbind(df1[10], df1[16]))$kappa
cohen.kappa(cbind(df1[11], df1[17]))$kappa






#--------------------Dec 1 2016 ------------------------------------------------
setwd("R:/GEMINI-DREAM")
df1 <- read_excel("smh.echo.in14.part1ISMAIL.xlsx", sheet = 1)
df2 <- read_excel("smh.echo.in14.part2SHAHINA.xlsx", sheet = 1)

ol <- intersect(df1$EncID.new, df2$EncID.new)

df1 <- df1[df1$EncID.new%in%ol,] %>% arrange(EncID.new) %>% 
  filter(num_echo_in_14days!=0)
df2 <- df2[df2$EncID.new%in%ol,] %>% arrange(EncID.new) %>% 
  filter(num_echo_in_14days!=0)
names(df1)
df1$EncID.new ==df2$EncID.new
kappa.with.100 <- kappa.table(df1, df2, c(7:22))


df1.rm100 <- df1 %>% filter(num_echo_in_14days!=0)
df2.rm100 <- df2 %>% filter(num_echo_in_14days!=0)
kappa.without100 <- kappa.table(df1.rm100, df2.rm100, c(7:22))
kappa <- data.frame(cbind(kappa.without100, kappa.with.100))
names(kappa) <- c("kappa.without.100","kappa.with.100")
write.csv(kappa,"H:/GEMINI/Results/DREAM/smh.kappatable.part1vspart2.csv")




#---------------- Dec 06 2016 --------------------------------------------------
setwd("R:/GEMINI-DREAM")
library(readxl)
df1 <- read_excel("SBK echos full report.xlsx", sheet = 1)
df2 <- read_excel("SBK echos summaries.xlsx", sheet = 1)
df2 <- df2[1:51, ]
names(df1)[1:21]==
names(df2)
kappa.table(df1, df2, c(7:21))
kappa.table.narm(df1,df2,c(7:21))


kappa <- cbind(kappa.table.narm(df1, df2, c(7:21)),
               kappa.table(df1,df2,c(7:21)))
names(kappa) <- c("kappa.without.100","kappa.with.100")
write.csv(kappa,"H:/GEMINI/Results/DREAM/sbk.kappa.full.vs.summary.csv")




#-------------------- Feb 16 2017 ----------------------------------------------
setwd("R:/GEMINI-DREAM/DATA to be analyzed")
library(readxl)
df1 <- read_excel("SMH chart pull ismail.xlsx", sheet = 1)
df2 <- read_excel("SMH chart pull Shahina.xlsx", sheet = 1)
df1 <- df1[!is.na(df1$EncID.new),] %>% 
  filter(EncID.new%in%overlap) %>% arrange(EncID.new)
df2 <- df2[!is.na(df2$EncID.new),] %>% 
  filter(EncID.new%in%overlap) %>% arrange(EncID.new)
intersect(df1$EncID.new, df2$EncID.new) %>% length
df1$EncID.new[1:35]
df2$EncID.new[1:35]

kappa.table.narm <- function(df1, df2, index, missingvalue){
  kappa <- NULL
  varname <- names(df1)[index]
  for(i in index){
    co.ka <- cohen.kappa(cbind(df1[df1[,i]!=missingvalue,i], df2[df2[,i]!=missingvalue,i]))
    kappa <- c(kappa, co.ka$kappa)
  }
  ka.table <- data.frame(rbind(kappa))
  names(ka.table) <- varname
  return(t(ka.table))
}
cbind(df1[,5], df2[,5])
cbind(kappa.table.narm(df1[-12,], df2[-12,], index = c(5:11), 500),
      kappa.table.narm(df1, df2, index = c(5:11), 500))%>%
  write.csv("H:/GEMINI/Results/DREAM/smh.echo.kappa.csv", row.names = T)

df3 <- read_excel("SBK echo shahina.xlsx", sheet = 1)
df4 <- read_excel("SBk echo ismail.xlsx", sheet = 1)
df3 <- df3[!is.na(df3$EncID.new), ]
overlap <- intersect(df3$EncID.new, df4$EncID.new)
df3 <- df3 %>% filter(EncID.new%in%overlap) %>%
  arrange(EncID.new)
df4 <- df4 %>% filter(EncID.new%in%overlap) %>%
  arrange(EncID.new) %>% unique

kappa.table.narm(df3, df4, index = c(6:20), 500)%>%
  write.csv("H:/GEMINI/Results/DREAM/sbk.echo.kappa.csv")
  
