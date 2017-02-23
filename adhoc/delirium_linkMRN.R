#---------------------- Delirium link MRN --------------------------------------
library(gemini)
library(readxl)
lib.pa()
rm(list = ls())
setwd("R:/GEMINI-DREAM/DELIRIUM Charts")
EncID.new <- NULL
files <- list.files()
dat1 <- fread(files[1])[1:200]
dat2 <- fread(files[2])
dat3 <- fread(files[3])
dat4 <- fread(files[4])
dat5 <- fread(files[5])

EncID.new <- c(dat1$EncID.new, dat2$EncID.new, dat3$EncID.new, dat4$EncID.new,
               dat5$EncID.new)


write.csv(EncID.new, "H:/GEMINI/Results/delirium/delirium.cohort.csv", row.names = F)


cohort <- fread("H:/GEMINI/Results/Delirium/delirium.mrn.csv")

cohort[,c("Living Situation (Category)", "Living Situation Phrase", 
          "Discharge Disposition", "Discharge Disposition Phrase",
          "Language_Category", "Language_Notes"):= NA]


cohort <- unique(cohort)
set.seed(20)
rand <- sample(21:2000, 80)
overlap <- cohort[c(1:20, rand), ]
nonoverlap <- cohort[-c(1:20, rand),]
cohort1 <- rbind(overlap, nonoverlap[1:950])
cohort2 <- rbind(overlap, nonoverlap[951:1900])
setwd("R:/GEMINI-DREAM/DELIRIUM Charts")

fwrite(cohort, "H:/GEMINI/Results/delirium/delirium.mrn.csv")
fwrite(cohort1, "H:/GEMINI/Results/delirium/delirium.mrn1.csv")
fwrite(cohort2, "H:/GEMINI/Results/delirium/delirium.mrn2.csv")
