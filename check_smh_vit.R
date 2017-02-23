#check the separate files in SMH/vitro/old
rm(list=ls())
library(gemini)
library(readxl)
lib.pa()
swdr("SMH/Vitals/old")
files <- list.files();files
for(i in 1:6){
  assign(paste("vit",i,sep=""), fread(files[(2*i+2)]))
}

vit <- rbind(vit1, vit2, vit3, vit4, vit5, vit6)
length(unique(vit$`Patient Account ID`))





#new vital data
swdr("SMH/Vitals/RAW")
files <- list.files()
for(i in 1:6){
  assign(paste("vit",i,sep =""), read_excel(files[i+1]))
}


setwd("..")
vitall <- fread("smh.vitals.nophi_new.csv")


vitmer<- rbind(vit1, vit2, vit3, vit4, vit5, vit6)
names(vit1)[1] <- "Patient Account ID"




dad <- readg(smh, dad)

dad$vitals <- dad$EncID.new%in% vitall$EncID.new

ggplot(dad, aes(x = mdy(Admit.Date), fill = vitals)) +
  geom_histogram(binwidth = 20, alpha = 0.5) +
  ggtitle("SMH Vitals vs Admitting Date")
