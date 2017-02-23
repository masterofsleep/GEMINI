#check the separate files in SMH/Micro/old
rm(list=ls())
library(gemini)
library(read)
lib.pa()
swdr("SMH/Micro/old")
files <- list.files()
for(i in 1:8){
  assign(paste("mic",i,sep=""), fread(files[(2*i-1)]))
}

mic <- rbind(mic1, mic2, mic3, mic4, mic5, mic6, mic7, mic8)

swdr("SMH/CIHI")
link <- fread("SMH.LINKLIST_NEWHASH.csv")

smh.micro <- merge(mic, link[,.(IPEncounterID, EncID.new)], 
                   by.x = "Encounter_ID", by.y = "IPEncounterID",
                   all.x = T, all.y = F)
write.csv(smh.micro[,-1, with = F], "H:/GEMINI/Data/SMH/Micro/smh.micro.new.csv",
          row.names = F)
