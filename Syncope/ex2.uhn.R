library(gemini)
lib.pa()
library(readxl)
setwd("H:/GEMINI/Results/Syncope")
withINR <- read_excel("anticoag.uhn.xlsx", sheet = 3) %>% data.table
enctoex <- read_excel("anticoag.uhn.xlsx", sheet = 4)[c(1:94),] %>% data.table

ex3 <- unique(withINR[`Anticoagulant Pre-Admission`=="Y", EncID.new])

ex4 <- enctoex$`These are the Encounters to Exclude`[1:32]

ex3[!ex3%in%ex4]

intersect(ex3, ex4)


write.csv(ex3, "uhn.pre.anticoag.csv", row.names = F)
