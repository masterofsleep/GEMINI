rm(list = ls())
library(gemini)
lib.pa()
library(readxl)
setwd("R:/GEMINI-SYNCOPE/watson")
smh.ctpa <- read_excel("smh.ctpa.xlsx")[1:178,]
sbk.ctpa <- read_excel("sbk.ctpa.xlsx")[1:165,] %>% data.table
uhn.ctpa <- read_excel("uhn.ctpa.xlsx")[1:221,] %>% data.table

full <- rbind(sbk.ctpa[,.(Test.Name, Results, `Contrast (y, n)`)],
              uhn.ctpa[,.(Test.Name = ProcedureName, 
                          Results = ReportText, `Contrast (y, n)`)])


table(full[,.(Test.Name, `Contrast (y, n)`)])

full <- full[Test.Name%in%c("CT chest", "CT Chest")]
full <- full[`Contrast (y, n)`!="x"]
library(tm)
set.seed(100)
train.ind <- sample(321, 250, replace = F)
train <- full[train.ind]
test <- full[-train.ind]


full[str_detect(toupper(Results), "UNENHANCED")|
       str_detect(toupper(Results), "UN-ENHANCED")|
       str_detect(toupper(Results), "UN- ENHANCED")|
       str_detect(toupper(Results), "NONCONTRAST")|
       str_detect(toupper(Results), "NON-CONTRAST")|
       (!str_detect(toupper(Results), "ENHANCED")&
          !str_detect(toupper(Results), "CONTRAST"))] -> check
check[`Contrast (y, n)`=="y", Results]


fwrite(full, "H:/GEMINI/Results/watson/nlp.data.csv")
