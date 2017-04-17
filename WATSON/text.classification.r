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


setwd("H:/GEMINI/Results/watson")
sink("ct.txt")
cat(paste(full$Results, collapse = " "))
sink()


prep_word2vec(origin="ct.txt",destination="ct.processed.txt",lowercase=T,bundle_ngrams=1)


if (!file.exists("ct.bin")) {
  model = train_word2vec("ct.processed.txt","ct.bin",vectors=40,
                         threads=4,window=4,iter=5,negative_samples=0)
} else model = read.vectors("ct.bin")

model %>% closest_to("enhanced")
model %>% closest_to("unenhanced")
model %>% closest_to("contrast")

terms <- model %>% 
  closest_to(model[[c("contrast", "enhanced")]],20)
term = model[[terms$word, average = F]]
plot(term, method = "pca")
set.seed(10)
centers = 10
clustering = kmeans(model,centers=centers,iter.max = 40)
sapply(sample(1:centers,10),function(n) {
  names(clustering$cluster[clustering$cluster==n][1:10])
})


enhanced = model[[c("contrast","unenhanced"),average=F]]
ce <- model %>% cosineSimilarity(enhanced)
ce <- ce[rank(-ce[,1])<10|rank(-ce[,2])<10, ]

plot(ce, type = "n")
text(ce,labels=rownames(ce))


model %>% closest_to(model[["contrast"]] + model[["enhanced"]]- model[["unenhanced"]])
