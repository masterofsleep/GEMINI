library(gemini)
lib.pa()
lab <- readg(smh, corelab)
samplelab <- lab[1:100,]

write.table(samplelab, "R:/GEMINI/Sample/sample_lab_100.txt", 
            sep = "|", na = "", quote = F,row.names = F)
