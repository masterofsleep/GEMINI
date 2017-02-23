library(gemini)
lib.pa()

smh.dad <- readg(smh, dad)

sample.inst.from.type.missing  <- smh.dad[is.na(InstitutionFrom.Type)]
sample.inst.from.type.missing <- sample.inst.from.type.missing[1:500]
fwrite(sample.inst.from.type.missing[1:500, .(Gender, Age, EncID.new)], 
       "H:/GEMINI/Results/Ad Hoc/smh.no.instfromtype.sample500.csv")
