# ----------------------- clinical frequency tables ----------------------------
library(gemini)
lib.pa()

# SMH lab
smh.lab <- readg(smh, lab)
smh.lab.freq <- smh.lab[, .N, by = .(Test.Name, Test.ID)][order(Test.Name, decreasing = T)]
smh.lab.freq[, dup_name := ifelse(duplicated(Test.Name)|duplicated(Test.Name, fromLast = T),
                                  "dup", "")]
smh.lab.freq[, dup_id := ifelse(duplicated(Test.ID)|duplicated(Test.ID, fromLast = T),
                                "dup", "")]
fwrite(smh.lab.freq, "H:/GEMINI/Results/DataSummary/clinical freq tables/lab.smh.csv")

sum(duplicated(smh.lab.freq$Test.Name))
sum(duplicated(smh.lab.freq$Test.ID))

