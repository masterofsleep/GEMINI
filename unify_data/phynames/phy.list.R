# ------------ create a complete physician name for Fahad to mark duplicates ---
# ---------------------------- 2017-03-23 --------------------------------------
library(gemini)
lib.pa()

phy.names <- rbind(fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/smh.names.csv"),
                   fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/sbk.names.csv"),
                   fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/msh.names.csv"),
                   fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/uhn.names.csv"),
                   fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/thp.names.csv"),
                   fill = T)

simpleCap <- function(x)gsub("(^|[[:space:]]|'|-)([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)

phy.names$first.name <- simpleCap(tolower(phy.names$first.name))
phy.names$last.name <- simpleCap(tolower(phy.names$last.name))

phy.names <- phy.names[order(last.name, first.name)]

table(phy.names$GIM, useNA = "ifany")

fwrite(phy.names[GIM!="n"], "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.all.physician.csv")

