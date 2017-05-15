# --- check diagnosis code for unknown physician with more than 50 patients ----
# ------------------------------ 2017-05-15 ------------------------------------
library(gemini)
lib.pa()
all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.csv")
phy <- readg(gim, all.phy)

phy.freq <- data.table(table(c(phy$adm.code.new, phy$dis.code.new, phy$mrp.code.new)))
phy.freq[N>=50, V1]

table(all.name$GIM)
to.check <- all.name[GIM%in%c("", "u")&code.new%in%phy.freq[N>=50, V1]]
ip_mrd <- readg(gim, ip_diag)[Diagnosis.Type =="M"]
icd.names <- fread("R:/GEMINI/Coding/CIHI/ICD_header.csv")
find_diag_freq <- function(x){
  name <- paste(to.check[code.new==x,.(first.name, last.name)][1], collapse = " ")
  patient <- phy[adm.code.new==x|
                   dis.code.new==x|
                   mrp.code.new==x, EncID.new]
  mrp <- ip_mrd[EncID.new%in%patient]
  mrp <- merge(mrp, icd.names[,.(Code, Desc1)], by.x = "Diagnosis.Code",
               by.y = "Code", all.x = T, all.y = F)
  fwrite(mrp[,.N, by = .(Diagnosis.Code, Desc1)][order(N, decreasing = T)],
         paste("mrd_freq_", name, ".csv", sep = ""))
}


setwd("H:/GEMINI/Results/DataSummary/physician_names/unknown_phy_mrd_freq")
for(i in c(4:10, 13)){
  find_diag_freq(to.check$code.new[i])
}


