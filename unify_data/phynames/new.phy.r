# -------------------- find all new physicians ----------------------------------
# ------------------------ 2017-04-07 ------------------------------------------
library(gemini)
lib.pa()
smh <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/smh.link.csv")
sbk <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/sbk.link.csv")
uhn <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/uhn.link.csv")
msh <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/msh.link.csv")
thp <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/thp.link.csv")
exclude <- readg(gim, notgim)
all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.csv")

find.new.code <- function(df, code.type.adm, code.type.mrp){
  df$adm.code = as.character(df$adm.code)
  df$dis.code = as.character(df$dis.code)
  df$mrp.code = as.character(df$mrp.code)
  df <- merge(df, all.name[code.type == code.type.adm, .(Code, code.new)], 
              by.x = "adm.code",by.y = "Code",
              all.x = T, all.y = F)
  df <- merge(df, all.name[code.type == code.type.adm, .(Code, code.new)], 
              by.x = "dis.code",by.y = "Code",
              all.x = T, all.y = F)
  df <- merge(df, all.name[code.type == code.type.mrp, .(Code, code.new, GIM)], 
              by.x = "mrp.code",by.y = "Code",
              all.x = T, all.y = F)
  names(df)[5:7] <- c("adm.code.new", "dis.code.new", "mrp.code.new")
  df
}
smh <- find.new.code(smh, "smh", "smh")
sbk <- find.new.code(sbk, "sbk", "sbk")
uhn <- find.new.code(uhn, "uhn.adm", "uhn.mrp")
msh <- find.new.code(msh, "msh", "msh")
thp <- find.new.code(thp, "thp", "thp")
sum(thp$mrp.code.new==thp$dis.code.new)
cohort <- rbind(smh, sbk, uhn, msh, thp, fill = T)[!EncID.new%in%exclude$EncID.new]
dad <- fread("H:/GEMINI/DataBackup/Data170214/UHN/CIHI/uhn.ip_dad.nophi.csv")
extra.enc <- dad[mdy(Discharge.Date)>ymd("20150331"), EncID.new]
cohort <- cohort[!EncID.new%in%extra.enc]



fwrite(cohort, "H:/GEMINI/Data/GEMINI/gim.all.phy.csv")
