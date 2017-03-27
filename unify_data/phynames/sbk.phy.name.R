# ==============================================================================
# =====================  SBK Physician Names  ==================================
# ======================    March 2 2017   =====================================
library(gemini)
lib.pa()
rm(list = ls())

names.marked <- fread("C:/Users/guoyi/Desktop/marked_names/names_code_link_ASW.csv")
adm.names <- fread("R:/GEMINI/_RESTORE/SBK/Physicians/adm.physician.hashes.csv")
dad.names <- fread("R:/GEMINI/_RESTORE/SBK/Physicians/dad.mrp.hashes.csv")

intersect(dad.names$mrpCode, adm.names$admitCode)
intersect(dad.names$mrpCode, adm.names$disCode)

adm.names <- merge(adm.names, names.marked[,.(hashed, Admiting.Name = Name, `Was GIM Attending`)],
                   by.x = "admitCode", by.y = "hashed",
                   all.x = T, all.y = F)

adm.names <- merge(adm.names, names.marked[,.(hashed, Discharging.Name = Name, `Was GIM Attending`)],
                   by.x = "disCode", by.y = "hashed",
                   all.x = T, all.y = F)
names(adm.names)[c(5,7)] <- c("gim.adm", "gim.dis")
sum(is.na(adm.names$gim.dis))
sum(is.na(adm.names$gim.adm))

adm.names[,gim:= ifelse(gim.adm=="Y"|gim.dis=="Y", "Y", 
                        ifelse(gim.adm=="N"&gim.dis=="N", "N", "U"))]
table(adm.names$gim)

fwrite(adm.names, "R:/GEMINI/_RESTORE/SBK/Physicians/adm.physician.hashes.marked.csv")
fwrite(adm.names[gim=="N", .(EncID.new, Admiting.Name, Discharging.Name)], "R:/GEMINI/_RESTORE/SBK/Physicians/list.not.gim.csv")
fwrite(adm.names[gim=="U", .(EncID.new, Admiting.Name, Discharging.Name)], "R:/GEMINI/_RESTORE/SBK/Physicians/list.unknown.csv")


# -------------------------- to create complete list ---------------------------
names.marked <- fread("C:/Users/guoyi/Desktop/marked_names/names_code_link_ASW.csv")
names.parse <- str_split(names.marked$Name, " ")%>% 
  ldply(function(x) data.frame(first.name = paste(x[1:(length(x)-1)], collapse = " "), last.name = tail(x,1)))
sbk.names <- cbind(Code = names.marked$hashed,
               code.type = "sbk",
               names.parse, 
               GIM = tolower(names.marked$`Was GIM Attending`))
adm.names <- fread("R:/GEMINI/_RESTORE/SBK/Physicians/adm.physician.hashes.csv")
dad.names <- fread("R:/GEMINI/_RESTORE/SBK/Physicians/dad.mrp.hashes.csv")
freq <- c(adm.names$admitCode, adm.names$disCode, dad.names$mrpCode) %>% table %>%
  data.table

sbk.names <- merge(sbk.names, freq, by.x = "Code", by.y = ".")


fwrite(sbk.names, "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/sbk.names.csv")




# ------------- create a complete link from enc id to phy names ----------------
adm.names <- fread("R:/GEMINI/_RESTORE/SBK/Physicians/adm.physician.hashes.csv")
dad.names <- fread("R:/GEMINI/_RESTORE/SBK/Physicians/dad.mrp.hashes.csv")
sbk.phy.link <- merge(adm.names, dad.names, by = "EncID.new")
sbk.phy.link$EncID.new <- paste("12", sbk.phy.link$EncID.new, sep = "")
names(sbk.phy.link)[2:4] <- c("adm.code", "dis.code","mrp.code")
fwrite(sbk.phy.link, "H:/GEMINI/Results/DataSummary/physician_names/link/sbk.link.csv")
