#summary language


smh <- readg(smh,adm)%>%select(Language)
sbk <- readg(sbk,adm)%>%select(Language)
uhn <- readg(uhn,adm)%>%select(Language)

smh.tab <- data.frame(table(smh$Language, useNA = "always"))
sbk.tab <- data.frame(table(sbk$Language, useNA = "always"))
uhn.tab <- data.frame(table(uhn$Language, useNA = "always"))


write.csv(smh.tab, "H:/GEMINI/Results/lang_sum/smh.csv", row.names = F)
write.csv(sbk.tab, "H:/GEMINI/Results/lang_sum/sbk.csv", row.names = F)
write.csv(uhn.tab, "H:/GEMINI/Results/lang_sum/uhn.csv", row.names = F)
