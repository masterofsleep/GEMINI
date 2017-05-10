library(gemini)
lib.pa()

phy.sum <- fread("C:/Users/guoyi/Desktop/to.adm/phy.summary.csv")
fwrite(phy.sum[,.(code.new, site, first.name, last.name)],
       "H:/GEMINI/Results/phy_gender/phy100.csv")

phy <- readg(gim, all.phy)
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")

phy20.code <- c(phy[,.N, by = "mrp.code.new"][N>=20, mrp.code.new],
phy[,.N, by = "adm.code.new"][N>=20, adm.code.new],
phy[,.N, by = "dis.code.new"][N>=20, dis.code.new])
all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.csv")

phy20 <- all.name[code.new%in%phy20.code]
table(phy20$GIM)
fwrite(phy20[GIM=="u"],
       "H:/GEMINI/Results/phy_gender/phy20.csv")
