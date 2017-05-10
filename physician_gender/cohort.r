library(gemini)
lib.pa()

phy.sum <- fread("C:/Users/guoyi/Desktop/to.adm/phy.summary.csv")
fwrite(phy.sum[,.(code.new, site, first.name, last.name)],
       "H:/GEMINI/Results/phy_gender/phy100.csv")
