# --------------------------- CMG group ----------------------------------------
# -------------------------- 2017-05-08 ----------------------------------------

library(gemini)
lib.pa()
smh <- readg(smh, ip_cmg)
sbk <- readg(sbk, ip_cmg)
uhn <- readg(uhn, ip_cmg)
msh <- readg(msh, ip_cmg)
thp <- readg(thp, ip_cmg)

cmg <- rbind(smh[, .(EncID.new, CMG)],
      sbk[, .(EncID.new, CMG)],
      uhn[, .(EncID.new, CMG)],
      msh[, .(EncID.new, CMG)],
      thp[, .(EncID.new, CMG)])

cmg.freq <- data.table(table(cmg$CMG))
names(cmg.freq)[1] <- ("CMG")

# fwrite(cmg.freq, "H:/GEMINI/Results/DesignPaper/cmg.freq.csv")


cmg.list <- readxl::read_excel("H:/GEMINI/Coding/CMG Listing.xlsx", skip = 4)
cmg.list$CMG <- as.character(cmg.list$CMG)
cmg <- merge(cmg, cmg.list[, c("CMG", "CMG Description")], by = "CMG", all.x = T)
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")
dad$EncID.new <- as.character(dad$EncID.new)
dad <- merge(dad, unique(cmg), by = "EncID.new", all.x = T, all.y = F)
cmg.freq <- dad[, .N, by = `CMG Description`][order(N, decreasing = T)]

dad[, top20.diag := ifelse(`CMG Description`%in%cmg.freq$`CMG Description`[1:20], `CMG Description`, "Other")]
top20diag  <- ddply(dad, ~top20.diag, summarize,
                    N = length(EncID.new),
                    Cost = median(Cost, na.rm = T)) %>% arrange(desc(N))
fwrite(cmg.freq, "H:/GEMINI/Results/DesignPaper/cmg.freq.csv")

names(top20diag)[1] <- "Diagnosis"
top20diag$ID <-c(21, 1:20)
library(treemap)
reds <- c(rgb(255/255, 255/255, 255/255, 1),
          rgb(255/255, 204/255, 204/255, 1),
          rgb(255/255, 153/255, 153/255, 1),
          rgb(255/255, 102/255, 102/255, 1),
          rgb(255/255, 51/255, 51/255, 1),
          rgb(255/255, 0/255, 0/255, 1),
          rgb(204/255, 0/255, 0/255, 1),
          rgb(153/255, 0/255, 0/255, 1))

png("C:/Users/guoyi/Desktop/to.adm/figures.v3/top20diag.png", res = 250, width = 2000, height = 1200)
treemap(top20diag,
        algorithm = "pivotSize",
        index = c("Diagnosis"),
        vSize = "N",
        vColor = "Cost",
        sortID = "ID",
        type = "value",
        palette = reds,
        title = "",
        title.legend = "Median Cost per Hospitalization",
        force.print.labels = T,
        mapping = c(2000, 6000, 10000)
)
dev.off()




ip_diag <- readg(gim, ip_diag)
ip_diag$EncID.new <- as.character(ip_diag$EncID.new)
dad <- merge(dad, ip_diag[Diagnosis.Type=="M"],
             by = "EncID.new", all.x = T, all.y = F)
icd.names <- fread("R:/GEMINI/Coding/CIHI/ICD_header.csv")

dad <- merge(dad, icd.names[, .(Code, Desc1)], by.x = "Diagnosis.Code",
             by.y = "Code", all.x = T, all.y = F)

cmg.freq <- dad[,.N, by = CMG]
names(cmg.freq)[2] <- "CMG_freq"
diag.freq.by.cmg <- dad[, .N, by = .(CMG, `CMG Description`, Diagnosis.Code, Desc1)]
diag.freq.by.cmg <- merge(cmg.freq, diag.freq.by.cmg, by = "CMG", all.x = T, all.y = F)

diag.freq.by.cmg <- diag.freq.by.cmg %>% arrange(desc(CMG_freq), `CMG Description`, desc(N))
fwrite(diag.freq.by.cmg, "H:/GEMINI/Results/DesignPaper/diag_freq_by_cmg.csv")
