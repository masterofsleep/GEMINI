# ----------------- Compare different Cost Methods -----------------------------
library(gemini)
lib.pa()

dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")[, .(EncID.new,
                                                                           Institution.Number,
                                                                           fiscal.year)]
CPWC <- readxl::read_excel("H:/GEMINI/Coding/CPWC_OCDM_Per Hospital.xlsx")
names(CPWC) <- c("Institution.Number", "2010", "2011", "2012", "2013", "2014")
CPWC.long <- melt(CPWC, id.vars = "Institution.Number", 
                  measure.vars = c("2010", "2011", "2012", "2013", "2014"), 
                  variable.name = "fiscal.year",
                  value.name = "CPWC", 
                  variable.factor = FALSE, value.factor = F)
CPWC.long$fiscal.year <- as.integer(as.character(CPWC.long$fiscal.year))
dad$EncID.new <- as.character(dad$EncID.new)

# Find Cost
smh.cmg <- readg(smh, ip_cmg)
sbk.cmg <- readg(sbk, ip_cmg)
uhn.cmg <- readg(uhn, ip_cmg)
msh.cmg <- readg(msh, ip_cmg)
thp.cmg <- readg(thp, ip_cmg)
ip_cmg <- rbind(smh.cmg[,.(EncID.new, RIW.15)],
                sbk.cmg[,.(EncID.new, RIW.15)],
                uhn.cmg[,.(EncID.new, RIW.15)],
                msh.cmg[,.(EncID.new, RIW.15)],
                thp.cmg[,.(EncID.new, RIW.15)])
dad[, RIW.15:=NULL]
dad <- merge(dad, ip_cmg, by = "EncID.new",all.x = T, all.y = F)
table(dad$Institution.Number)
smh <- readg(smh, ip_hig)
sbk <- readg(sbk, ip_hig)
uhn <- readg(uhn, ip_hig)
msh <- readg(msh, ip_hig)
thp <- readg(thp, ip_hig)
hig <- rbind(smh[,.(EncID.new, HIG.Weight.15)],
             sbk[,.(EncID.new, HIG.Weight.15)],
             uhn[,.(EncID.new, HIG.Weight.15)],
             msh[,.(EncID.new, HIG.Weight.15)],
             thp[,.(EncID.new, HIG.Weight.15)])[EncID.new%in%dad$EncID.new]

dad <- merge(dad, hig, by = "EncID.new", all.x = T, all.y = F)


dad[, CPWC := NULL]
dad <- merge(dad, CPWC.long, by = c("Institution.Number", "fiscal.year"), 
             all.x = T, all.y = F)
# compare cost by two methods
# RIW * CPWC
dad[, ':='(cost_riw = as.numeric(RIW.15) * CPWC,
           cost_hig = as.numeric(HIG.Weight.15) * CPWC) ]

cor.test(dad$cost_riw, dad$cost_hig, method = "pearson")
cor.test(dad$cost_riw, dad$cost_hig, method = "spearman")
t.test(dad$cost_riw, dad$cost_hig, paired = T)

dad[, ':='(mean.cost = (cost_hig + cost_riw)/2,
           diff.cost = cost_hig - cost_riw)]

ggplot(dad, aes(mean.cost, diff.cost)) + geom_point()
ggplot(dad, aes(diff.cost)) + geom_histogram(binwidth = 0.1)
