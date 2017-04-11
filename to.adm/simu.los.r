# --------------------------- test balance -------------------------------------
library(gemini)
lib.pa()
cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv", colClasses = list(character = "EncID.new"))
all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]
n.pat <- cohort[,.N, by = physician]
cohort.100p <- cohort[physician%in%n.pat[N>=100, physician]]
cohort.100p <- cohort.100p[LOS.without.ALC<=30]
sd(cohort.100p$LOS.without.ALC)



# los.quantile <- quantile(cohort.100p$LOS.without.ALC, probs = seq(0, 1, 0.01)) %>% data.table
# los.quantile$tile <- paste(0:100, "%", sep = "")
# names(los.quantile$tile) <- "LOS"
# fwrite(los.quantile, "H:/GEMINI/Results/to.administrator/los.percentile.csv")

cohort <- cohort.100p
ave.los <- ddply(cohort.100p, ~physician, summarize,
                 N = length(EncID.new), 
                 site = Institution.Number[1],
                 ave.los = mean(LOS.without.ALC)) %>% data.table
for(i in unique(ave.los$site)){
  ave.los[site ==i, ':='(phy = as.numeric(factor(physician, levels = physician[order(ave.los, decreasing = T)])),
                         avelos.group = cut(ave.los, breaks=quantile(ave.los, probs=seq(0,1, by=1/3), na.rm=TRUE), 
                                            include.lowest=TRUE,
                                            labels = 1:3))]
}
qplot(N, ave.los, color = site, data = ave.los)

ddply(ave.los, .(site, avelos.group), summarize,
      npatient = sum(N))
ave.los$avelos.group <- as.numeric(ave.los$avelos.group)
cohort.100p <- merge(cohort.100p, ave.los[,.(avelos.group, physician)],
                     by = "physician")



df <- ddply(cohort.100p[Institution.Number=="smh"], ~avelos.group, function(x){
  rbind(find.qq(x$LOS.without.ALC, 
                cohort.100p[avelos.group==4&Institution.Number=="smh",LOS.without.ALC]))
})
find.qq <- function(x, y){
  sx <- quantile(x, probs = seq(0, 1, 0.02))
  sy <- quantile(y, probs = seq(0, 1, 0.02))
  data.frame(sx, sy)
}
find.qq.bygroup <- function(data = NULL, var, group){
  group <- deparse(substitute(group))
  var <- deparse(substitute(var))
  mg <- max(data[[group]])
  ddply(data, as.formula(paste("~", group, sep = "")), function(x){
    find.qq(x[[var]], 
                  data[[var]][data[[group]]==mg])
  })
}
df <- NULL
for(i in unique(cohort.100p$Institution.Number)){
  df <- rbind(df, cbind(find.qq.bygroup(cohort.100p[Institution.Number==i], LOS.without.ALC, avelos.group),
                    site = i))
}
png("H:/GEMINI/Results/to.administrator/los_qq_no_outliers.png", res = 200, width = 2000, height = 2000)
ggplot(df[df$avelos.group!=5, ], aes(sx, sy, color = factor(avelos.group))) +
  geom_point(size = 1) + geom_abline(aes(slope = 1, intercept = 0)) + 
  facet_wrap(~site, nrow = 3) + xlim(0,30) + ylim(0,30)
dev.off()

png("H:/GEMINI/Results/to.administrator/los_qq.png", res = 200, width = 2000, height = 2000)
ggplot(df[df$avelos.group!=5, ], aes(sx, sy, color = factor(avelos.group))) +
  geom_point(size = 1) + geom_abline(aes(slope = 1, intercept = 0)) + 
  facet_wrap(~site, nrow = 3)
dev.off()

# ---------------------- mean difference plot ----------------------------------
df$mean <- (df$sx + df$sy)/2
df$diff <- df$sy/df$sx

ggplot(df, aes(mean, diff, color = factor(avelos.group))) + geom_point() + 
  facet_wrap(~site, nrow = 2)
