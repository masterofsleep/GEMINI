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

ave.los$avelos.group <- as.numeric(ave.los$avelos.group)
cohort.100p <- merge(cohort.100p, ave.los[,.(avelos.group, phy, physician)],
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



# ------------------------- simulation -----------------------------------------
plot.phy <- function(data, title, xlab = "physician", 
                     ylab, nextreme = 1,
                     ave.fun, xstart = -2){
  df <- ddply(data, ~physician, .fun = ave.fun) %>% data.table
  names(df)[4] <- "phy.ave"
  for(i in unique(df$site)){
    df[site ==i, phy := as.numeric(factor(physician, levels = physician[order(phy.ave, decreasing = T)]))]
  }
  p <- ggplot(df, aes(phy, phy.ave, fill = site)) + 
    geom_bar(stat = "identity", width = 0.5) + 
    facet_grid(.~site, scales = "free_x") + 
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    ylim(0, 6.5) +
    expand_limits(x = xstart) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  del <- ddply(df, ~site, summarise,
               xm = max(phy),
               ymi = quantile(phy.ave, probs = 0.25),
               yma = quantile(phy.ave, probs = 0.75),
               yav = quantile(phy.ave, probs = 0.5))
  ave.shift <- max(df$phy.ave) * 0.012
  p <- p + 
    #expand x limit
    geom_text(data = del, aes(x = xm+3, y = ymi - ave.shift, label = " "), size = 3) +
    # plot the 25% tile
    geom_segment(data = del, aes(x = 1, xend = xm, y = ymi, yend = ymi), linetype = 2) +
    geom_text(data = del, aes(x = xstart/2, y = ymi, label = sprintf("%.1f", ymi)), size = 3) +
    geom_text(data = del, aes(x = xm+2, y = ymi, label = "25%"), size = 3) +
    # plot the 50% tile
    geom_segment(data = del, aes(x = 1, xend = xm, y = yav, yend = yav), linetype = 2) +
    geom_text(data = del, aes(x = xstart/2, y = yav, label = sprintf("%.1f", yav)), size = 3) +
    geom_text(data = del, aes(x = xm+2, y = yav, label = "50%"), size = 3) +
    # plot the 75% tile
    geom_segment(data = del, aes(x = 1, xend = xm, y = yma, yend = yma), linetype = 2) +
    geom_text(data = del, aes(x = xstart/2, y = yma, label = sprintf("%.1f", yma)), size = 3) +
    geom_text(data = del, aes(x = xm+2, y = yma, label = "75%"), size = 3)
  print(p)
}





ddply(cohort.100p, ~Institution.Number, summarize,
      nphy =  max(phy))

simu.los <- function(x, ref.n = 3, ref.pctl = 0.75){
  x <- data.table(x)
  ref.pctl <- 1 - ref.pctl
  phy.ref <- as.numeric(quantile(unique(x$phy), ref.pctl))
  ref <- x[phy>(phy.ref-ref.n/2)&phy<=(phy.ref+ref.n/2), LOS.without.ALC]
  for(i in 1: floor(phy.ref-ref.n/2)){
    fn <- ecdf(x[phy==i, LOS.without.ALC])
    percentiles <- fn(x[phy==i, LOS.without.ALC])
    x[phy==i, simu.LOS := as.numeric(quantile(ref, probs = percentiles))]
  }
  x[is.na(simu.LOS), simu.LOS := LOS.without.ALC]
  return(x[,.(EncID.new, simu.LOS, LOS.without.ALC, physician, phy)])
}

## method 1, only simulate to those around the targeted percentile
los.to.75 <- ddply(cohort.100p, ~Institution.Number, function(x)simu.los(x, ref.pctl = 0.75)) %>% data.table
los.to.50 <- ddply(cohort.100p, ~Institution.Number, function(x)simu.los(x, ref.pctl = 0.5)) %>% data.table
los.to.25 <- ddply(cohort.100p, ~Institution.Number, function(x)simu.los(x, ref.pctl = 0.25)) %>% data.table


ave.simu.los <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$LOS.without.ALC, na.rm = T))
}
los.original <- los.to.75
los.original$simu.LOS <- los.original$LOS.without.ALC
setwd("C:/Users/guoyi/Desktop/to.adm/to.gemini.investigators")
png("simu.no.simu.png", res = 170, width = 2000, height = 1200)
plot.phy(cohort.100p, "Average Length-of-Stay", 
         ylab = "Average Length-of-Stay", 
         ave.fun = ave.simu.los)
dev.off()


png("C:/Users/guoyi/Desktop/to.adm/simu.los/simu.los.to.75.png", res = 250, width = 2000, height = 1200)
plot.phy(los.to.75, "Simulation to 75 th Percentile", 
         ylab = "Average Length-of-Stay (Days)", 
         ave.fun = ave.simu.los)
dev.off()
png("C:/Users/guoyi/Desktop/to.adm/simu.los/simu.los.to.50.png", res = 250, width = 2000, height = 1200)
plot.phy(los.to.50, "Simulation to 50 th Percentile", 
         ylab = "Average Length-of-Stay (Days)", 
         ave.fun = ave.simu.los)
dev.off()
png("C:/Users/guoyi/Desktop/to.adm/simu.los/simu.los.to.25.png", res = 250, width = 2000, height = 1200)
plot.phy(los.to.25, "Simulation to 25 th Percentile", 
         ylab = "Average Length-of-Stay (Days)", 
         ave.fun = ave.simu.los)
dev.off()



los.to.75[simu.LOS!=LOS.without.ALC, .(Institution.Number, phy)] %>% table
los.to.50[simu.LOS!=LOS.without.ALC, .(Institution.Number, phy)] %>% table
los.to.25[simu.LOS!=LOS.without.ALC, .(Institution.Number, phy)] %>% table
x[simu.LOS!=LOS.without.ALC, .(Institution.Number, phy)] %>% table

sum(los.to.75$EncID.new == los.to.25$EncID.new)
sum(los.to.75$EncID.new == los.to.50$EncID.new)
sum(los.to.75$EncID.new == cohort.100p$EncID.new)

simu <- data.frame(EncID.new = los.to.75$EncID.new, 
              los.to.75 = los.to.75$simu.LOS,
              los.to.50 = los.to.50$simu.LOS,
              los.to.25 = los.to.25$simu.LOS)

cohort.100p <- merge(cohort.100p, simu, by = "EncID.new")

simu.res <- ddply(cohort.100p, ~Institution.Number, summarize,
      saved.bd.75th  = sum(LOS.without.ALC) - sum(los.to.75),
      saved.bd.50th  = sum(LOS.without.ALC) - sum(los.to.50),
      saved.bd.25th  = sum(LOS.without.ALC) - sum(los.to.25),
      total.ALC = sum(Number.of.ALC.Days),
      total.Non.ALC = sum(LOS.without.ALC),
      total.LOS = sum(LoS)
)

fwrite(simu.res, "C:/Users/guoyi/Desktop/to.adm/simu.los.comparison.csv")
