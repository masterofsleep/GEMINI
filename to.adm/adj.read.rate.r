# ------------------------- final plots ----------------------------------------
cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv")
all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]
cap.co <- cohort[cap==T]
cap.co[Institution.Number%in%c("thp-m", "thp-c"),
       ':='(Institution.Number = "thp")]
cap.co[, physician := paste(Institution.Number, mrp.code.new, sep = "-")]
plot.phy <- function(data, leastpatient, title, xlab = "physician", 
                     ylab, nextreme = 1,
                     ave.fun, xstart = -2){
  data <- data[!is.na(read.in.30)]
  df <- ddply(data, ~physician, .fun = ave.fun) %>% data.table
  names(df)[5] <- "phy.ave"
  df <- df[N>=leastpatient]
  site.mean <- ddply(data[physician%in%df$physician], ~Institution.Number, .fun = ave.fun)
  names(site.mean)[5] <- "site.mean"
  df <- merge(df, site.mean[,c(1,5)], by.x = "site", by.y = "Institution.Number")
  for(i in unique(df$site)){
    df[site ==i, phy := as.numeric(factor(physician, levels = physician[order(phy.ave, decreasing = T)]))]
  }
  mod.read <- lm(phy.ave ~ site + ave.los, df)
  summary(mod.read)
  df$phy.ave <- mod.read$residuals + 
    predict(mod.read, newdata = data.frame(site = df$site, 
                                           ave.los = df$ave.los))
  p <- ggplot(df, aes(phy, phy.ave, fill = site)) + 
    geom_bar(stat = "identity", width = 0.5) + 
    geom_line(aes(x = phy, y = site.mean), alpha = 0.5,
              linetype = 2, size = 0.5) + 
    facet_grid(.~site, scales = "free_x") + 
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    expand_limits(x = xstart) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  del <- ddply(df, ~site, summarise,
               site.ave = sum(phy.ave * N)/sum(N),
               xm = max(phy),
               ymi = quantile(phy.ave, probs = 0.1),
               yma = quantile(phy.ave, probs = 0.9),
               yav = quantile(phy.ave, probs = 0.1)*0.5 + quantile(phy.ave, probs = 0.9)*0.5,
               ydiff = sprintf("%.1f", yma - ymi))
  ave.shift <- max(df$phy.ave) * 0.02
  p <- p + geom_errorbar(data = del, aes(x = xstart/2, y = NULL,ymin = ymi, ymax = yma), 
                         alpha = 0.3, width = 2) + 
    # plot the 10% - 90% range
    geom_rect(data = del, aes(x = NULL, y = NULL, xmin = xstart/2 - 1, xmax =  xstart/2 +1, 
                              ymin = yav-0.4*ave.shift, ymax = yav+0.4*ave.shift), fill = "#EEEEEE") + 
    geom_text(data = del, aes(x = xstart/2, y = yav, label = ydiff), size = 3) +
    # plot the label for average
    geom_text(data = del, aes(x = xm-3, y = site.ave + ave.shift, 
                              label = sprintf("%.1f", site.ave)),
              size = 3) 
  print(p)
}


read.rate <- function(x){
  data.frame(N = nrow(x),
             ave.los = mean(x$LOS.without.ALC),
             site = x$Institution.Number[1],
             ave = sum(x$read.in.30, na.rm = T)/sum(!is.na(x$read.in.30), na.rm = T)*100)
}
plot.phy(cohort, 100, "Overall", ylab = "Re-admission (within 30 days) Rate (%)", ave.fun = read.rate)
plot.phy(cap.co, 20, "Pneumonia", ylab = "Re-admission (within 30 days) Rate (%)", ave.fun = read.rate)

