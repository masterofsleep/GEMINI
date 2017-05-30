# -------------------------- Significance plot function ------------------------


plot.phy <- function(data, title, xlab = "Physician", 
                     ylab, nextreme = 1,
                     ave.fun, xstart = -2, digit = 1, show.sig = F, varname = NULL){
  data <- hide_site(data)
  df <- ddply(data, ~physician, .fun = ave.fun) %>% data.table
  digitform <- paste("%.", digit, "f", sep = "")
  names(df)[4] <- "phy.ave"
  site.mean <- ddply(data[data$physician%in%df$physician, ], ~Institution.Number, .fun = ave.fun)
  names(site.mean)[4] <- "site.mean"
  df <- merge(df, site.mean[,c(1,4)], by.x = "site", by.y = "Institution.Number")
  data <- data.frame(data)
  for(i in unique(df$site)){
    df[site ==i, phy := as.numeric(factor(physician, levels = physician[order(phy.ave, decreasing = T)]))]
    for(j in unique(df[df$site==i, physician])){
      if(length(unique(data[, varname]))>3)
        df$var_sig[df$site==i&df$physician==j] <- 
          t.test(data[data$Institution.Number==i&data$physician==j, varname],
                 data[data$Institution.Number==i&data$physician!=j, varname]
          )$p.value
      else{
        
      }
    }
  }
  df$col = as.character(df$site)
  df$col[df$var_sig>=0.05] = "nonsig"
  mycol = as.character(c("A" = "#F8766D",
                         "B" = "#B79F00",
                         "C" = "#00BA38",
                         "D" = "#00BFC4",
                         "E" = "#619CFF",
                         "F" = "#A58AFF",
                         "G" = "#FB61D7",
                         "nosig" = "#AAAAAA"))
  mycolscale <- scale_fill_manual(name = "showsig", values = mycol)
  p <- ggplot(df, aes(phy, phy.ave, fill = col)) + 
    geom_bar(stat = "identity", width = 0.5) + 
    #geom_line(aes(x = phy, y = site.mean), alpha = 0.5,
    #          linetype = 2, size = 0.5) + 
    facet_grid(.~site, scales = "free_x") + 
    mycolscale +
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    expand_limits(x = xstart) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none") +
    scale_colour_manual(values = c("Grey" = ""))
  # del <- ddply(df, ~site, summarise,
  #              site.ave = sum(phy.ave * N)/sum(N),
  #              #site.ave = mean(phy.ave), # for patient sum only
  #              xm = max(phy),
  #              ymi = quantile(phy.ave, probs = 0.25),
  #              yma = quantile(phy.ave, probs = 0.75),
  #              yav = quantile(phy.ave, probs = 0.5),
  #              ydiff = sprintf(digitform , yma - ymi))
  # ave.shift <- max(df$phy.ave) * 0.02
  # p <- p + geom_errorbar(data = del, aes(x = xstart/2, y = NULL,ymin = ymi, ymax = yma), 
  #                        alpha = 0.3, width = 2) + 
  #   # plot the 25% - 75% range
  #   geom_rect(data = del, aes(x = NULL, y = NULL, xmin = xstart/2 - 1, xmax =  xstart/2 +1, 
  #                             ymin = yav-0.5*ave.shift, ymax = yav+0.5*ave.shift), fill = "#EEEEEE") + 
  #   geom_text(data = del, aes(x = xstart/2, y = yav, label = ydiff), size = 3) +
  #   # plot the label for average
  #   geom_text(data = del, aes(x = xm-2, y = site.ave + ave.shift, 
  #                             label = sprintf(digitform , site.ave)),
  #             size = 3) 
  print(p)
}

ave.los <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Acute.LoS, na.rm = T))
}
#png("ave.los_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort,  "Average Acute Length-of-Stay (Days)", 
         ylab = "Average Length-of-Stay (Days)", ave.fun = ave.los,
         show.sig = T,
         varname = "Acute.LoS")
dev.off()


ave.cost <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Cost, na.rm = T))
}

#png("ave.cost.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort,  "Average Cost ($)", 
         ylab = "Average Cost ($)", ave.fun = ave.cost, xstart = -5, digit = 0,
         varname = "Cost")
dev.off()