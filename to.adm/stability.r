# --------------------- stability figures --------------------------------------
cohort <- find_cohort()
cohort[, fiscal.year.group := ifelse(ymd(Discharge.Date)<=ymd("2012-10-01"),
                                     1,
                                     2)]

phy.sum.by.year <- ddply(cohort, ~physician + fiscal.year.group, function(x)
  data.frame(N = nrow(x),
             code.new = x$mrp.code.new[1],
             GIM = x$mrp.GIM[1],
             site = x$Institution.Number[1],
             n.patient = nrow(x)/length(unique(x$physician)),
             ave.acute.los = mean(x$Acute.LoS, na.rm = T),
             med.los = median(x$Acute.LoS, na.rm = T),
             # ave.adj.los = mean(x$adj_acute_los, na.rm = T),
             # med.adj.los = median(x$adj_acute_los, na.rm = T),
             ave.alc = mean(x$Number.of.ALC.Days, na.rm = T),
             read.rate = sum(x$read.in.30, na.rm = T)/sum(!is.na(x$read.in.30), na.rm = T)*100,
             mortality = mean(x$Discharge.Disposition ==7, na.rm = T)*100,
             short.adm = mean(x$Acute.LoS < 2, na.rm = T)*100,
             icu.rate = mean(x$SCU.adm, na.rm = T)*100,
             cbc.per.patientday = sum(x$n.bloodtest)/sum(x$Acute.LoS),
             trans.with.prehgb80.per1000patient = sum(x$N.pre.tran.hgb.gt80)/nrow(x)*1000,
             aki.rate = sum(x$aki, na.rm = T)/nrow(x)*100,
             ave.nrad = mean(x$N.rad, na.rm = T),
             ave.cost = mean(x$Cost, na.rm = T))) %>% data.table

stab.site <- ddply(cohort, ~fiscal.year.group + Institution.Number, function(x)
  data.frame(N = nrow(x),
             code.new = x$mrp.code.new[1],
             GIM = x$mrp.GIM[1],
             site = x$Institution.Number[1],
             n.patient = nrow(x)/length(unique(x$physician)),
             ave.acute.los = mean(x$Acute.LoS, na.rm = T),
             med.los = median(x$Acute.LoS, na.rm = T),
             # ave.adj.los = mean(x$adj_acute_los, na.rm = T),
             # med.adj.los = median(x$adj_acute_los, na.rm = T),
             ave.alc = mean(x$Number.of.ALC.Days, na.rm = T),
             read.rate = sum(x$read.in.30, na.rm = T)/sum(!is.na(x$read.in.30), na.rm = T)*100,
             mortality = mean(x$Discharge.Disposition ==7, na.rm = T)*100,
             short.adm = mean(x$Acute.LoS < 2, na.rm = T)*100,
             icu.rate = mean(x$SCU.adm, na.rm = T)*100,
             cbc.per.patientday = sum(x$n.bloodtest)/sum(x$Acute.LoS),
             trans.with.prehgb80.per1000patient = sum(x$N.pre.tran.hgb.gt80)/nrow(x)*1000,
             aki.rate = sum(x$aki, na.rm = T)/nrow(x)*100,
             ave.cost = mean(x$Cost, na.rm = T),
             ave.nrad = mean(x$N.rad, na.rm = T))) %>% data.table


# keep only with > 100 patients per year
# phy.sum.by.year.100 <- phy.sum.by.year[N>=100]
# stab.phy <- phy.sum.by.year[physician%in%phy.sum.by.year.100[, .N, by = physician][N==2, physician]]

# keep only with > 200 patients in group1 and > 300 in group2
stab.phy <- phy.sum.by.year[
  physician%in% intersect(phy.sum.by.year[fiscal.year.group==1&N>=100, physician],
                          phy.sum.by.year[fiscal.year.group==2&N>=100, physician])]



plot_phy_stab <- function(Site, Var, ylab = "NULL"){
  stab.phy <- data.frame(stab.phy)
  stab.site <- data.frame(stab.site)
  stab.site$OVERALL = stab.site[, Var]
  df <- merge(stab.phy[, c("physician", "fiscal.year.group", Var, "site")],
              stab.site[, c("site", "OVERALL", "fiscal.year.group")], 
              by = c("site", "fiscal.year.group"))
  df$var = df[, Var]
  df.wide <- merge(df[df$fiscal.year.group==1, c("site", "physician", "var")],
                   df[df$fiscal.year.group==2, c("site", "physician", "var")],
                   by = c("site", "physician"))
  var.cor <- ddply(df.wide, ~site, summarize,
                   correlation = cor(var.x, var.y))
  df <- merge(df, var.cor)
  for(i in unique(df$site)){
    df$phy[df$site==i] <- as.numeric(factor(df$phy[df$site==i]))
  }
  ggplot(df[df$site==Site, ],
         aes(x = fiscal.year.group, y = var, colour = phy)) +
    geom_point(size = 0.5) +
    geom_line(size = 1, alpha = 0.5) +
    geom_line(aes(x = fiscal.year.group, y = OVERALL), 
              color = "#999999", size = 3, alpha = 0.8) +
    theme_bw() +
    xlab("Fiscal Year") + 
    ylab(ylab) +
    scale_x_continuous(breaks = c(0, 1, 2)) +
    facet_wrap(~site) +
    theme(legend.position = "none")
  # geom_text(data = df, aes(x = 1, y = 3, color = "#444444",
  #                          label = sprintf("%.2f", correlation)),
  #           color = "#444444")
}


dev.off()


Var = "ave.acute.los"








setwd("C:/Users/guoyi/Desktop/to.adm/figures.v4/stability")
for(i in c("SMH", "SHSC", "SHS", "UHN-TW", "UHN-TG", "THP-M")){
  for(j in c("ave.acute.los", "ave.cost", "read.rate", "mortality", "ave.nrad",
             "aki.rate")){
    title <- paste(i,"_", j, ".png", sep = "")
    png(title, res = 250, width = 2000, height = 1200)
    print(plot_phy_stab(i, j, ylab = paste(i, j, ylab = j)))
    dev.off()
  }
}

plot_phy_stab("SMH", "ave.acute.los", "Average Acute Length-of-Stay")
#plot_phy_stab("SMH", "med.adj.los")
#plot_phy_stab("SMH", "ave.adj.los")





plot_phy_stab("SMH", "ave.alc")
setwd("C:/Users/guoyi/Desktop/to.adm/figures.v4/stability")
for(i in unique(stab.phy$site)){
  for(j in names(stab.phy)[8:17]){
    title <- paste(i, "_", j, ".png", sep = "")
    png(title, res = 250, width = 2000, height = 1200)
    print(plot_phy_stab(i, j))
    dev.off()
  }
  
}




# median los by year
rbind(stab.phy[,c("physician", "fiscal.year.group", "med.los", "site")])
med.los.by.site <- ddply(cohort, ~fiscal.year.group + Institution.Number, summarize,
                         med.los  = median(Acute.LoS, na.rm = T))
med.los.by.site$site = med.los.by.site$Institution.Number
med.los.by.site$OVERALL <- med.los.by.site$med.los
med.los <- merge(stab.phy[,c("physician", "fiscal.year.group", "med.los", "site")],
                 med.los.by.site[, c("fiscal.year.group", "OVERALL", "site")],
                 by = c("site", "fiscal.year.group"))
# a function that produces similar type of figures for any variable
plot_medlos <- function(Site){
  ggplot(med.los[site==Site, ],
         aes(x = fiscal.year.group, y = med.los, colour = physician, size = Size)) +
    geom_point(size = 0.5) +
    geom_line(size = 1.5, alpha = 0.5) +
    geom_line(aes(x = fiscal.year.group, y = OVERALL), 
              color = "#999999", size = 3, alpha = 0.8) +
    theme_bw() +
    xlab("Fiscal Year") + 
    ylab("Median Acute Length-of-Stay") +
    scale_x_continuous(breaks = c(0, 1, 2)) 
}

setwd("C:/Users/guoyi/Desktop/to.adm/figures.v4/stability/by_site")
setwd("C:/Users/guoyi/Desktop/to.adm/figures.v4/stability/by_site/adjusted")
for(i in c("SMH", "SHSC", "SHS", "UHN-TW", "UHN-TG")){
  title <- paste(i, "_median_los.png", sep = "")
  png(title, res = 250, width = 2000, height = 1200)
  print(plot_medlos(i))
  dev.off()
}
i = "SMH"










med.los <- merge(stab.phy[fiscal.year.group==1, .(physician, med.los, site)],
                 stab.phy[fiscal.year.group==2, .(physician, med.los)],
                 by = "physician")

ddply(med.los, ~site, summarise,
      pearson.cor = cor(med.los.x, med.los.y),
      pearson.cor.p = cor.test(med.los.x, med.los.y)$p.value,
      spearman.cor = cor(med.los.x, med.los.y, method = "spearman"),
      spearman.cor.p = cor.test(med.los.x, med.los.y, method = "spearman")$p.value) %>%
  fwrite("C:/Users/guoyi/Desktop/to.adm/med.los.correlation.by.fiscal.year.group.csv")


ggplot(med.los, aes(med.los.x, med.los.y, color = site)) + geom_point()
