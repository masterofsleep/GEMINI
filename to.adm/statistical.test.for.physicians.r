# --------------- statistical tests for individual physicians ------------------
library(gemini)
lib.pa()
cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv")
all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]
n.pat <- cohort[,.N, by = physician]
cohort <- cohort[physician%in%n.pat[N>=100, physician]]
cohort <- cohort[LOS.without.ALC<=30]

ave.phy <- function(x){
  phy.ave = NULL
  site.ave = NULL
  p.value = NULL
  for(i in unique(x$physician)){
    phy = x$LOS.without.ALC[x$physician == i]
    other = x$LOS.without.ALC[x$physician != i]
    phy.ave <- c(phy.ave, mean(phy))
    site.ave <- c(site.ave, mean(other))
    p.value <- c(p.value, t.test(phy, other)$p.value)
  }
  data.frame(physician = unique(x$physician),
             phy.ave, site.ave, p.value)
}
ave.phy(x)

df <- ddply(cohort, ~Institution.Number, .fun = ave.phy) %>% data.table
for(i in unique(df$Institution.Number)){
  df[Institution.Number ==i, phy := as.numeric(factor(physician, levels = physician[order(phy.ave, decreasing = T)]))]
}
ggplot(df, aes(phy, phy.ave, fill = factor(p.value>=0.05))) + 
  geom_bar(stat = "identity", width = 0.5) +
  facet_grid(~Institution.Number, scales = "free_x") +
  theme(legend.position = "None")
