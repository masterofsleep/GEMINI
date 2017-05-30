# ----------------------- patient volumn visualization -------------------------

library(gtable)
library(grid)
library(extrafont)
extrafont::loadfonts(device="win")
library(ggplot2)
library(gemini)
lib.pa()
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")


fonts() # view available fonts
loadfonts()

site = "SMH"
df <- rbind(dad[,.(.N, Site = "Overall"), by = fiscal.year],
            dad[Institution.Number==site,.(.N, Site = site), 
                by = fiscal.year])
      
site_vol <- dad[Institution.Number==site,.(.N, Site = site), 
                by = fiscal.year]
all_vol <- dad[,.(.N, Site = "Overall"), by = fiscal.year]



p1 <- ggplot(site_vol, aes(fiscal.year, N)) + 
  geom_line(colour = "#68382C",size = 1.5) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(colour="#68382C", size = 14),
        axis.text.x = element_text(size = 14),
        panel.background = element_blank(),
        axis.ticks.length = unit(.25, "cm"),
        axis.ticks.y = element_blank(),
        text = element_text(family=font),
        plot.title = element_text(hjust = -0.006, vjust=2.12, colour="#68382C", 
                                  size = 14, family = font)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.9, 5000.9)) +
  ggtitle(paste("Number at", site, "\n")) + labs(x = NULL, y = NULL) +
  geom_text(aes(label = N), vjust = -1, color = "#68382C", family = font)
p1
p2 <- ggplot(all_vol, aes(fiscal.year, N)) + 
  geom_line(colour = "#00A4E6",size = 1.5) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank(),
        axis.text.y = element_text(colour="#00A4E6", size = 14),
        axis.text.x = element_text(size = 14),
        panel.background = element_blank(),
        axis.ticks.length = unit(.25, "cm"),
        axis.ticks.y = element_blank(),
        text = element_text(family=font),
        plot.title = element_text(hjust = 0.88, vjust=2.12, colour="#00A4E6", 
                                  size = 14, family = font)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.9, 50000.9)) +
  ggtitle(paste("Number of GEMINI\n")) + labs(x = NULL, y = NULL) +
  geom_text(aes(label = N), vjust = 1.5, color = "#00A4E6", family = font)

p2

dual_plot <- function (){
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  hinvert_title_grob <- function(grob){
    
    # Swap the widths
    widths <- grob$widths
    grob$widths[1] <- widths[3]
    grob$widths[3] <- widths[1]
    grob$vp[[1]]$layout$widths[1] <- widths[3]
    grob$vp[[1]]$layout$widths[3] <- widths[1]
    
    # Fix the justification
    grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
    grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
    grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
    grob
  }
  index <- which(g2$layout$name == "axis-l")  # Which grob
  yaxis <- g2$grobs[[index]]  
  ticks <- yaxis$children[[2]]
  ticks$widths <- rev(ticks$widths)
  ticks$grobs <- rev(ticks$grobs)
  plot_theme <- function(p) {
    plyr::defaults(p$theme, theme_get())
  }
  tml <- plot_theme(p1)$axis.ticks.length   # Tick mark length
  #ticks$grobs[[1]] <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
  yaxis$children[[2]] <- ticks
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  g1 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  left = textGrob("Number at SMH", x = 0, y = 0.9, just = c("left", "top"), 
                  gp = gpar(fontsize = 14, col =  "#68382C", fontfamily = font))
  right =  textGrob("Number of GEMINI", x = 1, y = 0.9, just = c("right", "top"), 
                    gp = gpar(fontsize = 14, col =  "#00a4e6", fontfamily = font))
  labs = gTree("Labs", children = gList(left, right))
  
  # New row in the gtable for labels
  height = unit(3, "grobheight", left)
  g1 <- gtable_add_rows(g1, height, 2)  
  
  # Put the label in the new row
  g1 = gtable_add_grob(g1, labs, t=3, l=3, r=5)
  
  # Turn off clipping in the plot panel
  g1$layout[which(g1$layout$name == "panel"), ]$clip = "off"
  
  # Print to PDF
  getwd()
  
  plot(g1)
}
dual_plot()
Site = "SMH"
font = "Britannic Bold"
plot_volumn <- function(Site, ontop = 1, font = "Trebuchet MS"){
  df <- rbind(dad[,.(.N, site = "ALL"), by = fiscal.year], 
              dad[Institution.Number==Site,.(.N, site = Site), by = fiscal.year])
  df <- merge(df, ddply(df, ~site, function(x) x[x$fiscal.year==2010, "N"]), by = "site")
  df$V2 <- df$N/df$V1
  ontop = ontop * 0.08
  maxy <-  ceiling(max(df$V2))
  mycol = c("#00A4E6", "#D95F0E")
  ggplot(df, aes(x = fiscal.year, y = V2, colour = site)) + geom_line(size = 1.4) +
    geom_point(size = 2) +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank(),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(family=font)) +
    scale_y_continuous(expand = c(0, 0), limits = c(-0.009, (maxy + 0.09)),
                       labels = scales::percent) +
    labs(x = "Fiscal Year", y = "Percent Change (%)") +
    scale_colour_manual(values = mycol) +
    geom_text(data = df[site=="ALL"], aes(x = fiscal.year, y = V2 -ontop,
                                          label = N), family = font) +
    geom_text(data = df[site==Site], aes(x = fiscal.year, y = V2 + ontop,
                                         label = N), family = font)
}
plot_volumn("SHS")
plot_volumn("SMH", -1)
plot_volumn("SHSC", -1)

plot_volumn("UHN-TG")
plot_volumn("UHN-TW", -1)
plot_volumn("THP-M")
plot_volumn("THP-C")
bi_order <- c(1, 1, -1, 1, 1, -1, -1)
for(i in 1:7){
  site = unique(dad$Institution.Number)[i]
  target <- paste("C:/Users/guoyi/Desktop/to.adm/figures.v4/patient_volume/", 
                  site, ".png", sep = "")
  png(target, res = 200, width = 1600, height = 1000)
  print(plot_volumn(site, bi_order[i]))
  dev.off()
}
i = 1
