# ----------------- Simulated data for GEMINI Portal demo ----------------------

simu_phy_sum <- data.frame(
  physician = letters[1:10],
  N = sample(c(100:1500), 10),
  ave.los = seq(3.5, 5.5, 0.201) + rnorm(10)/10,
  read.rate = seq(10, 16, 0.601) + rnorm(10))

simu_phy_sum$color <- 0
simu_phy_sum$color[4] <- 1


demo_plot <- function(xstart, fondsize)
setwd("C:/Users/guoyi/Desktop/to.adm/portal_demo")
png("los.png", res = 150, width = 1000, height = 600)
ggplot(simu_phy_sum, aes(physician[order(ave.los, decreasing = T)],
                         ave.los,
                         fill = factor(color))) + 
  geom_bar(stat = "identity", width = 0.5) +
  expand_limits(x = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none") +
  xlab("Physician") + 
  ylab("Average Acute Length-of-Stay") +
  geom_hline(aes(yintercept = 4.3), alpha = 0.5,
             linetype = 2, size = 1) 
  # geom_errorbar(aes(x = 0.4, y = NULL,ymin = 3.8, ymax = 4.86), 
  #                 alpha = 0.3, width = 0.5) +
  # # plot the 25% - 75% range
  # geom_rect(aes(x = NULL, y = NULL, xmin = 0, xmax =  0.5, 
  #                           ymin = 4.1, ymax = 4.5), fill = "#FFFFFF") + 
  # geom_text(aes(x = 0.4, y = 4.3, label = "0.4"), size = 3) +
  # # plot the label for average
  # geom_text(aes(x = 10, y = 4.5,label = "4.3"),size = 3) 
  dev.off()
  
  
patient_los <- rexp(simu_phy_sum$N[4], 1/simu_phy_sum$ave.los[4])
patient_los <- data.frame(patient_id = 1:length(patient_los),
                          los = patient_los)
ggplot(patient_los, aes(los)) + 
  geom_histogram(binwidth = 0.5, fill = "#5DD0D8", color = "#555555")  + 
  theme_bw() +
  xlab("Acute Length-of-Stay") + 
  ylab("Count")
