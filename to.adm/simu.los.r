# --------------------------- test balance -------------------------------------
library(gemini)
lib.pa()

cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv", colClasses = list(character = "EncID.new"))
all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]
n.pat <- cohort[,.N, by = physician]
cohort.100p <- cohort[physician%in%n.pat[N>=100, physician]]

ave.los <- ddply(cohort.100p, ~physician, summarize,
                 site = Institution.Number[1],
                 ave.los = mean(LOS.without.ALC)) %>% data.table
for(i in unique(ave.los$site)){
  ave.los[site ==i, ':='(phy = as.numeric(factor(physician, levels = physician[order(ave.los, decreasing = T)])),
                         avelos.group = cut(ave.los, breaks=quantile(ave.los, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                                            include.lowest=TRUE,
                                            labels = 1:4))]
}

cohort.100p <- merge(cohort.100p, ave.los[,.(avelos.group, physician)],
                     by = "physician")

cohort.100p <- cohort.100p[LOS.without.ALC>0]

smh.g1 <- cohort.100p[Institution.Number=="smh"&avelos.group==1]
smh.g4 <- cohort.100p[Institution.Number=="smh"&avelos.group==4]

StatQq_1 <- ggproto("StatQq", Stat,
                    default_aes = aes(y = ..sample.., x = ..theoretical..),
                    
                    required_aes = c("sample"),
                    
                    compute_group = function(data, scales, quantiles = NULL,
                                             distribution = stats::qnorm, dparams = list(),
                                             na.rm = FALSE) {
                      
                      sample <- sort(data$sample)
                      n <- length(sample)
                      
                      # Compute theoretical quantiles
                      if (is.null(quantiles)) {
                        quantiles <- stats::ppoints(n)
                      } else {
                        stopifnot(length(quantiles) == n)
                      }
                      
                      theoretical <- do.call(distribution, c(list(p = quote(quantiles)), dparams))
                      
                      data.frame(sample, theoretical)
                    }
)



ggplot(smh.g4) + stat_qq(aes(x = smh.g1$LOS.without.ALC, sample = LOS.without.ALC))
sx <- sort(smh.g4$LOS.without.ALC)
sy <- sort(smh.g1$LOS.without.ALC)
lenx <- length(sx)
leny <- length(sy)
if (leny < lenx) 
  sx <- approx(1L:lenx, sx, n = leny)$y
if (leny > lenx) 
  sy <- approx(1L:leny, sy, n = lenx)$y
if (plot.it) 
  plot(sx, sy, xlab = xlab, ylab = ylab)
invisible(list(x = sx, y = sy))



A <- ggproto("A", NULL,
             x = 1,
             inc = function(self) {
               self$x <- self$x + 1
             }
)
A$x
