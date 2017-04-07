# -------------------- to administrators new -----------------------------------
# ------------------------ 2017-04-03 ------------------------------------------

smh <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/smh.link.csv")
sbk <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/sbk.link.csv")
uhn <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/uhn.link.csv")
msh <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/msh.link.csv")
thp <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/thp.link.csv")
exclude <- readg(gim, notgim)
list.fr <- readxl::read_excel("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.all.physician.check_FR.xlsx")%>%
  data.table
list.fr <- list.fr[GIM!="n"]
list.fr <- list.fr[!(str_detect(first.name, "Temp")|
                       str_detect(first.name, "Resident")|
                       str_detect(first.name, "Doctor"))]
list.fr[!is.na(`Same Name (Definite)`), code.new := `Same Name (Definite)`]
list.fr[!is.na(`Same Name (Possible)`), code.new := `Same Name (Possible)`]
list.fr[`Same Name (Definite)`!=`Same Name (Possible)`]
range(list.fr$code.new, na.rm = T)
sum(is.na(list.fr$code.new))
list.fr[is.na(code.new), code.new := 214:(214+570)]
list.fr[code.new%in%c(708, 383, 710, 596)]
all.name <- list.fr[,.(Code, code.type, last.name, first.name, code.new, GIM)] %>% unique

fwrite(all.name, "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.all.phycisian.new.code.csv")

find.new.code <- function(df, code.type.adm, code.type.mrp){
  df$adm.code = as.character(df$adm.code)
  df$dis.code = as.character(df$dis.code)
  df$mrp.code = as.character(df$mrp.code)
  df <- merge(df, all.name[code.type == code.type.adm, .(Code, code.new)], 
              by.x = "adm.code",by.y = "Code",
              all.x = T, all.y = F)
  df <- merge(df, all.name[code.type == code.type.adm, .(Code, code.new)], 
              by.x = "dis.code",by.y = "Code",
              all.x = T, all.y = F)
  df <- merge(df, all.name[code.type == code.type.mrp, .(Code, code.new, GIM)], 
              by.x = "mrp.code",by.y = "Code",
              all.x = T, all.y = F)
  names(df)[5:7] <- c("adm.code.new", "dis.code.new", "mrp.code.new")
  df
}
smh <- find.new.code(smh, "smh", "smh")
sum(smh$dis.code.new==smh$dis.code.new&smh$adm.code.new==smh$mrp.code.new, na.rm = T)
sbk <- find.new.code(sbk, "sbk", "sbk")
uhn <- find.new.code(uhn, "uhn.adm", "uhn.mrp")
msh <- find.new.code(msh, "msh", "msh")
thp <- find.new.code(thp, "thp", "thp")
sum(thp$mrp.code.new==thp$dis.code.new)
cohort <- rbind(smh, sbk, uhn, msh, thp, fill = T)[!EncID.new%in%exclude$EncID.new]
cohort <- cohort[(adm.code.new==dis.code.new&adm.code.new==mrp.code.new)] %>% unique
dad <- fread("H:/GEMINI/DataBackup/Data170214/UHN/CIHI/uhn.ip_dad.nophi.csv")

extra.enc <- dad[mdy(Discharge.Date)>ymd("20150331"), EncID.new]
cohort <- cohort[!EncID.new%in%extra.enc]
fwrite(cohort, "H:/GEMINI/Results/to.administrator/cohort.csv")



cohort <- fread("H:/GEMINI/Results/to.administrator/cohort.csv")
sum(duplicated(cohort$EncID.new))
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
fwrite(dad, "C:/Users/guoyi/Desktop/to.adm/design.paper.dad.csv")
cohort <- merge(cohort, unique(dad[,.(EncID.new, Age, Gender, Discharge.Disposition,
                     Number.of.ALC.Days, Institution.Number, Admit.Date, Admit.Time,
                     Discharge.Date, Discharge.Time, 
                     LOS.without.ALC, LoS, SCU.adm,
                     Cost, n.comorb, read.in.30)]), by = "EncID.new")
cci <- readg(gim, cci)
cohort <- merge(cohort, cci, by = "EncID.new", all.x = T)
setwd("H:/GEMINI/Results/to.administrator")

copd <- fread("qbp.copd.csv")
cap <- fread("qbp.cap.csv")
uti <- fread("qbp.uti.csv")
stroke <- fread("qbp.stroke.csv")
chf <- fread("qbp.chf.csv")

cohort[,':='(copd = EncID.new%in%copd$EncID.new,
             cap = EncID.new%in%cap$EncID.new,
             chf = EncID.new%in%chf$EncID.new,
             stroke = EncID.new%in%stroke$EncID.new)]

#palliative care
xfer.smh <- readg(smh, xfer)
palli <- xfer.smh[Unit.Code == 7]
palli <- palli[!duplicated(EncID.new)]
cohort <- merge(cohort, palli[,.(EncID.new = as.integer(EncID.new), Date.Check.in, Time.Check.in)],
                 by = "EncID.new", all.x = T)
cohort[!is.na(Date.Check.in), ":="(LOS.without.ALC = as.numeric( 
                                     ymd_hm(paste(Date.Check.in, Time.Check.in)) - 
                                     ymd_hm(paste(Admit.Date, Admit.Time)))/(3600*24)
                                   )]
#why most of patients had several xfer to palliative care
# #ofxfer to palliative care 1   2   3   4   5 
# # of patients              60  92  16  10   2 

cohort$physician <- paste(cohort$Institution.Number, cohort$mrp.code.new, sep = "-")


# check balance
library(lme4)
mod1 <- lmer(Age ~ (physician|Institution.Number), data = cohort)
summary(mod1)

mod2 <- glmer(Gender ~ (physician|Institution.Number), data = cohort, family = binomial)

all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.all.phycisian.new.code.csv")
all.name <- all.name[!duplicated(code.new)]
table(cohort$Institution.Number)

fwrite(cohort, "C:/Users/guoyi/Desktop/to.adm/cohort.csv")
fwrite(all.name, "C:/Users/guoyi/Desktop/to.adm/all.name.csv")



# fix readmission 
dad <- fread("C:/Users/guoyi/Desktop/to.adm/design.paper.dad.csv")
table(dad$read.in.30, useNA = "ifany")
cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv")
cohort[, read.in.30 := NULL]
cohort <- merge(cohort, dad[,.(EncID.new, read.in.30)], by = "EncID.new",
                all.x = T, all.y = F)
fwrite(cohort, "C:/Users/guoyi/Desktop/to.adm/cohort.csv")

# calculate adjusted readmission rate and cost
mod.cost <- lm(Cost ~ Institution.Number + LoS, cohort[!is.na(Cost)])
summary(mod.cost)
adj.cost <- predict(mod.cost, 
                    newdata = data.frame(
                      LoS = mean(cohort$LoS),
                      Institution.Number = cohort$Institution.Number[!is.na(cohort$Cost)]))
adj.cost <- adj.cost + mod.cost$residuals
cohort[!is.na(Cost), adj.cost:= adj.cost]
fwrite(cohort, "C:/Users/guoyi/Desktop/to.adm/cohort.csv")



dad <- fread("C:/Users/guoyi/Desktop/to.adm/design.paper.dad.csv")



cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv")

all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]

plot.phy <- function(data, leastpatient, title, xlab = "physician", 
                     ylab, nextreme = 1,
                     ave.fun){
  df <- ddply(data, ~physician, .fun = ave.fun) %>% data.table
  names(df)[4] <- "phy.ave"
  df <- df[N>=leastpatient]
  site.mean <- ddply(data[physician%in%df$physician], ~Institution.Number, .fun = ave.fun)
  names(site.mean)[4] <- "site.mean"
  df <- merge(df, site.mean[,c(1,4)], by.x = "site", by.y = "Institution.Number")
  for(i in c("smh", "sbk", "uhn-general", "uhn-western" ,"msh", "thp-c", "thp-m")){
    df[site ==i, phy := as.numeric(factor(physician, levels = physician[order(phy.ave, decreasing = T)]))]
  }
  xstart = -log(max(df$phy.ave))-log(max(df$phy), base = 10) + 1
  p <- ggplot(df, aes(phy, phy.ave, fill = site)) + 
    geom_bar(stat = "identity", width = 0.5) + 
    geom_line(aes(x = phy, y = site.mean), alpha = 0.5,
              linetype = 2, size = 0.5) + 
    facet_grid(.~site, scales = "free_x") + 
    ggtitle(title) +
    xlab(xlab) +
    ylab(ylab) +
    expand_limits(x = xstart) +
    geom_text(aes(x = xstart/2, y = site.mean, label = sprintf("%.1f", site.mean)), colour = "#FF6666",
              size = 3, alpha = 0.3) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="none")
  del <- ddply(df, ~site, summarise,
               xm = max(phy),
               ymi = quantile(phy.ave, probs = 0.1),
               yma = quantile(phy.ave, probs = 0.9),
               yav = quantile(phy.ave, probs = 0.1)*0.5 + quantile(phy.ave, probs = 0.9)*0.5,
               ydiff = sprintf("%.1f", yma - ymi))
  p <- p + geom_errorbar(data = del, aes(x = 0, y = NULL,ymin = ymi, ymax = yma), 
                         alpha = 0.3, width = 2) +
    geom_rect(data = del, aes(x = NULL, y = NULL, xmin = -0.8, xmax =  + 0.8, 
                              ymin = yav*0.98, ymax = yav*1.02), fill = "#EEEEEE") + 
    geom_text(data = del, aes(x = 0, y = yav, label = ydiff), size = 3)
  # df <- df %>% arrange(site, phy)
  # extreme.values <- ddply(df, ~site, 
  #                         function(x)rbind(head(x, nextreme), tail(x,nextreme)))
  # extreme.values$code.new <- str_replace_all(extreme.values$physician, "[:alpha:]|-", "")
  # extreme.values <- merge(extreme.values, all.name[,.(code.new, first.name, last.name)],
  #                         by.x = "code.new", by.y = "code.new")
  # extreme.values <- extreme.values %>% arrange(site, phy)
  # 
  print(p)
  #print(extreme.values[,c(1,2,4,5,7:9)])
}

# ------------------------------- average length of stay -----------------------

ave.los <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$LOS.without.ALC, na.rm = T))
}
plot.phy(cohort, 100, "Overall", ylab = "Average Length-of-Stay (Days)", ave.fun = ave.los)
plot.phy(cohort[cap==T], 20, "Pneumonia", ylab = "Average Length-of-Stay (Days)", ave.fun = ave.los)
plot.phy(cohort[chf==T], 20, "CHF", ylab = "Average Length-of-Stay (Days)", ave.fun = ave.los)
plot.phy(cohort[copd==T], 20, "COPD", ylab = "Average Length-of-Stay (Days)", ave.fun = ave.los)
plot.phy(cohort[stroke==T], 20, "Stroke", ylab = "Average Length-of-Stay (Days)", ave.fun = ave.los)

# ---------------------------average alc days ----------------------------------
ave.alc <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Number.of.ALC.Days, na.rm = T))
}
plot.phy(cohort, 100, "Overall", ylab = "Average ALC Days", ave.fun = ave.alc)
plot.phy(cohort[cap==T], 20, "Pneumonia", ylab = "Average ALC Days", ave.fun = ave.alc)
plot.phy(cohort[chf==T], 20, "CHF", ylab = "Average ALC Days", ave.fun = ave.alc)
plot.phy(cohort[copd==T], 20, "COPD", ylab = "Average ALC Days", ave.fun = ave.alc)
plot.phy(cohort[stroke==T], 20, "Stroke", ylab = "Average ALC Days", ave.fun = ave.alc)


# ---------------------------- average cost -------------------------------------


ave.cost <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Cost, na.rm = T))
}
plot.phy(cohort, 100, "Overall", ylab = "Average Cost ($)", ave.fun = ave.cost)
plot.phy(cohort[cap==T], 20, "Pneumonia", ylab = "Average Cost ($)", ave.fun = ave.cost)
plot.phy(cohort[chf==T], 20, "CHF", ylab = "Average Cost ($)", ave.fun = ave.cost)
plot.phy(cohort[copd==T], 20, "COPD", ylab = "Average Cost ($)", ave.fun = ave.cost)
plot.phy(cohort[stroke==T], 20, "Stroke", ylab = "Average Cost ($)", ave.fun = ave.cost)


# ----------------------------- readmission rate -------------------------------
read.rate <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = sum(x$read.in.30, na.rm = T)/sum(!is.na(x$read.in.30), na.rm = T)*100)
}
plot.phy(cohort, 100, "Overall", ylab = "Re-admission (within 30 days) Rate (%)", ave.fun = read.rate)
plot.phy(cohort[cap==T], 20, "Pneumonia", ylab = "Re-admission (within 30 days) Rate (%)", ave.fun = read.rate)
plot.phy(cohort[chf==T], 20, "CHF", ylab = "Re-admission (within 30 days) Rate (%)", ave.fun = read.rate)
plot.phy(cohort[copd==T], 20, "COPD", ylab = "Re-admission (within 30 days) Rate (%)", ave.fun = read.rate)
plot.phy(cohort[stroke==T], 20, "Stroke", ylab = "Re-admission (within 30 days) Rate (%)", ave.fun = read.rate)

# ----------------------------- mortality rate ---------------------------------
mort <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$Discharge.Disposition ==7, na.rm = T)*100)
}

plot.phy(cohort, 100, "Overall", ylab = "In-hospital Mortality (%)", ave.fun = mort)
plot.phy(cohort[cap==T], 20, "Pneumonia", ylab = "In-hospital Mortality (%)", ave.fun = mort)
plot.phy(cohort[chf==T], 20, "CHF", ylab = "In-hospital Mortality (%)", ave.fun = mort)
plot.phy(cohort[copd==T], 20, "COPD", ylab = "In-hospital Mortality (%)", ave.fun = mort)
plot.phy(cohort[stroke==T], 20, "Stroke", ylab = "In-hospital Mortality (%)", ave.fun = mort)

# ------------------------------ short admission rate --------------------------
shortadm <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$LOS.without.ALC < 2, na.rm = T)*100)
}
plot.phy(cohort, 100, "Overall", ylab = "Short-Admission (<48h) Rate (%)", ave.fun = shortadm)
plot.phy(cohort[cap==T], 20, "Pneumonia", ylab = "Short-Admission (<48h) Rate (%)", ave.fun = shortadm)
plot.phy(cohort[chf==T], 20, "CHF", ylab = "Short-Admission (<48h) Rate (%)", ave.fun = shortadm)
plot.phy(cohort[copd==T], 20, "COPD",ylab = "Short-Admission (<48h) Rate (%)", ave.fun = shortadm)
plot.phy(cohort[stroke==T], 20, "Stroke", ylab = "Short-Admission (<48h) Rate (%)", ave.fun = shortadm)


# ----------------------------- Rate of ICU admission --------------------------
icuadm<- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = mean(x$SCU.adm, na.rm = T)*100)
}
plot.phy(cohort, 100, "Overall", ylab = "ICU Utilization Rate(%)", ave.fun = icuadm)
plot.phy(cohort[cap==T], 20, "Pneumonia", ylab = "ICU Utilization Rate(%)", ave.fun = icuadm)
plot.phy(cohort[chf==T], 20, "CHF", ylab = "ICU Utilization Rate(%)", ave.fun = icuadm)
plot.phy(cohort[copd==T], 20, "COPD",ylab = "ICU Utilization Rate(%)", ave.fun = icuadm)
plot.phy(cohort[stroke==T], 20, "Stroke", ylab = "ICU Utilization Rate(%)", ave.fun = icuadm)





## models to check ICC
## takes forever to run, do it in the night
n.patient <- cohort[,.N, by = physician]
library(lme4)
mod1 <- lmer(Age ~ (physician|Institution.Number), data = cohort[physician%in%n.patient[N>=100, physician]])
