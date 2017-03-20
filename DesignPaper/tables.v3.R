# ------------------------------------------------------------------------------
# -------------------- Design Paper Tables and Figures v3.0 -------------------- 
# --------------------------------- 2017-03-10 ---------------------------------

library(gemini)
lib.pa()
round1 <- function(x){
  sprintf("%.1f", round(x, 1))
}

round0 <- function(x){
  sprintf("%d", round(x, 0))
}


me.iqr <- function(x){
  paste(round1(median(x)), " (", round1(quantile(x)[2]), ", ",
        round1(quantile(x)[4]), ")", sep = "")
}

me.iqr0 <- function(x){
  paste(round0(median(x)), " (", round0(quantile(x)[2]), ", ",
        round0(quantile(x)[4]), ")", sep = "")
}

cat.prop <- function(x){
  cbind(table(x), 
        round1(table(x)/length(x)*100))
}

cat1.prop <- function(x, cat){
  paste(sum(x==cat), " (", 
        round1(sum(x==cat)/length(x)*100), ")", sep = "")
}


dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
# dad$xray <- dad$EncID.new%in%xray.enc
# dad[str_sub(EncID.new, 1, 2)=="15", xray:=NA]
# dad$ir <- dad$EncID.new%in%ir.enc
# dad[str_sub(EncID.new, 1, 2)=="15", ir:=NA]
# dad$dialysis <- dad$EncID.new%in%dia.enc
# dad$blood.trans <- dad$EncID.new%in%trans.enc
# dad[str_sub(EncID.new, 1, 2)%in%c("12","14", "15"), blood.trans:=NA]
# fwrite(dad, "H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad[, read.in.30 := ifelse(ymd(Discharge.Date)<ymd("2010-05-01"), NA,
                           ifelse(time.since.last.admission<=30&!is.na(time.since.last.admission), T, 
                                  ifelse(time.since.last.admission>30|is.na(time.since.last.admission), F, F)))]


#table 1
dad[, age.cat:= ifelse(Age<60, "<60", ifelse(Age>80, ">80", "60-80"))]
me.iqr(as.numeric(dad$Age))
cat.prop(dad$age.cat)
cat.prop(dad$Gender)
me.iqr(dad$n.comorb)
cat.prop(dad$Charlson.Comorbidity.Index)
cat.prop(dad$Discharge.Disposition)



#table 2
ddply(dad, ~fiscal.year, summarize,
      number.of.hospitalization = length(unique(EncID.new)),
      length.of.stay = paste(round1(quantile(LoS)[2:4]), collapse = ", "),
      # readmission.within.30.days = paste(sum(time.since.last.admission<=30, na.rm = T), ", ", 
      #                                    round1(sum(time.since.last.admission<=30, na.rm = T)/
      #                                            length(time.since.last.admission[
      #                                              ymd(Admit.Date)>=ymd("2010-05-01")]<=30)*100),
      #                                    sep = ""),
      readmission.within.30.days = cat1.prop(read.in.30[!is.na(read.in.30)], TRUE),
      Cost = paste(round1(quantile(Cost, na.rm = T)[2:4]), collapse = ", "),
      US.CT.MRI = paste(sum(ctmrius, na.rm = T), ", ", 
                        round1(sum(ctmrius, na.rm = T)/sum(!is.na(ctmrius))*100),
                        sep = ""),
      US = paste(sum(us, na.rm = T), ", ", 
                 round1(sum(us, na.rm = T)/sum(!is.na(us))*100),
                 sep = ""),
      CT = paste(sum(ct, na.rm = T), ", ", 
                 round1(sum(ct, na.rm = T)/sum(!is.na(ct))*100),
                 sep = ""),
      MRI = paste(sum(mri, na.rm = T), ", ", 
                  round1(sum(mri, na.rm = T)/sum(!is.na(mri))*100),
                  sep = "")
)-> table2

dad[, read.in.30 := ifelse(ymd(Discharge.Date)<ymd("2010-05-01"), NA,
                           ifelse(time.since.last.admission<=30&!is.na(time.since.last.admission), T, 
                                  ifelse(time.since.last.admission>30|is.na(time.since.last.admission), F, F)))]
dad[is.na(read.in.30), Discharge.Date] %>% ymd


table2 <- table2 %>% t %>% data.table
table2 <- table2[2:16, ]
names(table2) <- 2010:2014
fwrite(table2, "H:/GEMINI/Results/DesignPaper/table2.new.march10.csv", row.names = T)


library(lmtest)
n.hos <- table2[,2]
years <- 2010:2014
lrtest(lm(n.hos ~ years))
summary(lm(n.hos ~ years))

lm(Cost ~ fiscal.year, dad) %>% summary
lm(LoS ~ fiscal.year, dad) %>% summary
lrtest(glm(read.in.30 ~ fiscal.year, dad, family = binomial))
glm(read.in.30 ~ fiscal.year, dad, family = binomial) %>% summary
lrtest(glm(ctmrius ~ fiscal.year, dad, family = binomial))
glm(ctmrius ~ fiscal.year, dad, family = binomial) %>% summary
lrtest(glm(ct ~ fiscal.year, dad, family = binomial))
lrtest(glm(mri ~ fiscal.year, dad, family = binomial))
lrtest(glm(us ~ fiscal.year, dad, family = binomial))

#table 3
# diag.names <- readxl::read_excel("H:/GEMINI/Results/Diabetes/MRD.freqtable.xlsx")
# dad <- merge(dad, diag.names[,c("Diagnosis.Code", "Diagnosis")],
#              by.x = "Diag.Code", by.y = "Diagnosis.Code",
#              all.x = T)
diag.freq <- dad[, .N, by = Diagnosis]
diag.freq <- diag.freq[order(N, decreasing = T)]
tab3 <- function(x){
  data.frame(
    N = paste(length(x$EncID.new), " (",round1(length(x$EncID.new)/138485*100),
              ")", sep = ""),
    Age = me.iqr0(x$Age),
    Comorb = me.iqr(x$n.comorb),
    ICU = cat1.prop(x$SCU.adm, 1),
    death = cat1.prop(x$Discharge.Disposition, 7),
    readmission.within.30.days = paste(sum(x$time.since.last.admission<=30, na.rm = T), " (", 
                                       round1(sum(x$time.since.last.admission<=30, na.rm = T)/
                                                length(x$time.since.last.admission[
                                                  ymd(x$Admit.Date)>=ymd("2010-05-01")]<=30)*100),
                                       ")",sep = "")
  )
}

table3 <- tab3(dad)
for(i in diag.freq[1:5, Diagnosis]){
  table3 <- rbind(table3, tab3(dad[Diagnosis==i]))
}
table3 <- cbind(c("all", diag.freq[1:5, Diagnosis]),
                table3)
fwrite(table3, "H:/GEMINI/Results/DesignPaper/table3.new.march10.csv")
dad.sub <- dad[Diagnosis%in%diag.freq[1:5, Diagnosis]]
dad.sub$death <- dad.sub$Discharge.Disposition==4
lrtest(lm(Age ~ Diagnosis, dad.sub))
lrtest(lm(n.comorb ~ Diagnosis, dad.sub))
lrtest(lm(n.comorb ~ Diagnosis, dad.sub))
lrtest(glm(SCU.adm ~ Diagnosis, dad.sub, family = binomial))
lrtest(glm(death ~ Diagnosis, dad.sub, family = binomial))

prop.test(c(547, 526, 674, 230, 917), c(6623, 5289, 5618, 5421, 5240))
prop.test(c(1139, 580, 837, 611, 301), c(6623, 5289, 5618, 5421, 5240))



tab4 <- function(x){
  data.frame(
    N = paste(length(x$EncID.new), " (",round1(length(x$EncID.new)/138485*100),
              ")", sep = ""),
    Age = me.iqr0(x$Age),
    Comorb = me.iqr(x$n.comorb),
    ICU = cat1.prop(x$SCU.adm, 1),
    death = cat1.prop(x$Discharge.Disposition, 4),
    readmission.within.30.days = paste(sum(x$time.since.last.admission<=30, na.rm = T), " (", 
                                       round1(sum(x$time.since.last.admission<=30, na.rm = T)/
                                                length(x$time.since.last.admission[
                                                  ymd(x$Admit.Date)>=ymd("2010-05-01")]<=30)*100),
                                       ")",sep = "")
  )
}


# table 4 
dad$with.ALC <- dad$Number.of.ALC.Days>0

tab4 <- function(x){
  data.frame(
    with.alc = cat1.prop(x$with.ALC, TRUE),
    total.alc = paste(sum(x$Number.of.ALC.Days, na.rm = T), " (", 
                       round1(sum(x$Number.of.ALC.Days, na.rm = T)/289713*100), ")", sep = ""),
    total.los = me.iqr(x$LoS),
    totl.beddays = paste(round1(sum(x$LoS, na.rm = T)), " (", 
                         round1(sum(x$LoS, na.rm = T)/1335075*100), ")", sep = ""),
    cost = me.iqr(x$Cost[!is.na(x$Cost)])
  )
}
table4 <- tab4(dad)
for(i in diag.freq[1:5, Diagnosis]){
  table4 <- rbind(table4, tab4(dad[Diagnosis==i]))
}
table4<- cbind(c("all", diag.freq[1:5, Diagnosis]),
                table4)
fwrite(table4, "H:/GEMINI/Results/DesignPaper/table4.new.march10.csv")
dad.sub <- dad[Diagnosis%in%diag.freq[1:5, Diagnosis]]
dad.sub$death <- dad.sub$Discharge.Disposition==4
lrtest(lm(Cost ~ Diagnosis, dad.sub))
lrtest(lm(LoS ~ Diagnosis, dad.sub))
lrtest(glm(with.ALC ~ Diagnosis, dad.sub, family = binomial))




# table 5
dad$xray <- dad$EncID.new%in%xray.enc
dad[str_sub(EncID.new, 1, 2)=="15", xray:=NA]
dad$ir <- dad$EncID.new%in%ir.enc
dad[str_sub(EncID.new, 1, 2)=="15", ir:=NA]
tab5 <- function(x){
  data.frame(
    xray = cat1.prop(x$xray[!is.na(x$xray)], TRUE),
    ct = cat1.prop(x$ct[!is.na(x$ct)], TRUE),
    mri = cat1.prop(x$mri[!is.na(x$mri)], TRUE),
    us = cat1.prop(x$us[!is.na(x$us)], TRUE),
    ir = cat1.prop(x$ir[!is.na(x$ir)], TRUE),
    echo = NA,
    nmed = me.iqr(x$nmed[!is.na(x$nmed)]),
    blood.trans = cat1.prop(x$blood.trans[!is.na(x$blood.trans)], TRUE),
    dialysis = cat1.prop(x$dialysis[!is.na(x$dialysis)], TRUE),
    endo = cat1.prop(x$endo[!is.na(x$endo)], TRUE)
  )
}
table5 <- tab5(dad)
for(i in diag.freq[1:5, Diagnosis]){
  table5 <- rbind(table5, tab5(dad[Diagnosis==i]))
}
table5<- cbind(c("all", diag.freq[1:5, Diagnosis]),
               table5)
fwrite(table5, "H:/GEMINI/Results/DesignPaper/table5.new.march10.csv")



# table

smh <- readg(smh.er, .er.nophi)
sbk <- readg(sbk.er, .er.nophi,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn <- readg(uhn.er, .er.nophi,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh <- readg(msh, .er.nophi)
thp <- readg(thp, .er.nophi)

er <- c(smh$EncID.new, sbk$EncID.new, uhn$EncID.new, msh$EncID.new, thp$EncID.new)
dad$er.adm <- dad$EncID.new%in%er
ddply(dad, ~str_sub(EncID.new, 1, 2), summarize,
      N.hos = length(unique(EncID.new)),
      n.er = sum(er.adm),
      total.bed.days = sum(LoS)) -> table6
table6 <- table6[c(3,1,2,4,5),]
fwrite(table6, "H:/GEMINI/Results/DesignPaper/table6.new.march10.csv")



# figures 
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
# dad <- dad %>% arrange(Hash, ymd_hm(paste(Discharge.Date, Discharge.Time))) %>% data.table 
# dad[Hash %in% cohort[time.since.last.admission<0, Hash], 
#        .(EncID.new, Admit.Date, Admit.Time, Discharge.Date, Discharge.Time, Hash, time.since.last.admission)] -> 
#   check

# 32 of the encounters in the overall data sets are duplicates of other encounter

## number of admission
p1 <- dad[,.N, by = fiscal.year] %>%
  ggplot(aes(fiscal.year, N, label = N)) + 
  geom_line() + 
  geom_point() + 
  geom_text(vjust = -1) +
  #geom_bar(stat = "identity", width = 0.3) + 
  scale_y_continuous(limits = c(0, 35000)) +
  ggtitle("Number of Admission") +
  ylab("Number of Admission") +
  xlab(NULL) + 
  theme(plot.title = element_text(hjust = 0.5))

## length of stay
p2 <- ggplot(dad, aes(fiscal.year, LoS, group = fiscal.year)) + 
  geom_boxplot(outlier.size = NA) +
  scale_y_continuous(limits = c(0, 50))

p2 <- ddply(dad, ~fiscal.year, summarize, 
      median.los = median(LoS),
      upper.los = quantile(LoS, 0.75),
      lower.los = quantile(LoS, 0.25),
      max.los = min(max(LoS), (quantile(LoS, 0.75) + 1.5*IQR(LoS))),
      min.los = max(min(LoS), (quantile(LoS, 0.25) - 1.5*IQR(LoS)))) %>%
  ggplot(aes(x = fiscal.year)) + 
  geom_boxplot(aes(ymin = min.los, ymax = max.los, middle = median.los, 
                   upper = upper.los, lower = lower.los), 
               stat = "identity") +
  ggtitle("Length of Stay") +
  ylab("Length of Stay (Days)") +
  xlab(NULL)  + 
  theme(plot.title = element_text(hjust = 0.5))

## cost
ggplot(dad, aes(fiscal.year, Cost, group = fiscal.year)) + 
  geom_boxplot(outlier.size = NA) 

p3 <- ddply(dad[!is.na(Cost)], ~fiscal.year, summarize, 
            median.Cost = median(Cost),
            upper.Cost = quantile(Cost, 0.75),
            lower.Cost = quantile(Cost, 0.25),
            max.Cost = min(max(Cost), (quantile(Cost, 0.75) + 1.5*IQR(Cost))),
            min.Cost = max(min(Cost), (quantile(Cost, 0.25) - 1.5*IQR(Cost)))) %>%
  ggplot(aes(x = fiscal.year)) + 
  geom_boxplot(aes(ymin = min.Cost, ymax = max.Cost, middle = median.Cost, 
                   upper = upper.Cost, lower = lower.Cost), 
               stat = "identity") +
  ggtitle("Cost") +
  ylab("Cost (CAD)") +
  xlab(NULL)  + 
  theme(plot.title = element_text(hjust = 0.5))

## readsmission in 30 days 1
ggplot(dad[!is.na(read.in.30)], aes(x = fiscal.year, fill = read.in.30)) + 
  geom_bar(position = "fill", alpha = 0.5)

prev.ci <- function(x){
  prev <- mean(x)
  se <- sqrt(prev*(1-prev)/length(x))
  z <- qnorm(0.975)
  c(prev, prev-z*se, prev+z*se) * 100
}


p4 <- ddply(dad[!is.na(read.in.30)], ~fiscal.year, summarize, 
      med = prev.ci(read.in.30)[1],
      low = prev.ci(read.in.30)[2],
      up = prev.ci(read.in.30)[3]) %>%
  ggplot(aes(x = fiscal.year)) + 
  geom_ribbon(aes(ymin = low, ymax = up), fill = "grey70", alpha = 0.5) +
  geom_line(aes(y = med))+ 
  geom_text(aes(y = med,label = sprintf("%.1f", med)), vjust = -1) +
  ylim(0, 15) + 
  ggtitle("Readmission in 30 days") +
  ylab("Rate (%)") +
  xlab(NULL)  + 
  theme(plot.title = element_text(hjust = 0.5))



## readsmission in 30 days 2
ggplot(dad[!is.na(read.in.30)], aes(ymd(Discharge.Date), fill = read.in.30, color = read.in.30)) + 
  geom_histogram(binwidth = 30,alpha = 0.5, position = "fill")

ggplot(dad[!is.na(read.in.30)], aes(ymd(Discharge.Date), fill = read.in.30, color = read.in.30)) + 
  geom_histogram(binwidth = 30,alpha = 0.5)

## ct or mri or ultrasound
ggplot(dad[!is.na(ctmrius)], aes(ymd(Discharge.Date), fill = ctmrius, color = ctmrius)) + 
  geom_histogram(binwidth = 30,alpha = 0.5, position = "fill")



p5 <- ddply(dad[!is.na(ctmrius)], ~fiscal.year, summarize, 
            med = prev.ci(ctmrius)[1],
            low = prev.ci(ctmrius)[2],
            up = prev.ci(ctmrius)[3]) %>%
  ggplot(aes(x = fiscal.year)) + 
  geom_ribbon(aes(ymin = low, ymax = up), fill = "grey70", alpha = 0.5) +
  geom_line(aes(y = med))+ 
  geom_text(aes(y = med,label = sprintf("%.1f", med)), vjust = -1) +
  ylim(0, 70) +
  ggtitle("Rate of at least 1 CT/MRI/Ultrasound") +
  ylab("Rate (%)") +
  xlab(NULL)  + 
  theme(plot.title = element_text(hjust = 0.5))

library(gridExtra)
library(grid)


png("H:/GEMINI/Results/DesignPaper/figures.png", res = 200, width = 1000, height = 3000)
grid.arrange(p1, p2, p3, p4, p5, nrow = 5)
dev.off()




# --------------------------- all in one figure --------------------------------
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad[, read.in.30 := ifelse(ymd(Discharge.Date)<ymd("2010-05-01"), NA,
                           ifelse(time.since.last.admission<=30&!is.na(time.since.last.admission), T, 
                                  ifelse(time.since.last.admission>30|is.na(time.since.last.admission), F, F)))]


df <- ddply(dad, ~fiscal.year, summarize,
      Number.of.Hospitalization = length(EncID.new),
      Median.Length.of.Stay = median(LoS),
      Rate.of.Readmission.in.30.Days = sum(read.in.30==T, na.rm = T)/sum(!is.na(read.in.30))*100,
      Median.Cost = median(Cost, na.rm = T),
      "Rate.of.Radiology.Test.\n(CT/MRI/Ultrasound)" = sum(ctmrius==1, na.rm = T)/sum(!is.na(ctmrius))*100)

df[2, 2:6] <- (df[2, 2:6] - df[1, 2:6])/df[1, 2:6] * 100
df[3, 2:6] <- (df[3, 2:6] - df[1, 2:6])/df[1, 2:6] * 100
df[4, 2:6] <- (df[4, 2:6] - df[1, 2:6])/df[1, 2:6] * 100
df[5, 2:6] <- (df[5, 2:6] - df[1, 2:6])/df[1, 2:6] * 100
df[1, 2:6] <- 0

library(reshape2)
df <- melt(df, id.var = "fiscal.year")
# df$significant.level <- ifelse(df$variable%in%c("rate.of.readmission.in.30days", "median.cost",
#                                   "rate.of.radiology"), "significant", "not significant")
names(df)[2] <- " "
df$` ` <- str_replace_all(df$` `, "[.]", " ")

ggplot(df, aes(fiscal.year, value, group = ` `,
               color = ` `)) + 
  geom_point(size = 3, shape = 16) + geom_line(size = 1, alpha = 0.5) + 
  ylim(-10, 35) + 
  geom_abline(aes(slope = 0, intercept = 0), linetype = 2, alpha = 0.5) +
  theme_bw() +
  ylab("Change From Baseline (%)") +
  xlab("Fiscal Year")



# ------------------------ treemap ---------------------------------------------
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
length(unique(dad$Hash))
diag.freq <- table(dad$Diag.Code) %>% data.table %>% arrange(desc(N))
dad[, top20.diag := ifelse(Diag.Code%in%diag.freq$V1[1:20], Diagnosis, "Other")]


library(devtools)
library(treemapify)
top20diag  <- ddply(dad, ~top20.diag, summarize, 
                    N = length(EncID.new),
                    Cost = median(Cost, na.rm = T)) %>% arrange(desc(N))
top20diag$Prevalence <- sprintf("%.1f", top20diag$N/1384.85)
top20diag$top20.diag <- paste(top20diag$top20.diag, top20diag$Prevalence,
                              sep = "\n")
names(top20diag)[1] <- "Diagnosis"
top20diag$ID <-c(21, 1:20)
cbind(top20diag$Diagnosis, c("Other", 
                             "Heart Faliure",
                             "Pneumonia",
                             "COPD", 
                             "UTI",
                             "Stroke",
                             "Sepsis",
                             "Electrolyte Abnormality",
                             "Acute Kidney Injury",
                             "GI Bleed",
                             "Delirium",
                             "T2DM",
                             "Aspiration Pneumonitis",
                             "Cellulitis",
                             "Syncope",
                             "Palliative Care",
                             "Alcohol-Related",
                             "Falls",
                             "Chest Pain",
                             "Gastroenteritis",
                             "C. difficile"))
top20diag$Diagnosis <- c("Other", 
                         "Heart Faliure",
                         "Pneumonia",
                         "COPD", 
                         "UTI",
                         "Stroke",
                         "Sepsis",
                         "Electrolyte Abnormality",
                         "Acute Kidney Injury",
                         "GI Bleed",
                         "Delirium",
                         "T2DM",
                         "Aspiration Pneumonitis",
                         "Cellulitis",
                         "Syncope",
                         "Palliative Care",
                         "Alcohol-Related",
                         "Falls",
                         "Chest Pain",
                         "Gastroenteritis",
                         "C. difficile")
top20diag$label <- paste(top20diag$Diagnosis, "\n(", top20diag$Prevalence, ")",sep = "")          
                         

library(treemap)
reds <- c(rgb(255/255, 255/255, 255/255, 1),
          rgb(255/255, 204/255, 204/255, 1),
          rgb(255/255, 153/255, 153/255, 1),
          rgb(255/255, 102/255, 102/255, 1),
          rgb(255/255, 51/255, 51/255, 1),
          rgb(255/255, 0/255, 0/255, 1),
          rgb(204/255, 0/255, 0/255, 1),
          rgb(153/255, 0/255, 0/255, 1))

treemap(top20diag,
        algorithm = "pivotSize",
        index = c("label"),
        vSize = "N",
        vColor = "Cost",
        sortID = "ID",
        type = "value",
        palette = reds,
        title = "",
        title.legend = "Median Cost per Hospitalization",
        force.print.labels = T,
        mapping = c(2000, 6000, 10000)
)


p1 <-ggplot(dad, aes(Age)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2, alpha= 0.5,
                 color = "black", fill = "#FF6666") + 
  theme_bw() +
  xlab("Age") + ylab(NULL)

p2 <- ggplot(dad, aes(n.comorb)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 1, alpha= 0.5,
                 color = "black", fill = "#FF6666") + 
  theme_bw() +
  xlab("Number of Comorbidities")+ ylab(NULL)

p3 <- ggplot(dad, aes(LoS)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2, alpha= 0.5,
                 color = "black", fill = "#FF6666") + 
  theme_bw() +
  xlab("Length of Stay")+ ylab(NULL) +
  xlim(0, 100)



p4 <- ggplot(dad, aes(Cost)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 2000, alpha= 0.5,
                 color = "black", fill = "#FF6666") + 
  theme_bw() + 
  xlab("Cost")+ ylab(NULL) + xlim(0, 100000)

p4 <- ggplot(dad, aes(logcost)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.05, alpha= 0.5,
                 color = "black", fill = "#FF6666") 
  theme_bw() + scale_x_continuous(labels = trans_format("identity", math_format(10^.x))) +
  xlab("log10(Cost)")+ ylab(NULL)# + xlim(0, 100000) 

p4


library(gridExtra)
library(grid)

grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
             left = "Proportion of Population")



dad$age.65 <- ifelse(dad$Age<65, "<65", ">=65")
ddply(dad, ~age.65, summarize,
      number.of.hospitalization = length(unique(EncID.new)),
      readmission.within.30.days = cat1.prop(read.in.30[!is.na(read.in.30)], TRUE))
