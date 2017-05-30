# ------------------------ Design Paper Tables and Figures V4-------------------
# ------------------------      2017-05-17 -------------------------------------
library(gemini)
lib.pa()
round1 <- function(x){
  sprintf("%.1f", round(x, 1))
}

round0 <- function(x){
  sprintf("%d", round(x, 0))
}


me.iqr <- function(x){
  paste(round1(median(x, na.rm = T)), " (", round1(quantile(x, na.rm = T)[2]), ", ",
        round1(quantile(x, na.rm = T)[4]), ")", sep = "")
}

me.iqr0 <- function(x){
  paste(round0(median(x, na.rm = T)), " (", round0(quantile(x, na.rm = T)[2]), ", ",
        round0(quantile(x, na.rm = T)[4]), ")", sep = "")
}

cat.prop <- function(x){
  df <- cbind(table(x, useNA = "ifany"), 
        round1(table(x, useNA = "ifany")/length(x)*100))
  
  cbind(rownames(df), paste(df[,1], " (", df[,2], ")", sep = ""))
}

cat1.prop <- function(x, cat){
  paste(sum(x==cat, na.rm = T), " (", 
        round1(sum(x==cat, na.rm = T)/sum(!is.na(x))*100), ")", sep = "")
}

dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")

# -------------------------- table 1 -------------------------------------------
me.iqr0(dad$Age)
cat.prop(dad$age.cat)
cat.prop(dad$Gender)
me.iqr0(dad$Number.of.Comorbidity)
dad$Charlson.Comorbidity.Index <- as.character(dad$Charlson.Comorbidity.Index)
dad[!Charlson.Comorbidity.Index%in%c("0", "1"),
    Charlson.Comorbidity.Index:="2+"]
cat.prop(dad$Charlson.Comorbidity.Index)
cat.prop(dad$Discharge.Disposition1)
# find most prevalent diagnosis
ip.diag <- readg(gim, ip_diag)[EncID.new%in%dad$EncID.new]
diag.names <- readxl::read_excel("H:/GEMINI/Results/Diabetes/MRD.freqtable.xlsx")%>%data.table
ip.diag[, Diag3:= str_sub(Diagnosis.Code, 1, 3)]
ip.diag <- merge(ip.diag, diag.names[,.(Diagnosis.Code, Diagnosis)],
                 by.x = "Diag3", by.y = "Diagnosis.Code", all.x = T)
top10.diag <- ip.diag[, .N, by = .(Diag3, Diagnosis)][order(N, decreasing = T)][1:10]
top10.diag[, Prev := paste(N, " (",
                           sprintf("%.1f", N/136208*100), ")",
                           sep = "")]
fwrite(top10.diag, "H:/GEMINI/Results/DesignPaper/result.v4/diag.top10.csv")


# ------------------------- table 2 --------------------------------------------

tab2 <- function(x){
  data.frame(
    N = paste(length(x$EncID.new), " (",round1(length(x$EncID.new)/nrow(dad)*100),
              ")", sep = ""),
    Age = me.iqr0(x$Age),
    Comorb = me.iqr0(x$Number.of.Comorbidity),
    ICU = cat1.prop(x$SCU.adm, TRUE),
    death = cat1.prop(x$Discharge.Disposition, 7),
    read.in.30 = cat1.prop(x$read.in.30, TRUE)
  )
}


topdiag.table <- function(topn=5, tabfun){
  cmgtopn <- dad[, .N, by = .(CMG, CMG.Diagnosis)][order(N, decreasing = T)][1:topn]
  res.table <- tabfun(dad)
  for(i in cmgtopn$CMG){
    res.table <- rbind(res.table, tabfun(dad[CMG==i]))
  }
  res.table <- cbind(Dis.CMG = c("Total Cohort",
                                 cmgtopn$CMG.Diagnosis),
                     res.table)
  res.table
}

table2 <- topdiag.table(5, tab2)
fwrite(table2, "H:/GEMINI/Results/DesignPaper/result.v4/table2.csv")


# ------------------------- table 3 --------------------------------------------

dad$with.ALC <- as.numeric(dad$Number.of.ALC.Days)>0
tab3 <- function(x){
  data.frame(
    LOS = me.iqr(x$Acute.LoS),
    totl.beddays = paste(round1(sum(x$LoS, na.rm = T)), " (", 
                         round1(sum(x$LoS, na.rm = T)/sum(dad$LoS)*100), 
                         ")", sep = ""),
    with.alc = cat1.prop(x$with.ALC, TRUE),
    total.alc = paste(sum(x$Number.of.ALC.Days, na.rm = T), " (", 
                      round1(sum(x$Number.of.ALC.Days, na.rm = T)/sum(dad$Number.of.ALC.Days)*100), 
                      ")", sep = ""),
    cost = me.iqr(x$Cost[!is.na(x$Cost)])
  )
}
table3 <- topdiag.table(5, tab3)
fwrite(table3, "H:/GEMINI/Results/DesignPaper/result.v4/table3.csv")


# ------------------------- table 4 --------------------------------------------
tab4 <- function(x){
  data.frame(
    xray = cat1.prop(x$xray[!is.na(x$xray)], TRUE),
    ct = cat1.prop(x$ct[!is.na(x$ct)], TRUE),
    mri = cat1.prop(x$mri[!is.na(x$mri)], TRUE),
    us = cat1.prop(x$us[!is.na(x$us)], TRUE),
    ir = cat1.prop(x$ir[!is.na(x$ir)], TRUE),
    blood.trans = cat1.prop(x$RBC.trans[!is.na(x$RBC.trans)], TRUE),
    dialysis = cat1.prop(x$dialysis[!is.na(x$dialysis)], TRUE),
    endo = cat1.prop(x$endo[!is.na(x$endo)], TRUE)
  )
}
table4 <- topdiag.table(5, tab4)
fwrite(table4, "H:/GEMINI/Results/DesignPaper/result.v4/table4.csv")



# --------------------------- Figure 2 -----------------------------------------
fig2 <- function(dad){
  p1 <-ggplot(dad, aes(Age)) +
    geom_histogram(aes(y = ..count../136208),
                   binwidth = 2, alpha= 0.5,
                   color = "black", fill = "#FF6666") + 
    theme_bw() +
    xlab("Age") + ylab(NULL)
  p2 <- ggplot(dad, aes(Number.of.Comorbidity)) +
    geom_histogram(aes(y = ..count../136208),
                   binwidth = 1, alpha= 0.5,
                   color = "black", fill = "#FF6666") + 
    theme_bw() +
    xlab("Number of Comorbidities")+ ylab(NULL)
  
  p3 <- ggplot(dad, aes(Acute.LoS)) +
    geom_histogram(aes(y = ..count../136208),
                   binwidth = 2, alpha= 0.5,
                   color = "black", fill = "#FF6666") + 
    theme_bw() +
    xlab("Length of Stay")+ ylab(NULL) +
    xlim(0, 100)
  library(scales)
  p4 <- ggplot(dad, aes(Cost)) +
    geom_histogram(aes(y = ..count../136208),
                   binwidth = 2000, alpha= 0.5,
                   color = "black", fill = "#FF6666") + 
    theme_bw() + 
    xlab("Cost") + ylab(NULL) + 
    scale_x_continuous(labels = comma, limits = c(0, 100000)) 
  library(gridExtra)
  library(grid)
  grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
               left = "Proportion of Population")
}

png("H:/GEMINI/Results/DesignPaper/result.v4/figure2.png", 
    res = 250, width = 2000, height = 1600)
fig2(dad)
dev.off()


# --------------------------- Figure 3 -----------------------------------------
fig3 <- function(dad){
  cmg.freq <- dad[, .N, by = .(CMG, CMG.Diagnosis)][order(N, decreasing = T)]
  cmg.freq$top20.diag <- c("Pneumonia",
                               "Heart Failure",
                               "COPD", "UTI",
                               "Stroke",
                               "General Symptom",
                               "GI Bleed",
                               "Electrolyte Abnormality",
                               "GI Symptom",
                            "Kidney Injury",
                           "Sepsis",
                           "Delirium",
                           "Non-severe Enteritis",
                           "Diabetes",
                           "Aspiration Pneumonia",
                           "Cellulitis",
                           "Syncope",
                           "Poisoning",
                           "Seizure",
                           "Cirrhosis/\nHepatitis",
                           "Arrhythmia",
                           "Palliative Care",
                           "Severe Enteritis",
                           rep("Other", 426))
  dad <- merge(dad, cmg.freq[,.(CMG, top20.diag)],
               by = "CMG", all.x = T, all.y = F)
  top20diag  <- ddply(dad, ~top20.diag, summarize,
                      N = length(EncID.new),
                      Cost = median(Cost, na.rm = T)) %>% arrange(desc(N))
  names(top20diag)[1] <- "Diagnosis"
  top20diag$Diagnosis <- paste(top20diag$Diagnosis, "\n",
                               sprintf("%.1f", top20diag$N/136208*100), "%", sep = "")
  top20diag$ID <-c(24, 1:23)
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
          index = c("Diagnosis"),
          vSize = "N",
          vColor = "Cost",
          sortID = "ID",
          type = "value",
          palette = reds,
          title = "",
          title.legend = "Median Cost per Hospitalization",
          force.print.labels = T,
          mapping = c(2000, 6000, 10000),
         format.legend = list(scientific = FALSE, big.mark = ",")
          
  )
}
fig3(dad)
png("H:/GEMINI/Results/DesignPaper/result.v5/figure3.png", res = 250, width = 2000, height = 2000)
fig3(dad)
dev.off()



# ------------------------------- Figure 4 -------------------------------------
fig4 <- function(dad){
  df <- ddply(dad, ~fiscal.year, summarize,
              "Number of Hospitalizations" = length(EncID.new),
              "Median Acute Length-of-Stay" = median(Acute.LoS),
              "30-day Readmission" = sum(read.in.30==T, na.rm = T)/sum(!is.na(read.in.30))*100,
              "Median Cost" = median(Cost, na.rm = T),
              "Advanced Imaging" = sum(ctmrius==1, na.rm = T)/sum(!is.na(ctmrius))*100)
  df[2, 2:6] <- (df[2, 2:6] - df[1, 2:6])/df[1, 2:6] * 100
  df[3, 2:6] <- (df[3, 2:6] - df[1, 2:6])/df[1, 2:6] * 100
  df[4, 2:6] <- (df[4, 2:6] - df[1, 2:6])/df[1, 2:6] * 100
  df[5, 2:6] <- (df[5, 2:6] - df[1, 2:6])/df[1, 2:6] * 100
  df[1, 2:6] <- 0
  df <- melt(df, id.var = "fiscal.year")
  names(df)[2] <- " "
  df$` ` <- str_replace_all(df$` `, "[.]", " ")
  df$` ` <- factor(df$` `, levels = c("Number of Hospitalizations",
                                          "30-day Readmission",
                                          "Median Cost",
                                          "Median Acute Length-of-Stay",
                                          "Advanced Imaging"))
  ggplot(df, aes(fiscal.year, value, group = ` `,
                 color = ` `)) + 
    geom_point(size = 3, shape = 16) + geom_line(size = 1, alpha = 0.5) + 
    ylim(-10, 35) + 
    geom_abline(aes(slope = 0, intercept = 0), linetype = 2, alpha = 0.5) +
    theme_bw() +
    ylab("Change From Baseline (%)") +
    xlab("Fiscal Year")
}
png("H:/GEMINI/Results/DesignPaper/result.v5/figure4.png", res = 250, width = 2000, height = 1200)
fig4(dad)
dev.off()




# -------------------------------- Table 5 -------------------------------------
tab5 <- function(dad){
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
  return(ddply(dad, ~str_sub(EncID.new, 1, 2), summarize,
        N.hos = length(unique(EncID.new)),
        n.er = sum(er.adm),
        total.bed.days = sum(LoS)))
}

table5 <- tab5(dad)
fwrite(table5, "H:/GEMINI/Results/DesignPaper/result.v4/table5.csv")



# ---------------------------- Appendix Table 2 --------------------------------
df <- ddply(dad, ~fiscal.year, summarize,
      number.of.hospitalization = length(unique(EncID.new)),
      acute.los = me.iqr(Acute.LoS),
      read.in.30 = cat1.prop(read.in.30, TRUE),
      cost = me.iqr(Cost),
      US.CT.MRI = cat1.prop(ctmrius, TRUE),
      US = cat1.prop(us, TRUE),
      CT = cat1.prop(ct, TRUE),
      MRI = cat1.prop(mri, TRUE)
)
fwrite(data.frame(t(df)), "H:/GEMINI/Results/DesignPaper/result.v4/appen.table2.csv")

library(lmtest)
# number of hospitalization
summary(lm(number.of.hospitalization ~ fiscal.year, data = df))

# length of stay
lm(Acute.LoS ~ fiscal.year, dad) %>% summary

# readmission
glm(read.in.30 ~ fiscal.year, dad, family = binomial) %>% summary

# Cost
lm(Cost ~ fiscal.year, dad) %>% summary

# CT MRI US
glm(ctmrius ~ fiscal.year, dad, family = binomial) %>% summary

#CT
glm(ct ~ fiscal.year, dad, family = binomial) %>% summary
#MRI
glm(mri ~ fiscal.year, dad, family = binomial) %>% summary
#US
glm(us ~ fiscal.year, dad, family = binomial) %>% summary



# ----------------------------- Appendix Table 1 -------------------------------
appen_table1 <- function(){
  ip.diag <- readg(gim, ip_diag)
  # find validated cohort 
  # copd
  copd.cohort <- ip.diag[Diagnosis.Type=="M"&
                           startwith.any(Diagnosis.Code, c("J41", "J42", "J43", "J44")), EncID.new]
  # pneumonia
  cap.code <- c("J100", "J110", "J120", "J121", "J122", "J128", "J129",
                "J13", "J14", "J15", "J160", "J168", "J17", "J18")
  cap.cohort <- ip.diag[Diagnosis.Type=="M"&
                          startwith.any(Diagnosis.Code, 
                                        cap.code), EncID.new]
  # heart failure
  chf.inc1 <- ip.diag[Diagnosis.Type =="M"&
                        startwith.any(Diagnosis.Code, 
                                      c("I50", "I255", "I40", "I41", "I42", "I43")), 
                      EncID.new]
  chf.inc2 <- intersect(ip.diag[Diagnosis.Type =="M"&
                                  startwith.any(Diagnosis.Code, 
                                                c("I11")), EncID.new], 
                        ip.diag[Diagnosis.Type%in%c("1", "2","W","X","Y")&
                                  startwith.any(Diagnosis.Code, 
                                                c("I50")), EncID.new])
  chf.inc3 <- intersect(ip.diag[Diagnosis.Type =="M"&
                                  startwith.any(Diagnosis.Code, 
                                                c("I13")), EncID.new], 
                        ip.diag[Diagnosis.Type%in%c("1", "2","W","X","Y")&
                                  startwith.any(Diagnosis.Code, 
                                                c("I50")), EncID.new])
  chf.cohort <- unique(c(chf.inc1, chf.inc2, chf.inc3))
  # stroke
  stro.cohort <- ip.diag[Diagnosis.Type=="M"&startwith.any(Diagnosis.Code,
                                                      c("I63", "I64", "H341")), EncID.new]
  # uti
  uti.code <- c("N10", "N12", "N151", "N300", "N308", "N309","N410", "N412",
                "N413", "N510", "N390")
  uti.cohort <- ip.diag[Diagnosis.Type=="M"&
                          startwith.any(Diagnosis.Code, uti.code), EncID.new]
  vali_dad <- dad[, .(EncID.new, Age, Gender, CMG, CMG.Diagnosis)]
  vali_dad[, ':='(
    vali_copd = EncID.new%in%copd.cohort,
    vali_cap = EncID.new%in%cap.cohort,
    vali_hf = EncID.new%in%chf.cohort,
    vali_stroke = EncID.new%in%stro.cohort,
    vali_uti = EncID.new%in%uti.cohort
  )]
  vali_dad <- merge(vali_dad, ip_diag[Diagnosis.Type=="M",
                                      .(Diagnosis.Code, EncID.new)],
                    by = "EncID.new", all.x = T, all.y = F)
  vali_dad[, Diagnosis.Code:=str_sub(Diagnosis.Code, 1, 3)]
  vali_dad[, ':='(
    cmg_copd = CMG==139,
    cmg_cap = CMG==138,
    cmg_hf = CMG==196,
    cmg_stroke = CMG==26,
    cmg_uti = CMG==487,
    icd_copd = Diagnosis.Code=="J44",
    icd_cap = Diagnosis.Code=="J18",
    icd_hf = Diagnosis.Code=="I50",
    icd_stroke = Diagnosis.Code=="I63",
    icd_uti = Diagnosis.Code=="N39"
  )]
  vali_dad_noNA <- vali_dad[!is.na(CMG)]
  sensi <- function(x, y){
    sum(x==T&y==T)/sum(y==T)
  }
  speci <- function(x, y){
    sum(x==F&y==F)/sum(y==F)
  }
  sensi <- c(sensi(vali_dad_noNA[, cmg_copd], vali_dad_noNA[, vali_copd]),
    sensi(vali_dad_noNA[, cmg_cap], vali_dad_noNA[, vali_cap]),
    sensi(vali_dad_noNA[, cmg_hf], vali_dad_noNA[, vali_hf]),
    sensi(vali_dad_noNA[, cmg_stroke], vali_dad_noNA[, vali_stroke]),
    sensi(vali_dad_noNA[, cmg_uti], vali_dad_noNA[, vali_uti]))
  
  speci <- c(speci(vali_dad_noNA[, cmg_copd], vali_dad_noNA[, vali_copd]),
    speci(vali_dad_noNA[, cmg_cap], vali_dad_noNA[, vali_cap]),
    speci(vali_dad_noNA[, cmg_hf], vali_dad_noNA[, vali_hf]),
    speci(vali_dad_noNA[, cmg_stroke], vali_dad_noNA[, vali_stroke]),
    speci(vali_dad_noNA[, cmg_uti], vali_dad_noNA[, vali_uti]))
  cbind(Diag = c("COPD", "CAP", "Heart Failure",
                        "Stroke", "UTI"),
        sensi,
        speci)
}

appen_table1()
apply(vali_dad_noNA[, .(cmg_copd,cmg_cap,cmg_hf,cmg_stroke,cmg_uti,
                   vali_copd, vali_cap, vali_hf, vali_stroke, vali_uti)],
      2, sum)


# --------------------------- number of physicians -----------------------------
phy.all <- readg(gim, all.phy)
gemini.inc <- phy.all[(adm.GIM%in%c("y", "GP-GIM")|dis.GIM%in%c("y", "GP-GIM"))
                      |str_sub(EncID.new,1, 2)=="15"]
all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.new.csv")
gemini.phy <- c(gemini.inc$adm.code.new, gemini.inc$dis.code.new, gemini.inc$mrp.code.new)

gemini.phy <- all.name[code.new%in%gemini.phy]
gemini.phy <- gemini.phy[!duplicated(code.new)]


# ---------------------- number of hospitalization by site by year --------------
df <- dad[,.N, by = .(site = str_sub(EncID.new, 1, 2), fiscal.year)]
library(reshape2)
nbysiteyear <- dcast(df, site~ fiscal.year, value.var = "N")
fwrite(nbysiteyear, "H:/GEMINI/Results/DesignPaper/result.v4/table6.csv")


# ----------------------- update paper -----------------------------------------
# number of unique patient
sum(dad$Hash=="")
dad[Hash==""]
dad[Hash!="", Hash] %>% unique %>% length 

# ER admission and Bed-days by year
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
dad[str_sub(EncID.new, 1, 2)=="15", sum(er.adm), by = fiscal.year][order(fiscal.year)]
dad[str_sub(EncID.new, 1, 2)=="14", sum(LoS), by = fiscal.year][order(fiscal.year)]
dad[str_sub(EncID.new, 1, 2)=="15", sum(LoS), by = fiscal.year][order(fiscal.year)]

# cmg freq
cmg.freq <- dad[, .N, by = .(CMG, CMG.Diagnosis)][order(N, decreasing = T)]
cmg.freq$prop <- cmg.freq$N/136208
sum(cmg.freq$prop[1:10])
