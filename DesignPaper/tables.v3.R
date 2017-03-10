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
      readmission.within.30.days = paste(sum(time.since.last.admission<=30, na.rm = T), ", ", 
                                         round1(sum(time.since.last.admission<=30, na.rm = T)/
                                                 length(time.since.last.admission[
                                                   ymd(Admit.Date)>=ymd("2010-05-01")]<=30)*100),
                                         sep = ""),
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

n.hos <- t(table2[1, ])
years <- 2010:2014
lrtest(lm(n.hos ~ years))
summary(lm(n.hos ~ years))

lm(Cost ~ fiscal.year, dad) %>% summary
lm(LoS ~ fiscal.year, dad) %>% summary
lrtest(glm(read.in.30 ~ fiscal.year, dad, family = binomial))

lrtest(glm(ctmrius ~ fiscal.year, dad, family = binomial))

#table 3
diag.names <- readxl::read_excel("H:/GEMINI/Results/Diabetes/MRD.freqtable.xlsx")
dad <- merge(dad, diag.names[,c("Diagnosis.Code", "Diagnosis")],
             by.x = "Diag.Code", by.y = "Diagnosis.Code",
             all.x = T)
diag.freq <- dad[, .N, by = Diagnosis]
diag.freq <- diag.freq[order(N, decreasing = T)]
tab3 <- function(x){
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
    totl.beddays = paste(sum(x$Number.of.ALC.Days, na.rm = T), " (", 
                         round1(sum(x$Number.of.ALC.Days, na.rm = T)/1335075*100), ")", sep = ""),
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



