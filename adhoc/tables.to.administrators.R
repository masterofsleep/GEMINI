# ----------------------- Analysis 


# ======================== QBP Cohort Selection ================================


rm(list = ls())
library(gemini)
lib.pa()
ip.diag <- readg(gim, ip_diag)
ip.int <- readg(gim, ip_int)
# ---------------------------- CAP ---------------------------------------------

cap.inc <- ip.diag[Diagnosis.Type=="M"&
                      startwith.any(Diagnosis.Code,
                              c("J13", "J14", "J15",
                                "J16", "J18", "J12",
                                "J170", "J171", "J178",
                                "J100", "J110")),
                    EncID.new]

diag.ex <- c("B24", "Z21", "D71", "J172", "J173", "J690",  "J691", "J699",
             "D700", "Z511","Z542", "Z926", "Z940", "Z940", "Z941", "Z942",
             "Z943", "Z944", "Z945", "Z946", "Z947", "Z949",
             "Z515", "T86000", "T86001", "Z9480", "Z9483", "Z9480", "Z9481",
             "Z9482", "Z9483", "Z9488")
ex1 <- ip.diag[startwith.any(Diagnosis.Code, diag.ex), EncID.new]


cap.ex1 <- cap.inc[!cap.inc%in%ex1]

int.ex <- c("1WY19", "1LZ19HHU7A", "1LZ19HHU7J",
            "1ZZ35CAM", "1ZZ35HAM", "1ZZ35YAM")

ex2 <- ip.int[startwith.any(Intervention.Code, int.ex), EncID.new]


cap.cohort <- intersect(cap.ex1[!cap.ex1%in%ex2],
                        cohort[Age>=18, EncID.new])

#-------------------------- COPD -----------------------------------------------


copd.inc <- ip.diag[Diagnosis.Type=="M"&
  startwith.any(Diagnosis.Code, c("J41", "J42", "J43", "J44"))&
  (!str_sub(Diagnosis.Code,1,4)%in%c("J431", "J432", "J430")), EncID.new]



copd.cohort <- intersect(copd.inc, cohort[Age>=35, EncID.new])
smh.hig <- readg(smh, ip_hig)
sbk.hig <- readg(sbk, ip_hig)
uhn.hig <- readg(uhn, ip_hig)
msh.hig <- readg(msh, ip_hig)
thp.hig <- readg(thp, ip_hig)
ip.hig <- rbind(smh.hig[,.(EncID.new, HIG)],
                sbk.hig[,.(EncID.new, HIG)],
                uhn.hig[,.(EncID.new, HIG)],
                msh.hig[,.(EncID.new, HIG)],
                thp.hig[,.(EncID.new, HIG)])

ex.hig <- c("110", "112", "113", "114", "115", "116", "117", "119", "904")
ex1 <- ip.hig[HIG%in%ex.hig, EncID.new]
copd.cohort <- intersect(copd.inc, cohort[Age>=35, EncID.new])





#---------------------------- CHF ----------------------------------------------

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
chf.inc <- intersect(c(chf.inc1, chf.inc2, chf.inc3),
                     cohort[Age>=20, EncID.new])
chf.cohort <- unique(c(chf.inc1, chf.inc2, chf.inc3))



#--------------------------- STROKE --------------------------------------------

stroke.code <- c("G45", "I61", "I63", "I64", "H341")

stro.inc <- ip.diag[Diagnosis.Type=="M"&
                      (startwith.any(Diagnosis.Code, stroke.code)&
                         !Diagnosis.Code%in%c("G454", "I636")), EncID.new]

stro.ex <- ip.diag[Diagnosis.Type=="2"&
                     (startwith.any(Diagnosis.Code, stroke.code)&
                        !Diagnosis.Code%in%c("G454", "I636")), EncID.new]
stro.inc <- stro.inc[!stro.inc%in%stro.ex]

stro.inc <- intersect(stro.inc, cohort[Age>=18, EncID.new])




# ------------------------------------------------------------------------------
cohort <- rbind(readg(smh, dad, select = c("EncID.new", "Age", "MostResponsible.DocterCode")),
                readg(sbk, dad, select = c("EncID.new", "Age", "MostResponsible.DocterCode")),
                readg(uhn, dad, select = c("EncID.new", "Age", "MostResponsible.DocterCode")),
                readg(msh, dad, select = c("EncID.new", "Age", "MostResponsible.DocterCode")),
                readg(thp, dad, select = c("EncID.new", "Age", "MostResponsible.DocterCode")))





cohort[, ':='(MRP = paste(MostResponsible.DocterCode, str_sub(EncID.new, 1, 2), sep = "-"),
              pneumonia = EncID.new%in%cap.cohort,
              copd = EncID.new%in%copd.cohort,
              chf = EncID.new%in%chf.cohort,
              stroke = EncID.new%in%stro.inc)]
los <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv", 
             select = c("EncID.new", "LoS", "Cost", "Discharge.Disposition",
                        "read.in.30"))
los[EncID.new%in%los[duplicated(EncID.new), EncID.new]]
los$EncID.new <- as.character(los$EncID.new)
cohort <- merge(cohort, unique(los), by = "EncID.new")
fwrite(cohort, "H:/GEMINI/Results/Ad Hoc/cohort.to.administrators.csv")

diag.by.phy <- ddply(cohort, ~MRP, summarize,
      n.cap = sum(pneumonia, na.rm = T),
      n.chf = sum(chf, na.rm = T),
      n.copd = sum(copd, na.rm = T),
      n.stroke = sum(stroke, na.rm = T))
tab2a <- NULL
for(i in c(5,10,20,50)){
  tab2a <- rbind(tab2a, apply(diag.by.phy[,2:5], 2, function(x)sum(x > i)))
}
fwrite(data.frame(t(tab2a)), "H:/GEMINI/Results/Ad Hoc/for.administrators.tab2a.csv")





cohort2 <- rbind(readg(smh, adm, select = c("EncID.new", "Admitting.Code", "Discharging.Code")),
                 readg(uhn, adm, select = c("EncID.new", "Admitting.Code", "Discharging.Code")),
                 readg(thp, adm, select = c("EncID.new", "Admitting.Code", "Discharging.Code")),fill = T) %>% 
  unique
sbk <- fread("R:/GEMINI/_RESTORE/SBK/Physicians/adm.physician.hashes.csv")
sbk$EncID.new <- paste("12", sbk$EncID.new, sep = "")
names(sbk) <- c("Admitting.Code", "Discharging.Code","EncID.new")
cohort2 <- rbind(cohort2, sbk)
msh.adm <-  readg(msh, adm, colClasses = list(character = "EncID.new"))
msh.names.full <- fread("R:/GEMINI/_RESTORE/MSH/Physician Names/adm.full.csv")
msh.names.full[,EncID.new := paste("14" ,EncID.new, sep = "")]
msh <- msh.names.full[EncID.new%in%msh.adm$EncID.new,.(EncID.new,
                                            Admitting.Code = adm.code,
                                            Discharging.Code = dis.code)]
cohort2 <- rbind(cohort2, msh) %>% unique

# find those with same admitting and discharging doctor code
cohort2.same.phy <- cohort2[Admitting.Code==Discharging.Code|is.na(Discharging.Code)]

los <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv", 
             select = c("EncID.new", "LoS", "Cost", "Discharge.Disposition",
                        "read.in.30"))

cohort2.same.phy <- cohort2.same.phy[EncID.new%in%los[LoS<35, EncID.new]]

los$EncID.new <- as.character(los$EncID.new)
cohort2.same.phy<- merge(cohort2.same.phy, unique(los), by = "EncID.new")

cohort2.same.phy[, ':='(ADMP = paste(Admitting.Code, str_sub(EncID.new, 1, 2), sep = "-"),
              pneumonia = EncID.new%in%cap.cohort,
              copd = EncID.new%in%copd.cohort,
              chf = EncID.new%in%chf.cohort,
              stroke = EncID.new%in%stro.inc)]
fwrite(cohort2.same.phy, "H:/GEMINI/Results/Ad Hoc/cohort2.to.administrators.csv")


diag.by.phy <- ddply(cohort2.same.phy, ~ADMP, summarize,
                     n.cap = sum(pneumonia, na.rm = T),
                     n.chf = sum(chf, na.rm = T),
                     n.copd = sum(copd, na.rm = T),
                     n.stroke = sum(stroke, na.rm = T))
tab2b <- NULL
for(i in c(5,10,20,50)){
  tab2b <- rbind(tab2b, apply(diag.by.phy[,2:5], 2, function(x)sum(x > i)))
}
fwrite(data.frame(t(tab2b)), "H:/GEMINI/Results/Ad Hoc/for.administrators.tab2b.csv")



# --------------------------- Figures ------------------------------------------
# --------------------------- 2017 03 21 ---------------------------------------
exclude <- fread("H:/GEMINI/Data/GEMINI/gim.notgim.csv")
cohort <- fread("H:/GEMINI/Results/Ad Hoc/cohort.to.administrators.csv")[!EncID.new%in%exclude$EncID.new]
cohort2 <- fread("H:/GEMINI/Results/Ad Hoc/cohort2.to.administrators.csv")[!EncID.new%in%exclude$EncID.new]

site.map <- data.table(
  code = c("11", "12","13","14","15"),
  site = c("smh", "sbk", "uhn", "msh", "thp")
)
fig1 <- function(dat, title){
  df <- ddply(dat, ~MRP, summarize,
        ave.los = mean(LoS),
        N = length(EncID.new))
  if(title == "Overall"){
    df <- df[df$N>100, ]
  } else{
    df <- df[df$N>20,]
  }
  df <- data.table(df)
  df$code <- str_sub(df$MRP, -2, -1)
  df <- merge(df, site.map, by = "code")
  for(i in c("smh", "sbk", "uhn", "msh", "thp")){
    df[site ==i, mrp := as.numeric(factor(MRP, levels = MRP[order(ave.los, decreasing = T)]))]
  }
  ggplot(df, aes(mrp, ave.los)) + geom_bar(stat = "identity", width = 0.6)+ 
    ggtitle(paste(title, "by MRP", sep = " ")) + 
    facet_grid(. ~site) +
    theme(plot.title = element_text(hjust = 0.5))
}



fig1.2 <- function(dat, title){
  df <- ddply(dat, ~ADMP, summarize,
              ave.los = mean(LoS),
              N = length(EncID.new))
  if(title == "Overall"){
    df <- df[df$N>100, ]
  } else{
    df <- df[df$N>20,]
  }
  df <- data.table(df)
  df$code <- str_sub(df$ADMP, -2, -1)
  df <- merge(df, site.map, by = "code")
  for(i in c("smh", "sbk", "uhn", "msh", "thp")){
    df[site ==i, admp := as.numeric(factor(ADMP, levels = ADMP[order(ave.los, decreasing = T)]))]
  }
  ggplot(df, aes(admp, ave.los)) + geom_bar(stat = "identity", width = 0.6)+ 
    ggtitle(paste(title, "by adm/dis physician", sep = " ")) + 
    facet_grid(. ~site) +
    theme(plot.title = element_text(hjust = 0.5))
}
fig1(cohort, "Overall")
fig1(cohort[pneumonia==T], "Pneumonia")
fig1(cohort[chf==T], "CHF")
fig1(cohort[copd==T], "COPD")
fig1(cohort[stroke==T], "Stroke")


fig1.2(cohort2, "Overall")
fig1.2(cohort2[pneumonia==T], "Pneumonia")
fig1.2(cohort2[chf==T], "CHF")
fig1.2(cohort2[copd==T], "COPD")
fig1.2(cohort2[stroke==T], "Stroke")






fig2 <- function(dat, title){
  df <- ddply(dat, ~MRP, summarize,
              ave.cost = mean(Cost, na.rm = T),
              N = length(EncID.new))
  if(title == "Overall"){
    df <- df[df$N>100, ]
  } else{
    df <- df[df$N>20,]
  }
  df <- data.table(df)
  df$code <- str_sub(df$MRP, -2, -1)
  df <- merge(df, site.map, by = "code")
  for(i in c("smh", "sbk", "uhn", "msh", "thp")){
    df[site ==i, mrp := as.numeric(factor(MRP, levels = MRP[order(ave.cost, decreasing = T)]))]
  }
  ggplot(df, aes(mrp, ave.cost)) + geom_bar(stat = "identity", width = 0.6)+ 
    facet_grid(. ~site) + ggtitle(paste(title, "by MRP", sep = " ")) + 
    theme(plot.title = element_text(hjust = 0.5))
}

fig2.2 <- function(dat, title){
  df <- ddply(dat, ~ADMP, summarize,
              ave.cost = mean(Cost, na.rm = T),
              N = length(EncID.new))
  if(title == "Overall"){
    df <- df[df$N>100, ]
  } else{
    df <- df[df$N>20,]
  }
  df <- data.table(df)
  df$code <- str_sub(df$ADMP, -2, -1)
  df <- merge(df, site.map, by = "code")
  for(i in c("smh", "sbk", "uhn", "msh", "thp")){
    df[site ==i, admp := as.numeric(factor(ADMP, levels = ADMP[order(ave.cost, decreasing = T)]))]
  }
  ggplot(df, aes(admp, ave.cost)) + geom_bar(stat = "identity", width = 0.6)+ 
    facet_grid(. ~site) + 
    ggtitle(paste(title, "by adm/dis physician", sep = " ")) + 
    theme(plot.title = element_text(hjust = 0.5))
}
fig2(cohort, "Overall")
fig2(cohort[pneumonia==T], "Pneumonia")
fig2(cohort[chf==T], "CHF")
fig2(cohort[copd==T], "COPD")
fig2(cohort[stroke==T], "Stroke")


fig2.2(cohort2, "Overall")
fig2.2(cohort2[pneumonia==T], "Pneumonia")
fig2.2(cohort2[chf==T], "CHF")
fig2.2(cohort2[copd==T], "COPD")
fig2.2(cohort2[stroke==T], "Stroke")



fig3 <- function(dat, title){
  df <- ddply(dat, ~MRP, summarize,
              readmission.rate = mean(read.in.30, na.rm = T),
              ave.los = mean(LoS, na.rm = T),
              N = length(EncID.new))
  if(title == "Overall"){
    df <- df[df$N>100, ]
  } else{
    df <- df[df$N>20,]
  }
  df <- data.table(df)
  df$code <- str_sub(df$MRP, -2, -1)
  df <- merge(df, site.map, by = "code")
  ggplot(df, aes(ave.los, readmission.rate)) + geom_jitter() + 
    ggtitle(paste(title, "by MRP", sep = " ")) + 
    theme(plot.title = element_text(hjust = 0.5)) + facet_grid( .~site)
}



fig3.2 <- function(dat, title){
  df <- ddply(dat, ~ADMP, summarize,
              readmission.rate = mean(read.in.30, na.rm = T),
              ave.los = mean(LoS, na.rm = T),
              N = length(EncID.new))
  if(title == "Overall"){
    df <- df[df$N>100, ]
  } else{
    df <- df[df$N>20,]
  }
  df <- data.table(df)
  df$code <- str_sub(df$ADMP, -2, -1)
  df <- merge(df, site.map, by = "code")
  ggplot(df, aes(ave.los, readmission.rate)) + geom_jitter() + 
    ggtitle(paste(title, "by adm/dis physician", sep = " ")) + 
    theme(plot.title = element_text(hjust = 0.5)) + facet_grid( .~site)
}
fig3(cohort, "Overall")
fig3(cohort[pneumonia==T], "Pneumonia")
fig3(cohort[chf==T], "CHF")
fig3(cohort[copd==T], "COPD")
fig3(cohort[stroke==T], "Stroke")

fig3.2(cohort2, "Overall")
fig3.2(cohort2[pneumonia==T], "Pneumonia")
fig3.2(cohort2[chf==T], "CHF")
fig3.2(cohort2[copd==T], "COPD")
fig3.2(cohort2[stroke==T], "Stroke")


fig4 <- function(dat, title){
  df <- ddply(dat, ~MRP, summarize,
              rate.of.inhospital.death = mean(Discharge.Disposition==7, na.rm = T),
              ave.los = mean(LoS, na.rm = T),
              N = length(EncID.new))
  if(title == "Overall"){
    df <- df[df$N>100, ]
  } else{
    df <- df[df$N>20,]
  }
  df <- data.table(df)
  df$code <- str_sub(df$MRP, -2, -1)
  df <- merge(df, site.map, by = "code")
  ggplot(df, aes(ave.los, rate.of.inhospital.death)) + geom_jitter() + 
    ggtitle(paste(title, "by MRP", sep = " ")) + 
    theme(plot.title = element_text(hjust = 0.5)) + facet_grid(.~ site)
}



fig4.2 <- function(dat, title){
  df <- ddply(dat, ~ADMP, summarize,
              rate.of.inhospital.death = mean(Discharge.Disposition==7, na.rm = T),
              ave.los = mean(LoS, na.rm = T),
              N = length(EncID.new))
  if(title == "Overall"){
    df <- df[df$N>100, ]
  } else{
    df <- df[df$N>20,]
  }
  df <- data.table(df)
  df$code <- str_sub(df$ADMP, -2, -1)
  df <- merge(df, site.map, by = "code")
  ggplot(df, aes(ave.los, rate.of.inhospital.death)) + geom_jitter() + 
    ggtitle(paste(title, "by adm/dis physician", sep = " ")) + 
    theme(plot.title = element_text(hjust = 0.5)) + facet_grid(.~ site)
}

fig4(cohort, "Overall")
fig4(cohort[pneumonia==T], "Pneumonia")
fig4(cohort[chf==T], "CHF")
fig4(cohort[copd==T], "COPD")
fig4(cohort[stroke==T], "Stroke")


fig4.2(cohort2, "Overall")
fig4.2(cohort2[pneumonia==T], "Pneumonia")
fig4.2(cohort2[chf==T], "CHF")
fig4.2(cohort2[copd==T], "COPD")
fig4.2(cohort2[stroke==T], "Stroke")

exclude <- fread("H:/GEMINI/Data/GEMINI/gim.notgim.csv")
cohort <- fread("H:/GEMINI/Results/Ad Hoc/cohort.to.administrators.csv")[!EncID.new%in%exclude$EncID.new]

smh.names <- fread("R:/GEMINI/Check/physician_names/smh.mrp.csv")
uhn.names <- fread("R:/GEMINI/Check/physician_names/uhn.mrp.freq.csv")
msh.names <- fread("R:/GEMINI/_RESTORE/MSH/Physician Names/dad_names.csv")

sbk.hash <- fread("R:/GEMINI/_RESTORE/SBK/Physicians/dad.mrp.hashes.csv")
sbk.marked <- fread("C:/Users/guoyi/Desktop/marked_names/names_code_link_ASW.csv")
sbk.names <- merge(sbk.hash, sbk.marked,
                   by.x = "mrpCode", by.y = "hashed", all.x = T)
sbk.names$EncID.new <- paste("12", sbk.names$EncID.new, sep = "")
cohort$EncID.new <- as.character(cohort$EncID.new)
sbk.names <- merge(sbk.names, cohort[,.(EncID.new, MRP)], by = "EncID.new" )
sbk.names <- unique(sbk.names[,.(MRP.Code = MRP, Name)])
smh.names <- smh.names[,.(MRP.Code = paste(MRP.Code, "11", sep = "-"), 
                          Name = paste(MostResponsiblePhysicianFirstName, MostResponsiblePhysicianLastName))]
uhn.names <- uhn.names[,.(MRP.Code = paste(MostResponsibleDoctorCode, "13", sep = "-"),
                          Name = paste(mostresponsiblefirstname, mostresponsiblelastname))]
msh.names <- msh.names[,.(MRP.Code = paste(Most.Responsible.Doctor.Code, "14", sep = "-"),
                          Name = paste(Most.Responsible.Physician.First.Name, Most.Responsible.Physician.Last.Name))]
cohort$EncID.new <- as.character(cohort$EncID.new)

thp.names <- fread("R:/GEMINI/_RESTORE/THP/phynames/thp.dad.LINKINGLIST_physicians.csv")
cohort <- merge(cohort, thp.names[,.(EncID.new = paste("15", EncID.new, sep = ""),
                                     MRP.Code = paste(MostResponsibleDoctorCode, "-15", sep = ""))],
                by = "EncID.new", all.x = T)
cohort[str_sub(EncID.new, 1, 2)=="15", MRP := MRP.Code]
cohort[, MRP.Code:=NULL]


thp.names <- thp.names[,.(MRP.Code = paste(MostResponsibleDoctorCode,"-15", sep = ""), 
                          Name = paste(MostResponsiblePhysicianFirstName, MostResponsiblePhysicianLastName))] %>% unique
name <- rbind(smh.names,
              sbk.names,
              uhn.names,
              msh.names,
              thp.names) %>% unique
site.map <- data.table(
  code = c("11", "12","13","14","15"),
  site = c("smh", "sbk", "uhn", "msh", "thp")
)


                
by.mrp <-   ddply(cohort, ~MRP, summarize,
                  number.of.patient = length(EncID.new),
                  ave.los = mean(LoS, na.rm = T),
                  median.los = median(LoS, na.rm = T),
                  ave.cost = mean(Cost, na.rm = T),
                  median.cost = median(Cost, na.rm = T),
                  rate.of.inhospital.death = mean(Discharge.Disposition==7, na.rm = T),
                  readmission.rate = mean(read.in.30, na.rm = T))

by.mrp <- merge(by.mrp, name, by.x = "MRP", by.y = "MRP.Code", all.x = T)
by.mrp$code <- str_sub(by.mrp$MRP, -2, -1)
by.mrp <- merge(by.mrp, site.map, by = "code")


fwrite(by.mrp, "H:/GEMINI/Results/Ad Hoc/mrp.summary.csv")
