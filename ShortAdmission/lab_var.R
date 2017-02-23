library(gemini)
lib.pa()

## ----------------  find variables in lab data --------------------------------
cohort <- fread("H:/GEMINI/Results/Shortadm/cohort5.csv")
#
smh.lab <- readg(smh, corelabs)
smh.lab <- smh.lab[smh.lab$EncID.new%in%cohort$EncID.new]

# remove results with non-numeric value
smh.lab<- smh.lab[str_detect(str_sub(Result.Value,1,1), "[:digit:]")|
                    str_sub(Result.Value,1,1)%in%c("-", "<", ">")]
smh.lab.nonum <- smh.lab[is.na(as.numeric(Result.Value))]

smh.lab <- merge(smh.lab, cohort[,.(EncID.new, Admit.Date, Admit.Time)], 
                 all.x = T, all.y = F)
smh.lab <- smh.lab %>% arrange(EncID.new, ymd_hms(Collection.DtTm)) %>% data.table

#only keep those before the admission
smh.lab <- smh.lab[ymd_hms(Collection.DtTm)<ymd_hm(paste(Admit.Date, Admit.Time))]
sodium.smh <- smh.lab[Test.Name=="Sodium"] %>% filter(!duplicated(EncID.new)) %>%data.table
hgb.smh <- smh.lab[Test.Name=="HGB"] %>% filter(!duplicated(EncID.new)) %>%data.table
wbc.smh <- smh.lab[Test.Name=="WBC"] %>% filter(!duplicated(EncID.new)) %>%data.table
plt.smh <- smh.lab[Test.Name=="PLT"] %>% filter(!duplicated(EncID.new)) %>%data.table
creatinine.smh <- smh.lab[Test.Name=="Creatinine"] %>% filter(!duplicated(EncID.new)) %>%data.table
albumin.smh <- smh.lab[Test.Name=="Albumin"] %>% filter(!duplicated(EncID.new)) %>%data.table
lactate.smh <- smh.lab[Test.Name%in%c("Lactate Venous", "Lactate Arterial")] %>% 
  filter(!duplicated(EncID.new)) %>%data.table
troponin.smh <- smh.lab[Test.Name=="Troponin I"] %>% filter(!duplicated(EncID.new)) %>%data.table
potassium.smh <- smh.lab[Test.Name=="Potassium"] %>% filter(!duplicated(EncID.new)) %>%data.table
calcium.smh <- smh.lab[Test.Name=="Calcium"] %>% filter(!duplicated(EncID.new)) %>%data.table
ast.smh <- smh.lab[Test.Name=="AST"] %>% filter(!duplicated(EncID.new)) %>%data.table
alt.smh <- smh.lab[Test.Name=="ALT"] %>% filter(!duplicated(EncID.new)) %>%data.table
alp.smh <- smh.lab[Test.Name=="ALP"] %>% filter(!duplicated(EncID.new)) %>%data.table
mcv.smh <- smh.lab[Test.Name=="MCV"] %>% filter(!duplicated(EncID.new)) %>%data.table
glucose.smh <- smh.lab[Test.Name=="Glucose Random"] %>% filter(!duplicated(EncID.new)) %>%data.table





# smh.nonume.sample <- rbind(hgb.smh[is.na(as.numeric(Result.Value))],
#                            wbc.smh[is.na(as.numeric(Result.Value))],
#                            creatinine.smh[is.na(as.numeric(Result.Value))],
#                            lactate.smh[is.na(as.numeric(Result.Value))],
#                            troponin.smh[is.na(as.numeric(Result.Value))],
#                            mcv.smh[is.na(as.numeric(Result.Value))],
#                            alt.smh[is.na(as.numeric(Result.Value))],
#                            albumin.smh[is.na(as.numeric(Result.Value))],
#                            sodium.smh[is.na(as.numeric(Result.Value))],
#                            plt.smh[is.na(as.numeric(Result.Value))])%>%
#   filter(!startsWith(Result.Value, "<"))%>%
#   filter(!startsWith(Result.Value, ">"))%>%
#   filter(!duplicated(Result.Value)) %>%
#   filter(!duplicated(str_sub(Result.Value, -3,-1)))
# write.csv(smh.nonume.sample, "H:/GEMINI/Results/Shortadm/sample.lab.none.num.smh.csv",
#           row.names = F, na = "")


#categorize hgb
hgb.smh <- merge(hgb.smh, cohort[,.(EncID.new, Gender)], by = "EncID.new", 
                 all.x = T, all.y = F) %>% data.table
hgb.smh[is.na(as.numeric(Result.Value))]
hgb.smh$Result.Value <- str_replace_all(hgb.smh$Result.Value, "[@A-z]","")


hgb.smh[Gender=="F", Hgb:= ifelse(as.numeric(Result.Value)<120, "low",
                                  ifelse(as.numeric(Result.Value)<=140, 
                                         "normal", "high"))]
hgb.smh[Gender=="M", Hgb:= ifelse(as.numeric(Result.Value)<140, "low",
                                  ifelse(as.numeric(Result.Value)<=180, 
                                         "normal", "high"))]
hgb.smh[is.na(Hgb)]
sum(is.na(hgb.smh$Hgb))


#categorize wbc
wbc.smh[is.na(as.numeric(Result.Value))]
wbc.smh$Result.Value <- str_replace_all(wbc.smh$Result.Value, "[@A-z]","")
wbc.smh[, WBC:= ifelse(as.numeric(Result.Value)<4, "low",
                       ifelse(as.numeric(Result.Value)<=12,"normal","high"))]
range(wbc.smh[WBC=="low", as.numeric(Result.Value)])
range(wbc.smh[WBC=="normal", as.numeric(Result.Value)])
range(wbc.smh[WBC=="high", as.numeric(Result.Value)])

#categorize plt
plt.smh[is.na(as.numeric(Result.Value))]
plt.smh$Result.Value <- str_replace_all(plt.smh$Result.Value, "[@A-z]","")
plt.smh[,plt:= ifelse(as.numeric(Result.Value)< 100|startsWith(Result.Value, "<3"), "low",
                      ifelse(as.numeric(Result.Value)<= 400,"normal","high"))]
range(plt.smh[plt=="low", as.numeric(Result.Value)])
range(plt.smh[plt=="normal", as.numeric(Result.Value)])
range(plt.smh[plt=="high", as.numeric(Result.Value)])



# categorize sodium
sodium.smh[is.na(as.numeric(Result.Value))]
sodium.smh$Result.Value <- str_replace_all(sodium.smh$Result.Value, "[@A-z]","")
sodium.smh[, sodium := ifelse(as.numeric(Result.Value)<135|startsWith(Result.Value, "<"), "low",
                              ifelse(as.numeric(Result.Value)>145, "high", "normal"))]
sum(is.na(sodium.smh$sodium))
range(sodium.smh[sodium=="low", as.numeric(Result.Value)], na.rm = T)
range(sodium.smh[sodium=="high", as.numeric(Result.Value)], na.rm = T)

#categorize potassium                                                           # there are some <20 ones cannot be categorized
potassium.smh[is.na(as.numeric(Result.Value))]
potassium.smh$Result.Value <- str_replace_all(potassium.smh$Result.Value, "[@A-z]","")
potassium.smh[is.na(as.numeric(Result.Value)), potassium:= 
                ifelse(startsWith(Result.Value, "<"),"low",
                       ifelse(startsWith(Result.Value, ">"), 
                              "high", "normal"))]
potassium.smh[!is.na(as.numeric(Result.Value)), potassium:= 
                ifelse(as.numeric(Result.Value)<3.5, "low",
                       ifelse(as.numeric(Result.Value)>5.1, "high", "normal"))]
sum(is.na(potassium.smh$potassium))
range(potassium.smh[potassium=="low", as.numeric(Result.Value)], na.rm = T)
range(potassium.smh[potassium=="normal", as.numeric(Result.Value)], na.rm = T)
range(potassium.smh[potassium=="high", as.numeric(Result.Value)], na.rm = T)


#categorize creatinine: need to calculate eGFR
creatinine.smh$Result.Value <- str_replace_all(creatinine.smh$Result.Value, "[@A-z]","")
creatinine.smh[, creatinine:= ifelse(as.numeric(Result.Value)<4|startsWith(Result.Value, "<20"), 
                                     "low",
                                     ifelse(as.numeric(Result.Value)<=12,"normal","high"))]



#categorize troponin
troponin.smh[is.na(as.numeric(Result.Value))&Result.Value!="<0.006"]
troponin.smh$Result.Value <- str_replace_all(troponin.smh$Result.Value, "[@A-z]","")
troponin.smh[Result.Value=="<0.006",troponin:="normal"]
troponin.smh[Result.Value==">50.000",troponin:="abnormal"]
troponin.smh[Result.Value!="<0.006"&Result.Value!=">50.000",
             troponin:= ifelse(as.numeric(Result.Value)<=0.006, 
                               "normal", "abnormal")]


#categorize lactate
lactate.smh[is.na(as.numeric(Result.Value))]                                    
lactate.smh$Result.Value <- str_replace_all(lactate.smh$Result.Value, "[@A-z]","")
lactate.smh[Test.ID=="VLACT", lactate:=ifelse(as.numeric(Result.Value)<=2.3, "normal",
                                              "abnormal")]

lactate.smh[Test.ID=="ALACT", lactate:=ifelse(as.numeric(Result.Value)<=1.8, "normal",
                                              "abnormal")]
range(lactate.smh[lactate=="normal", as.numeric(Result.Value)])
range(lactate.smh[lactate=="abnormal", as.numeric(Result.Value)])

#categorize albumin   
albumin.smh[is.na(as.numeric(Result.Value))]
albumin.smh$Result.Value <- str_replace_all(albumin.smh$Result.Value, "[@A-z]","")
albumin.smh[, albumin := ifelse(as.numeric(Result.Value)<35|startsWith(Result.Value, "<"), "low","normal")]
albumin.smh[startsWith(Result.Value, ">"), albumin:="normal"]

# categorize calcium
calcium.smh[is.na(as.numeric(Result.Value))]
calcium.smh$Result.Value <- str_replace_all(calcium.smh$Result.Value, "[<@A-z]","")
sum(calcium.smh$EncID.new%in%albumin.smh$EncID.new)                             # not all calcium tests had corresponding albumin



# categorize AST
ast.smh[is.na(as.numeric(Result.Value))]
ast.smh$Result.Value <- str_replace_all(ast.smh$Result.Value, "[<@A-z]","")
ast.smh[,ast:= ifelse(as.numeric(Result.Value)>40, "abnormal", "normal")]
range(ast.smh[ast=="normal", as.numeric(Result.Value)])

# categorize ALT
alt.smh[is.na(as.numeric(Result.Value))]
alt.smh$Result.Value <- str_replace_all(alt.smh$Result.Value, "[<@A-z]","")
alt.smh[,alt:= ifelse(as.numeric(Result.Value) > 35, "abnormal", "normal")]
range(alt.smh[alt=="abnormal", as.numeric(Result.Value)])

# categorize MCV
mcv.smh[is.na(as.numeric(Result.Value))]
mcv.smh$Result.Value <- str_replace_all(mcv.smh$Result.Value, "[@A-z]","")
mcv.smh[,mcv:= ifelse(as.numeric(Result.Value) > 97, "high", 
                      ifelse(as.numeric(Result.Value) < 82, "low", "normal"))]
range(mcv.smh[mcv=="low", as.numeric(Result.Value)])
range(mcv.smh[mcv=="normal", as.numeric(Result.Value)])

# categorize ALP
alp.smh[is.na(as.numeric(Result.Value))]
alp.smh[,alp:= ifelse(as.numeric(Result.Value) > 160, "abnormal", "normal")]
sum(is.na(alp.smh$alp))
range(alp.smh[alp=="normal", as.numeric(Result.Value)])

#categorize glucose
glucose.smh[is.na(as.numeric(Result.Value))]
glucose.smh$Result.Value <- str_replace_all(glucose.smh$Result.Value, "[@A-z]","")
glucose.smh$Result.Value <- as.numeric(glucose.smh$Result.Value)
glucose.smh[,glucose:= ifelse(Result.Value > 11.1, "abnormal", "normal")]
range(glucose.smh[glucose=="abnormal", Result.Value])
sum(is.na(glucose.smh$glucose))















#------------------------------ Sunny Brook ------------------------------------ 
sbk.laber <- readg(sbk, labs_er)
sbk.labip <- readg(sbk, labs_ip)                                                # use both ip and er because some of the ip tests were performed before admission
sbk.lab <- rbind(sbk.laber, sbk.labip)
sum(duplicated(sbk.lab))

sbk.lab <- sbk.laber[sbk.laber$EncID.new%in%cohort$EncID.new]
sbk.lab <- merge(sbk.lab, cohort[,.(EncID.new, Admit.Date, Admit.Time)], 
                 all.x = T, all.y = F)
#only keep those before the admission
sbk.lab <- sbk.lab[ymd_hms(Collection.DtTm)<ymd_hm(paste(Admit.Date, Admit.Time))]
sbk.lab.nonum <- sbk.lab[is.na(as.numeric(Result.Value))]
sbk.lab<- sbk.lab[str_detect(str_sub(Result.Value,1,1), "[:digit:]")|
                    str_sub(Result.Value,1,1)%in%c("-", "<", ">")]





sodium.sbk <- sbk.lab[Test.Name=="Sodium"] %>% filter(!duplicated(EncID.new)) %>%data.table
hgb.sbk <- sbk.lab[Test.Name=="Hemoglobin"] %>% filter(!duplicated(EncID.new)) %>%data.table
wbc.sbk <- sbk.lab[Test.Name=="WBC Count"] %>% filter(!duplicated(EncID.new)) %>%data.table
plt.sbk <- sbk.lab[Test.Name=="Platelet Count"] %>% filter(!duplicated(EncID.new)) %>%data.table
creatinine.sbk <- sbk.lab[Test.Name=="Creatinine (renal)"] %>% filter(!duplicated(EncID.new)) %>%data.table
albumin.sbk <- sbk.lab[Test.Name=="Albumin"] %>% filter(!duplicated(EncID.new)) %>%data.table
lactate.sbk <- sbk.lab[Test.Name%in%c("Lactate - Serum", "Lactate, Arterial")] %>% 
  filter(!duplicated(EncID.new)) %>%data.table
troponin.sbk <- sbk.lab[Test.Name%in%c("Troponin T, High Sensitivity","Troponin T")] %>% 
  filter(!duplicated(EncID.new)) %>% data.table
potassium.sbk <- sbk.lab[Test.Name=="Potassium"] %>% filter(!duplicated(EncID.new)) %>% data.table
calcium.sbk <- sbk.lab[Test.Name=="Calcium"] %>% filter(!duplicated(EncID.new)) %>% data.table
ast.sbk <- sbk.lab[Test.Name=="AST"] %>% filter(!duplicated(EncID.new)) %>% data.table
alt.sbk <- sbk.lab[Test.Name=="ALT"] %>% filter(!duplicated(EncID.new)) %>% data.table
alp.sbk <- sbk.lab[Test.Name=="ALP"] %>% filter(!duplicated(EncID.new)) %>% data.table
mcv.sbk <- sbk.lab[Test.Name=="MCV"] %>% filter(!duplicated(EncID.new)) %>% data.table
glucose.sbk <- sbk.lab[Test.Name=="Glucose- Random"] %>% filter(!duplicated(EncID.new)) %>%data.table




# sbk.nonume.sample <- rbind(hgb.sbk[is.na(as.numeric(Result.Value))],
#                            wbc.sbk[is.na(as.numeric(Result.Value))],
#                            creatinine.sbk[is.na(as.numeric(Result.Value))],
#                            lactate.sbk[is.na(as.numeric(Result.Value))],
#                            troponin.sbk[is.na(as.numeric(Result.Value))],
#                            mcv.sbk[is.na(as.numeric(Result.Value))],
#                            alt.sbk[is.na(as.numeric(Result.Value))],
#                            albumin.sbk[is.na(as.numeric(Result.Value))],
#                            sodium.sbk[is.na(as.numeric(Result.Value))],
#                            plt.sbk[is.na(as.numeric(Result.Value))])%>%
#   filter(!startsWith(Result.Value, "<"))%>%
#   filter(!startsWith(Result.Value, ">"))%>%
#   filter(!duplicated(Result.Value))
# write.csv(sbk.nonume.sample, "H:/GEMINI/Results/Shortadm/sample.lab.none.num.sbk.csv",
#           row.names = F, na = "")



#categorize hgb
hgb.sbk <- merge(hgb.sbk, cohort[,.(EncID.new, Gender)], by = "EncID.new", 
                 all.x = T, all.y = F) %>% data.table
hgb.sbk$Result.Value <- str_replace(hgb.sbk$Result.Value, "[@A-z]","")
hgb.sbk[Gender=="F", Hgb:= ifelse(as.numeric(Result.Value)<120, "low",
                                  ifelse(as.numeric(Result.Value)<=140, 
                                         "normal", "high"))]
hgb.sbk[Gender=="M", Hgb:= ifelse(as.numeric(Result.Value)<140, "low",
                                  ifelse(as.numeric(Result.Value)<=180, 
                                         "normal", "high"))]
sum(is.na(hgb.sbk$Hgb))
range(hgb.sbk[Hgb=="low", as.numeric(Result.Value)],na.rm = T)

#categorize wbc
wbc.sbk[is.na(as.numeric(Result.Value))]
wbc.sbk[, WBC:= ifelse(as.numeric(Result.Value)<4, "low",
                       ifelse(as.numeric(Result.Value)<=12,"normal","high"))]
sum(is.na(wbc.sbk$WBC))
range(wbc.sbk[WBC=="normal", as.numeric(Result.Value)])
range(wbc.sbk[WBC=="low", as.numeric(Result.Value)])

#categorize plt
plt.sbk[is.na(as.numeric(Result.Value))]
plt.sbk[,plt:= ifelse(as.numeric(Result.Value)< 100, "low",
                      ifelse(as.numeric(Result.Value)<= 400,"normal","high"))]
sum(is.na(plt.sbk$plt))
range(plt.sbk[plt=="normal", as.numeric(Result.Value)])
range(plt.sbk[plt=="low", as.numeric(Result.Value)])
range(plt.sbk[plt=="high", as.numeric(Result.Value)])

# categorize sodium
sodium.sbk[is.na(as.numeric(Result.Value))]
sodium.sbk[, sodium := ifelse(as.numeric(Result.Value)<135, "low",
                              ifelse(as.numeric(Result.Value)>145, "high", "normal"))]
sum(is.na(sodium.sbk$sodium))
range(sodium.sbk[sodium=="high", as.numeric(Result.Value)])
range(sodium.sbk[sodium=="low", as.numeric(Result.Value)])
range(sodium.sbk[sodium=="normal", as.numeric(Result.Value)])

#categorize potassium
potassium.sbk[is.na(as.numeric(Result.Value))]
potassium.sbk[!is.na(as.numeric(Result.Value)), potassium:= 
                ifelse(as.numeric(Result.Value)<3.5, "low",
                       ifelse(as.numeric(Result.Value)>5.1, "high", "normal"))]
sum(is.na(potassium.sbk$potassium))
range(potassium.sbk[potassium=="high", as.numeric(Result.Value)])
range(potassium.sbk[potassium=="low", as.numeric(Result.Value)])
range(potassium.sbk[potassium=="normal", as.numeric(Result.Value)])

#categorize creatinine: need to calculate eGFR
creatinine.sbk[is.na(as.numeric(Result.Value))] # two kinds, "<18" and ">2210"
creatinine.sbk[, creatinine:= ifelse(as.numeric(Result.Value)<4|startsWith(Result.Value, "<20"), 
                                     "low",
                                     ifelse(as.numeric(Result.Value)<=12,"normal","high"))]



#categorize troponin
troponin.sbk[is.na(as.numeric(Result.Value))] -> check # only two type, <0.01 and <5 both are normal
troponin.sbk[Test.ID=="TROPT",troponin:= 
               ifelse(as.numeric(Result.Value)>0.1, "abnormal", "normal")]
troponin.sbk[Test.ID=="TRPTHS",troponin:= 
               ifelse(as.numeric(Result.Value)>15, "abnormal", "normal")]
troponin.sbk[startsWith(Result.Value, "<"), troponin:="normal"]
sum(is.na(troponin.sbk$troponin))
troponin.sbk[Test.ID=="TROPT"&troponin=="abnormal"]

#categorize lactate
lactate.sbk[is.na(as.numeric(Result.Value))]
lactate.sbk[Test.Name=="Lactate, Arterial", lactate:=
              ifelse(as.numeric(Result.Value)<=1.7, "normal","abnormal")]

lactate.sbk[Test.Name=="Lactate - Serum", lactate:=
              ifelse(as.numeric(Result.Value)<=2, "normal","abnormal")]         # ref range 0.5 - 2, here only those > 2 is marked abnormal
sum(is.na(lactate.sbk$lactate))
lactate.sbk[Test.Name=="Lactate, Arterial"]

#categorize albumin   
albumin.sbk[is.na(as.numeric(Result.Value))]
albumin.sbk[, albumin := ifelse(as.numeric(Result.Value)<=35, "normal","abnormal")]
sum(is.na(albumin.sbk$albumin))
range(albumin.sbk[albumin=="normal", as.numeric(Result.Value)])

# categorize calcium
calcium.sbk[is.na(as.numeric(Result.Value))]                                  
calcium.sbk$Result.Value <- str_replace_all(calcium.sbk$Result.Value, "[<@A-z]","")
sum(calcium.sbk$EncID.new%in%albumin.sbk$EncID.new)
calcium.sbk <- merge(calcium.sbk, albumin.sbk[,.(EncID.new, Result.Value)],
                     by = "EncID.new", all.x = T, all.y = F)

calcium.sbk[,calcium.corrected.num:= 
              as.numeric(Result.Value.x) + 0.2 * (40 - as.numeric(Result.Value.y))/10]
calcium.sbk[,calcium.corrected:= 
              ifelse(calcium.corrected.num<2.1, "low", 
                     ifelse(calcium.corrected.num>2.5, "high", "normal"))]
# categorize AST
ast.sbk[is.na(as.numeric(Result.Value))]
ast.sbk$Result.Value <- str_replace_all(ast.sbk$Result.Value, "[<@A-z]","")
ast.sbk[,ast:= ifelse(as.numeric(Result.Value)>40, "abnormal", "normal")]
range(ast.sbk[ast =="normal", as.numeric(Result.Value)])

# categorize ALT
alt.sbk[is.na(as.numeric(Result.Value))]
alt.sbk[,alt:= ifelse(as.numeric(Result.Value) > 35|startsWith(Result.Value, "<"), 
                      "abnormal", "normal")]
range(alt.sbk[alt=="normal", as.numeric(Result.Value)])

# categorize MCV
mcv.sbk[is.na(as.numeric(Result.Value))]
mcv.sbk[,mcv:= ifelse(as.numeric(Result.Value) > 97, "high", 
                      ifelse(as.numeric(Result.Value) < 82, "low", "normal"))]
range(mcv.sbk[mcv=="normal", as.numeric(Result.Value)])
# categorize ALP

alp.sbk[is.na(as.numeric(Result.Value))]
alp.sbk[,alp:= ifelse(as.numeric(Result.Value) > 160, "abnormal", "normal")]
sum(is.na(alp.sbk$alp))
range(alp.sbk[alp=="normal",as.numeric(Result.Value)])
range(alp.sbk[alp=="abnormal",as.numeric(Result.Value)])


#categorize glucose
glucose.sbk[is.na(as.numeric(Result.Value))]
glucose.sbk[,glucose:= ifelse(as.numeric(Result.Value) > 11.1, "abnormal", "normal")]
sum(is.na(glucose.sbk$glucose))
range(glucose.sbk[glucose=="normal",as.numeric(Result.Value)])
range(glucose.sbk[glucose=="abnormal",as.numeric(Result.Value)])



# ----------------------------- uhn lab ----------------------------------------

cohort <- fread("H:/GEMINI/Results/Shortadm/cohort4.csv")

uhn.lab <- readg(uhn, labs)
#table(uhn.lab$Test.Item, uhn.lab$Test.Name) %>% data.table -> uhn.lab.freq
#names(uhn.lab.freq) <- c("Test.Item", "Test.Name","Freq")
#uhn.lab.freq <- uhn.lab.freq[Freq!=0]
# uhn.nonume.sample <- rbind(uhn[is.na(as.numeric(Result.Value))])%>%
#   filter(Test.Name%in%c("Sodium, Plasma", "Plasma Hemoglobin", 
#                         "Creatine, Plasma", "Albumin, Plasma",
#                         "ALT", "Potassium, Plasma", "AST",
#                         "ALP"))%>%
#   filter(!startsWith(Result.Value, "<"))%>%
#   filter(!startsWith(Result.Value, ">"))%>%
#   filter(!duplicated(Result.Value))
# 
# hgb.uhn <- uhn[Test.Name == "Plasma Hemoglobin"]
# 
# 
# write.csv(uhn.nonume.sample, "H:/GEMINI/Results/Shortadm/sample.lab.none.num.uhn.csv",
#           row.names = F, na = "")
# 
# write.csv(uhn.lab.freq, "H:/GEMINI/Results/Shortadm/lab.freq.uhn.new.csv",
#          row.names = F, na = "")

uhn.lab <- uhn.lab[uhn.lab$EncID.new%in%cohort$EncID.new]
uhn.lab <- merge(uhn.lab, cohort[,.(EncID.new, Admit.Date, Admit.Time)], 
                 all.x = T, all.y = F)
#only keep those before the admission
# test date, test time are the same as specimen.collected.date, specimen.collected.time
uhn.lab <- uhn.lab[ymd_hm(paste(Test.Date, 
                                Test.Time))<
                     ymd_hm(paste(Admit.Date, Admit.Time))]
uhn.lab.nonum <- uhn.lab[is.na(as.numeric(Result.Value))]
uhn.lab<- uhn.lab[str_detect(str_sub(Result.Value,1,1), "[:digit:]")|
                    str_sub(Result.Value,1,1)%in%c("-", "<", ">")]





sodium.uhn <- uhn.lab[Test.Item=="Sodium"&
                        Test.Name%in%c("Electrolytes, Plasma",
                                       "Electrolytes, Creatinine, Glucose Profile",
                                       "Sodium, Plasma",
                                       "Electrolytes, Creatinine, Profile")] %>% 
  filter(!duplicated(EncID.new)) %>%data.table

hgb.uhn <- uhn.lab[Test.Item=="Hb"&Test.Name=="CBC"] %>% filter(!duplicated(EncID.new)) %>%data.table

wbc.uhn <- uhn.lab[Test.Item=="WBC"&Test.Name=="CBC"] %>% filter(!duplicated(EncID.new)) %>%data.table
plt.uhn <- uhn.lab[Test.Item=="Plt"&Test.Name=="CBC"] %>% filter(!duplicated(EncID.new)) %>%data.table
creatinine.uhn <- uhn.lab[Test.Item=="Creatinine"&
                            Test.Name%in%c("Creatinine, Plasma",
                                           "Electrolytes, Creatinine, Glucose Profile",
                                           "Electrolytes, Creatinine, Profile")] %>% 
  filter(!duplicated(EncID.new)) %>%data.table
albumin.uhn <- uhn.lab[Test.Item=="Albumin"&Test.Name=="Albumin, Plasma"] %>% filter(!duplicated(EncID.new)) %>%data.table
lactate.uhn <- uhn.lab[Test.Item=="Lactate"&
                         Test.Name%in%c("Lactate, Plasma",
                                        "POCT Blood Gas Arterial",
                                        "Blood Gas, Arterial",
                                        "POCT Blood Gas Venous")] %>% 
  filter(!duplicated(EncID.new)) %>%data.table
troponin.uhn <- uhn.lab[Test.Item%in%c("Troponin I","Troponin I (HS)")&
                          Test.Name%in%c("Troponin I","Troponin I (HS)")] %>% 
  filter(!duplicated(EncID.new)) %>% data.table
potassium.uhn <- uhn.lab[Test.Item=="Potassium"&Test.Name%in%c(
  "Electrolytes, Plasma", "Electrolytes, Creatinine, Glucose Profile",
  "Potassium, Plasma", "Electrolytes, Creatinine, Profile")] %>% 
  filter(!duplicated(EncID.new)) %>% data.table
calcium.uhn <- uhn.lab[Test.Item=="Calcium"&
                         Test.Name%in%c("Calcium Total, Plasma",
                                        "Calcium Corrected, Plasma")] %>% 
  filter(!duplicated(EncID.new)) %>% data.table
ast.uhn <- uhn.lab[Test.Item=="AST"&Test.Name=="AST"] %>% 
  filter(!duplicated(EncID.new)) %>% data.table
alt.uhn <- uhn.lab[Test.Item=="ALT"&Test.Name=="ALT"] %>% filter(!duplicated(EncID.new)) %>% data.table
alp.uhn <- uhn.lab[Test.Item=="ALP"&Test.Name=="ALP"] %>% filter(!duplicated(EncID.new)) %>% data.table
mcv.uhn <- uhn.lab[Test.Item=="MCV"&Test.Name=="CBC"] %>% filter(!duplicated(EncID.new)) %>% data.table
glucose.uhn <- uhn.lab[Test.Item=="Glucose"&
                         Test.Name%in%c("Glucose, Random-Green",
                                        "Electrolytes, Creatinine, Glucose Profile",
                                        "Glucose, Random-Grey")] %>% 
  filter(!duplicated(EncID.new)) %>%data.table





#categorize hgb
hgb.uhn <- merge(hgb.uhn, cohort[,.(EncID.new, Gender)], by = "EncID.new", 
                 all.x = T, all.y = F) %>% data.table
hgb.uhn[is.na(as.numeric(Result.Value))]
hgb.uhn[Gender=="F", Hgb:= ifelse(as.numeric(Result.Value)<120, "low",
                                  ifelse(as.numeric(Result.Value)<=140, 
                                         "normal", "high"))]
hgb.uhn[Gender=="M", Hgb:= ifelse(as.numeric(Result.Value)<140, "low",
                                  ifelse(as.numeric(Result.Value)<=180, 
                                         "normal", "high"))]
range(hgb.uhn[Hgb=="low", as.numeric(Result.Value)],na.rm = T)
range(hgb.uhn[Hgb=="normal", as.numeric(Result.Value)],na.rm = T)


#categorize wbc
wbc.uhn[is.na(as.numeric(Result.Value))]                                        
wbc.uhn[, WBC:= ifelse(as.numeric(Result.Value)<4|startsWith(Result.Value, "<"), "low",
                       ifelse(as.numeric(Result.Value)<=12,"normal","high"))]
sum(is.na(wbc.uhn$WBC))
range(wbc.uhn[WBC=="normal", as.numeric(Result.Value)])
range(wbc.uhn[WBC=="low", as.numeric(Result.Value)], na.rm = T)

#categorize plt
plt.uhn[is.na(as.numeric(Result.Value))]
plt.uhn[,plt:= ifelse(as.numeric(Result.Value)< 100|startsWith(Result.Value, "<"), "low",
                      ifelse(as.numeric(Result.Value)<= 400,"normal","high"))]
sum(is.na(plt.uhn$plt))
range(plt.uhn[plt=="normal", as.numeric(Result.Value)])
range(plt.uhn[plt=="low", as.numeric(Result.Value)])
range(plt.uhn[plt=="high", as.numeric(Result.Value)])
# categorize sodium
sodium.uhn[is.na(as.numeric(Result.Value))]
sodium.uhn[, sodium := ifelse(as.numeric(Result.Value)<135, "low",
                              ifelse(as.numeric(Result.Value)>145, "high", "normal"))]
sum(is.na(sodium.uhn$sodium))
range(sodium.uhn[sodium=="high", as.numeric(Result.Value)])
range(sodium.uhn[sodium=="low", as.numeric(Result.Value)])
range(sodium.uhn[sodium=="normal", as.numeric(Result.Value)])

#categorize potassium
table(potassium.uhn$Test.Name)
potassium.uhn[is.na(as.numeric(Result.Value))]                                  #6 different test names available, all same ref range?
potassium.uhn[!is.na(as.numeric(Result.Value)), potassium:= 
                ifelse(as.numeric(Result.Value)<3.5, "low",
                       ifelse(as.numeric(Result.Value)>5.1, "high", "normal"))]
potassium.uhn[is.na(as.numeric(Result.Value)), potassium:= 
                ifelse(startsWith(Result.Value, "<"), "low",
                       ifelse(startsWith(Result.Value, ">"), "high", "normal"))]
sum(is.na(potassium.uhn$potassium))

range(potassium.uhn[potassium=="high", as.numeric(Result.Value)])
range(potassium.uhn[potassium=="low", as.numeric(Result.Value)])
range(potassium.uhn[potassium=="normal", as.numeric(Result.Value)])

#categorize creatinine: need to calculate eGFR                                  # need to be calculated
creatinine.uhn[is.na(as.numeric(Result.Value))] # two kinds, "<18" and ">2210"
creatinine.uhn[, creatinine:= ifelse(as.numeric(Result.Value)<4|startsWith(Result.Value, "<20"), 
                                     "low",
                                     ifelse(as.numeric(Result.Value)<=12,"normal","high"))]

# lactate is 0.5-2.0 mmol/L
#  
# troponin <26 ng/L

#categorize troponin
troponin.uhn[is.na(as.numeric(Result.Value))] -> check                          # reference range need to be determined
range(troponin.uhn[Test.Name=="Troponin I", as.numeric(Result.Value)], na.rm = T)
range(troponin.uhn[Test.Name=="Troponin I (HS)", as.numeric(Result.Value)], na.rm = T)

troponin.uhn[Test.Name=="Troponin I",troponin:= 
               ifelse(as.numeric(Result.Value)>0.1, "abnormal", "normal")]
troponin.uhn[Test.Name="Troponin I (HS)",troponin:= 
               ifelse(as.numeric(Result.Value)>26, "abnormal", "normal")]
troponin.uhn[startsWith(Result.Value, "<"), troponin:="normal"]
sum(is.na(troponin.uhn$troponin))


#categorize lactate
lactate.uhn[is.na(as.numeric(Result.Value))]                                    # reference range required
lactate.uhn[Test.Name=="Lactate, Plasma"]
lactate.uhn[Test.Name=="Blood Gas, Arterial"]
lactate.uhn[, lactate:=
              ifelse(as.numeric(Result.Value)<=2, "normal","abnormal")]
range(lactate.uhn[lactate=="normal", as.numeric(Result.Value)])
range(lactate.uhn[lactate=="abnormal", as.numeric(Result.Value)], na.rm = T)
sum(is.na(lactate.uhn$lactate))

#categorize albumin   
albumin.uhn[is.na(as.numeric(Result.Value))]
albumin.uhn[, albumin := ifelse(as.numeric(Result.Value)<=35, "normal","abnormal")]
sum(is.na(albumin.uhn$albumin))
range(albumin.uhn[albumin=="normal", as.numeric(Result.Value)])
range(albumin.uhn[albumin=="abnormal", as.numeric(Result.Value)])


# categorize calcium                                                            # not done yet
calcium.uhn[is.na(as.numeric(Result.Value))]                                  
calcium.uhn$Result.Value <- str_replace_all(calcium.uhn$Result.Value, "[<@A-z]","")
sum(calcium.uhn$EncID.new%in%albumin.uhn$EncID.new)
calcium.uhn <- merge(calcium.uhn, albumin.uhn[,.(EncID.new, Result.Value)],
                     by = "EncID.new", all.x = T, all.y = F)

calcium.uhn[,calcium.corrected.num:= 
              as.numeric(Result.Value.x) + 0.2 * (40 - as.numeric(Result.Value.y))/10]
calcium.uhn[,calcium.corrected:= 
              ifelse(calcium.corrected.num<2.1, "low", 
                     ifelse(calcium.corrected.num>2.5, "high", "normal"))]
# categorize AST
ast.uhn[is.na(as.numeric(Result.Value))]
ast.uhn[,ast:= ifelse(as.numeric(Result.Value)>40, "abnormal", "normal")]
range(ast.uhn[ast =="normal", as.numeric(Result.Value)])
range(ast.uhn[ast =="abnormal", as.numeric(Result.Value)])

# categorize ALT
alt.uhn[is.na(as.numeric(Result.Value))]
table(alt.uhn[is.na(as.numeric(Result.Value)), Result.Value])
alt.uhn[,alt:= ifelse(as.numeric(Result.Value) <= 35|str_sub(Result.Value, 1,1)=="<", "normal", "abnormal")]
alt.uhn[is.na(alt)]
range(alt.uhn[alt=="normal", as.numeric(Result.Value)], na.rm = T)


# categorize MCV
mcv.uhn[is.na(as.numeric(Result.Value))]
mcv.uhn[,mcv:= ifelse(as.numeric(Result.Value) > 97, "high", 
                      ifelse(as.numeric(Result.Value) < 82, "low", "normal"))]
range(mcv.uhn[mcv=="normal", as.numeric(Result.Value)])
range(mcv.uhn[mcv=="low", as.numeric(Result.Value)])
range(mcv.uhn[mcv=="high", as.numeric(Result.Value)])

# categorize ALP
alp.uhn[is.na(as.numeric(Result.Value))]
alp.uhn[,alp:= ifelse(as.numeric(Result.Value)<= 160|str_sub(Result.Value, 1,1)=="<", "normal", "abnormal")]
sum(is.na(alp.uhn$alp))

range(alp.uhn[alp=="normal",as.numeric(Result.Value)], na.rm = T)
range(alp.uhn[alp=="abnormal",as.numeric(Result.Value)])

#categorize glucose
glucose.uhn[is.na(as.numeric(Result.Value))]
glucose.uhn[,glucose:= ifelse(as.numeric(Result.Value) > 11.1, "abnormal", "normal")]
glucose.uhn[str_sub(Result.Value, 1,1)=="<", glucose:= "normal",]
glucose.uhn[str_sub(Result.Value, 1,1)==">", glucose:= "abnormal",]
sum(is.na(glucose.uhn$glucose))

range(glucose.uhn[glucose=="normal",as.numeric(Result.Value)], na.rm = T)
range(glucose.uhn[glucose=="abnormal",as.numeric(Result.Value)], na.rm = T)
