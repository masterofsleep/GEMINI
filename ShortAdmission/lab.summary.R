# ----------------------- Summary of Lab Tests ---------------------------------
# --------------------------- 2017-03-16 ---------------------------------------
library(gemini)
lib.pa()
rm(list = ls())

smh.lab <- readg(smh, corelabs, dt = T)
sbk.lab <- rbind(readg(sbk, labs_er, dt = T), readg(sbk, labs_ip, dt = T))
uhn.lab <- readg(uhn, labs, dt = T)
msh.lab <- readg(msh, lab, dt = T)
#msh.lab <- readg(msh, lab, dt= T)
# msh.lab.freq <- table(msh.lab[,.(Name, TEST_ID)]) %>% data.table 
# msh.lab.freq <- msh.lab.freq[N!=0]
# fwrite(msh.lab.freq, "H:/GEMINI/Results/Shortadm/msh.lab.freq.csv")

smh.lab[str_sub(Result.Value,1,1)%in%c(0:9)&is.na(as.numeric(Result.Value))&
          !is.na(Result.Value),
        Result.Value := str_replace_all(Result.Value, "[@A-z!]","")]
smh.lab[str_sub(Result.Value,1,1)%in%c(">", "<")&str_detect(Result.Value, "@"),
        Result.Value := str_replace_all(Result.Value, "[@A-z]","")]

smh.lab <- smh.lab[,.(EncID.new, Test.Name, Test.ID, Result.Value,
                        Result.Unit, Reference.Range, Collection.DtTm = ymd_hms(Collection.DtTm), Site = "SMH")]
sbk.lab <- sbk.lab[,.(EncID.new, Test.Name, Test.ID, Result.Value,
                      Result.Unit, Reference.Range, Collection.DtTm = ymd_hms(Collection.DtTm), Site = "SBK")]
uhn.lab <- uhn.lab[,.(EncID.new, Test.Name, Test.ID, Test.Item, Result.Value ,
                      Result.Unit = Result.Units, 
                      Collection.DtTm = ymd_hms(paste(Specimen.Col, Specimen.C)),
                      Site = "UHN")]
msh.lab <- msh.lab[,.(EncID.new, Test.Name = Name, Test.ID = TEST_ID, 
                      Collection.DtTm = ymd_hms(paste(DATE_COLLECT, TIME_COLLECT)),
                      Site = "MSH", Result.Value = RESULT, Result.Unit = UNITES,
                      Reference.Range = REFERANCE_LAB)]
msh.lab[str_sub(Result.Value,1,1)%in%c(0:9)&is.na(as.numeric(Result.Value))&
          !is.na(Result.Value),
        Result.Value := str_replace_all(Result.Value, "[@A-z!]","")]
names(smh.lab)
names(sbk.lab)
names(uhn.lab)
lab.desc <- function(x, y, z){
  cat("### Summary of numbers\n")
  "Numeric Values (N, %)" <- c(
    paste(sum(!is.na(as.numeric(x))),
          " (", round(sum(!is.na(as.numeric(x)))/length(x)*100, 2),")", sep = ""),
    paste(sum(!is.na(as.numeric(y))),
          " (", round(sum(!is.na(as.numeric(y)))/length(y)*100, 2),")", sep = ""),
    paste(sum(!is.na(as.numeric(z))),
          " (", round(sum(!is.na(as.numeric(z)))/length(z)*100, 2),")", sep = "")
  )
  "Non-numeric Values (N, %)" <-c(
    paste(sum(is.na(as.numeric(x))),
          " (", round(sum(is.na(as.numeric(x)))/length(x)*100, 2),")", sep = ""),
    paste(sum(is.na(as.numeric(y))),
          " (", round(sum(is.na(as.numeric(y)))/length(y)*100, 2),")", sep = ""),
    paste(sum(is.na(as.numeric(z))),
          " (", round(sum(is.na(as.numeric(z)))/length(z)*100, 2),")", sep = "")
  )
  Total <- c(length(x), length(y), length(z))
  tab1 <- data.frame(Total,
                     `Numeric Values (N, %)`,
                     `Non-numeric Values (N, %)`,
                     Site = c("SMH", "SBK", "UHN"))
  names(tab1)[c(2,3)] <- c("Numeric Values (N, %)", "Non-numeric Values (N, %)")
  print(kable(tab1))
  cat("### Summary of numeric values\n")
  tab2 <- cbind(Site = c("SMH", "SBK", "UHN"),
                rbind(summary(as.numeric(x[!is.na(as.numeric(x))])),
                summary(as.numeric(y[!is.na(as.numeric(y))])),
                summary(as.numeric(z[!is.na(as.numeric(z))])))) %>% data.frame
  print(kable(tab2, caption = "summary of numeric values", format = "markdown"))
  x[is.na(as.numeric(x))&str_detect(x, "@")&!is.na(x)] <- 
    str_replace_all(x[is.na(as.numeric(x))&str_detect(x, "@")&!is.na(x)], "@", "@ ")
  y[is.na(as.numeric(y))&str_detect(y, "@")&!is.na(y)] <- 
    str_replace_all(y[is.na(as.numeric(y))&str_detect(y, "@")&!is.na(y)], "@", "@ ")
  tab3 <- rbind(data.table(table(x[is.na(as.numeric(x))], useNA = "ifany"), site = "SMH"),
                data.table(table(y[is.na(as.numeric(y))], useNA = "ifany"), site = "SBK"),
                data.table(table(z[is.na(as.numeric(z))], useNA = "ifany"), site = "UHN"))
  names(tab3)[1:2] <- c("Non-numeric Result Value", "N")
  cat("### Summary of non-numeric values\n")
  kable(tab3)
}


# ------------------------------- hgb ------------------------------------------
hgb.smh <- smh.lab[Test.Name=="HGB"]
hgb.sbk <- sbk.lab[Test.Name=="Hemoglobin"] %>%data.table
hgb.uhn <- uhn.lab[Test.Item=="Hb"&Test.Name=="CBC"] %>%data.table
hgb.msh <- msh.lab[Test.ID%in%c("HGB")]
#hgb.msh[is.na(as.numeric(Result.Value)), Result.Value] %>% table
# lab.desc(hgb.smh$Result.Value,
#          hgb.sbk$Result.Value,
#          hgb.uhn$Result.Value)

hgb <- rbind(hgb.smh, 
             hgb.sbk,
             hgb.uhn, 
             hgb.msh, fill = T)

hgb[is.na(as.numeric(Result.Value)), Result.Value] %>% table
hgb[Result.Value=="70.0 g/L   specimen is hemolysed", Result.Value:=70]
fwrite(data.table(table(hgb[is.na(as.numeric(Result.Value)), Result.Value])),
       "H:/GEMINI/Results/DataSummary/nonnum.lab.values/hgb.csv")
fwrite(hgb[!is.na(as.numeric(Result.Value))], "H:/GEMINI/Data/GEMINI/Lab/lab.hgb.csv")


# ------------------------------- wbc ------------------------------------------
wbc.smh <- smh.lab[Test.Name=="WBC"]
wbc.sbk <- sbk.lab[Test.Name=="WBC Count"] 
wbc.uhn <- uhn.lab[Test.Item=="WBC"&Test.Name=="CBC"]
wbc.msh <- msh.lab[Test.ID=="WBC"]
# lab.desc(wbc.smh$Result.Value,
#          wbc.sbk$Result.Value,
#          wbc.uhn$Result.Value)
# wbc.smh[str_sub(Result.Value,1,1)%in%c(1:9, "0.")&is.na(as.numeric(Result.Value))]&
#           !is.na(Result.Value)&str_detect(Result.Value, "@")]
wbc <- rbind(wbc.smh,
             wbc.sbk,
             wbc.uhn, 
             wbc.msh, fill = T)
wbc[is.na(as.numeric(Result.Value)), Result.Value] %>% table %>% data.table
wbc[Result.Value=="< 0.1", Result.Value:= 0.1]
wbc[Result.Value=="118.53 @?CKD", Result.Value:= 188.53]
wbc[Result.Value=="7.87.", Result.Value:= 7.87]
fwrite(data.table(table(wbc[is.na(as.numeric(Result.Value)), Result.Value])),
       "H:/GEMINI/Results/DataSummary/nonnum.lab.values/wbc.csv")
fwrite(wbc[!is.na(as.numeric(Result.Value))], "H:/GEMINI/Data/GEMINI/Lab/lab.wbc.csv")


# ---------------------------- platelet ----------------------------------------
plt.smh <- smh.lab[Test.Name=="PLT"]
plt.sbk <- sbk.lab[Test.Name=="Platelet Count"] 
plt.uhn <- uhn.lab[Test.Item=="Plt"&Test.Name=="CBC"]
plt.msh <- msh.lab[Test.ID=="PLT"]

# lab.desc(plt.smh$Result.Value,
#          plt.sbk$Result.Value,
#          plt.uhn$Result.Value)
plt <- rbind(plt.smh,
             plt.sbk,
             plt.uhn,
             plt.msh, fill = T)
plt[is.na(as.numeric(Result.Value)), Result.Value] %>% table %>% data.table ->check
plt[startsWith(Result.Value, "<"), Result.Value:=3]
plt[Result.Value=="93 .", Result.Value:=93]
fwrite(data.table(table(plt[is.na(as.numeric(Result.Value)), Result.Value])),
       "H:/GEMINI/Results/DataSummary/nonnum.lab.values/plt.csv")
fwrite(plt[!is.na(as.numeric(Result.Value))], "H:/GEMINI/Data/GEMINI/Lab/lab.plt.csv")

# ---------------------------- sodium ------------------------------------------
sodium.smh <- smh.lab[Test.Name=="Sodium"]
sodium.sbk <- sbk.lab[Test.Name=="Sodium"] 
sodium.uhn <- uhn.lab[Test.Item=="Sodium"&
                        Test.Name%in%c("Electrolytes, Plasma",
                                       "Electrolytes, Creatinine, Glucose Profile",
                                       "Sodium, Plasma",
                                       "Electrolytes, Creatinine, Profile")]
sodium.msh <- msh.lab[Test.Name%in%c("Sodium plasma", "Sodium blood", "Sodium serum",
                                     "Sodium, Venous")&Test.ID%in%
                        c("NAPL", "NAW", "NAS", "NAV")]
table(sodium.msh[, .(Test.Name, Test.ID)])
sodium.msh[is.na(as.numeric(Result.Value)), Result.Value] %>% table
# lab.desc(sodium.smh$Result.Value,
#          sodium.sbk$Result.Value,
#          sodium.uhn$Result.Value)
sodium <- rbind(sodium.smh,
                sodium.sbk,
                sodium.uhn, 
                sodium.msh, fill = T)
sodium[is.na(as.numeric(Result.Value)), Result.Value] %>% table
sodium[Result.Value%in%c("<110", "< 100", "<110 "), Result.Value:=110]
sodium[Result.Value%in%c(">180", "> 200", ">200"), Result.Value:=180]

fwrite(data.table(table(sodium[is.na(as.numeric(Result.Value)), Result.Value])),
       "H:/GEMINI/Results/DataSummary/nonnum.lab.values/sodium.csv")
sodium[!is.na(as.numeric(Result.Value))] %>%fwrite("H:/GEMINI/Data/GEMINI/Lab/lab.sodium.csv")
#fwrite("H:/GEMINI/Results/DataSummary/unlikely lab value/sodium.lt100.csv")

# ---------------------------- potassium ---------------------------------------
potassium.smh <- smh.lab[Test.Name=="Potassium"]
potassium.sbk <- sbk.lab[Test.Name=="Potassium"]
potassium.uhn <- uhn.lab[Test.Item=="Potassium"&Test.Name%in%c(
  "Electrolytes, Plasma", "Electrolytes, Creatinine, Glucose Profile",
  "Potassium, Plasma", "Electrolytes, Creatinine, Profile")]
potassium.msh <- msh.lab[Test.ID%in%c("KPL", "KW", "KS", "KV")]
table(potassium.msh$Test.Name)
# lab.desc(potassium.smh$Result.Value,
#          potassium.sbk$Result.Value,
#          potassium.uhn$Result.Value)
data.table(table(potassium.msh[is.na(as.numeric(Result.Value)), Result.Value])) -> check
potassium <- rbind(potassium.smh,
                   potassium.sbk,
                   potassium.uhn, 
                   potassium.msh, fill = T)
potassium[Result.Value%in%c("<2.0", "<1.0", "<2.0 ", "<2.0  "), Result.Value := "2.0"]
potassium[Result.Value%in%c(">8.0", "> 10.0", ">10.0 ", ">20.0", ">8.0 ",
                            ">8.0  ", ">10.0"), Result.Value := "8.0"]
potassium[Result.Value=="K=6.3", Result.Value := "6.3"]
potassium[Result.Value=="K=6.4.", Result.Value := "6.4"]
potassium[Result.Value=="5.1,", Result.Value := "5.1"]
fwrite(data.table(table(potassium[is.na(as.numeric(Result.Value)), Result.Value])),
       "H:/GEMINI/Results/DataSummary/nonnum.lab.values/potassium.csv")
potassium[!is.na(as.numeric(Result.Value))] %>%fwrite("H:/GEMINI/Data/GEMINI/Lab/lab.potassium.csv")
# fwrite(data.table(table(plt[is.na(as.numeric(Result.Value)), Result.Value])),
       # "H:/GEMINI/Results/DataSummary/nonnum.lab.values/plt.csv")


potassium[as.numeric(Result.Value)<1|as.numeric(Result.Value)>10] %>% 
  fwrite("H:/GEMINI/Results/DataSummary/unlikely lab value/potassium.lt1orgt10.csv")

# ---------------------------- troponin ----------------------------------------
troponin.smh <- smh.lab[Test.Name=="Troponin I"]
troponin.sbk <- sbk.lab[Test.Name%in%c("Troponin T, High Sensitivity","Troponin T")]
troponin.uhn <- uhn.lab[Test.Item%in%c("Troponin I","Troponin I (HS)")&
                          Test.Name%in%c("Troponin I","Troponin I (HS)")]
troponin.msh <- msh.lab[Test.ID%in%c("TNTP4", "TNTP3", "TNTS4", "TNTS3", "TNTQ4", "TNTQ3",
                                     "TRPQ3", "TNTPP")]
table(troponin.msh$Test.Name)
# lab.desc(troponin.smh$Result.Value,
#          troponin.sbk$Result.Value,
#          troponin.uhn$Result.Value)

troponin <- rbind(troponin.smh,
                  troponin.sbk,
                  troponin.uhn,
                  troponin.msh, fill = T)

fwrite(data.table(table(troponin[is.na(as.numeric(Result.Value)), Result.Value])),
       "H:/GEMINI/Results/DataSummary/nonnum.lab.values/troponin.csv")
troponin[!is.na(as.numeric(Result.Value))] %>%fwrite("H:/GEMINI/Data/GEMINI/Lab/lab.troponin.csv")

# ---------------------------- lactate -----------------------------------------
lactate.smh <- smh.lab[Test.Name%in%c("Lactate Venous", "Lactate Arterial")]
lactate.sbk <- sbk.lab[Test.Name%in%c("Lactate - Serum", "Lactate, Arterial")]
lactate.uhn <- uhn.lab[Test.Item=="Lactate"&
                         Test.Name%in%c("Lactate, Plasma",
                                        "POCT Blood Gas Arterial",
                                        "Blood Gas, Arterial",
                                        "POCT Blood Gas Venous")]
lab.desc(lactate.smh$Result.Value,
         lactate.sbk$Result.Value,
         lactate.uhn$Result.Value)
# ---------------------------- albumin -----------------------------------------
albumin.smh <- smh.lab[Test.Name=="Albumin"]
albumin.sbk <- sbk.lab[Test.Name=="Albumin"]
albumin.uhn <- uhn.lab[Test.Item=="Albumin"&Test.Name=="Albumin, Plasma"]
lab.desc(albumin.smh$Result.Value,
         albumin.sbk$Result.Value,
         albumin.uhn$Result.Value)
# ---------------------------- calcium -----------------------------------------
calcium.smh <- smh.lab[Test.Name=="Calcium"]
calcium.sbk <- sbk.lab[Test.Name=="Calcium"]
calcium.uhn <- uhn.lab[Test.Item=="Calcium"&
                         Test.Name%in%c("Calcium Total, Plasma",
                                        "Calcium Corrected, Plasma")]
lab.desc(calcium.smh$Result.Value,
         calcium.sbk$Result.Value,
         calcium.uhn$Result.Value)
# ------------------------------ AST -------------------------------------------
ast.smh <- smh.lab[Test.Name=="AST"]
ast.sbk <- sbk.lab[Test.Name=="AST"]
ast.uhn <- uhn.lab[Test.Item=="AST"&Test.Name=="AST"]
lab.desc(ast.smh$Result.Value,
         ast.sbk$Result.Value,
         ast.uhn$Result.Value)
# ------------------------------ ALT -------------------------------------------
alt.smh <- smh.lab[Test.Name=="ALT"]
alt.sbk <- sbk.lab[Test.Name=="ALT"]
alt.uhn <- uhn.lab[Test.Item=="ALT"&Test.Name=="ALT"]
lab.desc(alt.smh$Result.Value,
         alt.sbk$Result.Value,
         alt.uhn$Result.Value)
# ------------------------------ MCV -------------------------------------------
mcv.smh <- smh.lab[Test.Name=="MCV"]
mcv.sbk <- sbk.lab[Test.Name=="MCV"]
mcv.uhn <- uhn.lab[Test.Item=="MCV"&Test.Name=="CBC"]
lab.desc(mcv.smh$Result.Value,
         mcv.sbk$Result.Value,
         mcv.uhn$Result.Value)
mcv <- rbind(mcv.smh,
             mcv.sbk,
             mcv.uhn, fill = T)
mcv[as.numeric(Result.Value)<40] %>% 
  fwrite("H:/GEMINI/Results/DataSummary/unlikely lab value/mcv.lt40.csv")

# ------------------------------ ALP -------------------------------------------
alp.smh <- smh.lab[Test.Name=="ALP"]
alp.sbk <- sbk.lab[Test.Name=="ALP"]
alp.uhn <- uhn.lab[Test.Item=="ALP"&Test.Name=="ALP"]
lab.desc(alp.smh$Result.Value,
         alp.sbk$Result.Value,
         alp.uhn$Result.Value)

# ---------------------------- glucose -----------------------------------------
glucose.smh <- smh.lab[Test.Name=="Glucose Random"]
glucose.sbk <- sbk.lab[Test.Name=="Glucose- Random"]
glucose.uhn <- uhn.lab[Test.Item=="Glucose"&
                         Test.Name%in%c("Glucose, Random-Green",
                                        "Electrolytes, Creatinine, Glucose Profile",
                                        "Glucose, Random-Grey")]
lab.desc(glucose.smh$Result.Value,
         glucose.sbk$Result.Value,
         glucose.uhn$Result.Value)

glucose <- rbind(glucose.smh,
                 glucose.sbk,
                 glucose.uhn, fill = T)
glucose[as.numeric(Result.Value)>40] %>% 
  fwrite("H:/GEMINI/Results/DataSummary/unlikely lab value/glucose.gt40.csv")

glucose[as.numeric(Result.Value)>40, Site] %>% table 



# -------------------------- creatinine ----------------------------------------
crea.smh <- smh.lab[Test.Name=="Creatinine"]
crea.sbk <- sbk.lab[Test.Name=="Creatinine (renal)"]
crea.uhn <- uhn.lab[Test.Item=="Creatinine"&
                    Test.Name%in%c("Creatinine, Plasma",
                                     "Electrolytes, Creatinine, Glucose Profile",
                                     "Electrolytes, Creatinine, Profile")]
crea.msh <- msh.lab[Test.Name%in%c("Creatinine Plasma", "Creatinine Serum")&
                      Test.ID%in%c("CREAP", "CREAS")]
table(crea.msh[,.(Test.Name, Test.ID)])

crea <- rbind(crea.smh,
              crea.sbk,
              crea.uhn, 
              crea.msh, fill = T)
#crea.msh[is.na(as.numeric(Result.Value)), Result.Value] %>% table
crea[is.na(as.numeric(Result.Value)), Result.Value] %>% table %>% data.table
crea[startsWith(Result.Value, "<"), Result.Value:=20]
crea[as.numeric(Result.Value)>40, Site] %>% table 
crea[!is.na(as.numeric(Result.Value))] %>%fwrite("H:/GEMINI/Data/GEMINI/Lab/lab.creatinine.csv")
