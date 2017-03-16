# ----------------------- Summary of Lab Tests ---------------------------------
# --------------------------- 2017-03-16 ---------------------------------------
library(gemini)
lib.pa()
rm(list = ls())

smh.lab <- readg(smh, corelabs, dt = T)
sbk.lab <- rbind(readg(sbk, labs_er, dt = T), readg(sbk, labs_ip, dt = T))
uhn.lab <- readg(uhn, labs, dt = T)
msh.lab <- readg(msh, lab, dt= T)
# msh.lab.freq <- table(msh.lab[,.(Name, TEST_ID)]) %>% data.table 
# msh.lab.freq <- msh.lab.freq[N!=0]
# fwrite(msh.lab.freq, "H:/GEMINI/Results/Shortadm/msh.lab.freq.csv")
lab.desc <- function(x){
  cat("Number(%) of numeric value is ",
      sum(!is.na(as.numeric(x))),
      "(", round(sum(!is.na(as.numeric(x)))/length(x)*100, 2), ")\n")
  cat("Number(%) of non numeric value is ",
      sum(is.na(as.numeric(x))),
      "(", round(sum(is.na(as.numeric(x)))/length(x)*100,2), ")\nSummary of numeric values:\n")
  print(summary(as.numeric(x[!is.na(as.numeric(x))])))
  cat("Freq table of non numeric values:\n")
  freq.table <- data.table(table(x[is.na(as.numeric(x))]))
  names(freq.table) <- c("Result.Value", "Frequency")
  print(freq.table)
}

smh.lab[str_sub(Result.Value,1,1)%in%c(1:9)&is.na(as.numeric(Result.Value))] -> check
        Result.Value := str_replace_all(Result.Value, "[@A-z]","")]

# ------------------------------- hgb ------------------------------------------
hgb.smh <- smh.lab[Test.Name=="HGB"] %>% data.table
hgb.sbk <- sbk.lab[Test.Name=="Hemoglobin"] %>%data.table
hgb.uhn <- uhn.lab[Test.Item=="Hb"&Test.Name=="CBC"] %>%data.table
hgb.smh[str_sub(Result.Value,1,1)%in%c(1:9)&is.na(as.numeric(Result.Value)),
        Result.Value := str_replace_all(Result.Value, "[@A-z]","")]
lab.desc(hgb.smh)
lab.desc(hgb.sbk$Result.Value)
lab.desc(hgb.uhn$Result.Value)


# ------------------------------- wbc ------------------------------------------
wbc.smh <- smh.lab[Test.Name=="WBC"]
wbc.sbk <- sbk.lab[Test.Name=="WBC Count"] 
wbc.uhn <- uhn.lab[Test.Item=="WBC"&Test.Name=="CBC"]
wbc.smh$Result.Value <- str_replace_all(wbc.smh$Result.Value, "[@A-z]","")

lab.desc(wbc.smh$Result.Value)
lab.desc(wbc.sbk$Result.Value)
lab.desc(wbc.uhn$Result.Value)


# ---------------------------- platelet ----------------------------------------
plt.smh <- smh.lab[Test.Name=="PLT"]
plt.sbk <- sbk.lab[Test.Name=="Platelet Count"] 
plt.uhn <- uhn.lab[Test.Item=="Plt"&Test.Name=="CBC"]
plt.smh$Result.Value <- str_replace_all(plt.smh$Result.Value, "[@A-z]","")

lab.desc(plt.smh$Result.Value)
lab.desc(plt.sbk$Result.Value)
lab.desc(plt.uhn$Result.Value)

# ---------------------------- sodium ------------------------------------------
sodium.smh <- smh.lab[Test.Name=="Sodium"]
sodium.sbk <- sbk.lab[Test.Name=="Sodium"] 
sodium.uhn <- uhn.lab[Test.Item=="Sodium"&
                        Test.Name%in%c("Electrolytes, Plasma",
                                       "Electrolytes, Creatinine, Glucose Profile",
                                       "Sodium, Plasma",
                                       "Electrolytes, Creatinine, Profile")]
sodium.smh$Result.Value <- str_replace_all(sodium.smh$Result.Value, "[@A-z]","")

lab.desc(sodium.smh$Result.Value)
lab.desc(sodium.sbk$Result.Value)
lab.desc(sodium.uhn$Result.Value)


# ---------------------------- potassium ---------------------------------------
potassium.smh <- smh.lab[Test.Name=="Potassium"]
potassium.sbk <- sbk.lab[Test.Name=="Potassium"]
potassium.uhn <- uhn.lab[Test.Item=="Potassium"&Test.Name%in%c(
  "Electrolytes, Plasma", "Electrolytes, Creatinine, Glucose Profile",
  "Potassium, Plasma", "Electrolytes, Creatinine, Profile")]
potassium.smh$Result.Value <- str_replace_all(potassium.smh$Result.Value, "[@A-z]","")

lab.desc(sodium.smh$Result.Value)
lab.desc(sodium.sbk$Result.Value)
lab.desc(sodium.uhn$Result.Value)

# ---------------------------- troponin ----------------------------------------
troponin.smh <- smh.lab[Test.Name=="Troponin I"]
troponin.sbk <- sbk.lab[Test.Name%in%c("Troponin T, High Sensitivity","Troponin T")]
troponin.uhn <- uhn.lab[Test.Item%in%c("Troponin I","Troponin I (HS)")&
                          Test.Name%in%c("Troponin I","Troponin I (HS)")]
lab.desc(troponin.smh$Result.Value)
lab.desc(troponin.sbk$Result.Value)
lab.desc(troponin.uhn$Result.Value)
# ---------------------------- lactate -----------------------------------------
lactate.smh <- smh.lab[Test.Name%in%c("Lactate Venous", "Lactate Arterial")]
lactate.sbk <- sbk.lab[Test.Name%in%c("Lactate - Serum", "Lactate, Arterial")]
lactate.uhn <- uhn.lab[Test.Item=="Lactate"&
                         Test.Name%in%c("Lactate, Plasma",
                                        "POCT Blood Gas Arterial",
                                        "Blood Gas, Arterial",
                                        "POCT Blood Gas Venous")]
lab.desc(lactate.smh$Result.Value)
lab.desc(lactate.sbk$Result.Value)
lab.desc(lactate.uhn$Result.Value)
# ---------------------------- albumin -----------------------------------------
albumin.smh <- smh.lab[Test.Name=="Albumin"]
albumin.sbk <- sbk.lab[Test.Name=="Albumin"]
albumin.uhn <- uhn.lab[Test.Item=="Albumin"&Test.Name=="Albumin, Plasma"]
lab.desc(albumin.smh$Result.Value)
lab.desc(albumin.sbk$Result.Value)
lab.desc(albumin.uhn$Result.Value)
# ---------------------------- calcium -----------------------------------------
calcium.smh <- smh.lab[Test.Name=="Calcium"]
calcium.sbk <- sbk.lab[Test.Name=="Calcium"]
calcium.uhn <- uhn.lab[Test.Item=="Calcium"&
                         Test.Name%in%c("Calcium Total, Plasma",
                                        "Calcium Corrected, Plasma")]
lab.desc(calcium.smh$Result.Value)
lab.desc(calcium.sbk$Result.Value)
lab.desc(calcium.uhn$Result.Value)
# ------------------------------ AST -------------------------------------------
ast.smh <- smh.lab[Test.Name=="AST"]
ast.sbk <- sbk.lab[Test.Name=="AST"]
ast.uhn <- uhn.lab[Test.Item=="AST"&Test.Name=="AST"]
lab.desc(ast.smh$Result.Value)
lab.desc(ast.sbk$Result.Value)
lab.desc(ast.uhn$Result.Value)
# ------------------------------ ALT -------------------------------------------
alt.smh <- smh.lab[Test.Name=="ALT"]
alt.sbk <- sbk.lab[Test.Name=="ALT"]
alt.uhn <- uhn.lab[Test.Item=="ALT"&Test.Name=="ALT"]
lab.desc(alt.smh$Result.Value)
lab.desc(alt.sbk$Result.Value)
lab.desc(alt.uhn$Result.Value)
# ------------------------------ MCV -------------------------------------------
mcv.smh <- smh.lab[Test.Name=="MCV"]
mcv.sbk <- sbk.lab[Test.Name=="MCV"]
mcv.uhn <- uhn.lab[Test.Item=="MCV"&Test.Name=="CBC"]
lab.desc(mcv.smh$Result.Value)
lab.desc(mcv.sbk$Result.Value)
lab.desc(mcv.uhn$Result.Value)

# ------------------------------ ALP -------------------------------------------
alp.smh <- smh.lab[Test.Name=="ALP"]
alp.sbk <- sbk.lab[Test.Name=="ALP"]
alp.uhn <- uhn.lab[Test.Item=="ALP"&Test.Name=="ALP"]
lab.desc(alp.smh$Result.Value)
lab.desc(alp.sbk$Result.Value)
lab.desc(alp.uhn$Result.Value)

# ---------------------------- glucose -----------------------------------------
glucose.smh <- smh.lab[Test.Name=="Glucose Random"]
glucose.sbk <- sbk.lab[Test.Name=="Glucose- Random"]
glucose.uhn <- uhn.lab[Test.Item=="Glucose"&
                         Test.Name%in%c("Glucose, Random-Green",
                                        "Electrolytes, Creatinine, Glucose Profile",
                                        "Glucose, Random-Grey")]
lab.desc(glucose.smh$Result.Value)
lab.desc(glucose.sbk$Result.Value)
lab.desc(glucose.uhn$Result.Value)