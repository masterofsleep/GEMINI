# --------------------------- Lab Summary --------------------------------------
# ---------------------------- 2017-06-28 --------------------------------------
library(gemini)
lib.pa()
rm(list = ls())

smh.lab <- readg(smh, corelabs, dt = T)
sbk.lab <- rbind(readg(sbk, labs_er, dt = T), readg(sbk, labs_ip, dt = T))
uhn.lab <- readg(uhn, labs, dt = T)
msh.lab <- readg(msh, lab, dt = T)
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



lab_map <- readxl::read_excel("H:/GEMINI/Coding/GEMINI-Lab Mapping-UHN_SMH_SBK_SHS_06.15.17.xlsx",
                              sheet = 3)
# creating long format
for(i in 2:nrow(lab_map)){
  if(is.na(lab_map$`Test `[i])){
    lab_map$`Test `[i] <- lab_map$`Test `[i-1] 
  }
}
find_test <- function(test_name){
  test_map <- lab_map[lab_map$`Test `==test_name, ]
  test_smh <- smh.lab[paste(Test.Name, Test.ID)%in%
                        paste(test_map$`SMH Test.Name`, test_map$`SMH Test.ID`)]
  test_sbk <- sbk.lab[paste(Test.Name, Test.ID)%in%
                        paste(test_map$`SBK Test.Name`, test_map$`SMH Test.ID`)]
  test_uhn <- uhn.lab[paste(Test.Item, Test.Name)%in%
                        paste(test_map$`UHN (test.item)`, test_map$`UHN Test.Name`)]
  test_msh <- msh.lab[paste(Test.Name, Test.ID)%in%
                        paste(test_map$`Sinai Name`, test_map$`Sinai TEST_ID`)]
  test <- rbind(test_smh, test_sbk, test_uhn, test_msh, fill = T)
  test$Test <- test_name
  return(test)
}
all_lab <- NULL
for(i in unique(lab_map$`Test `)){
  all_lab <- rbind(all_lab, find_test(i))
}

fwrite(all_lab, "H:/GEMINI/Data/GEMINI/Lab/gim.lab.csv")
all_lab <- fread("H:/GEMINI/Data/GEMINI/Lab/gim.lab.csv")

all_lab[, Result.Value:=  str_replace_all(Result.Value, ",","")]
all_lab[, Result.Value:=  str_replace_all(Result.Value, "[@A-z]","")]
all_lab[, Result.Value.Number:=  as.numeric(Result.Value)]
all_lab[startwith.any(Result.Value, c("<",">")), Result.Value.Number:=  as.numeric(str_sub(Result.Value, 2, -1))]

# all_lab[is.na(as.numeric(Result.Value))] -> check
# check[!startwith.any(Result.Value, c("<",">"))] -> check2
# check[startwith.any(Result.Value, c("<",">"))] -> check3
# check2[, .N, by = Result.Value] -> nfreq
# check3[, .N, by = Result.Value] -> nfreq

lab_summary <- ddply(all_lab, ~Test.Name + Test.ID + Test.Item + Site,  .fun = function(x) 
      data.frame(Test = x$Test[1],
                 n.records = nrow(x),
                 value.min = x$Result.Value[which.min(x$Result.Value.Number)],
                 value.max = x$Result.Value[which.max(x$Result.Value.Number)],
                 value.25 = quantile(x$Result.Value.Number, probs = 0.25, na.rm = T),
                 value.50 = quantile(x$Result.Value.Number, probs = 0.5, na.rm = T),
                 value.75 = quantile(x$Result.Value.Number, probs = 0.75, na.rm = T)
                 ))

lab_summary <- lab_summary%>% arrange(Test, Site, Test.Name, Test.ID, Test.Item) %>%
  select(Test, Site, everything())

fwrite(lab_summary, "H:/GEMINI/Results/DataSummary/lab_value_summary.csv")
