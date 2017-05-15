# ----------------------- Clinical Validation ----------------------------------
# ------------------------------ 2017-05-15 ------------------------------------
find_100 <- function(){
  phy.all <- readg(gim, all.phy)
  link <- readg(SMH, LINKLIST_NEWHASH)[,.(MRN, EncID.new = paste("11", EncID.new, sep = ""))]
  smh.dad <- readg(smh, dad)
  set.seed(100)
  sample_enc <- sample(phy.all[str_sub(EncID.new,1,2)=="11"&(
                               adm.GIM%in%c("y", "GP-GIM")|
                                    adm.GIM%in%c("y", "GP-GIM")),
                                  EncID.new], 100, replace = F)
  smh.dad <- merge(smh.dad, link[,.(EncID.new, MRN)], all.x = T, all.y = F)
  smh.dad[EncID.new%in%sample_enc, .(MRN, EncID.new,
                                     Admit.Date, Admit.Time,
                                     Discharge.Date, Discharge.Time)]
}  
smh_vali <- find_100()  
Result.Category = c("Lab","Lab", "Lab", "Lab",
                    "Lab","Lab","Lab","Radiology","Radiology",
                    "Culture","Culture")
Test = c("Hgb", "Sodium", "Creatinine", "INR", "Calcium", "AST",
         "Troponin", "CT Head", "Xray Chest", "Blood", "Urine")
cbind(smh_vali[1],Result.Category, Test ,
      Collection.Performed.Date = "",
      Collection.Performed.Time = "",
      Result.Value = "")
for(i in 1:100){
  fwrite(cbind(smh_vali[i],Result.Category, Test ,
        Collection.Performed.Date = "",
        Collection.Performed.Time = "",
        Result.Value = ""),
        paste("H:/GEMINI/Results/Clinical Validation/smh_patient_", i,".csv",
              sep = ""))
}
