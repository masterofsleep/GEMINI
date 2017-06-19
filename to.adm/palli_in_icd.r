# ------------------------ Palliative in Diagnosis -----------------------------

ip.diag <- readg(gim, ip_diag)
er.diag <- readg(gim, er_diag)
palli <- c(ip.diag[startwith.any(Diagnosis.Code, "Z515"), EncID.new],
           er.diag[startwith.any(ER.Diagnosis.Code, "Z515"), EncID.new])

ip_palli <- ip.diag[startwith.any(Diagnosis.Code, "Z515")&Diagnosis.Type=="M", EncID.new]
er_palli <- er.diag[startwith.any(ER.Diagnosis.Code, "Z515")&
          ER.Diagnosis.Type=="M", EncID.new]
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")
sum(dad$EncID.new%in%ip_palli&dad$Discharge.Disposition==7)
sum(dad$EncID.new%in%er_palli&dad$Discharge.Disposition==7)
palli <- unique(c(ip_palli, er_palli))
length(unique(c(ip_palli, er_palli)))

toadm.cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv")

sum(toadm.cohort$EncID.new%in%palli)
table(toadm.cohort[EncID.new%in%palli, Discharge.Disposition])
dad[EncID.new%in%palli, Discharge.Disposition] %>% table
