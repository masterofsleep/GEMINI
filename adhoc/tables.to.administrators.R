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

los <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv", select = c("EncID.new", "LoS"))
cohort2.same.phy <- cohort2.same.phy[EncID.new%in%los[LoS<35, EncID.new]]


cohort2.same.phy[, ':='(ADMP = paste(Admitting.Code, str_sub(EncID.new, 1, 2), sep = "-"),
              pneumonia = EncID.new%in%cap.cohort,
              copd = EncID.new%in%copd.cohort,
              chf = EncID.new%in%chf.cohort,
              stroke = EncID.new%in%stro.inc)]


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

