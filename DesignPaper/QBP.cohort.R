# ======================== QBP Cohort Selection ================================


rm(list = ls())
library(gemini)
lib.pa()
ip.diag <- readg(gim, ip_diag)
ip.int <- readg(gim, ip_int)
cohort <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
find.diag <- function(Diagnosis.Code, diag.to.be.find){
  str_sub(Diagnosis.Code, 1, 3)%in%diag.to.be.find[nchar(diag.to.be.find)==3]|
    str_sub(Diagnosis.Code, 1, 4)%in%diag.to.be.find[nchar(diag.to.be.find)==4]|
    str_sub(Diagnosis.Code, 1, 5)%in%diag.to.be.find[nchar(diag.to.be.find)==5]|
    str_sub(Diagnosis.Code, 1, 6)%in%diag.to.be.find[nchar(diag.to.be.find)==6]
}


find.int <- find.diag <- function(Int.code, int.to.be.find){
  str_sub(Int.code, 1, 5)%in%int.to.be.find[nchar(int.to.be.find)==5]|
    str_sub(Int.code, 1, 6)%in%int.to.be.find[nchar(int.to.be.find)==6]|
    str_sub(Int.code, 1, 7)%in%int.to.be.find[nchar(int.to.be.find)==7]|
    str_sub(Int.code, 1, 8)%in%int.to.be.find[nchar(int.to.be.find)==8]|
    str_sub(Int.code, 1, 9)%in%int.to.be.find[nchar(int.to.be.find)==9]|
    str_sub(Int.code, 1, 10)%in%int.to.be.find[nchar(int.to.be.find)==10]
}
# ---------------------------- CAP ---------------------------------------------

cap.inc <- ip.diag[Diagnosis.Type=="M"&
                      find.diag(Diagnosis.Code, 
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
ex1 <- ip.diag[find.diag(Diagnosis.Code, diag.ex), EncID.new]


cap.ex1 <- cap.inc[!cap.inc%in%ex1]

int.ex <- c("1WY19", "1LZ19HHU7A", "1LZ19HHU7J", 
            "1ZZ35CAM", "1ZZ35HAM", "1ZZ35YAM")

ex2 <- ip.int[find.int(Intervention.Code, int.ex), EncID.new]


cap.cohort <- intersect(cap.ex1[!cap.ex1%in%ex2],
                        cohort[Age>=18, EncID.new])





#-------------------------- COPD -----------------------------------------------
rm(list = ls())

copd.inc <- ip.diag[Diagnosis.Type=="M"&
  find.diag(Diagnosis.Code, c("J41", "J42", "J43", "J44"))&
  (!str_sub(Diagnosis.Code,1,4)%in%c("J431", "J432", "J430"))]



copd.cohort <- intersect(copd.inc, cohort[Age>=35, EncID.new])
# smh.hig <- readg(smh, ip_hig)
# sbk.hig <- readg(sbk, ip_hig)
# uhn.hig <- readg(uhn, ip_hig)
# msh.hig <- readg(msh, ip_hig)
# thp.hig <- readg(thp, ip_hig)
# ip.hig <- rbind(smh.hig[,.(EncID.new, HIG)],
#                 sbk.hig[,.(EncID.new, HIG)],
#                 uhn.hig[,.(EncID.new, HIG)],
#                 msh.hig[,.(EncID.new, HIG)],
#                 thp.hig[,.(EncID.new, HIG)])
# 
# ex.hig <- c("110", "112", "113", "114", "115", "116", "117", "119", "904")
# ex1 <- ip.hig[HIG%in%ex.hig, EncID.new]
copd.ex1 <- intersect(copd.inc, cohort[Age>=35, EncID.new])
 
 





copd.inc[!copd.inc%in%ex1] %>% length





#---------------------------- CHF ----------------------------------------------

chf.inc1 <- ip.diag[Diagnosis.Type =="M"&
                    find.diag(Diagnosis.Code, 
                              c("I50", "I255", "I40", "I41", "I42", "I43")), 
                    EncID.new]
chf.inc2 <- intersect(ip.diag[Diagnosis.Type =="M"&
                                find.diag(Diagnosis.Code, 
                                          c("I11")), EncID.new], 
                      ip.diag[Diagnosis.Type%in%c("1", "2","W","X","Y")&
                                find.diag(Diagnosis.Code, 
                                          c("I50")), EncID.new])
chf.inc3 <- intersect(ip.diag[Diagnosis.Type =="M"&
                                find.diag(Diagnosis.Code, 
                                          c("I13")), EncID.new], 
                      ip.diag[Diagnosis.Type%in%c("1", "2","W","X","Y")&
                                find.diag(Diagnosis.Code, 
                                          c("I50")), EncID.new])
chf.inc <- intersect(c(chf.inc1, chf.inc2, chf.inc3),
                     cohort[Age>=20, EncID.new])




#--------------------------- STROKE --------------------------------------------

stroke.code <- c("G45", "I61", "I63", "I64", "H341")

stro.inc <- ip.diag[Diagnosis.Type=="M"&
                      (find.diag(Diagnosis.Code, stroke.code)&
                         !Diagnosis.Code%in%c("G454", "I636")), EncID.new]

stro.ex <- ip.diag[Diagnosis.Type=="2"&
                     (find.diag(Diagnosis.Code, stroke.code)&
                        !Diagnosis.Code%in%c("G454", "I636")), EncID.new]
stro.inc <- stro.inc[!stro.inc%in%stro.ex]

stro.inc <- intersect(stro.inc, cohort[Age>=18, EncID.new])
