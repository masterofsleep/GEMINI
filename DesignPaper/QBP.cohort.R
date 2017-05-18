# ======================== QBP Cohort Selection ================================


rm(list = ls())
library(gemini)
lib.pa()
ip.diag <- readg(gim, ip_diag)
ip.int <- readg(gim, ip_int)
cohort <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
# ---------------------------- CAP ---------------------------------------------

# cap.inc <- ip.diag[Diagnosis.Type=="M"&
#                       startwith.any(Diagnosis.Code, 
#                               c("J13", "J14", "J15", 
#                                 "J16", "J18", "J12",
#                                 "J170", "J171", "J178",
#                                 "J100", "J110")),
#                     EncID.new]
# 
# diag.ex <- c("B24", "Z21", "D71", "J172", "J173", "J690",  "J691", "J699", 
#              "D700", "Z511","Z542", "Z926", "Z940", "Z940", "Z941", "Z942",
#              "Z943", "Z944", "Z945", "Z946", "Z947", "Z949",
#              "Z515", "T86000", "T86001", "Z9480", "Z9483", "Z9480", "Z9481", 
#              "Z9482", "Z9483", "Z9488")
# ex1 <- ip.diag[startwith.any(Diagnosis.Code, diag.ex), EncID.new]
# 
# 
# cap.ex1 <- cap.inc[!cap.inc%in%ex1]
# 
# int.ex <- c("1WY19", "1LZ19HHU7A", "1LZ19HHU7J", 
#             "1ZZ35CAM", "1ZZ35HAM", "1ZZ35YAM")
# 
# ex2 <- ip.int[startwith.any(Intervention.Code, int.ex), EncID.new]
# 
# 
# cap.cohort <- intersect(cap.ex1[!cap.ex1%in%ex2],
#                         cohort[Age>=18, EncID.new])

table(ip.diag[startsWith(Diagnosis.Code, "J1"), Diagnosis.Code])
cap.code <- c("J100", "J110", "J120", "J121", "J122", "J128", "J129",
              "J13", "J14", "J15", "J160", "J168", "J17", "J18")

cap.cohort <- ip.diag[Diagnosis.Type=="M"&
                        startwith.any(Diagnosis.Code, 
                                      cap.code), EncID.new]

sensi.cap <- sum(cap.cohort%in%cohort$EncID.new[cohort$Diag.Code=="J18"])/length(cap.cohort);sensi.cap
cohort$EncID.new[cohort$Diag.Code=="J18"] %>% length

table(str_sub(cap.cohort, 1, 2))
table(cohort[Diag.Code=="J44", Institution.Number])
#-------------------------- COPD -----------------------------------------------
rm(list = ls())
# 
# copd.inc <- ip.diag[Diagnosis.Type=="M"&
#   startwith.any(Diagnosis.Code, c("J41", "J42", "J43", "J44"))&
#   (!str_sub(Diagnosis.Code,1,4)%in%c("J431", "J432", "J430")), EncID.new]
# 
# 
# 
# copd.cohort <- intersect(copd.inc, cohort[Age>=35, EncID.new])
# # smh.hig <- readg(smh, ip_hig)
# # sbk.hig <- readg(sbk, ip_hig)
# # uhn.hig <- readg(uhn, ip_hig)
# # msh.hig <- readg(msh, ip_hig)
# # thp.hig <- readg(thp, ip_hig)
# # ip.hig <- rbind(smh.hig[,.(EncID.new, HIG)],
# #                 sbk.hig[,.(EncID.new, HIG)],
# #                 uhn.hig[,.(EncID.new, HIG)],
# #                 msh.hig[,.(EncID.new, HIG)],
# #                 thp.hig[,.(EncID.new, HIG)])
# # 
# # ex.hig <- c("110", "112", "113", "114", "115", "116", "117", "119", "904")
# # ex1 <- ip.hig[HIG%in%ex.hig, EncID.new]
# copd.ex1 <- intersect(copd.inc, cohort[Age>=35, EncID.new])
# 


copd.cohort <- ip.diag[Diagnosis.Type=="M"&
                      startwith.any(Diagnosis.Code, c("J41", "J42", "J43", "J44")), EncID.new]
sensi.copd <- sum(copd.cohort%in%cohort$EncID.new[cohort$Diag.Code=="J44"])/length(copd.cohort);sensi.copd
cohort$EncID.new[cohort$Diag.Code=="J44"] %>% length




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

sensi.chf <- sum(chf.cohort%in%cohort$EncID.new[cohort$Diag.Code=="I50"])/length(chf.cohort);sensi.chf
cohort$EncID.new[cohort$Diag.Code=="I50"] %>% length

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
fwrite(data.table(EncID.new= stro.inc), "H:/GEMINI/Results/to.administrator/qbp.stroke.csv")

stro.cohort <- ip.diag[Diagnosis.Type=="M"&startwith.any(Diagnosis.Code,
                                                         c("I63", "I64", "H341")), EncID.new]

sensi.stro <- sum(stro.cohort%in%cohort$EncID.new[cohort$Diag.Code=="I63"])/length(stro.cohort);sensi.stro
cohort$EncID.new[cohort$Diag.Code=="I63"] %>% length
 # ---------------------------- UTI ---------------------------------------------

uti.code <- c("N10", "N12", "N151", "N300", "N308", "N309","N410", "N412",
              "N413", "N510", "N390")
all.type <- NULL
for(i in uti.code){
  all.type <- c(all.type, sum(startwith.any(ip.diag$Diagnosis.Code, i)))
}

main.only <- NULL
ip.diag.main <- ip.diag[Diagnosis.Type == "M"]
for(i in uti.code){
  main.only <- c(main.only, sum(startwith.any(ip.diag.main$Diagnosis.Code, i)))
}


cbind(uti.code, all.type, main.only) %>% data.table %>% merge(icd.names, by.x = "uti.code", by.y = "Code",
                                                              all.x = T) %>% data.table %>% 
  fwrite("H:/GEMINI/Results/DesignPaper/uti.codes.numbers.csv")

uti.cohort <- ip.diag.main[startwith.any(Diagnosis.Code, uti.code), EncID.new]
fwrite(data.table(EncID.new = uti.cohort), "H:/GEMINI/Results/to.administrator/qbp.uti.csv")

sum(cohort$Diag.Code=="N39")

sensi.uti <- sum(uti.cohort%in%cohort$EncID.new[cohort$Diag.Code=="N39"])/length(uti.cohort);sensi.uti





icd.names <- fread("R:/GEMINI/Coding/CIHI/ICD_header.csv", select = c("Code", "Desc1"))
# ------------------- Z51 4 digit frequency ------------------------------------
all.type <- table(ip.diag[startwith.any(Diagnosis.Code, "Z51"), str_sub(Diagnosis.Code, 1, 4)]) %>% data.table
main <- table(ip.diag.main[startwith.any(Diagnosis.Code, "Z51"), str_sub(Diagnosis.Code, 1, 4)]) %>% data.table

freq.z51 <- merge(all.type, main, by = "V1", all.x = T)

names(freq.z51) <- c("code", "N.all.type", "N.main")
freq.z51$N.main[is.na(freq.z51$N.main)] <- 0
freq.z51 <- merge(freq.z51, icd.names, by.x = "code", by.y = "Code",
                  all.x = T)
fwrite(freq.z51, "H:/GEMINI/Results/DesignPaper/z51.freq.csv")


# ------------------- R29 4 digit frequency ------------------------------------
all.type <- table(ip.diag[startwith.any(Diagnosis.Code, "R29"), str_sub(Diagnosis.Code, 1, 4)]) %>% data.table
main <- table(ip.diag.main[startwith.any(Diagnosis.Code, "R29"), str_sub(Diagnosis.Code, 1, 4)]) %>% data.table

freq.r29 <- merge(all.type, main, by = "V1", all.x = T)

names(freq.r29) <- c("code", "N.all.type", "N.main")
freq.r29$N.main[is.na(freq.r29$N.main)] <- 0
freq.r29 <- merge(freq.r29, icd.names, by.x = "code", by.y = "Code",
                  all.x = T)
fwrite(freq.r29, "H:/GEMINI/Results/DesignPaper/r29.freq.csv")


# ------------------- A04 4 digit frequency ------------------------------------
all.type <- table(ip.diag[startwith.any(Diagnosis.Code, "A04"), str_sub(Diagnosis.Code, 1, 4)]) %>% data.table
main <- table(ip.diag.main[startwith.any(Diagnosis.Code, "A04"), str_sub(Diagnosis.Code, 1, 4)]) %>% data.table

freq.a04 <- merge(all.type, main, by = "V1", all.x = T)

names(freq.a04) <- c("code", "N.all.type", "N.main")
freq.a04$N.main[is.na(freq.a04$N.main)] <- 0
freq.a04 <- merge(freq.a04, icd.names, by.x = "code", by.y = "Code",
                  all.x = T)
fwrite(freq.a04, "H:/GEMINI/Results/DesignPaper/a04.freq.csv")

