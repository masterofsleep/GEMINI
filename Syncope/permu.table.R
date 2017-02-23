#===================   Syncope =================================================
library(gemini)
lib.pa()
#----------------- permutation table -------------------------------------------
rm(list = ls())

syncope.tab <- function(site){
  swdh()
  files <- list.files(recursive = T)
  ip_diag_path <- files[grepl(site, files)&grepl("ip_diag", files)]
  ip.dia <- fread(ip_diag_path, na.strings = c(NA,NULL,"", "NA", " "))
  ip.dia$Diagnosis.Type[is.na(ip.dia$Diagnosis.Type)] <- "3"
  er_diag_path <- files[grepl(site, files)&grepl("er_diag", files)]
  er.dia <- fread(er_diag_path, na.strings = c(NA,NULL,"", "NA", " "),
                  colClasses = list(character = c("NACRSRegistrationNumber",
                                                  "EncID.new")))
  er.dia$ER.Diagnosis.Type[is.na(er.dia$ER.Diagnosis.Type)] <- "3"
  mrd.syn.ip <- ip.dia[str_sub(Diagnosis.Code, 1, 3) ==
                                   "R55"& Diagnosis.Type%in%c("M","1"), 
                       EncID.new]
  mrd.syn.er <- er.dia[str_sub(ER.Diagnosis.Code, 1, 3) == 
                         "R55"& ER.Diagnosis.Type%in%c("M","1"),
                       EncID.new]
  cormo.any <- ip.dia[Diagnosis.Type%in%c("2","3"), EncID.new]%>%
    unique
  num1 <- length(intersect(mrd.syn.er, mrd.syn.ip))
  num2 <- length(intersect(intersect(mrd.syn.er, mrd.syn.ip), cormo.any))
  #with not syncope in dad mrd but syncope in dad type 2,3
  mrd.nosyn.co.syn <- ip.dia[str_sub(Diagnosis.Code, 1, 3) ==
                               "R55"& Diagnosis.Type%in%c("2","3"), EncID.new]
  num3 <- length(intersect(mrd.syn.er, mrd.nosyn.co.syn))
  mrd.nosyn.co.nosyn <- unique(ip.dia$EncID.new[!ip.dia$EncID.new%in%
                                           ip.dia[(str_sub(Diagnosis.Code, 1, 3)=="R55"& 
                                                     Diagnosis.Type%in%c("1","M", "2","3")), 
                                          EncID.new]])
  num4 <- length(intersect(mrd.syn.er, mrd.nosyn.co.nosyn))
  check4 <- intersect(mrd.syn.er, mrd.nosyn.co.nosyn)
  num5 <- length(unique(mrd.syn.ip))
  co.dvt.or.pe <- ip.dia[(str_sub(Diagnosis.Code, 1, 3)%in%c("I26", "I80")|
                           str_sub(Diagnosis.Code,1,4)%in%
                           c("I821", "I828","I829"))&
                           Diagnosis.Type%in%c("2","3"), EncID.new]
  num6 <- length(intersect(mrd.syn.ip, co.dvt.or.pe))
  mrd.dvt.or.pe <- ip.dia[(str_sub(Diagnosis.Code, 1, 3)%in%c("I26", "I80")|
                             str_sub(Diagnosis.Code,1,4)%in%
                             c("I821", "I828","I829"))&
                            Diagnosis.Type%in%c("M","1"), EncID.new]
  num7 <- length(intersect(mrd.syn.er, mrd.dvt.or.pe))
  num8 <- length(intersect(mrd.syn.er, co.dvt.or.pe))
  num9 <- length(union(mrd.syn.er, mrd.syn.ip))
  check9 <- union(mrd.syn.er, mrd.syn.ip)
  
  unique(mrd.syn.ip)
  return(c(num1, num2, num3, num4, num5, num6, num7, num8, num9))
}
smh <- syncope.tab("smh")
sbk <- syncope.tab("sbk")
uhn <- syncope.tab("uhn")
permu.table <- data.frame(cbind(smh, sbk, uhn))
permu.table$total <- permu.table$smh + permu.table$sbk + permu.table$uhn

write.csv(permu.table, "H:/GEMINI/Results/Syncope/Permutations.csv", row.names = F)
#validation
dadm123 <- unique(ip.dia[str_sub(Diagnosis.Code,1,3)=="R55"&Diagnosis.Type%in%c("M", "1", "2", "3"), 
                         EncID.new])


site <- "uhn"
dadm1 <- unique(ip.dia[str_sub(Diagnosis.Code,1,3)=="R55"&Diagnosis.Type%in%c("M", "1"), 
                       EncID.new])
dad23 <- unique(ip.dia[str_sub(Diagnosis.Code,1,3)=="R55"&Diagnosis.Type%in%c("2", "3"), 
                       EncID.new])
sum((!mrd.syn.er%in%dadm1)&(mrd.syn.er%in%dad23))

sum(!mrd.syn.er%in%dadm123)

site = "sbk"
