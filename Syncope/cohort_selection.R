################################################################################
##############    Syncope project Patient Selection   ##########################
################################################################################
rm(list = ls())
library(gemini)
lib.pa()

#read in all ip_diag and er-diag data
smh.ip <- readg(smh, ip_diag)
sbk.ip <- readg(sbk, ip_diag)
uhn.ip <- readg(uhn, ip_diag)

ip.diag <- rbind(smh.ip, sbk.ip[,c(1:4), with = F], uhn.ip)
rm(smh.ip, sbk.ip, uhn.ip)


smh.er <- readg(smh, er_diag)
sbk.er <- readg(sbk, er_diag)
uhn.er <- readg(uhn, er_diag)
sum(uhn.er$ER.Diagnosis.Type=="M", na.rm = T)


er.diag <- rbind(smh.er, sbk.er[,c(1,3,2,4), with = F], uhn.er)
rm(smh.er, sbk.er, uhn.er)

table(ip.diag$Diagnosis.Type)
table(er.diag$ER.Diagnosis.Type)


inc.ip <- ip.diag[Diagnosis.Type%in%c("M")&
                    str_sub(Diagnosis.Code, 1, 3)=="R55", EncID.new]
inc.er <- er.diag[ER.Diagnosis.Type%in%c("M", "1")&
                    str_sub(ER.Diagnosis.Code, 1, 3)=="R55", EncID.new]


#all patients diaged with syncope as mrd
inc.all <- union(inc.ip, inc.er)



#read in all the dad data
smh.dad <- readg(smh, dad) %>% 
  select(EncID.new, Admit.Date, Discharge.Date, Gender, Age)
sbk.dad <- readg(sbk, dad) %>% 
  select(EncID.new, Admit.Date, Discharge.Date, Gender, Age)
uhn.dad <- readg(uhn, dad) %>% 
  select(EncID.new, Admit.Date, Discharge.Date, Gender, Age)
dad <- rbind(smh.dad, sbk.dad, uhn.dad)
rm(smh.dad, sbk.dad, uhn.dad)


inc.date <- dad %>% 
  filter(Discharge.Date >= ymd("2011-04-01")&
           Discharge.Date <=ymd("2015-03-31")&
           EncID.new%in%inc.all)

#read in all adm data
smh.adm <- readg(smh, adm) %>% select(Hash, EncID.new)
sbk.adm <- readg(sbk, adm) %>% select(Hash, EncID.new)
uhn.adm <- readg(uhn, adm) %>% select(Hash, EncID.new)
adm <- rbind(smh.adm, sbk.adm, uhn.adm)
rm(smh.adm, sbk.adm, uhn.adm)
sum(adm$Hash == "c3ed0844860fb77e4fcacbc5124ad71bede04a0579a862a5301a8dd132957692")

inc.date <- merge(inc.date, adm, by = "EncID.new", all.x = T, all.y = F)
#check how many have multiple admissions for syncope
sum(duplicated(inc.date$Hash))
inc <- inc.date %>% arrange(Hash, Discharge.Date) %>% filter(!duplicated(Hash))


#exclusion 1a

ex1a.bydate <- dad %>% 
  filter(Discharge.Date >= ymd("2010-04-01")&
           Discharge.Date <=ymd("2011-03-31")) %>% 
  select(EncID.new) %>% as.vector

ex1a.bydia <- union(ip.diag[str_sub(Diagnosis.Code, 1, 3)=="R55", EncID.new],
                 er.diag[str_sub(ER.Diagnosis.Code, 1, 3)=="R55", EncID.new])

ex1a <- adm %>% 
  filter(EncID.new %in% intersect(ex1a.bydate$EncID.new, ex1a.bydia)) %>%
  select(Hash)

sum(inc$Hash%in%ex1a$Hash)
inc.ex1a <- inc %>% filter(!Hash%in%ex1a$Hash)
#excluded 26


ex1b <- ip.diag[Diagnosis.Type=="1"&
                str_sub(Diagnosis.Code, 1, 3)=="R55", EncID.new]
sum(inc.ex1a$EncID.new%in%ex1b)

inc.ex1b <- inc.ex1a%>% filter(!EncID.new%in%ex1b) %>%data.table

write.csv(inc.ex1b, "H:/GEMINI/Results/Syncope/inc.ex1b.csv", row.names = F)
#excluded 140




#exclusion 3


inc.ex1b <- fread("H:/GEMINI/Results/Syncope/inc.ex1b.csv")

ex3a <- ip.diag %>% filter((startsWith(Diagnosis.Code, "O")|
                            startsWith(Diagnosis.Code, "Z3"))&
                            Diagnosis.Type%in%c("M","1","2","3"))%>%
                    select(EncID.new)



sum(inc.ex1b$EncID.new %in% ex3a$EncID.new)

table(dad$Gender)
ex3.list <- inc.ex1b%>%
  filter((EncID.new%in%ex3a|(Gender=="F"&Age <50))& (startsWith(EncID.new,"13")))

preg.list.uhn <- inc.ex1b%>%
  filter((EncID.new%in%ex3a|(Gender=="F"&Age <50))& (startsWith(EncID.new,"13")))
write.csv(preg.list.uhn, "H:/GEMINI/Results/Syncope/preg.list.uhn.csv", 
          row.names = F)
#merge mrn to the list for hassan to check
swdr("SMH/CIHI/Archive")
link <- fread("smh.GIM_IP_LINKING_LIST.csv")
link$EncID.new <- paste("11", link$EncID.new, sep = "")

ex3.list.mrn <- merge(ex3.list, link[,.(MRN, EncID.new)], by = "EncID.new",
                      all.x = T, all.y = F)
write.csv(ex3.list.mrn, "H:/GEMINI/Results/Syncope/preg.list.smh.csv", 
          row.names = F)


inc.ex1b <- inc.ex1b%>%
  filter(!EncID.new%in%ex3.list$EncID.new) 



write.csv(inc.ex3, "H:/GEMINI/Results/Syncope/inc.ex3.csv", row.names = F)



#secondary outcome
inc.ex3 <- fread("H:/GEMINI/Results/Syncope/inc.ex3.csv")


#number of D-DIMER


smh.lab <- readg(smh, corelabs)
ddm.smh <- smh.lab[Test.Name=="d-Dimer", ]
rm(smh.lab)


sbk.labip <- readg(sbk, labs_ip)
sbk.laber <- readg(sbk, labs_er)
ddm.sbkip <- sbk.labip[Test.Name=="D-Dimers", ]
ddm.sbker <- sbk.laber[Test.Name=="D-Dimers", ]


ddm.sbk <- rbind(ddm.sbker[,c(3:10), with = F],
                 ddm.sbkip[,c(3:10), with = F])


uhn.lab <- readg(uhn, labs)
ddm.uhn <- uhn.lab[Test.Name=="D-Dimers", ]

ddm.uhn[,Collection.DtTm:= ymd_hm(paste(Specimen.Collected.Date, Specimen.Collected.Time))]

names(ddm.smh)
names(ddm.sbk)
names(ddm.uhn)


ddm.uhn[,Collection.DtTm:=as.character(ymd_hm(paste(Test.Date, Test.Time)))]
ddm.smh[,Site:="SMH"]
ddm.sbk[,Site:="SBK"]
ddm.uhn[,Site:="UHN"]

ddm <- rbind(ddm.smh[,.(Test.Name, Collection.DtTm, Result.Value, Result.Unit,
                        Reference.Range, EncID.new, Site)],
             ddm.sbk[,.(Test.Name, Collection.DtTm, Result.Value, Result.Unit,
                        Reference.Range, EncID.new, Site)],
             ddm.uhn[,.(Test.Name, Collection.DtTm, Result.Value, Result.Unit,
                        EncID.new, Site)], fill = T)

ddm <- ddm[EncID.new%in%inc.ex2$EncID.new]
write.csv(ddm, "H:/GEMINI/Results/Syncope/ddm.csv", row.names = F, na = "")
#--------------------------- CTPA ----------------------------------------------
inc.ex1b <- fread("H:/GEMINI/Results/Syncope/inc.ex1b.csv")
smh.ct <- readg(smh, ct)

testnames <- unique(smh.ct$proc_desc_long)

smh.ctpe.names1 <- c("THORAX -/+ CONT",
                    "THORAX + CONT",
                    "THORAX HI-RES LOW-RES CONT",
                    "THORAX PE",
                    "THORAX PE/ABDOMEN/PELVIS + CONT",
                    "THORAX/ABDOMEN + CONT",
                    "THORAX/ABDOMEN/PELVIS -/+ CONT",
                    "THORAX/ABDOMEN/PELVIS + CONT",
                    "THORAX HI-RES",
                    "THORAX HI-RES LO-RES",
                    "THORAX LOW DOSE")
smh.ctpe.names <- c("THORACIC ANEUR - CONT",
                    "THORACIC ANEUR CTA",
                    "THORACO-ABDO ANEUR - CONT",
                    "THORACO-ABDO ANEUR -/CTA CONT",
                    "THORACO-ABDO ANEUR CTA",
                    "THORACO-ABDO ANEUR POST TEVAR",
                    "THORACO-ABDO DISSECT -/CTA CONT",
                    "THORACO-ABDO DISSECT -/CTA/+ CON",
                    "THORAX - CONT",
                    "THORAX -/+ CONT",
                    "THORAX + CONT",
                    "THORAX HHT",
                    "THORAX HHT LOW DOSE",
                    "THORAX HI-RES",
                    "THORAX HI-RES LO-RES",
                    "THORAX HI-RES LOW-RES CONT",
                    "THORAX LOW DOSE",
                    "THORAX PE",
                    "THORAX PE/ABDOMEN/PELVIS + CONT",
                    "THORAX/ABDOMEN - CONT",
                    "THORAX/ABDOMEN + CONT",
                    "THORAX/ABDOMEN/PELVIS - CONT",
                    "THORAX/ABDOMEN/PELVIS -/+ CONT",
                    "THORAX/ABDOMEN/PELVIS + CONT" )
smh.ctpe.names %in% testnames
smh.ctpe <- smh.ct[proc_desc_long%in%smh.ctpe.names& 
                     EncID.new%in% inc.ex1b$EncID.new]

smh.ctpe %>% select(EncID.new, proc_desc_long, result, impression) %>%
  write.csv("H:/GEMINI/Results/Syncope/ctpa.smh.new.csv", row.names = F)

sbk.radip <- readg(sbk, rad_ip, colClasses = list(character = c("SID", "PID", "EncID.new")))
sbk.rader <- readg(sbk, rad_er, colClasses = list(character = c("SID", "PID", "EncID.new")))
sbk.res <- readg(sbk, rad_res)
sbk.radip <- merge(sbk.radip, sbk.res, by = "SID", all.x = T, all.y = F)
sbk.rader <- merge(sbk.rader, sbk.res, by = "SID", all.x = T, all.y = F)




testnames <- unique(c(sbk.rader$TestName, sbk.radip$`Test Name`))
sbk.ctpe.names <- c("CT Angio chest for pulmonary emboli",
                    "CT Angiogram chest",
                    "CT Angiogram chest, abdomen, pelvis",
                    "CT Chest",
                    "CT chest",
                    "CT chest, abdomen, pelvis")
sbk.ctpe.ip <- sbk.radip[`Test Name`%in% sbk.ctpe.names&
                              EncID.new%in% inc.ex1b$EncID.new]

                  
sbk.ctpe.er <- sbk.rader[TestName %in% sbk.ctpe.names&
                           EncID.new%in% inc.ex1b$EncID.new]
names(sbk.ctpe.ip)[3] <- "TestName"
sbk.ctpe <- rbind(sbk.ctpe.er[,.(EncID.new, SID, TestName, Results)], 
                  sbk.ctpe.ip[,.(EncID.new, SID, TestName, Results)])
write.csv(sbk.ctpe, "H:/GEMINI/Results/Syncope/ctpa.sbk.csv", row.names = F)




uhn.radip <- readg(uhn, rad_ip)
uhn.rader <- readg(uhn, rad_er)

testnames <- unique(c(uhn.rader$ProcedureName, uhn.radip$ProcedureName))
uhn.ctpe.names <- c("Angiography Body Diagnostic",
                    "Angiography Body Angiogram Venous Subclavian",
                    "Angiography Body Angiogram Thoracic/Abdominal/Pelvic",
                    "CT Angiography Pulmonary Arteries",
                    "CT Angiography Dissection Chest",
                    "CT Chest")
uhn.ctpe.names %in% testnames
uhn.ctpe.ip <- uhn.radip[ProcedureName%in%uhn.ctpe.names&
                           EncID.new%in%inc.ex1b$EncID.new]
uhn.ctpe.er <- uhn.rader[ProcedureName%in%uhn.ctpe.names&
                           EncID.new%in%inc.ex1b$EncID.new]



rbind(uhn.ctpe.er[,.(EncID.new, ProcedureName, ReportText)], 
      uhn.ctpe.ip[,.(EncID.new, ProcedureName, ReportText)]) %>%
  write.csv("H:/GEMINI/Results/Syncope/ctpa.uhn.new.csv", row.names = F)


sum(duplicated(smh.ctpe$EncID.new))
sum(duplicated(sbk.ctpe.ip$EncID.new))
sum(duplicated(sbk.ctpe.er$EncID.new))
sum(duplicated(uhn.ctpe.ip$EncID.new))
sum(duplicated(uhn.ctpe.er$EncID.new))




#----------------------- VQ Scan -----------------------------------------------
inc.ex1b <- fread("H:/GEMINI/Results/Syncope/inc.ex1b.csv")
smh.nuc <- readg(smh, nuc)

testnames <- unique(smh.nuc$proc_desc_long)


smh.vq.names <- testnames[startsWith(testnames, "LUNG")]
smh.vq <- smh.nuc[proc_desc_long%in%smh.vq.names & 
                     EncID.new%in%inc.ex1b$EncID.new]
smh.vq %>% select(EncID.new, proc_desc_long, result, impression) %>%
  write.csv("H:/GEMINI/Results/Syncope/vq.smh.csv", row.names = F)

sbk.radip <- readg(sbk, rad_ip, colClasses = list(character = c("SID", "PID", "EncID.new")))
sbk.rader <- readg(sbk, rad_er, colClasses = list(character = c("SID", "PID", "EncID.new")))
sbk.res <- readg(sbk, rad_res)
sbk.radip <- merge(sbk.radip, sbk.res, by = "SID", all.x = T, all.y = F)
sbk.rader <- merge(sbk.rader, sbk.res, by = "SID", all.x = T, all.y = F)




testnames <- unique(c(sbk.rader$TestName, sbk.radip$`Test Name`))
sbk.vq.names <- c("Emerg-Lung Scan V/Q(Aero+PERF",
                                      "Lung Perfusion Scan",
                                      "Lung Scan Vent / Perf")
sbk.vq.names%in% testnames
                                      
sbk.vq.ip <- sbk.radip[`Test Name`%in% sbk.vq.names &
                           EncID.new%in%inc.ex1b$EncID.new]


sbk.vq.er <- sbk.rader[TestName %in% sbk.vq.names &
                           EncID.new%in%inc.ex1b$EncID.new]
names(sbk.vq.ip)[3] <- "TestName"
sbk.vq <- rbind(sbk.vq.er[,.(EncID.new, SID, TestName, Results)], 
                  sbk.vq.ip[,.(EncID.new, SID, TestName, Results)])
write.csv(sbk.vq, "H:/GEMINI/Results/Syncope/vq.sbk.csv", row.names = F)




uhn.radip <- readg(uhn, rad_ip)
uhn.rader <- readg(uhn, rad_er)

testnames <- unique(c(uhn.rader$ProcedureName, uhn.radip$ProcedureName))
uhn.vq.names <- c("NM Perfusion Lung Scan",
                  "NM Quantitative Perfusion Lung Scan",
                  "NM Quantitative Ventilation Perfusion Lung Scan",
                  "NM V/Q Ventilation Perfusion Lung Scan")
uhn.vq.names%in%testnames
                  
uhn.vq.ip <- uhn.radip[ProcedureName%in%uhn.vq.names&
                           EncID.new%in%inc.ex1b$EncID.new]
uhn.vq.er <- uhn.rader[ProcedureName%in%uhn.vq.names&
                           EncID.new%in%inc.ex1b$EncID.new]



rbind(uhn.vq.er[,.(EncID.new, ProcedureName, ReportText)], 
      uhn.vq.ip[,.(EncID.new, ProcedureName, ReportText)]) %>%
  write.csv("H:/GEMINI/Results/Syncope/vq.uhn.csv", row.names = F)


#------------------ Doppler Ultrasound -----------------------------------------

inc.ex1b <- fread("H:/GEMINI/Results/Syncope/inc.ex1b.csv")
smh.us <- readg(smh, us)

testnames <- unique(smh.us$proc_desc_long)


smh.du.names <- c("DOP LEG VEIN/EXTREMITY BILAT",
                  "DOP LEG VEIN/EXTREMITY UNILAT",
                  "DOP VEN EXTREMITY BILAT",
                  "DOP VEN EXTREMITY UNIL",
                  "DOP ILIAC VESSELS BILAT",
                  "EXTREMITY BILAT",
                  "EXTREMITY UNILAT",
                  "EXTREMITY LEFT")
smh.du.names%in%testnames
smh.du <- smh.us[proc_desc_long%in%smh.du.names & 
                    EncID.new%in%inc.ex1b$EncID.new]
smh.du %>% select(EncID.new, proc_desc_long, result, impression) %>%
  write.csv("H:/GEMINI/Results/Syncope/du.smh.csv", row.names = F)

sbkrad <- readg(sbk.rad, rad.csv)


testnames <- unique(sbkrad$Test.Name)
sbk.du.names <- c("Abdomen Ltd.+Bil leg doppler",
                  "Abdomen Ltd.+LT leg doppler",
                  "Abdomen Ltd.+RT leg doppler",
                  "Abdomen US + Right Leg Doppler",
                  "Abdomen US + Left Leg Doppler",
                  "Abdomen+Leg Doppler: Bil US",
                  "Arm Venous C Doppler Bil US",
                  "Arm Venous C Doppler Left US",
                  "Arm Venous C Doppler Right US",
                  "Leg Doppler Bilateral",
                  "Leg Doppler Left",
                  "Leg Doppler Right",
                  "Subclavian/Jug Doppler US: RT",
                  "Subclavian/Jug Doppler US:Left",
                  "Subclavian/Jug Doppler US:BIL",
                  "Venous Doppler & US:  Left leg",
                  "Venous Doppler & US: Bil leg",
                  "Venous Doppler & US: Right leg",
                  "WB: Arm Venous C Doppler Left US",
                  "WB: Leg Venous C Doppler Bilateral US",
                  "WB: Leg Venous C Doppler Left US",
                  "WB: Leg Venous C Doppler Right US",
                  "Abdomen + pelvic + leg: bil US",
                  "Groin doppler Bil: US")
sbk.du.names%in% testnames

sbk.du.ip <- sbk.radip[`Test Name`%in% sbk.du.names &
                         EncID.new%in%inc.ex1b$EncID.new]


sbk.du.er <- sbk.rader[TestName %in% sbk.du.names &
                         EncID.new%in%inc.ex1b$EncID.new]
names(sbk.du.ip)[3] <- "TestName"
sbk.du <- rbind(sbk.du.er[,.(EncID.new, SID, TestName, Results)], 
                sbk.du.ip[,.(EncID.new, SID, TestName, Results)])
write.csv(sbk.du, "H:/GEMINI/Results/Syncope/du.sbk.csv", row.names = F)




uhn.radip <- readg(uhn, rad_ip)
uhn.rader <- readg(uhn, rad_er)

testnames <- unique(c(uhn.rader$ProcedureName, uhn.radip$ProcedureName))
uhn.du.names <- c("US Vascular Peripheral Vein Doppler",
                  "US Vascular Peripheral Vein Doppler Lower Extremity",
                  "US Vascular Peripheral Vein Doppler Upper Extremity",
                  "US Calf",
                  "US Extremity",
                  "US Thigh",
                  "US Vascular Jugular Vein Doppler")
uhn.du.names%in%testnames

uhn.du.ip <- uhn.radip[ProcedureName%in%uhn.du.names&
                         EncID.new%in%inc.ex1b$EncID.new]
uhn.du.er <- uhn.rader[ProcedureName%in%uhn.du.names&
                         EncID.new%in%inc.ex1b$EncID.new]



rbind(uhn.du.er[,.(EncID.new, ProcedureName, ReportText)], 
      uhn.du.ip[,.(EncID.new, ProcedureName, ReportText)]) %>%
  write.csv("H:/GEMINI/Results/Syncope/du.uhn.csv", row.names = F)






#-------------------------- exclusion 2 ----------------------------------------
#din code for anticoagulation
rm(list = ls())

warfarin <- as.character(c(1918311, 2007959, 1918338, 2229741, 1918362, 
                           1918346, 2240205, 
              2240206, 2244462, 2244463, 2244466, 2244467, 2287498, 2265273, 
              2265311, 2265346, 2344092, 2344068, 2344114, 2344076, 10308, 
              2245618, 2242687, 2244465, 2265303, 2265338, 2265281, 2344033, 
              2344025, 2344041, 2344084, 2242680, 2242681, 2242683, 2242684, 
              2242685, 2242686, 2287528, 2265354, 2265370, 2344106, 2242929, 
              2242928, 2242697, 2244464, 2287501, 1918354, 2242925, 2242926, 
              2242927, 2242682, 2242924, 2245817, 2245818, 2311097, 2311119, 
              2311135, 2152460, 2152479, 2152487, 2152495, 2152509))

heparin <- as.character(c(283150, 353760, 740527, 727520, 2264307, 740578, 
                          2230084, 1900757, 725315, 894605, 1935941, 2382334, 
                          453811, 579718, 453781, 725323, 1935933, 828750, 
                          740497, 740535, 740519, 2303086, 2264315, 2303094, 
                          2303108, 886629, 640468, 1913255, 2008181, 667285, 
                          2209713))
enoxaparin <- as.character(c(2378469, 2236883, 2236564, 2378442, 2378426, 
                             2378434, 2012472, 2242692, 2265206, 2265214, 
                             2265230, 2265249))

dalteparin <- as.character(c(2132621, 2352656, 2132656, 2132648, 2231171, 
                             2352648, 2352672, 2352664, 2352680, 2377454, 
                             2132664))
tinzaparin <- as.character(c(2358158, 2056682, 2358182, 2167859, 2358166, 
                             2231478, 2229755, 2358174, 2429470, 2167840, 
                             2429462, 2229515, 2429489))

fondaparinux <- as.character(c(2245531, 2258056, 2406853))
dabigatran <- as.character(c(2312441, 2358808, 2312433))
rivaroxaban <-as.character(c(2378612, 2316986, 2378604))
apixaban <- as.character(c(2377233, 2397714))
danaparoid <- as.character(c(2129043))
nadroparin <-  as.character(c(2236913, 2240114))

anticoag <- c(warfarin, heparin, enoxaparin, dalteparin, tinzaparin,
              fondaparinux, dabigatran, rivaroxaban, apixaban, danaparoid,
              nadroparin)
anticoag.names <- c("warfarin", "heparin", "enoxaparin", "dalteparin", 
                     "tinzaparin", "fondaparinux", "dabigatran", "rivaroxaban", 
                     "apixaban", "danaparoid", "nadroparin")
rm(warfarin, heparin, enoxaparin, dalteparin, tinzaparin,
   fondaparinux, dabigatran, rivaroxaban, apixaban, danaparoid,
   nadroparin)


inc.ex1b <- fread("H:/GEMINI/Results/Syncope/inc.ex1b.csv")

smh.phar <- readg(smh, phar)


smh.anticoag <-
  smh.phar %>%filter(EncID.new %in% inc.ex1b$EncID.new &
                       (din %in% anticoag|
                          str_detect(tolower(generic_name),paste(anticoag.names, collapse = "|"))))
                                                     

write.csv(smh.anticoag, "H:/GEMINI/Results/Syncope/anticoag.smh.csv",
          row.names = F)


sbk.phar <- readg(sbk, phar)
sbk.anticoag <- sbk.phar %>%
  filter(EncID.new %in% inc.ex1b$EncID.new &(ndc_din %in% anticoag|
  str_detect(tolower(generic_name),paste(anticoag.names, collapse = "|"))))
write.csv(sbk.anticoag, "H:/GEMINI/Results/Syncope/anticoag.sbk.csv",
          row.names = F)



uhn.phar <- readg(uhn.phar, phar.nophi)
uhn.anticoag <- uhn.phar %>%
  filter(EncID.new %in% inc.ex1b$EncID.new &(DIN %in% anticoag|
  str_detect(tolower(Generic_Name),paste(anticoag.names, collapse = "|"))))
write.csv(uhn.anticoag, "H:/GEMINI/Results/Syncope/anticoag.uhn.csv",
          row.names = F)


sum(uhn.anticoag$EncID.new%in%inc.ex1b$EncID.new)





#----------- Dec 09 2016 find INR ----------------------------------------------

smh.lab <- readg(smh, corelabs)
inr.smh <- smh.lab[Test.Name == "INR",]
rm(smh.lab)




rm(uhn.laber, uhn.labip)
write.csv(inr.smh, "H:/GEMINI/Results/Syncope/inr.smh.csv",row.names = F)
write.csv(inr.sbk, "H:/GEMINI/Results/Syncope/inr.sbk.csv",row.names = F)
write.csv(inr.uhn, "H:/GEMINI/Results/Syncope/inr.uhn.csv",row.names = F)


library(readxl)
anticoag.smh <- read_excel("H:/GEMINI/Results/Syncope/anticoag.smh.xlsx", sheet = 2)[,c(12,16:23,31)]
anticoag.sbk <- read_excel("H:/GEMINI/Results/Syncope/anticoag.sbk.xlsx", sheet = 2)
anticoag.uhn <- read_excel("H:/GEMINI/Results/Syncope/anticoag.uhn.xlsx", sheet = 2)



smh.dad <- readg(smh, dad)
sbk.dad <- readg(sbk, dad)
uhn.dad <- readg(uhn, dad)

inr.smh <- inr.smh %>% arrange(EncID.new, ymd_hms(Collection.DtTm)) %>% 
  filter(!duplicated(EncID.new))
inr.smh <- data.table(inr.smh)
anticoag.smh <- merge(anticoag.smh, inr.smh[,.(Test.Name, CollectedDtTm, Result.Value,
                                               Result.Unit, Reference.Range, EncID.new)],
                      by = "EncID.new", all.x = T, all.y = F)
anticoag.smh <- merge(anticoag.smh, smh.dad[,.(EncID.new, Admit.Date, Admit.Time)],
                      by = "EncID.new", all.x = T, all.y = F)

anticoag.smh.old <- fread("H:/GEMINI/Results/Syncope/anticoag.smh.new.csv")
sum(anticoag.smh.old$Result.Value == anticoag.smh$Result.Value)



write.csv(anticoag.smh, "H:/GEMINI/Results/Syncope/anticoag.smh.new.csv",row.names = F)

sbk.labip <- readg(sbk, labs_ip)
sbk.laber <- readg(sbk, labs_er)
inr.sbkip <- sbk.labip[TestName=="PT (INR)"]
inr.sbker <- sbk.laber[TestName=="PT (INR)"]
names(inr.sbkip)[6] <- "CollectionDate"
rm(sbk.laber, sbk.labip)
inr.sbk <- rbind(inr.sbker[,c(3:9), with = F],
                 inr.sbkip[,c(4:10), with = F],fill = T)


inr.sbk[str_sub(CollectionDate, -2, -1)=="PM", 
        Collection.DtTm:= (mdy_hms(str_sub(CollectionDate, 1, 20)) + hours(12))]
inr.sbk[str_sub(CollectionDate, -2, -1)=="AM", 
        Collection.DtTm:= mdy_hms(str_sub(CollectionDate, 1, 20))]
inr.sbk <- inr.sbk%>%arrange(EncID.new, Collection.DtTm) %>%
  filter(!duplicated(EncID.new))%>%data.table

anticoag.sbk <- merge(anticoag.sbk, inr.sbk[,.(TestName, Collection.DtTm, 
                                               ResultValue, ResultUnit, ReferenceRange,
                                               EncID.new)], 
                      by = "EncID.new", all.x = T, all.y = F)

anticoag.sbk <- merge(anticoag.sbk, sbk.dad[,.(EncID.new, Admit.Date, Admit.Time)],
                      by = "EncID.new", all.x = T, all.y = F)
write.csv(anticoag.sbk, "H:/GEMINI/Results/Syncope/anticoag.sbk.new.csv",row.names = F)

uhn.labip <- readg(uhn, labs_ip)
uhn.laber <- readg(uhn, labs_er)
inr.uhn <- rbind(uhn.labip[Test.Name == "PT/INR"], 
                 uhn.laber[Test.Name == "PT/INR" ])

inr.uhn <- inr.uhn[Test.Item =="INR"]

inr.uhn[str_sub(Test.Date, -5,-5)=="/", Test.Date:= as.character(mdy(Test.Date))]
inr.uhn[str_sub(Test.Date, -5,-5)!="/", Test.Date:= as.character(ymd(Test.Date))]


inr.uhn <- inr.uhn%>% arrange(EncID.new, ymd_hm(paste(Test.Date, Test.Time))) %>%
  filter(!duplicated(EncID.new)) %>% data.table

anticoag.uhn <- merge(anticoag.uhn, inr.uhn[,.(EncID.new, Test.Name, Test.Date,
                                               Test.Time, Result.Value)],
                      by = "EncID.new", all.x = T, all.y = F)

anticoag.uhn <- merge(anticoag.uhn, uhn.dad[,.(EncID.new, Admit.Date, Admit.Time)],
                      by = "EncID.new", all.x = T, all.y = F)


write.csv(anticoag.uhn, "H:/GEMINI/Results/Syncope/anticoag.uhn.new.csv",row.names = F)






#-----------------------------  Dec 19 2016  -----------------------------------

#-----------------------------  Exclusion 2  -----------------------------------

inc.ex1b <- fread("H:/GEMINI/Results/Syncope/inc.ex1b.csv")
ex2 <- fread("H:/GEMINI/Results/Syncope/ex2.csv")

sum(inc.ex1b$EncID.new%in%ex2$ex2a)
sum(inc.ex1b$EncID.new%in%ex2$ex2b)
inc.ex2a <- inc.ex1b[!EncID.new%in%ex2$ex2a] 
inc.ex2b <- inc.ex2a[!EncID.new%in%ex2$ex2b]
fwrite(inc.ex2b, "H:/GEMINI/Results/Syncope/inc.ex2.csv")



#----------------------------- Jan 16 2017 -------------------------------------

#----------------------------- number from er and ip ---------------------------

sum(inc$EncID.new%in%inc.er)
sum(inc$EncID.new%in%inc.ip)
