library(gemini)
lib.pa()
#ad hot 
# 1. What proportion of patients receive the following antibiotics (can use generic name):
#   
#   Ceftriaxone
# Azithromycin
# levofloxacin
# moxifloxacin
# any medication with the word "amoxicillin"
# any medication with the word "piperacillin"
# 
# 2. Can you look at all patients at UHN, SBK, SMH with Type M diagnosis of heart failure
# 
# What proportion of patients receive 'furosemide'?

#read in all pharm data
smh.phar <- readg(smh, phar)
sbk.phar <- readg(sbk, phar)
uhn.phar <- readg(uhn.phar, phar.nophi)

c("ceftriaxone",
"azithromycin",
"levofloxacin",
"moxifloxacin")%in%tolower(smh.phar$generic_name)

antibio.smh <- smh.phar[str_detect(generic_name, toupper("ceftriaxone"))|
           str_detect(generic_name, toupper("azithromycin"))|
           str_detect(generic_name, toupper("levofloxacin"))|
           str_detect(generic_name, toupper("moxifloxacin"))|
           str_detect(generic_name, toupper("amoxicillin"))|
           str_detect(generic_name, toupper("piperacillin"))]
antibio.sbk <- sbk.phar[str_detect(generic_name, toupper("ceftriaxone"))|
                          str_detect(generic_name, toupper("azithromycin"))|
                          str_detect(generic_name, toupper("levofloxacin"))|
                          str_detect(generic_name, toupper("moxifloxacin"))|
                          str_detect(generic_name, toupper("amoxicillin"))|
                          str_detect(generic_name, toupper("piperacillin"))]
uhn.phar$generic_name <- toupper(uhn.phar$Generic_Name)
antibio.uhn <- uhn.phar[str_detect(generic_name, toupper("ceftriaxone"))|
                          str_detect(generic_name, toupper("azithromycin"))|
                          str_detect(generic_name, toupper("levofloxacin"))|
                          str_detect(generic_name, toupper("moxifloxacin"))|
                          str_detect(generic_name, toupper("amoxicillin"))|
                          str_detect(generic_name, toupper("piperacillin"))]

antibio.enc <- unique(c(antibio.smh$EncID.new, 
                        antibio.sbk$EncID.new, 
                        antibio.uhn$EncID.new))
smh.diag <- readg(smh, ip_diag)
sbk.diag <- readg(sbk, ip_diag)
uhn.diag <- readg(uhn, ip_diag)
ip.diag <- rbind(smh.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)],
                 sbk.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)],
                 uhn.diag[,.(EncID.new, Diagnosis.Code, Diagnosis.Type)])
smh.furo <- smh.phar[str_detect(generic_name, toupper("furosemide"))]
sbk.furo <- sbk.phar[str_detect(generic_name, toupper("furosemide"))]
uhn.furo <- uhn.phar[str_detect(generic_name, toupper("furosemide"))]
furo.enc <- unique(c(smh.furo$EncID.new, 
                     sbk.furo$EncID.new, 
                     uhn.furo$EncID.new))
# Top Diagnosis Codes
mrd <- ip.diag[Diagnosis.Type=="M"]




# answer to question 1
pneu <- mrd[str_sub(Diagnosis.Code,1,3)=="J18"]
pneu[EncID.new%in%antibio.enc] %>% dim




# answer to question 2
hf <- mrd[str_sub(Diagnosis.Code,1,3)=="I50"]
hf[EncID.new%in%furo.enc] %>% dim
