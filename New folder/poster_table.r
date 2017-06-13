# ------------------------- Delirium Poster Table ------------------------------
# ------------------------------ 2017-06-13 ------------------------------------
library(gemini)
lib.pa()

# chart review results
setwd("R:/GEMINI/DELIRIUM/Access DBs - 13-06-2017")
list.files()
library(RODBC)
conn <- odbcConnectAccess2007("GEMINI-Delirium - v7.2 - Jana.accdb")



ip.diag <- readg(gim, ip_diag)
icd.del <- ip.diag[startsWith(Diagnosis.Code, "F05"), EncID.new]
icd.del%in%chart.res$EncID.new %>% sum
chart.del <- chart.res[del_present=="1", EncID.new]
chart.nod <- chart.res[del_present=="2", EncID.new]
icd.del <- icd.del[icd.del%in%chart.res$EncID.new]

dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
chart.res$EncID.new%in%dad$EncID.new %>% sum
median(dad[EncID.new%in%chart.del, LOS.without.ALC])
median(dad[EncID.new%in%chart.nod, LOS.without.ALC])
median(dad[EncID.new%in%icd.del, LOS.without.ALC])

sum(dad[EncID.new%in%chart.del, SCU.adm]);mean(dad[EncID.new%in%chart.del, SCU.adm])
sum(dad[EncID.new%in%chart.nod, SCU.adm]);mean(dad[EncID.new%in%chart.nod, SCU.adm])
sum(dad[EncID.new%in%icd.del, SCU.adm]);mean(dad[EncID.new%in%icd.del, SCU.adm])

smh.phar <- readg(smh, phar)
antipsychotic <- c("ARIPIPRAZOLE", "ASENAPINE","CLOZAPINE", "LURASIDONE", 
                   "OLANZAPINE", "QUETIAPINE", "PALIPERIDONE", "RISPERIDONE",
                   "ZIPRASIDONE", "CHLORPROMAZINE", "flupenthixol",
                   "fluphenazine", "haloperidol", "loxapine", "methotimeprazine",
                   "perphenazine", "pimozide", "thiothixene", "trifluoperazine",
                   "zuclopentixol", "flupenthixol", "fluphenazine", 
                   "haloperidol", "pipotiazine", "paliperidone",
                   "risperidone", "zulcopenthixol") %>% toupper()
for(i in antipsychotic){
  print(sum(str_detect(smh.phar$generic_name, i)))
}

for(i in antipsychotic){
  print(sum(startsWith(smh.phar$generic_name, i)))
}

smh.antipsychotic <- smh.phar[startwith.any(generic_name, antipsychotic), EncID.new]

antipsy <- function(x){
  print(paste(sum(x%in%smh.antipsychotic), " (",
              sprintf("%.1f", sum(x%in%smh.antipsychotic)/length(x)*100), ")",
              sep = ""))
}
antipsy(chart.del)
antipsy(chart.nod)
antipsy(icd.del)



# read in discharge summary
setwd("R:/GEMINI/DELIRIUM")
files
dc_sum <- NULL
for(i in 12:19){
  dat <- readxl::read_excel(files[i]) %>% data.table
  dc_sum <- rbind(dc_sum, dat, fill = T)
}

length(unique(dc_sum$recordID))

length(unique(dc_sum$subject_id))
