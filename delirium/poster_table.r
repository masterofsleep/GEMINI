# ------------------------- Delirium Poster Table ------------------------------
# ------------------------------ 2017-06-13 ------------------------------------
library(gemini)
lib.pa()

# chart review results
setwd("H:/GEMINI/Results/delirium/db")
files <- list.files();files
charts <- NULL
for(i in c(1:9)[c(7,6,2,1,9,5,8,4,3)]){
  dat <- data.table(readxl::read_excel(files[i]))
  charts <- rbind(charts, dat, fill = T)
}
charts <- unique(charts)
unique(charts[, .(subject_id, del_present)])[duplicated(subject_id)|duplicated(subject_id, fromLast = T),
                                             .(subject_id, del_present)][order(subject_id)]

charts_del <- unique(charts[, .(subject_id, del_present)])
#charts_del$dc <- charts_del$subject_id%in%dc$subject_id
ov <- readxl::read_excel("R:/GEMINI/DELIRIUM/overlapping results.xls", skip = 1)
charts_del <- rbind(charts_del[!subject_id%in%ov$subject_id],
                    ov[, 1:2]) %>% arrange(subject_id) %>% data.table
dup_sub <- charts_del[duplicated(subject_id)|duplicated(subject_id, fromLast = T)][order(subject_id)]
charts_uni <- charts_del[!duplicated(subject_id)]





dc <- NULL
for(i in c(10:18)[c(7,6,2,1,9,5,8,4,3)]){
  dat <- data.table(readxl::read_excel(files[i]))
  dc <- rbind(dc, dat, fill = T)
}
dc <- unique(dc)
dc <- dc[order(subject_id), .(subject_id, diag_syn_del,
                              hosp_course_del_syn_yn,
                              del_spec_instr, instr_taper, rec_monitoring,
                              rec_follow_appt, rec_follow_cog_test)] %>% unique
dc_dup <- dc[duplicated(subject_id)|duplicated(subject_id, fromLast = T)]
dc_uni <- dc[!duplicated(subject_id)]

table(charts_uni$del_present, useNA = "ifany")
charts_uni$EncID.new <- paste("11", charts_uni$subject_id, sep = "")


ip.diag <- readg(gim, ip_diag)
icd.del <- ip.diag[startsWith(Diagnosis.Code, "F05"), EncID.new]
icd.del%in%charts_uni$EncID.new %>% sum
chart.del <- charts_uni[del_present%in%c("1", "Yes"), EncID.new]
chart.nod <- charts_uni[del_present%in%c("2", "No"), EncID.new]
icd.del <- icd.del[icd.del%in%charts_uni$EncID.new]

dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
charts_uni$EncID.new%in%dad$EncID.new %>% sum
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

my_tab <- function(x){
  x[is.na(x)] <- "Uncertain"
  df <- data.frame(table(x, useNA = "always"))
  df$label <- paste(df$Freq, " (", sprintf("%.1f", df$Freq/length(x)*100), ")", sep = "")
  df
}

dc_uni <- dc_uni[subject_id%in%charts_uni[del_present%in%c("1", "Yes"), subject_id]]
charts_uni[del_present%in%c("1", "Yes")&!subject_id%in%dc_uni$subject_id]

my_tab(dc_uni$diag_syn_del)
my_tab(dc_uni$hosp_course_del_syn_yn)
my_tab(dc_uni$del_spec_instr)
my_tab(dc_uni$instr_taper)
my_tab(dc_uni$rec_monitoring)
my_tab(dc_uni$rec_follow_appt)
my_tab(dc_uni$rec_follow_cog_test)
