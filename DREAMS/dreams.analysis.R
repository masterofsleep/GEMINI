# ==============================================================================
# =========================  DREAMS ANALYSIS  ==================================
# =========================    March 1 2017   ==================================
library(gemini)
lib.pa()


#task1 
sbkechocombined <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SBK ECHO COMBINED.xlsx")%>%
  data.table %>% unique
names(sbkechocombined)

sbk.deid <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/Combined SBK Chart Pulls Deidentified.xlsx")%>%
  data.table %>% unique

setdiff(sbkechocombined$EncID.new, sbk.deid$`encoutner ID`)
setdiff(sbk.deid$`encoutner ID`, sbkechocombined$EncID.new)

sum(sbkechocombined$EncID.new%in%sbk.deid$`encoutner ID`)
sum(sbk.deid$`encoutner ID`%in%sbkechocombined$EncID.new)
sbkechocombined[EncID.new%in%sbkechocombined$EncID.new[duplicated(sbkechocombined$EncID.new)], 
                duplicated:= TRUE]
sbk.deid[`encoutner ID`%in%sbk.deid$`encoutner ID`[duplicated(sbk.deid$`encoutner ID`)],
         duplicated:= TRUE]
int.ip <- readg(sbk, ip_int)
int.er <- readg(sbk, er_int)
tpa <- c("1KG35HH1C",
         "1KV35HA1C",
         "1JW35HA1C",
         "1JW35HH1C",
         "1AA35HH1C",
         "1ZZ35HA1C",
         "1ZZ35YA1C")
unique(c(int.ip[Intervention.Code%in%tpa, EncID.new],
         int.er[Occurrence.Type%in%tpa, EncID.new])) %>% str_sub(3, 8) -> tpa.ex

sum(sbkechocombined$EncID.new%in%tpa.ex)
sum(sbk.deid$`encoutner ID`%in%tpa.ex)

sbkechocombined[EncID.new%in%sbk.deid$`encoutner ID`&!EncID.new%in%tpa.ex]%>%
  unique() %>% 
  fwrite("H:/GEMINI/Results/DREAM/201703/SBK ECHO COMBINED_processed.csv")
sbk.deid[!`encoutner ID`%in%tpa.ex] %>%
  fwrite("H:/GEMINI/Results/DREAM/201703/Combined SBK Chart Pulls Deidentified_processed.csv")



# task 2
process.dup <- function(dat){
  print(c(nrow(dat), nrow(unique(dat))))
  dat <- unique(dat)
  dat[EncID.new%in%dat$EncID.new[duplicated(dat$EncID.new)], duplicated := TRUE]
  return(dat)
}

smh.chart.combined <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SMH chart pulls COMBINED.xlsx")%>%
  filter(!is.na(EncID.new)) %>% data.table
process.dup(smh.chart.combined) %>%
  fwrite("H:/GEMINI/Results/DREAM/201703/SMH chart pulls COMBINED_processed.csv")

smh.echo.combined <- readxl::read_excel("H:/GEMINI/Results/DREAM/201703/SMH ECHO COMBINED.xlsx")%>%
  filter(!is.na(EncID.new)) %>% data.table
process.dup(smh.echo.combined) %>%
  fwrite("H:/GEMINI/Results/DREAM/201703/SMH ECHO COMBINED_processed.csv")


# variable create
# charts part
smh.chart <- fread("H:/GEMINI/Results/DREAM/201703/files_NG/SMH chart pulls COMBINED_processed.csv")
smh.chart[, ACNEW := ifelse(Acprior==10&ACDC%in%c(1:9),
                                     1, 2)]
sbk.chart <- fread("H:/GEMINI/Results/DREAM/201703/files_NG/Combined SBK Chart Pulls Deidentified_processed NG.csv")
names(sbk.chart) <- str_replace_all(names(sbk.chart), " ", "")
sbk.chart[, ACNEW := ifelse(ACprior==10&ACDC%in%c(1:9),
                            1, 2)]
