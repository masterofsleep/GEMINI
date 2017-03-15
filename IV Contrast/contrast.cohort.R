rm(list = ls())
library(gemini)
lib.pa()
smh.adm <- readg(smh, adm)
sbk.adm <- readg(sbk, adm)
uhn.adm <- readg(uhn, adm)
hcn <- rbind(smh.adm[, .(Hash, EncID.new)],
             sbk.adm[, .(Hash, EncID.new)],
             uhn.adm[, .(Hash, EncID.new)])
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad$EncID.new <- as.character(dad$EncID.new)
dad$Institution.Number[dad$Institution.Number=="54265"] <- "uhn-general"
dad$Institution.Number[dad$Institution.Number=="54266"] <- "uhn-western"
dad$Institution.Number[dad$Institution.Number=="M"] <- "thp-m"
dad$Institution.Number[dad$Institution.Number=="C"] <- "thp-c"
dad <- dad[ymd(Discharge.Date) <= ymd("2015-03-31")]

hcn <- hcn[EncID.new%in%dad$EncID.new]

sum(duplicated(hcn$Hash))
hcn <- hcn[sample(nrow(hcn))]

ip.diag <- readg(gim, ip.diag, 
                 colClasses = list(character = "EncID.new"))[
                   startwith.any(EncID.new, c("11", "12", "13"))]
er.diag <- readg(gim, er.diag, 
                 colClasses = list(character = "EncID.new"))[
                   startwith.any(EncID.new, c("11", "12", "13"))]
ip.int <- readg(gim, ip.int, 
                colClasses = list(character = "EncID.new"))[
                  startwith.any(EncID.new, c("11", "12", "13"))]
er.int <- readg(gim, er.int, 
                colClasses = list(character = "EncID.new"))[
                  startwith.any(EncID.new, c("11", "12", "13"))]

end.stage.kidney.disease.enc <- c(
  ip.diag[startwith.any(Diagnosis.Code, c("Z49", "Z992")), EncID.new],
  er.diag[startwith.any(ER.Diagnosis.Code, c("Z49", "Z992")), EncID.new],
  ip.int[startsWith(Intervention.Code, "1PZ21"), EncID.new],
  er.int[startsWith(Occurrence.Type, "1PZ21"), EncID.new]
)
angi.code <- c("3IJ30GP", "3HZ30GP", "2HZ24GPKJ", "2HZ24GPKL", "2HZ24GPKM", 
               "2HZ24GPXJ", "2HZ28GPPL", "2HZ71GP", "3IP10", "3IS10")
angiogram.enc <- c(ip.int[startwith.any(Intervention.Code, angi.code), EncID.new],
               er.int[startwith.any(Occurrence.Type, angi.code), EncID.new])
  
  
hcn[, end.stage.kidney.disease:= EncID.new%in%
      end.stage.kidney.disease.enc]
sum(hcn$end.stage.kidney.disease)
hcn[, angiogram := EncID.new%in%angiogram.enc]

hcn <- merge(hcn, dad[,.(Admit.Date, EncID.new)], by = "EncID.new") %>%
  arrange(Hash, ymd(Admit.Date)) %>% data.table


dad[is.na(ymd(Admit.Date))]

find.kidney.hist <- function(x){
  admdate.x <- hcn[EncID.new==x, Admit.Date]
  sum(hcn[Hash==hcn[EncID.new==x, Hash]&ymd(Admit.Date)<=ymd(admdate.x), 
      end.stage.kidney.disease])>0
}
for(i in 1:nrow(hcn)){
  hcn$end.stage.kidney.disease.history[i] <- find.kidney.hist(hcn$EncID.new[i])
}
kid.hist <- apply(hcn, 1, function(x) sum(hcn[Hash==x["Hash"]&ymd(Admit.Date)<=ymd(x["Admit.Date"]), 
                                  end.stage.kidney.disease])>0)
hcn <- cbind(hcn, kid.hist)

hcn <- hcn[sample(nrow(hcn))]
fwrite(hcn, "H:/GEMINI/Results/Contrast/hcn.full.csv")

hcn <- fread("H:/GEMINI/Results/Contrast/hcn.full.csv")
#order by hash
hcn <- hcn[order(Hash)]
# shuffle all rows and randomly select 1 admission for each patients
set.seed(100)
hcn <- hcn[sample(nrow(hcn))]
hcn <- hcn[!duplicated(Hash)]

hcn <- hcn[kid.hist==F]
hcn <- hcn[angiogram==F]

fwrite(hcn, "H:/GEMINI/Results/Contrast/cohort.csv")


cohort <- fread("H:/GEMINI/Results/Contrast/cohort.csv")
ip.int <- readg(gim, ip.int, 
                colClasses = list(character = "EncID.new"))[
                  startwith.any(EncID.new, c("11", "12", "13"))]
ip.int[str_sub(EncID.new,1,2)=="13", Intervention.Start.Date := 
         ymd(Intervention.Episode.Start.Date)]
ip.int[str_sub(EncID.new,1,2)%in%c("11", "12"), Intervention.Start.Date := 
         mdy(Intervention.Episode.Start.Date)]

er.int <- readg(gim, er.int, 
                colClasses = list(character = "EncID.new"))[
                  startwith.any(EncID.new, c("11", "12", "13"))]

cci.code <- readxl::read_excel("H:/GEMINI/Feasibility/Contrast/Contrast CCI Codes.xlsx", sheet = 1)
cci.code$Description <- str_sub(cci.code$`CCI Code`, 9, -1)
cci.code$`CCI Code` <- str_sub(cci.code$`CCI Code`, 1, 7)
ip.cec <- ip.int %>% 
  filter(EncID.new%in%cohort$EncID.new&Intervention.Code%in%cci.code$`CCI Code`) %>%
  merge(cci.code, by.x = "Intervention.Code", by.y = "CCI Code",
        all.x = T, all.y = F) %>% 
  select(EncID.new, Intervention.Code, Description, `Body Site`, Intervention.Start.Date) %>% 
  data.table
er.cec <- er.int %>% 
  filter(EncID.new%in%cohort$EncID.new&Occurrence.Type%in%cci.code$`CCI Code`) %>%
  merge(cci.code, by.x = "Occurrence.Type", by.y = "CCI Code",
        all.x = T, all.y = F) %>% 
  select("Intervention.Code" = Occurrence.Type, EncID.new, Description, `Body Site`) %>%
  data.table
contrast.enhanced <- rbind(ip.cec, er.cec, fill = T)
contrast.enhanced[,.N, by = "Body Site"]

table(contrast.enhanced[is.na(Intervention.Start.Date), str_sub(EncID.new,1,2)])

smh.ct <- readg(smh, ct)
sbk.rad <- readg(sbk, rad.csv)
uhn.rad <- rbind(readg(uhn, rad_ip),
                 readg(uhn, rad_er))
map.sbk <- readxl::read_excel("H:/GEMINI/Results/DesignPaper/rad.freq.table.new_AV.xlsx", sheet = 1)
sbk.ct <- sbk.rad[Test.Name %in% map.sbk$Test.Name[map.sbk$Test.Type==3]]
uhn.ct <- uhn.rad[str_sub(ProcedureName, 1, 2)=="CT"]

control.ct <- c(smh.ct$EncID.new, sbk.ct$EncID.new, uhn.ct$EncID.new)

cohort[, ':='(contrast.enhanced = EncID.new%in%contrast.enhanced$EncID.new,
              control.ct = EncID.new%in%control.ct)]
sum(cohort$control.ct)
sum(cohort$contrast.enhanced)
fwrite()
setwd("H:/GEMINI/Results/Contrast")
smh.ct[,.N,by = proc_desc_long] %>% fwrite("smh.ct.freq.csv")
sbk.ct[,.N,by = Test.Name] %>% fwrite("sbk.ct.freq.csv")
uhn.ct[,.N,by = ProcedureName] %>% fwrite("uhn.ct.freq.csv")

fwrite(cohort, "H:/GEMINI/Results/Contrast/cohort.csv")


smh.lab <- readg(smh, corelabs)
sbk.laber <- readg(sbk, labs_er)
sbk.labip <- readg(sbk, labs_ip)
sbk.lab <- rbind(sbk.laber, sbk.labip)
uhn.lab <- readg(uhn, labs)
smh.lab[Test.Name=="Creatinine", 
        .(EncID.new, Test.Name, Result.Value, Result.Unit, Reference.Range, Collection.DtTm)] %>% 
  fwrite("smh.creatinine.csv")
sbk.lab[Test.Name=="Creatinine (renal)", 
        .(EncID.new, Test.Name, Result.Value, Result.Unit, Reference.Range, Collection.DtTm)] %>% 
  fwrite("sbk.creatinine.csv")
uhn.lab[Test.Item=="Creatinine"&
          Test.Name%in%c("Creatinine, Plasma",
                         "Electrolytes, Creatinine, Glucose Profile",
                         "Electrolytes, Creatinine, Profile"), 
        .(EncID.new, Test.Name, Test.Item, Result.Value, Result.Units, Collection.DtTm = paste(Test.Date, Test.Time))] %>% 
  fwrite("uhn.creatinine.csv")
creatinine.smh <- smh.lab[Test.Name=="Creatinine"&EncID.new%in%cohort$EncID.new]
creatinine.sbk <- sbk.lab[Test.Name=="Creatinine (renal)"&EncID.new%in%cohort$EncID.new]
creatinine.uhn <- uhn.lab[Test.Item=="Creatinine"&
                            Test.Name%in%c("Creatinine, Plasma",
                                           "Electrolytes, Creatinine, Glucose Profile",
                                           "Electrolytes, Creatinine, Profile")&
                            EncID.new%in%cohort$EncID.new]

creatinine.smh$Result.Value <- str_replace_all(creatinine.smh$Result.Value, "[@A-z]","")
summary(as.numeric(creatinine.smh$Result.Value), na.rm = T)
crea.smh <- creatinine.smh[!(is.na(as.numeric(Result.Value))&!startwith.any(Result.Value, c("<", ">")))]
crea.sbk <- creatinine.sbk[!(is.na(as.numeric(Result.Value))&!startwith.any(Result.Value, c("<", ">")))]
crea.uhn <- creatinine.uhn[!(is.na(as.numeric(Result.Value))&!startwith.any(Result.Value, c("<", ">")))]
library(plyr)
base1 <- rbind(ddply(crea.smh, ~EncID.new, summarize, 
                     base1 = min(as.numeric(Result.Value))),
               ddply(crea.sbk, ~EncID.new, summarize, 
                     base1 = min(as.numeric(Result.Value))),
               ddply(crea.uhn, ~EncID.new, summarize, 
                     base1 = min(as.numeric(Result.Value))))

check <- rbind(crea.smh[is.na(as.numeric(Result.Value)), .(Result.Value, EncID.new)],
               crea.sbk[is.na(as.numeric(Result.Value)), .(Result.Value, EncID.new)],
               crea.uhn[is.na(as.numeric(Result.Value)), .(Result.Value, EncID.new)])

# descriptive for creatinine
crea.smh <- fread("H:/GEMINI/Results/Contrast/smh.creatinine.csv")
crea.smh$Result.Value <- str_replace_all(crea.smh$Result.Value, "[@A-z]","")
crea.sbk <- fread("H:/GEMINI/Results/Contrast/sbk.creatinine.csv")
crea.uhn <- fread("H:/GEMINI/Results/Contrast/uhn.creatinine.csv")

lab.desc <- function(x){
  cat("Number(%) of numeric value is ",
      sum(!is.na(as.numeric(x))),
      "(", round(sum(!is.na(as.numeric(x)))/length(x)*100, 2), ")\n")
  cat("Number(%) of non numeric value is ",
      sum(is.na(as.numeric(x))),
      "(", round(sum(is.na(as.numeric(x)))/length(x)*100,2), ")\nSummary of numeric values:\n")
  print(summary(as.numeric(x[!is.na(as.numeric(x))])))
  cat("Freq table of non numeric values:\n")
  fwrite(data.table(table(x[is.na(as.numeric(x))])), "H:/GEMINI/Results/Contrast/crea.nonum.csv")
}

lab.desc(crea.smh$Result.Value)
lab.desc(crea.sbk$Result.Value)
lab.desc(crea.uhn$Result.Value)

x <- crea.smh$Result.Value
a<- (data.table(table(x[is.na(as.numeric(x))])))


# ------------------------------------------------------------------------------
# --------------------- check non numeric creatinine value --------------------- 
# --------------------------------- 2017-03-10 --------------------------------- 
# descriptive for creatinine
crea.smh <- fread("H:/GEMINI/Results/Contrast/smh.creatinine.csv")
crea.smh$Result.Value <- str_replace_all(crea.smh$Result.Value, "[@A-z]","")
crea.sbk <- fread("H:/GEMINI/Results/Contrast/sbk.creatinine.csv")
crea.uhn <- fread("H:/GEMINI/Results/Contrast/uhn.creatinine.csv")

crea <- rbind(crea.smh[, .(EncID.new, Test.Name, Result.Value, Result.Unit, Collection.DtTm)],
               crea.sbk[, .(EncID.new, Test.Name, Result.Value, Result.Unit, Collection.DtTm)],
               crea.uhn[, .(EncID.new, Test.Name, Result.Value, Result.Unit = Result.Units, Collection.DtTm)])

check <- rbind(crea.smh[is.na(as.numeric(Result.Value)), .(Result.Value, EncID.new)],
               crea.sbk[is.na(as.numeric(Result.Value)), .(Result.Value, EncID.new)],
               crea.uhn[is.na(as.numeric(Result.Value)), .(Result.Value, EncID.new)])
nonumeric.values <- unique(check$Result.Value)
enc.with.nonum <- crea[EncID.new%in%check[Result.Value%in%nonumeric.values[c(1,2,10,11,17, 20)], EncID.new]]
enc.with.nonum <- enc.with.nonum[order(EncID.new)]

fwrite(enc.with.nonum, "H:/GEMINI/Results/Contrast/crea.all.with.non.num.csv")
