################################################################################
##################  Find Patients based on Intervention ########################
################################################################################
library(gemini)
lib.pa()

icd <- fread("//vs-research/research/GEMINI/Coding/CIHI/ICD_header.csv")

swdh("SMH/CIHI")
diagno <- fread("smh.ip_diag.nophi.csv")
#check whether all the diagcode is in the icd
sum(!diagno$Diagnosis.Code %in% icd$Code)
#93806 out of 94934 for smh


xtable(data.frame(table(diagno[!diagno$Diagnosis.Code %in% icd$Code, Diagnosis.Code])))
#including chriteria
# wrong one inc_diag <- c("G450","G451", "G452", "G453", "G458, G459", "H341", "I63", "I64")
inc_diag <- c("G450","G451", "G452", "G453", "G458", "G459", "H341", "I63", "I64")
inc_id <- diagno[(str_sub(Diagnosis.Code, 1, 4)%in%inc_diag|
                    (str_sub(Diagnosis.Code, 1, 3)%in%inc_diag))&
                   Diagnosis.Type == "M", EncID.new]
length(unique(inc_id))


#****
#682 included


dad <- fread("smh.ip_dad.nophi.csv")
echo <- dad[EncID.new%in%inc_id, .(EncID.new, Gender, Age, ADMITDATE, 
                                   DISCHARGEDATE)]

inc_diagno <- diagno[EncID.new %in% inc_id]

cov_code <- list(c("I48"), 
                 c("I10","I11", "I12", "I13", "I15"),
                 c("E78"),
                 c("I60", "I61", "I62"),
                 c("E10","E11","E13","E14"),
                 c("I21", "I22","123","I24","I25"),
                 c("I50", "I110", "I130","I132"))
cov_names <- c("atrial_fib_or_flutter", "hypertension","hyperlipidemia", 
               "hemorrhagic_stroke","diabetes","coronary_artery", "cong_heart_fail")
#add binary covariates to the data
#hypertension
num_uni_id <- NULL
for(i in 1:7){
  cov_id <- inc_diagno[str_sub(Diagnosis.Code, 1, 3) %in% cov_code[[i]]|
                         str_sub(Diagnosis.Code, 1, 4) %in% cov_code[[i]]|
                         str_sub(Diagnosis.Code, 1, 6) %in% cov_code[[i]], EncID.new]
  num_uni_id <- c(num_uni_id, length(unique(cov_id)))
  assign(cov_names[i], echo$EncID.new%in%cov_id)
  #validated, 173 with hypertension
}
echo.smh <-cbind(echo, atrial_fib_or_flutter, hypertension, hyperlipidemia, 
                 hemorrhagic_stroke, diabetes, coronary_artery, cong_heart_fail)
rbind(num_uni_id, apply(echo.smh[,6:12, with = F], MARGIN = 2, FUN = sum))
#result validated
#save csv
write.csv(echo.smh, "H:/GEMINI/Results/DREAM/echo.smh.csv", row.names = F)
#result validated







rm(list = ls())



















##SBK

swdh("SBK/CIHI")
diagno <- fread("sbk.ip_diag.merged.nophi.csv")

sum(!diagno$Diagnosis.Code %in% icd$Code)
xtable(data.frame(table(diagno[!diagno$Diagnosis.Code %in% icd$Code, Diagnosis.Code])))

#including chriteria
inc_diag <- c("G450","G451", "G452", "G453", "G458, G459", "H341", "I63", "I64")
inc_id <- diagno[(str_sub(Diagnosis.Code, 1, 4)%in%inc_diag|
                    (str_sub(Diagnosis.Code, 1, 3)%in%inc_diag))&
                   Diagnosis.Type == "M", EncID.new]
length(unique(inc_id))
#****
#1109 included


dad <- fread("sbk.ip_dad.nophi.csv")
echo <- dad[EncID.new%in%inc_id, .(EncID.new, Gender, Age, AdmitDate, DischDate)]

inc_diagno <- diagno[EncID.new %in% inc_id]

cov_code <- list(c("I48"), 
                 c("I10","I11", "I12", "I13", "I15"),
                 c("E78"),
                 c("I60", "I61", "I62"),
                 c("E10","E11","E13","E14"),
                 c("I21", "I22","123","I24","I25"),
                 c("I50", "I110", "I130","I132"))
cov_names <- c("atrial_fib_or_flutter", "hypertension","hyperlipidemia", 
               "hemorrhagic_stroke","diabetes","coronary_artery", "cong_heart_fail")
#add binary covariates to the data
#hypertension
num_uni_id <- NULL
for(i in 1:7){
  cov_id <- inc_diagno[str_sub(Diagnosis.Code, 1, 3) %in% cov_code[[i]]|
                         str_sub(Diagnosis.Code, 1, 4) %in% cov_code[[i]]|
                         str_sub(Diagnosis.Code, 1, 6) %in% cov_code[[i]], EncID.new]
  num_uni_id <- c(num_uni_id, length(unique(cov_id)))
  assign(cov_names[i], echo$EncID.new%in%cov_id)
  #validated, 173 with hypertension
}
echo.sbk <-cbind(echo, atrial_fib_or_flutter, hypertension, hyperlipidemia, 
                 hemorrhagic_stroke, diabetes, coronary_artery, cong_heart_fail)
rbind(num_uni_id, apply(echo.sbk[,6:12, with = F], MARGIN = 2, FUN = sum))
#result validated

write.csv(echo.sbk, "H:/GEMINI/Results/DREAM/echo.sbk.csv", row.names = F)



rm(list=ls())


swdh("../Results/DREAM")
smh <- fread("echo.smh.csv")
sbk <- fread("echo.sbk.csv")




















#---------------------Add Echo information old----------------------------------
#----------------------------SMH------------------------------------------------
swdh("SMH/ECHO ECG")
list.files()
echo.smh <- fread("smh.echo_merged.csv")


#create a variable indicating how many echo reports each enc have within 14 days of admission
smh.echo.count <- data.table(table(echo.smh$EncID.new))
names(smh.echo.count) <- c("EncID.new", "num_echo")
smh.echo.count$EncID.new <- as.integer(smh.echo.count$EncID.new)

smh <- merge(smh, smh.echo.count, by = "EncID.new", all.x = T, all.y = F)
smh$num_echo[is.na(smh$num_echo)] <- 0

#combine MRN to the data
swdr("SMH/CIHI/Archive")
list.files()
link <- fread("smh.GIM_IP_LINKING_LIST.csv")
smh.echo <- merge(link[,.(MRN, EncID.new)],smh, by = "EncID.new",
                  all.x = F, all.y = T)
data.frame(apply(smh.echo[,6:13, with = FALSE], MARGIN = 2, FUN = sum))
smh.echo$site <- "SMH"
write.csv(smh.echo, "H:/GEMINI/Results/DREAM/smh.echo.csv", row.names = F)


#----------------------------SBK------------------------------------------------

swdh("SBK/Echo")
echo.sbk <- fread("sbk.echo.csv", na.strings = c("", NA, NULL))
#create a variable indicating how many echo reports each enc have
sbk.echo.count <- data.table(table(echo.sbk$EncID.new))
names(sbk.echo.count) <- c("EncID.new", "num_echo")
sbk.echo.count$EncID.new <- as.integer(sbk.echo.count$EncID.new)

sbk <- merge(sbk, sbk.echo.count, by = "EncID.new", all.x = T, all.y = F)
sbk$num_echo[is.na(sbk$num_echo)] <- 0

data.frame(apply(sbk[,6:13, with = FALSE], MARGIN = 2, FUN = sum))
sbk$site <- "SBK"

write.csv(sbk, "H:/GEMINI/Results/DREAM/sbk.echo.csv", row.names = F)


rm(smh)




#--------Add Echo information new ( echo within 14 days only)-------------------
#----------------------------SMH------------------------------------------------
rm(list = ls())

swdh("../Results/DREAM")
smh <- fread("echo.smh.csv")

swdh("SMH/ECHO ECG")
list.files()
echo.smh <- fread("smh.echo_merged.csv")


#create a variable indicating how many echo reports each enc have within 14 days of admission
echo.smh.in14 <- echo.smh[((dmy(StudyStartDateTime)-dmy(ADMITDATE))<=14)]
smh.echo.count <- data.table(table(echo.smh.in14$EncID.new))
names(smh.echo.count) <- c("EncID.new", "num_echo_in_14days")
smh.echo.count$EncID.new <- as.integer(smh.echo.count$EncID.new)

smh <- merge(smh[,.(EncID.new, ADMITDATE, DISCHARGEDATE)], 
             smh.echo.count, by = "EncID.new", all.x = T, all.y = F)
smh$num_echo_in_14days[is.na(smh$num_echo_in_14days)] <- 0


#combine MRN to the data
swdr("SMH/CIHI/Archive")
list.files()
link <- fread("smh.GIM_IP_LINKING_LIST.csv")
smh.echo <- merge(link[,.(MRN, EncID.new)],smh, by = "EncID.new",
                  all.x = F, all.y = T)

#merge echo summary to the data
smh.echo.in14 <- merge(smh.echo, echo.smh.in14[,.(EncID.new, Conclusions)], 
                  by = "EncID.new", all.x = T, all.y = F)
write.csv(smh.echo.in14, "H:/GEMINI/Results/DREAM/smh.echo.in14.csv", 
          row.names = F)




#------------------------ SBK --------------------------------------------------

#---------------merge sbk echo measurements and interpretations ----------------
swdh("SBK/Echo")
echo.sbk <- readg(sbk, echo)
echo.sbk.int <- readg(sbk, echo_int)

#check SIDs
sum(echo.sbk.int$SID %in% echo.sbk$SID)
echo.sbk[!echo.sbk$SID%in% echo.sbk.int$SID,]

echo.sbk.merged <- merge(echo.sbk, echo.sbk.int,by = "SID",
                         all.x = T)
write.csv(echo.sbk.merged, "H:/GEMINI/Data/SBK/Echo/sbk.echo.csv",
          row.names = F, na = "")


#-------------------------pull data --------------------------------------------
rm(list = ls())

swdh("../Results/DREAM")
sbk <- fread("echo.sbk.csv")
swdh("SBK/Echo")
dad <- readg(sbk, dad)
echo.sbk <- readg(sbk, echo)
echo.sbk <- echo.sbk[,.(`Test Performed Date/time`, EncID.new, Report)]
echo.sbk$TestDate <- 
  mdy(str_sub(echo.sbk$`Test Performed Date/time`, 1, 11))

echo.sbk <- merge(echo.sbk, dad[,.(EncID.new, Admit.Date)], by = "EncID.new",
                  all.x = T, all.y = F)
#create a variable indicating how many echo reports each enc have within 14 days of admission
echo.sbk.in14 <- echo.sbk[((ymd(`TestDate`)-mdy(Admit.Date))<=14), c(1,3,4,5),
                          with = F]
sbk.echo.count <- data.table(table(echo.sbk.in14$EncID.new))
names(sbk.echo.count) <- c("EncID.new", "num_echo_in_14days")
sbk.echo.count$EncID.new <- as.integer(sbk.echo.count$EncID.new)

sbk <- merge(sbk[,.(EncID.new, AdmitDate, DischDate)], 
             sbk.echo.count, by = "EncID.new", all.x = T, all.y = F)
sbk$num_echo_in_14days[is.na(sbk$num_echo_in_14days)] <- 0






#merge echo summary to the data
sbk.echo.in14 <- merge(sbk, echo.sbk.in14[,.(EncID.new, Report)], 
                       by = "EncID.new", all.x = T, all.y = F)
sbk.echo.in14 <- arrange(sbk.echo.in14, EncID.new, mdy(AdmitDate))
write.csv(sbk.echo.in14, "H:/GEMINI/Results/DREAM/sbk.echo.in14.csv", 
          row.names = F)



























#---------------------  old ----------------------------------------------------
##50 for primary inspect
swdh("../Results/DREAM")
smh.echo <- fread("smh.echo.csv")

file50 <- smh.echo[(682-49):682, ]


write.csv(file50, "echo50.csv", row.names = F)


##split smh data to two parts with ~100 overlapping
(682 - 100)/2 + 100
smh.echo1 <- smh.echo[1:391, ]
smh.echo2 <- smh.echo[c(1:100, 392:682),]
write.csv(smh.echo1, "smh.echo1.csv", row.names = F)
write.csv(smh.echo2, "smh.echo2.csv", row.names = F)


#--------------------- new divide files  ---------------------------------------
rm(list= ls())
##50 for primary inspect
swdh("../Results/DREAM")
smh.echo.in14 <- fread("smh.echo.in14.csv")
uni.enc <- unique(smh.echo.in14$EncID.new)
682*0.05
(682-35)/2
set.seed(123)
overlap <- sample(uni.enc, size = 35, replace = F)
enc1 <- c(overlap, uni.enc[!uni.enc%in% overlap][1:323])
enc2 <- c(overlap, uni.enc[!uni.enc%in% overlap][324:647])
length(intersect(enc1, enc2))
length(union(enc1, enc2))

smh.echo.in14.1 <- smh.echo.in14[EncID.new%in%enc1]
smh.echo.in14.1.sorted <- arrange(smh.echo.in14.1, MRN, mdy(ADMITDATE))
write.csv(smh.echo.in14.1.sorted, "H:/GEMINI/Results/DREAM/smh.echo.in14.part1.csv", 
          row.names = F)
smh.echo.in14.2 <- smh.echo.in14[EncID.new%in%enc2]
smh.echo.in14.2.sorted <- arrange(smh.echo.in14.2, MRN, mdy(ADMITDATE))
write.csv(smh.echo.in14.2.sorted, "H:/GEMINI/Results/DREAM/smh.echo.in14.part2.csv", 
          row.names = F)

intersect(overlap, intersect(df1$EncID.new, df2$EncID.new))




#divide files for sbk Nov 25 2016

1109 *0.05 # = 55.45, put 56 in overlap
56/1109 #= 0.0505
(1109 - 56)/2 # = 526.5, put 526 and 527 in each

rm(list= ls())
swdh("../Results/DREAM")
sbk.echo.in14 <- fread("sbk.echo.in14.csv")

uni.enc <- unique(sbk.echo.in14$EncID.new)
set.seed(123)
overlap <- sample(uni.enc, size = 56, replace = F)
enc1 <- c(overlap, uni.enc[!uni.enc%in% overlap][1:526])
enc2 <- c(overlap, uni.enc[!uni.enc%in% overlap][527:1053])
length(intersect(enc1, enc2))
length(union(enc1, enc2))
length(sbk.echo.in14$EncID.new %in% unique(union(enc1, enc2)))

teach <- sample(uni.enc, size = 50, replace = F)
teach.data <- sbk.echo.in14[EncID.new%in%teach,]


sbk.echo.in14.1 <- sbk.echo.in14[EncID.new%in%enc1]
sbk.echo.in14.1.sorted <- arrange(sbk.echo.in14.1, EncID.new, mdy(AdmitDate))
write.csv(sbk.echo.in14.1.sorted, "H:/GEMINI/Results/DREAM/sbk.echo.in14.part1.csv", 
          row.names = F, na = "")
sbk.echo.in14.2 <- sbk.echo.in14[EncID.new%in%enc2]
sbk.echo.in14.2.sorted <- arrange(sbk.echo.in14.2, EncID.new, mdy(AdmitDate))
write.csv(sbk.echo.in14.2.sorted, "H:/GEMINI/Results/DREAM/sbk.echo.in14.part2.csv", 
          row.names = F, na = "")






























sum(smh$EncID.new %in% unique(echo.smh$EncID.new))
#how many patients have multiple echo record
sum(smh$EncID.new %in% unique(echo.smh$EncID.new[duplicated(echo.smh$EncID.new)]))

echo.smh.in <- echo.smh[echo.smh$EncID.new %in% smh$EncID.new, ]
sort(table(echo.smh.in$EncID.new),decreasing = T)[1:60]

smh.echo <- merge(smh, echo.smh.in, by.x = "EncID.new", by.y = "EncID.new", 
                  all.y = T, all.x = T)


swdh("../Results/DREAM")
write.csv(smh.echo, "dream.smh.csv", row.names = FALSE)


#creating the column names for the new wide data frame()


##creating wide format data
wide <- NULL
for(i in unique(echo.smh.in$EncID.new)){
  enci <- echo.smh.in[echo.smh.in$EncID.new == i,]
  widei <- cbind(EncID.new = i, enci[1,1:57, with = FALSE])
  for(j in 2:nrow(enci)){
    widei <- cbind(widei, enci[j, 5:57, with = FALSE])
  }
  wide <- rbind(wide, widei, fill = TRUE)
}


#change variable names
for(i in 2:6){
  names(wide)[(53*(i-1)+6): (53*i+5)] <- paste(names(wide)[6:58], i, sep = "_")
}
names(wide)
sum(!is.na(wide$StudyId_6))





smh.echo.wide <- merge(smh, wide, by = "EncID.new", all.x = T, all.y = T) 

swdh("../Results/DREAM")
write.csv(smh.echo.wide, "dream.smh.wide.csv", row.names = FALSE)


table(table(echo.smh.in$EncID.new))




#--------------------- feb 9 2017 sbk cihi interv data--------------------------
sbk.echo.in14 <- fread("H:/GEMINI/Results/DREAM/sbk.echo.in14.csv")
sbk.echo.in14$EncID.new <- paste("12", sbk.echo.in14$EncID.new, sep = "")
int.ip <- readg(sbk, ip_int)[EncID.new%in%sbk.echo.in14$EncID.new]
int.er <- readg(sbk, er_int)[EncID.new%in%sbk.echo.in14$EncID.new]

sbk.int.freq <- table(c(int.er$Occurrence.Type, int.ip$Intervention.Code)) %>% data.table
names(sbk.int.freq) <- c("code", "freq")
int.names <- fread("R:/GEMINI/Coding/CIHI/CCI_Code_Eng_Desc_2014_V1_0.csv")

sbk.int.freq <- merge(sbk.int.freq, int.names, by = "code", all.x = T)
fwrite(sbk.int.freq, "H:/GEMINI/Results/DREAM/sbk.stroke.int.csv")


tpa <- c("1KG35HH1C",
         "1KV35HA1C",
         "1JW35HA1C",
         "1JW35HH1C",
         "1AA35HH1C",
         "1ZZ35HA1C",
         "1ZZ35YA1C")

unique(c(int.ip[Intervention.Code%in%tpa, EncID.new],
       int.er[Occurrence.Type%in%tpa, EncID.new])) %>% data.table -> tpa.ex
tpa.ex$. <- str_sub(tpa.ex$., 3, 8)
names(tpa.ex) <- "tpa.ex"
fwrite(tpa.ex, "H:/GEMINI/Results/DREAM/tpa.exclude.csv")

unique(c(int.ip[, EncID.new],
       int.er[, EncID.new])) %>% length
