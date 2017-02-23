#============================ Pharmacy === =====================================
#------------------  available for SMH, SBK, UHN  ------------------------------
library(gemini)
lib.pa()
rm(list = ls())

smh <- readg(smh, phar)
names(smh)

#fix shifted hospital code
#those shited by "," in number
smh[doseAmount=="smh"&str_detect(str_sub(ndc, 1,1), "[:digit:]"), 
    generic_name:=paste(generic_name, ndc, sep ="")]
#those shifted by "," between words
smh[doseAmount=="smh"&str_detect(str_sub(ndc, 1,1), "[:alpha:]"), 
    generic_name:=paste(generic_name, ndc, sep =" ")]

#shift all columns by one
smh[doseAmount=="smh",`:=`(ndc = din, din = Hospital_Specific_Drug_Code, 
               Hospital_Specific_Drug_Code = doseAmount, doseAmount = doseUnit,
               doseUnit = route, route = ord_frequency, 
               ord_frequency = component_type, component_type = Addl_sig,
               Addl_sig = final_volume, final_volume =  rate,
               rate = X, X = X.1, X.1 = X.2, X.2 = X.3, X.3 = X.4,
               X.4 = X.5, X.5 = X.6)]

smh[route=="34U"]


checksmh0 <- smh[!is.na(smh$X)]
table(checksmh0$din)
check <- smh[din=="SMH"]
check <- smh[din=="TPL"]
checksmh1 <- smh[!is.na(smh$X.1)]
checksmh2 <- smh[!is.na(smh$X.2)]
checksmh3 <- smh[!is.na(smh$X.3)]
checksmh4 <- smh[!is.na(smh$X.4)]
checksmh5 <- smh[!is.na(smh$X.5)]
checksmh6 <- smh[!is.na(smh$X.6)]
sum(smh$doseAmount=="smh", na.rm = T)
sum(smh$doseUnit=="smh")
checksmh0 <- checksmh0[X!="NULL"]

route.table <- table(smh$route) %>% data.table

check <- smh[route%in%route.table[N<10, V1]] 

write.csv(checksmh0, "H:/GEMINI/Results/Check/check.smh.phar.csv",
          row.names = F, na = "")


write.csv(smh, "H:/GEMINI/Data/SMH/Pharmacy/smh.phar.csv", row.names = F,
          na = "")



#------------------------ SBK --------------------------------------------------
sbk <- readg(sbk, phar)
check <- sbk[X!=""]
names(sbk)
sbk[!is.na(X), 
    ord_description:=paste(ord_description, ndc_din, sep ="")]
sbk[!is.na(X),`:=`(ndc_din = master_code, master_code=drug_qty, 
                   drug_qty = strength_unit, strength_unit = route, 
                   route = frequency, frequency = total_volume,
                   total_volume = rate_mlhr, rate_mlhr = X)]
sbk[, X:=NULL]

sbk$ndc_din <- gsub("(?<![0-9])0+", "", sbk$ndc_din, perl = TRUE)
write.csv(sbk, "H:/GEMINI/Data/SBK/Pharmacy/sbk.phar.csv", row.names = F,
          na = "")


#------------------------ UHN --------------------------------------------------
uhn <- readg(uhn.phar, phar.nophi)

uhn$EncID.new <- paste("13", EncID.new,sep = "")
uhn$DIN <- gsub("(?<![0-9])0+", "", uhn.phar$DIN, perl = TRUE)
write.csv(uhn, "H:/GEMINI/Data/UHN/Pharmacy/uhn.phar.nophi.csv", row.names = F,
          na = "")




#---------------------- Jan 20 2017 new uhn phar data --------------------------
uhn.new <- fread("R:/GEMINI/_RESTORE/UHN/Medications/uhn.meds.nophi.cols_fixed.csv")

uhn.new$EncID.new <- paste("13", uhn.new$EncID.new,sep = "")
sum(uhn.new$EncID.new=="13NA")
uhn.new$DIN <- gsub("(?<![0-9])0+", "", uhn.new$DIN, perl = TRUE)
write.csv(uhn.new, "H:/GEMINI/Data/UHN/Pharmacy/uhn.phar.nophi.csv", row.names = F,
          na = "")



# ----------------- Feb 2 2017 new uhn phar data quality check -----------------
library(gemini)
lib.pa()
uhn.phar <- fread("H:/GEMINI/Data/UHN/Pharmacy/uhn.phar.nophi.csv")

uhn.dad <- readg(uhn, dad)
uhn.dad$med <- uhn.dad$EncID.new %in% uhn.phar$EncID.new

ggplot(uhn.dad, aes(x = ymd(Discharge.Date), fill = med)) + 
  geom_histogram(binwidth = 5)

range(uhn.dad[med==TRUE, ymd(Discharge.Date)])





# --------------------Feb 9 2017 new sbk phar data quality check ---------------
setwd("R:/GEMINI/_RESTORE/SBK/Pharm")
files <- list.files()
sbk <- NULL
for(i in files[2:6]){
  dat <- fread(i)
  sbk <- rbind(sbk, dat, fill = T)
  print(dim(sbk))
}
sbk$EncID.new <- paste("12", sbk$EncID.new, sep = "")
sbk <- unique(sbk)
old <- readg(sbk, phar)
sbk.dad <- readg(sbk, dad)
sbk.dad$phar <- sbk.dad$EncID.new%in%sbk$EncID.new

ggplot(sbk.dad, aes(ymd(Discharge.Date), fill = phar)) +
  geom_histogram(binwidth = 10) + ggtitle("sbk pharmacy")

fwrite(sbk, "H:/GEMINI/Data/SBK/Pharmacy/sbk.phar.csv", na = "")





# ----------------- Feb 9 2017 new uhn phar data quality check -----------------
library(gemini)
lib.pa()
uhn.phar.old <- readg(uhn, phar.nophi)
table(uhn.phar.old$Route_Code, useNA = "ifany")
uhn.phar <- fread("R:/GEMINI/_RESTORE/UHN/Medications/uhn.meds.nophi-feb2017.csv")
uhn.phar$EncID.new <- paste("13", uhn.phar$EncID.new, sep = "")
uhn.dad <- readg(uhn, dad)
uhn.dad$med <- uhn.dad$EncID.new %in% uhn.phar$EncID.new
sum(uhn.dad$med)
ggplot(uhn.dad, aes(x = mdy(Discharge.Date), fill = med)) + 
  geom_histogram(binwidth = 5)

range(uhn.dad[med==TRUE, ymd(Discharge.Date)])

head(uhn.phar)

fwrite(uhn.phar, "H:/GEMINI/Data/UHN/Pharmacy/uhn.phar.nophi.csv", na = "")



# ------------------ new sbk phar data: fix shifted columns --------------------
rm(list = ls())
library(gemini)
lib.pa()
setwd("R:/GEMINI/_RESTORE/SBK/Pharm")
files <- list.files()
sbk <- NULL
for(i in files[2:6]){
  dat <- fread(i)
  sbk <- rbind(sbk, dat, fill = T)
  print(dim(sbk))
}
sbk$EncID.new <- paste("12", sbk$EncID.new, sep = "")
sbk <- unique(sbk)

check1 <- sbk[!X.1==""]
check2 <- sbk[!X==""]
sbk[startsWith(ord_description, "\"\"")&endsWith(ndc_din, "\"\"")] -> check
check[startsWith(generic_name, "\"\"")]
#fix those caused by ord_description and shifted 1 column
sbk[startsWith(ord_description, "\"\"")&endsWith(ndc_din, "\"\""),
    `:=`(ord_description = paste(ord_description, ndc_din, sep = ""))]
sbk[startsWith(ord_description, "\"\"")&endsWith(ndc_din, "\"\""),
    `:=`(ndc_din = master_code, 
         master_code=drug_qty,
         drug_qty = strength_unit,
         strength_unit = route,
         route = frequency, 
         frequency = total_volume,
         total_volume = rate_mlhr,
         rate_mlhr = X,
         X = X.1,
         X.1 = "")]


#fix those caused by generic names and shifted 1 column
sbk[startsWith(generic_name, "\"\"")&endsWith(ord_description, "\"\""),
    `:=`(generic_name = paste(generic_name, ord_description, sep = ""))]
sbk[startsWith(generic_name, "\"\"")&endsWith(ord_description, "\"\""),
    `:=`(ord_description = ndc_din,
         ndc_din = master_code, 
         master_code=drug_qty,
         drug_qty = strength_unit,
         strength_unit = route,
         route = frequency, 
         frequency = total_volume,
         total_volume = rate_mlhr,
         rate_mlhr = X,
         X = X.1,
         X.1 = "")]


#fix those caused by generic names and shifted 2 column
sbk[startsWith(generic_name, "\"\"")&endsWith(ndc_din, "\"\""),
    `:=`(generic_name = paste(generic_name, ord_description, ndc_din, sep = ""))]
sbk[startsWith(generic_name, "\"\"")&endsWith(ndc_din, "\"\""),
    `:=`(ord_description = master_code,
         ndc_din =drug_qty, 
         master_code = strength_unit,
         drug_qty = route,
         strength_unit = frequency,
         route = total_volume, 
         frequency = rate_mlhr,
         total_volume = X,
         rate_mlhr = X.1,
         X = "",
         X.1 = "")]

apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))

sbk[is.na(mdy_hms(paste(start_date, start_time)))] -> check
sbk[, X:= NULL]
sbk[, X.1:= NULL]
fwrite(sbk, "H:/GEMINI/Data/SBK/Pharmacy/sbk.phar.csv", na = "")




# ------------------------- feb 22 2017 ----------------------------------------
# trim the leading 0s in sbk phar
sbk.phar <- readg(sbk, phar)
sbk.phar$ndc_din <- gsub("(?<![0-9])0+", "", sbk.phar$ndc_din, perl = TRUE)
fwrite(sbk.phar, "H:/GEMINI/Data/SBK/Pharmacy/sbk.phar.csv", na = "")

# convert 12h to 24h format in uhn phar
uhn.phar <- readg(uhn, phar.nophi)
time.convert <- function(x){
  ifelse(str_sub(x, -2,-1)=="pm"&as.numeric(str_sub(x, 1, 2))<12, 
         paste(as.numeric(str_sub(x, 1, 2)) + 12, str_sub(x, 3, 5), sep = ""),
         ifelse((str_sub(x, -2,-1)=="am"&as.numeric(str_sub(x, 1, 2))==12),
                paste(as.numeric(str_sub(x, 1, 2)) - 12, str_sub(x, 3, 5), sep = ""),
                paste(as.numeric(str_sub(x, 1, 2)), str_sub(x, 3, 5), sep = "")))
}
time.convert("12:15 pm")
time.convert("12:15 am")
time.convert("09:15 am")
time.convert("09:15 pm")


uhn.phar[,`:=`(Order_Start_Time = time.convert(Order_St),
               Order_Stop_Time = time.convert(Order_St.1))]
uhn.phar$DIN <- gsub("(?<![0-9])0+", "", uhn.phar$DIN, perl = TRUE)
fwrite(uhn.phar, "H:/GEMINI/Data/UHN/Pharmacy/uhn.phar.nophi.csv", na = "")

