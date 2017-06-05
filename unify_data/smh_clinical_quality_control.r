library(gemini)
lib.pa()

# check Encounter and Orders that are not in GEMINI 
smh.lab <- readg(smh, labs)
smh.lab <- smh.lab[Order.!="J2241772"]


plot_test <- function(testname){
  ggplot(smh.lab[Test.Name==testname],
         aes(ymd(str_sub(Collection.DtTm, 1, 10)))) + 
    geom_histogram(binwidth = 5) +
           ggtitle(testname)
}
plot_test("Sodium")
plot_test("AST")
plot_test("Calcium")
plot_test("Creatinine")
plot_test("HGB")
plot_test("INR")
plot_test("Troponin I")

ggplot(smh.lab[ymd(str_sub(Collection.DtTm, 1, 10))>=ymd("2010-01-01")],
       aes(ymd(str_sub(Collection.DtTm, 1, 10)))) + 
  geom_histogram(binwidth = 50)

# TESTS that are good
plot_test("Glucose POC (Lifescan/Abbott)")



# Radiology
smh.rad <- readg(smh, rad)
ggplot(smh.rad, aes(ymd(str_sub(proc_dtime, 1, 10)), fill = dept)) + geom_histogram(binwidth = 10)

# Microbiology
smh.micro <- readg(smh, micro, dt = T)
smh.micro[ResultIsFinal=="Y", Collection.DtTm := mdy(str_sub(`Specimen_Collection_Date\\Time`, 1, -6))]
smh.micro[is.na(ResultIsFinal), Collection.DtTm := mdy(`Specimen_Collection_Date\\Time`)]
ggplot(smh.micro, aes(Collection.DtTm, fill = ResultIsFinal)) + geom_histogram(binwidth = 5)
smh.dad <- readg(smh, dad)
smh.dad$with_micro <- smh.dad$EncID.new%in%smh.micro$EncID.new
ggplot(smh.dad, aes(ymd(Admit.Date), fill = with_micro)) +
  geom_histogram(binwidth = 10)

range(smh.micro$Collection.DtTm, na.rm = T)
smh.micro[which.max(Collection.DtTm)]
smh.micro[which.min(Collection.DtTm)]

smh.micro[Collection.DtTm>=ymd_hm(paste(Discharge.Date, Discharge.Time))] %>% 
  ggplot(aes(Collection.DtTm, fill = ResultIsFinal)) + geom_histogram(binwidth = 10)


# Blood Bank
smh.bb <- readg(smh, bb)
ggplot(smh.bb, aes(mdy_hm(UseDtTm))) + geom_histogram(bins = 80)


# echo
smh.echo <- readg(smh, echo)
ggplot(smh.echo, aes(dmy(StudyStartDateTime))) + geom_histogram(binwidth = 30)

# ecg
smh.ecg <- readg(smh, ecg)
ggplot(smh.ecg, aes(mdy_hm(Acquisition.Date.Time))) + geom_histogram(bins = 50)

# diet
smh.diet <- readg(smh, diet)
ggplot(smh.diet, aes(mdy_hm(CreationTime))) + geom_histogram(bins = 50)

# pharmacy
library(scales)
smh.phar <- readg(smh, phar)
ggplot(smh.phar, aes(ymd(start_date))) + geom_histogram(binwidth = 30) +
  scale_x_date(labels = date_format("%Y-%m"))


# vitals
smh.vitals <- readg(smh, vitals)
ggplot(smh.vitals, aes(mdy_hm(Collected.DT))) + geom_histogram(bins = 50)





# SBK 
sbk.bb <- readg(sbk, bb)
ggplot(sbk.bb, aes(ymd(Issue.Date))) + geom_histogram(bins = 50)

sbk.echo <- readg(sbk, echo)
ggplot(sbk.echo, aes(mdy(str_sub(`Test Performed Date/time`, 1, 11)))) + geom_histogram(bins = 50)

sbk.lab.er <- readg(sbk, labs_er)
ggplot(sbk.lab.er, aes(ymd_hms(Collection.DtTm))) + geom_histogram(bins = 50)

sbk.lab.ip <- readg(sbk, labs_ip)
ggplot(sbk.lab.ip, aes(ymd_hms(Collection.DtTm))) + geom_histogram(bins = 50)

sbk.mic.pos <- readg(sbk, micro_pos.csv)
ggplot(sbk.mic.pos, aes(mdy_hm(specimen_collection_datetime))) + geom_histogram(bins = 50)

sbk.mic.neg <- readg(sbk, micro_neg.csv)
ggplot(sbk.mic.neg, aes(mdy_hm(specimen_collection_datetime))) + geom_histogram(bins = 50)

sbk.phar <- readg(sbk, phar)
ggplot(sbk.phar[mdy(start_date)<=ymd("2016-01-01")&
                  mdy(start_date)>=ymd("2010-01-01")]
       , aes(mdy(start_date))) + geom_histogram(bins = 50)
range(mdy(sbk.phar$start_date), na.rm = T)
sbk.phar[mdy(start_date)>=ymd("2016-01-01")]

sbk.rad <- readg(sbk, rad.csv)
ggplot(sbk.rad, aes(ymd_hms(Performed.DtTm))) + geom_histogram(bins = 50)

sbk.trans <- readg(sbk, trans.csv)
ggplot(sbk.trans, aes(dmy(Issue.Date))) + geom_histogram(bins =50)



# uhn lab
uhn.lab <- readg(uhn, lab)
ggplot(uhn.lab, aes(ymd(Test.Date))) + geom_histogram(bins = 50)


# uhn microbiology
tgh.micro <- readg(tgh, micro)
ggplot(tgh.micro, aes(ymd(CDATE))) + geom_histogram(bins = 50)
twh.micro <- readg(twh, micro)
ggplot(twh.micro, aes(ymd(CDATE))) + geom_histogram(bins = 50)                  # seems suspicious
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")
dad[, twh.micro := EncID.new%in%twh.micro$EncID.new]
library(scale)
ggplot(dad[Institution.Number=="UHN-TW"], aes(ymd(Discharge.Date), fill = twh.micro)) +
  geom_histogram(bins = 50) + scale_y_continuous(labels=percent)

# uhn pharmacy
uhn.phar <- readg(uhn, phar)
ggplot(uhn.phar, aes(dmy(Order_Sta))) + geom_histogram(bins= 50)

# uhn radiology
uhn.rader <- readg(uhn, rad_er)
uhn.radip <- readg(uhn, rad_ip)
ggplot(uhn.rader, aes(mdy_hm(ScanStartDateTime))) + geom_histogram(bins=50)
ggplot(uhn.radip, aes(mdy_hm(ScanStartDateTime))) + geom_histogram(bins=50)

# uhn transfusion
uhn.txm.er <- readg(uhn, txm_er)
uhn.txm.ip <- readg(uhn, txm_ip)

ggplot(uhn.txm.er, aes(mdy(Date_Component_Issued_from_Lab))) + geom_histogram(binwidth =  30)
ggplot(uhn.txm.ip, aes(mdy(Date_Component_Issued_from_Lab))) + geom_histogram(binwidth = 30)

uhn.txm.er[mdy(Date_Component_Issued_from_Lab)<=ymd("2010-01-01")]
# a chunk before 2010
dad[EncID.new=="13227789"]



# ------------------------------- sinai ----------------------------------------
rm(list = ls())
# bb
msh.bb <- readg(msh, bb)
ggplot(msh.bb, aes(ymd(DATE))) + geom_histogram(bins = 50)

# diet
msh.diet <- readg(msh, diet)
ggplot(msh.diet, aes(ymd(ORDER_DATE))) + geom_histogram(bins = 50)

# ecg
msh.ecg <- readg(msh, ecg)
ggplot(msh.ecg, aes(mdy(X.No.column.name.)))  + geom_histogram(bins = 50)

# lab
msh.lab <- readg(msh, lab)
ggplot(msh.lab, aes(ymd(DATE_COLLECT) )) + geom_histogram(bins = 50)

#pharmacy
msh.phar <- readg(msh, phar)                                                    # there is a volume peak in 2011
ggplot(msh.phar[EncID.new%in%dad$EncID.new], aes(ymd(STOP_DATE), fill = is.na(START_DATE)))  + geom_histogram(bins = 50)
ggplot(msh.phar, aes(ymd(START_DATE)))  + geom_histogram(bins = 50)
dad$msh.phar <- dad$EncID.new%in%msh.phar$EncID.new
ggplot(dad[Institution.Number=="SHS"], aes(ymd(Discharge.Date), fill = msh.phar)) +
  geom_histogram(bins = 50)

# rad
msh.rader <- readg(msh, rad_er)
msh.radip <- readg(msh, rad_ip)
ggplot(msh.rader, aes(ymd_hm(ScanStartDateTime))) + geom_histogram(bins=50)
ggplot(msh.radip, aes(ymd_hm(ScanStartDateTime))) + geom_histogram(bins=50)

# transfusion
msh.trans <- readg(msh, trans)
ggplot(msh.trans, aes(ymd(ORDER_DATE))) + geom_histogram(bins = 50)
