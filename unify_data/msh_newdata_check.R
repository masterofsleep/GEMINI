## -------------------- Feb 3 ------ MSH DATA ----------------------------------
library(gemini)
lib.pa()
msh <- fread("R:/GEMINI/_RESTORE/MSH/CIHI/msh.adm.nophi.csv")
# compare with dad
dad <- readg(msh, dad)
sum(is.na(msh$EncID.new))
sum(duplicated(msh$EncID.new))
msh$EncID.new <- paste("14", msh$EncID.new, sep = "")

sum(!msh$EncID.new%in%dad$EncID.new)
enc.new <- msh$EncID.new[!msh$EncID.new%in%dad$EncID.new]
enc.old <- dad$EncID.new[!dad$EncID.new%in%msh$EncID.new]
setwd("H:/GEMINI/Data/MSH")
files <- list.files(recursive = T)
for(i in files){
  dat <- fread(i)
  print(paste(i, sum(dat$EncID.new%in%enc.new)))
}

phar <- fread("R:/GEMINI/_RESTORE/MSH/Phar/msh.phar.nophi.csv")
diet <- fread("R:/GEMINI/_RESTORE/MSH/Diet/msh.diet.nophi.csv")
trans <- fread("R:/GEMINI/_RESTORE/MSH/Diet/msh.diet.nophi.csv")

sum(is.na(phar$EncID.new))
phar$EncID.new <- paste("14", phar$EncID.new, sep = "")
sum(is.na(diet$EncID.new))
diet[, EncID.new:= paste("14", EncID.new, sep = "")]
sum(is.na(trans$EncID.new))
trans[, EncID.new:= paste("14", EncID.new, sep = "")]

sum(phar$EncID.new%in%enc.new)
sum(diet$EncID.new%in%enc.new)
sum(trans$EncID.new%in%enc.new)


fwrite(msh, "H:/GEMINI/Data/MSH/CIHI/msh.adm.nophi.csv")
fwrite(phar, "H:/GEMINI/Data/MSH/Pharmacy/msh.phar.nophi.csv")
fwrite(diet, "H:/GEMINI/Data/MSH/Diets/msh.diet.nophi.csv")
fwrite(trans, "H:/GEMINI/Data/MSH/Transfusion/msh.trans.nophi.csv")

setwd("H:/GEMINI/Data/MSH")
files <- list.files(recursive = T)
for(i in files){
  dat <- fread(i)
  print(paste(i, sum(dat$EncID.new%in%enc.old)))
}
sum(phar$EncID.new%in%enc.old)
sum(diet$EncID.new%in%enc.old)
sum(trans$EncID.new%in%enc.old)


msh$phar<- msh$EncID.new%in%phar$EncID.new
msh$diet <- msh$EncID.new%in%diet$EncID.new
msh$trans <- msh$EncID.new%in%trans$EncID.new

ggplot(msh, aes(ymd(ADMIT_DATE), fill = trans)) + 
  geom_histogram(binwidth = 10) + ggtitle("transfusion medicine")
length(unique(trans$EncID.new))

ggplot(msh, aes(ymd(ADMIT_DATE), fill = diet)) + 
  geom_histogram(binwidth = 10) + ggtitle("diet")

ggplot(msh, aes(ymd(ADMIT_DATE), fill = phar)) + 
  geom_histogram(binwidth = 10) + ggtitle("phar")



# -------------------  feb 15 2017 ---------------------------------------------
# ------------------- use concordant data only for now -------------------------

rm(list = ls())
library(gemini)
lib.pa()
msh <- fread("R:/GEMINI/_RESTORE/MSH/CIHI/msh.adm.nophi.csv")
# compare with dad
dad <- fread("H:/GEMINI/DataBackup/Data170214/MSH/CIHI/msh.ip_dad.nophi.csv")
msh$EncID.new <- paste("14", msh$EncID.new, sep = "")
enc.new <- msh$EncID.new[!msh$EncID.new%in%dad$EncID.new]
enc.old <- dad$EncID.new[!dad$EncID.new%in%msh$EncID.new]
discor <- unique(c(enc.new, enc.old))
fwrite(msh[!EncID.new%in%dad$EncID.new, .(ADMIT_DATE, EncID.new)], 
       "H:/GEMINI/Results/Check/msh.disc.adm.csv")
fwrite(dad[!EncID.new%in%msh$EncID.new, .(Admit.Date, EncID.new)], 
       "H:/GEMINI/Results/Check/msh.disc.dad.csv")

swdh("MSH")
files <- list.files(recursive = T)
msh.rm.disc <- function(){
  for(i in files){
    dat <- NULL
    dat <- fread(i)
    if("NACRSRegistrationNumber"%in%names(dat))
      dat <- fread(i, colClasses = list(character = "NACRSRegistrationNumber"))
    dat <- dat[!EncID.new%in%discor]
    fwrite(dat, i)
  }
}

msh.rm.disc()



gim.rm.extra <- function(){
  setwd("H:/GEMINI/Data/GEMINI")
  files <- list.files(recursive = T)
  for(i in files){
    dat <- NULL
    dat <- fread(i)
    dat <- dat[!EncID.new%in%discor]
    fwrite(dat, i)
    print(i)
  }
}

gim.rm.extra()

# ---------------------------- Feb 28 2017 -------------------------------------
# -------------------- further check discordant patients -----------------------

rm(list = ls())
library(gemini)
lib.pa()
msh <- fread("R:/GEMINI/_RESTORE/MSH/CIHI/msh.adm.nophi.csv")
# compare with dad
dad <- fread("H:/GEMINI/DataBackup/Data170214/MSH/CIHI/msh.ip_dad.nophi.csv")
ggplot(dad[!EncID.new%in%msh$EncID.new],
       aes(x = ymd(Discharge.Date))) + geom_histogram(binwidth = 10)
ggplot(dad[!EncID.new%in%msh$EncID.new],
       aes(x = ymd(Admit.Date))) + geom_histogram(binwidth = 10)
ggplot(msh[!EncID.new%in%dad$EncID.new],
       aes(x = ymd(ADMIT_DATE))) + geom_histogram(binwidth = 10)

