################################################################################
##################### Reorganize all data files ################################
################################################################################


#following code are used to check some unclear data files and 
#validate some merged files
library(R.utils)

#functions used in this whole section
sf <- function(sitefolder){
#a function takes the site and folder and set the working directory to the folder
  wd <- paste("//vs-research/research/GEMINI/_RESTORE", sitefolder, sep = "/")
  setwd(wd)
}


finfo <- function(x){
  #a function to find the size and variable names in a file
  print(x)
  size <- file.size(x)
  if(size > 1000000){
    file <- fread(x, nrow = 5)
    print(c(countLines(x)[1], ncol(file)))
    print(names(file))
  } else {
    file <- fread(x)
    print(dim(file))
    print(names(file))
  }
}












##########S M H ################################################################
##check BB folder
sf("SMH/BB")
list.files(); files <- list.files()
finfo(files[1])
finfo(files[3])
#result: keep smh.bb.csv



sf("SMH/CoreLab")
#files are huge
list.files(recursive = T); files <- list.files(recursive = T)
finfo(files[2])
finfo(files[5]) #file too large, need 64 bit machine
finfo(files[6]) #file too large, need 64 bit machine
finfo(files[8]) #same as above
finfo(files[9]) #same as above
###this need to be solved later#########################                        need to be solved later!!


sf("SMH/Diet")
list.files(recursive = T); files <- list.files(recursive = T)
finfo(files[1]) # raw data from soarian
finfo(files[2]) # raw data from soarian
finfo(files[4])

#use smh.diet


sf("SMH/Pharmacy")
list.files(recursive = T); files <- list.files(recursive = T)
finfo(files[1]) # raw data from soarian, no var names 47686 * 11
finfo(files[2]) # raw data from soarian, with var names, 49936 * 31
finfo(files[4]) # to be used, 527008 * 31

sf("SMH/Radiology")
list.files(recursive = T); files <- list.files(recursive = T)
finfo(files[17]) #us_enc.csv unknown file. very different from smh.us           ask about it!!!
finfo(files[15]) #smh.us.csv


##########S B K ################################################################


#validate the merged result in CIHI

sf("SBK/CIHI")
list.files(recursive = T); files <- list.files(recursive = T)


#sbk.ip_int-merged vs int, int2

f1 <- fread(files[11])
f2 <- fread(files[13])
fm <- fread(files[68])
f1 == f2
#sbk.ip_int, skb.ip_int2 has same dimension, same var names
####same unique EncID, but still many difference inside                         ask about it!!!How they were merged?
#merged data has same unique EncIDs
#merged data has site added IntervTime
length(unique(f1$EncID.new))
length(unique(f2$EncID.new))
length(unique(fm$EncID.new))
length(intersect(f1$EncID, f2$EncID.new))



#sbk.ip_diag.merged vs sbk.ip_diag, diag2, diag3


f1 <- fread(files[5]);finfo(files[5])
f2 <- fread(files[7]);finfo(files[7])
f3 <- fread(files[9]);finfo(files[9])
fm <- fread(files[64]);finfo(files[64])

rbind(dim(f1), dim(f2),dim(f3),dim(fm)); sum(a[1:3,])
# #ofobs in merge is not sum of three: merge has 151704, sum is 151719

rbind(names(f1),names(f2), names(f3), names(fm))
#diag 1, 2, 3 and merged file has same variable names

length(unique(f1$EncID.new))
length(unique(f2$EncID.new))
length(unique(f3$EncID.new))
length(intersect(f1$EncID, f2$EncID.new))
length(intersect(f1$EncID, f3$EncID.new))
length(intersect(f2$EncID, f3$EncID.new))

sum(f2$EncID.new == intersect(f1$EncID, f2$EncID.new))
sum(f1$EncID.new == intersect(f1$EncID, f2$EncID.new))

sum(f2$EncID.new == intersect(f3$EncID, f2$EncID.new))
sum(f3$EncID.new == intersect(f3$EncID, f2$EncID.new))

#there are duplicated entries in f1,f2 and in f2 f3, the merge is correct





#sbk.er_int

f1 <- fread(files[1])
f2 <- fread(files[3])
fm <- fread(files[58])
f1 == f2
length(unique(f1$EncID.new))+
length(unique(f2$EncID.new))
length(unique(fm$EncID.new))
length(intersect(f1$EncID, f2$EncID.new))
#correctly merge






#check sbk.endo and sbk.endo2
f1 <- fread("H:/GEMINI/Data/SBK/CIHI/sbk.Endo.nophi.csv")
f2 <- fread(files[47])
#same, Endo2 is better formatted



#SBK Echo
sf("SBK/Echo")
list.files(recursive = T); files <- list.files(recursive = T)
for(i in c(1,2,3,4,5,6,13,14)){
  finfo(files[i])
}


#Radiology
sf("SBK/Rad")
list.files(recursive = T); files <- list.files(recursive = T)

for(i in c(14:19)){
  finfo(files[i])
}
