install.packages(data.table)
install.packages(stringr)
library(data.table)
library(stringr)
# note that all the "/" in the file path cannot be replaced by "\"
# change the path and file name to the one that you want to link MRN to
file <- fread("H:/GEMINI/Results/Syncope/anticoag.smh.new.csv")
# change the path and file name to the path and name of the linking file with MRN and EncID.new
link <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/SMH.LINKLIST_NEWHASH.csv")
# change the path and file name to dad file
dad <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/smh.ip_dad.nophi.csv")
if(nchar(file$EncID.new[1])==8){
  file$EncID.new <- str_sub(file$EncID.new, 3, 8)
}
file$EncID.new <- as.character(file$EncID.new)
link$EncID.new <- as.character(link$EncID.new)
dad$EncID.new <- as.character(dad$EncID.new)
# change the "MRN" below to the column name of the MRN in the link file 
new.file <- merge(file, link[,.(EncID.new, MRN)], 
                  all.x = T, all.y = F, sort = F)
# change the "ADMITDATE" AND "DISCHARGEDATE" to column names in dad file
# skip this line if you do not need admit and discharge date
new.file <- merge(new.file, dad[,.(EncID.new, ADMITDATE, DISCHARGEDATE)],
                 by = "EncID.new", all.x = T, all.y = F, sort = F)
#change the path and file name to where you would like to store the file
fwrite(new.file, "C:/Users/guoyi/Desktop/newfile.csv")

