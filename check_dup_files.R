################################################################################
######   Quality Control: Find and check Duplicated files ######################
################################################################################
lib.pa <- function(){
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(xtable)
  library(readr)
}
lib.pa()


#compare ip_adm and ip_dad sbk
setwd("//vs-research/research/GEMINI/_RESTORE/SBK/CIHI")
sbk.adm <- read_csv("sbk.adm.nophi.csv")
sbk.dad <- read_csv("sbk.ip_dad.nophi.csv")

#check uniqueness of encid in both files
length(unique(sbk.adm$EncID.new))
length(unique(sbk.dad$EncID.new))
length(intersect(sbk.adm$EncID.new, sbk.adm$EncID.new))

#a function to check the duplicates for each sites
#suspicious were defined as two files with same name but different sizes
checkdup<-function(site){
  path <- paste("//vs-research/research/GEMINI/_RESTORE", site, sep = "/")
  setwd(path)
  pathString <- list.files(recursive = TRUE)
  pathString <- paste(path, pathString, sep = "/")
  #find the path for all files in _RESTORE folder
  findpath <- function(x){
    paste(x[1: (length(x)-1)], collapse = "/")
  }
  pathsplit <- str_split(pathString,'/')
  paths <- sapply(pathsplit, findpath)
  
  #list all file names
  files <- tolower(sapply(pathsplit, last))
  filepath <- data.frame(files, pathString, stringsAsFactors = F)
  filetable = table(filepath$file)
  dupfile <- names(filetable[filetable>=2])
  dups <- NULL
  for(i in 1:length(dupfile)){
    pathi <- filepath[filepath$file==dupfile[i],"pathString"]
    for(j in 2:length(pathi)){
      if(file.size(pathi[j-1])!= file.size(pathi[j])){
        dups <- rbind(dups, c(dupfile[i], pathi[j-1], pathi[j]))
      }
    }
  }
  return(dups)
}

#a function to find all duplicated files with different sizes
check_all_dup <- function(){
  sites <- c("SMH","MSH", "THP", "SBK", "UHN")
  dup <- NULL
  for(i in 1:5){
    dup <- rbind(dup, checkdup(sites[i]))
  }
  return(dup)
}
diff_dups <- check_all_dup()
#remove those called missing.csv
diff_dups <- diff_dups[7:29, ]
write.csv(diff_dups, "H:/GEMINI/Results/dup_files.csv")

#see what happened in the two files
i = 0
i
i = i+1;f1 <- read.csv(diff_dups[i, 2],stringsAsFactors = F);f2 <- read.csv(diff_dups[i, 3], stringsAsFactors = F)
f1 == f2
dim(f1);dim(f2)
names(f1)
names(f2)
names(f1) == names(f2)
length(intersect(f1$Encounter_ID, f2$EncID.new))
cat("Variable names in file 1:", names(f1),"\n", "variable names in file 2", names(f2))
cat("same variable names, but f1 has more obs(", dim(f1)[1], ") than f2 (", dim(f2)[1],"), ", length(intersect(f1$EncID.new, f2$EncID.new))," in common")
cat("same variable names, but f1 has less obs(", dim(f1)[1], ") than f2 (", dim(f2)[1],"), ", length(intersect(f1$EncID.new, f2$EncID.new))," in common")

length(unique(f1$EncID.new))
length(unique(f2$EncID.new))
length(intersect(f1$Encounter_ID, f2$EncID.new))

length(unique(f1$Encounter_ID))
length(unique(f2$EncID.new))
