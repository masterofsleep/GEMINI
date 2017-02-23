################################################################################
#############   Find all the data files and their paths   ######################
################################################################################
library(gemini)
lib.pa()
library(data.tree)
rm(list = ls())
setwd("H:/GEMINI/Data")
pathString <- list.files(recursive = T)
#find the path for all files in _RESTORE folder
findpath <- function(x){
  paste(x[1: (length(x)-1)], collapse = "/")
}
pathsplit <- str_split(pathString,'/')
paths <- sapply(pathsplit, findpath)

#list all file names
files <- sapply(pathsplit, last)

#create a table for all files and their paths
data.frame(files, paths) %>% 
  write.csv("H:/GEMINI/Results/filepaths.csv")
pathString <- paste("Data/", pathString, sep = "")
#create the hierarchy of the files and their paths
hier <- as.Node(data.frame(cbind(files, paths, pathString), stringsAsFactors = F))
sink("H:/GEMINI/Results/filepaths.txt")
print(hier, "files", "paths", limit = 700)
sink()



#check duplicated files
files <- table(tolower(read_csv("H:/GEMINI/Results/filepaths.csv")$files))
sort(files[files>1])
