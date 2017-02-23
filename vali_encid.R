################################################################################
####  check whether all the EncIDs in all data files are subset in adm #########
################################################################################
rm(list = ls())
library(gemini)
lib.pa()


vali_encid <- function(admin_index, files_index){
  ##a function to get summary information for all files and to validate whether 
  ## admin data contains all the EncIDs for each site
  adm <- fread(files[admin_index])
  file_name <- NULL
  num_row <- NULL
  num_col <- NULL
  var_names <- NULL
  num_uniq_encid <- NULL
  num_in_admin <- NULL
  num_uniq_in_admin <- NULL
  for(i in files_index){
    file <- files[i]
    data <- fread(file)
    file_name <- c(file_name, files[i])
    num_row <- c(num_row, nrow(data))
    num_col <- c(num_col, ncol(data))
    var_names <- c(var_names, paste(names(data), collapse="/"))
    num_uniq_encid <- c(num_uniq_encid, length(unique(data$EncID.new)))
    num_in_admin <- c(num_in_admin, sum(data$EncID.new %in% adm$EncID.new))
    num_uniq_in_admin <-c(num_uniq_in_admin, 
                          sum(unique(data$EncID.new) %in% adm$EncID.new))
  }
  return(data.frame(file_name, num_row, num_in_admin, num_col, num_uniq_encid, 
                    num_uniq_in_admin, var_names))
}




#SMH
swdh("SMH")
list.files(recursive = T); files <- list.files(recursive = T)
#find the index of files to be examed (exclude interv/diag codes &stuff)
fi <- c(1:19, 23:32)
smh_summary <- vali_encid(2, fi)
write.csv(smh_summary, "H:/GEMINI/Results/DataSummary/smh_summary.csv", 
          row.names = F)
smh_summary$num_row == smh_summary$num_in_admin
smh_summary$num_uniq_encid == smh_summary$num_uniq_in_admin



#SBK
swdh("SBK")
list.files(recursive = T); files <- list.files(recursive = T)
fi <- c(1:26) #exlude 'new.hash' because it cannot be read into R
sbk_summary <- vali_encid(1, fi)
write.csv(sbk_summary, "H:/GEMINI/Results/DataSummary/sbk_summary.csv", 
          row.names = F)
sbk_summary$num_row == sbk_summary$num_in_admin
sbk_summary$num_uniq_encid == sbk_summary$num_uniq_in_admin


#UHN
swdh("UHN")
list.files(recursive = T); files <- list.files(recursive = T)
fi <- c(1:22) #exlude 'new.hash' because it cannot be read into R
uhn_summary <- vali_encid(1, fi)
write.csv(uhn_summary, "H:/GEMINI/Results/DataSummary/uhn_summary.csv", 
          row.names = F)
uhn_summary$num_row == uhn_summary$num_in_admin
uhn_summary$num_uniq_encid == uhn_summary$num_uniq_in_admin



#THP
swdh("THP")
list.files(recursive = T); files <- list.files(recursive = T)
fi <- c(1:13) #exlude 'new.hash' because it cannot be read into R
thp_summary <- vali_encid(1, fi)
write.csv(thp_summary, "H:/GEMINI/Results/DataSummary/thp_summary.csv", 
          row.names = F)
thp_summary$num_row == thp_summary$num_in_admin
thp_summary$num_uniq_encid == thp_summary$num_uniq_in_admin



#MSH
swdh("MSH")
list.files(recursive = T); files <- list.files(recursive = T)
fi <- c(1:14) #exlude 'new.hash' because it cannot be read into R
msh_summary <- vali_encid(8, fi)
write.csv(msh_summary, "H:/GEMINI/Results/DataSummary/msh_summary.csv", 
          row.names = F)
msh_summary$num_row == msh_summary$num_in_admin
msh_summary$num_uniq_encid == msh_summary$num_uniq_in_admin


#warnings returned because for some columns most of the cells are empty










##find the EncIDs that are not in the adm data
find_ex_encid <- function(admin_index, files_index){
  ##a function that find the unique encIDs that are not in the adm data and return
  #these endIDs with the file name 
  adm <- fread(files[admin_index])
  uniq_enc <- NULL
  file_name <- NULL
  for(i in files_index){
    file <- files[i]
    data <- fread(file)
    if(length(unique(data$EncID.new))>sum(unique(data$EncID.new) %in% adm$EncID.new)){
      file_name <- c(file_name, file)
      uniq_enc <- c(uniq_enc, unique(data$EncID.new[!data$EncID.new%in% adm$EncID.new]))
    }
  }
  return(data.frame(file_name, uniq_enc))
}

#SBK
swdh("SBK")
list.files(recursive = T); files <- list.files(recursive = T)
fi <- c(5,12,15:18) #exlude 'new.hash' because it cannot be read into R
sbk_uniq_enc <- find_ex_encid(2, fi)
rm(data)
#all are NA

#THP
swdh("THP")
list.files(recursive = T); files <- list.files(recursive = T)
fi <- 1
thp_uniq_enc <- find_ex_encid(1, 2)

#UHN
swdh("UHN")
list.files(recursive = T); files <- list.files(recursive = T)
uhn_uniq_enc <- find_ex_encid(2, 19)

#MSH
swdh("MSH")
list.files(recursive = T); files <- list.files(recursive = T)
fi = c(2,13)
msh_uniq_enc <- find_ex_encid(8, fi)











rm(list=ls())






##check whether there are duplicated EncIDs in different sites
##read in all the encIDs from different sites
swdh("SMH/CIHI")
enc.smh <- fread("smh.adm.nophi.csv", select = "EncID.new")
swdh("MSH/CIHI")
enc.msh <- fread("msh.ip_dad.nophi.csv", select = "EncID.new")
swdh("UHN/CIHI")
enc.uhn <- fread("uhn.adm.nophi.csv", select = "EncID.new")
swdh("THP/CIHI")
enc.thp <- fread("thp.adm.nophi.csv", select = "EncID.new")
swdh("SBK/CIHI")
enc.sbk <- fread("sbk.adm.nophi.csv", select = "EncID.new")

#duplicates within sites?
sum(duplicated(enc.smh))
sum(duplicated(enc.msh))
sum(duplicated(enc.thp)) 
sum(duplicated(enc.uhn))
sum(duplicated(enc.sbk))

#64 in thp, 5 in msh


enc.thp.uni <- unique(enc.thp)
enc.msh.uni <- unique(enc.msh)
encid <- list(enc.msh.uni$EncID.new, enc.sbk$EncID.new, enc.smh$EncID.new,
              enc.thp.uni$EncID.new, enc.uhn$EncID.new)

overlap <- matrix(0, 5, 5)
sites <- c("MSH", "SBK", "SMH", "THP", "UHN")
for(i in 1:5){
  for(j in i:5){
    overlap[i, j] <- length(intersect(encid[[i]], encid[[j]]))
  }
}
overlap <- data.frame(overlap)
row.names(overlap) <- sites
names(overlap) <- sites

write.csv(overlap, "H:/GEMINI/Results/DataSummary/encid_overlap.csv")












#check NAs in enc id
library(gemini)
lib.pa()


swdh("SBK")
list.files(recursive = T); files <- list.files(recursive = T)

file <- fread(files[5])
file[is.na(EncID.new)]

file <- fread(files[2])
row <- file[is.na(EncID.new)]




swdh("THP")
list.files(recursive = T); files <- list.files(recursive = T)
file <- fread(files[2])
row <- file[is.na(EncID.new)]



swdh("UHN")
list.files(recursive = T); files <- list.files(recursive = T)
file <- fread(files[19])
row <- file[is.na(EncID.new)]





swdh("MSH")
list.files(recursive = T); files <- list.files(recursive = T)
file <- read.csv(files[13])
row <- file[is.na(file$EncID.new), ]

