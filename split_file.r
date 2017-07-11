split_file <- function(file_path, percent_overlap = 10){
  file_name <- substr(file_path, 1, nchar(file_path)-5)
  dat <- read.csv(file_path)
  all_enc <- unique(dat$EncID.new)
  n_enc <- length(all_enc)
  overlap <- sample(all_enc, ceiling(n_enc*percent_overlap*0.01))
  rest_enc <- all_enc[!all_enc%in%overlap]
  enc_g1 <- sample(rest_enc, ceiling(length(rest_enc)/2))
  enc_g2 <- rest_enc[!rest_enc%in%enc_g1]
  file1 <- rbind(dat[dat$EncID.new%in%overlap,],
                 dat[dat$EncID.new%in%enc_g1,])
  file2 <- rbind(dat[dat$EncID.new%in%overlap,],
                 dat[dat$EncID.new%in%enc_g2,])
  write.csv(file1, paste(file_name, "_1.csv", sep = ""), row.names = F, na = "")
  write.csv(file2, paste(file_name, "_2.csv", sep = ""), row.names = F, na = "")
}

