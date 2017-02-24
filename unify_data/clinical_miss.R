library(gemini)
missing.cluster <- function(site, data_name, colClasses = NULL){
  swdh()
  files <- list.files(recursive = T)
  site <- deparse(substitute(site))
  data_name <- deparse(substitute(data_name))
  filepath <- files[grepl(site, files)&grepl(data_name, files)]
  dat <- fread(filepath, na.strings = c(NA,NULL,"", "NA", " "),
               colClasses = colClasses, showProgress = T)
  dat$EncID.new <- as.character(dat$EncID.new)
  dadpath <- files[grepl(site, files)&grepl("dad", files)]
  dad <- fread(dadpath, na.strings = c(NA,NULL,"", "NA", " "))
  dad$var <- dad$EncID.new%in%dat$EncID.new
  ggplot(dad, aes(x = ymd(Discharge.Date), fill = var)) + 
           geom_histogram(binwidth = 20, alpha = 0.5) + 
    ggtitle(paste(site, data_name))
}


# --------------------------- St. Michael's ------------------------------------
missing.cluster(smh, bb)
missing.cluster(smh, phar)
missing.cluster(smh, labs) # more missingness in early 2 years, but not cluster
missing.cluster(smh, micro)
missing.cluster(smh, echo)
missing.cluster(smh, ecg)
missing.cluster(smh, diet)
missing.cluster(smh, vitals)


# ---------------------------- Sunny Brook -------------------------------------
missing.cluster(sbk, echo)
missing.cluster(sbk, labs_er) # missing 2010
missing.cluster(sbk, labs_ip)
missing.cluster(sbk, micro_neg)
missing.cluster(sbk, micro_pos.csv)
missing.cluster(sbk, phar)
missing.cluster(sbk, rad.csv)

# -------------------------------- UHN -----------------------------------------
missing.cluster(uhn, echo) # missing last two years
missing.cluster(uhn, labs)
missing.cluster(uhn, phar)
missing.cluster(uhn, rad_er)
missing.cluster(uhn, rad_ip)
missing.cluster(uhn, txm_er)
missing.cluster(uhn, txm_ip)

# ----------------------------- Mount Sinai ------------------------------------
missing.cluster(msh, diet)
missing.cluster(msh, ecg)
missing.cluster(msh, phar)
missing.cluster(msh, rad_ip)
missing.cluster(msh, rad_er)
missing.cluster(msh, trans)
