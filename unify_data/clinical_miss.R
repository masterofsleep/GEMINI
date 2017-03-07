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
missing.cluster(sbk, rad_ip)
missing.cluster(sbk, rad_er)

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
missing.cluster(msh, lab)




# -------- feb 27 2017 see if missing sbk lab er are in lab ip -----------------
sbk.laber <- readg(sbk, labs_er, dt = TRUE)
sbk.labip <- readg(sbk, labs_ip, dt = TRUE)
check <- sbk.labip[EncID.new%in%sbk.labip[!EncID.new%in%sbk.laber$EncID.new, EncID.new]]
check1 <- check[ymd_hms(Collection.DtTm)<ymd_hm(paste(Admit.Date, Admit.Time))]





# --------------- feb 29 check all sbk clinical variables ----------------------
dat <- readg(sbk, echo)
dat <- readg(sbk, phar)
dat <- readg(sbk, labs_er)
dat <- readg(sbk, labs_ip)
dat <- readg(sbk, micro_neg)
dat <- readg(sbk, micro_pos.csv)
dat <- readg(sbk, phar)
dat <- readg(sbk, rad.csv)



# ------------------- march 6 list of encounters of missing er lab -------------
sbk.laber <- readg(sbk, labs_er)
sbk.dad <- readg(sbk, dad)
sbk.no.laber <- sbk.dad[!EncID.new%in%sbk.laber$EncID.new, .(Admit.Date, Discharge.Date, EncID.new)] %>% 
  arrange(ymd(Discharge.Date)) %>% data.table
sbk.no.laber$EncID.new <- str_sub(sbk.no.laber$EncID.new, 3, 8)
fwrite(sbk.no.laber, "H:/GEMINI/Results/Ad Hoc/sbk.nolaber.csv")


# ------------------- check missingness in uhn micro ---------------------------
setwd("H:/GEMINI/Data/UHN/Micro/TW")
files <- list.files()
tw.micro <- NULL
for(i in files){
  dat <- fread(i)
  tw.micro <- c(tw.micro, dat$EncID.new)
}


setwd("H:/GEMINI/Data/UHN/Micro/TGH")
files <- list.files()
tg.micro <- NULL
for(i in files){
  dat <- fread(i)
  tg.micro <- c(tg.micro, dat$EncID.new)
}
uhn.adm <- readg(uhn, adm)
uhn.dad <- readg(uhn, dad)
uhn.dad <- merge(uhn.dad, uhn.adm[,.(EncID.new, Institution.Number)])
uhn.dad$micro <- uhn.dad$EncID.new%in%c(tw.micro, tg.micro)
ggplot(uhn.dad[Institution.Number=="54265"], aes(x = ymd(Discharge.Date), fill = micro)) + 
  geom_histogram(binwidth = 5)

ggplot(uhn.dad[Institution.Number=="54266"], aes(x = ymd(Discharge.Date), fill = micro)) + 
  geom_histogram(binwidth = 5)


# ------------------------ check new sbk lab er --------------------------------
sbk.lab.er1 <- fread("R:/GEMINI/_RESTORE/SBK/Lab/sbk.labs_er1.csv")
sbk.lab.er2 <- fread("R:/GEMINI/_RESTORE/SBK/Lab/sbk.labs_er2.csv")
sbk.lab.er1$EncID.new <- paste("12", sbk.lab.er1$EncID.new, sep = "")
sbk.lab.er2$EncID.new <- paste("12", sbk.lab.er2$EncID.new, sep = "")
sbk.dad <- readg(sbk, dad)

sbk.dad$laber <- ifelse(sbk.dad$EncID.new%in%sbk.lab.er1$EncID.new, "sbk.lab.er1",
                        ifelse(sbk.dad$EncID.new%in%sbk.lab.er2$EncID.new, "sbk.lab.er2", "no.lab.er"))
ggplot(sbk.dad, aes(ymd(Discharge.Date), fill = laber)) + geom_histogram(binwidth = 5, alpha = 0.6)
