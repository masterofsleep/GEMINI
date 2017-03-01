
#============================ Microbiology =====================================
#-----------------------  available for SMH, SBK  ------------------------------
library(gemini)
lib.pa()
rm(list = ls())


smh <- readg(smh, micro)
names(smh)
sum(duplicated(smh))
smh <- smh[!duplicated(smh)]
head(smh)
apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
write.csv(smh, "H:/GEMINI/Data/SMH/Micro/smh.micro.csv",
          row.names = F, na = "")



sbkneg <- readg(sbk, micro_neg.csv)
names(sbkneg)
sum(duplicated(sbkneg))
apply(sbkneg, MARGIN = 2, FUN = function(x)sum(is.na(x)))

sbkpos <- readg(sbk, micro_pos.csv)
names(sbkpos)
sum(duplicated(sbkpos))
sbkpos <- sbkpos[!duplicated(sbkpos)]
apply(sbkpos, MARGIN = 2, FUN = function(x)sum(is.na(x)))
write.csv(sbkpos, "H:/GEMINI/Data/SBK/Micro/sbk.micro_pos.csv",
          row.names = F, na = "")


apply(sbkpos[,c(13:70), with = F], MARGIN = 2, FUN = table)


#transpose pos to long format
library(reshape2)
sbkpos.long <- melt(sbkpos, id.vars = names(sbkpos)[c(1:10,71)],
                    variable.name = "Drug.Name", 
                    value.name = "Antimicrobial.Susceptibility")
names(sbkpos.long)
apply(sbkpos.long, MARGIN = 2, FUN = function(x)sum(is.na(x)))
write.csv(sbkpos.long, "H:/GEMINI/Data/SBK/Micro/sbk.micro_pos_long.csv",
          row.names = F, na = "")




#--------------- Feb 2 2-17 UHN Micro quality check ----------------------------
rm(list = ls())
library(gemini)
lib.pa()
setwd("R:/GEMINI/_RESTORE/UHN/Micro/TW")
files <- list.files()
micro <- NULL
for(i in 1:length(files)){
  dat <- fread(files[i])
  dat$EncID.new <- paste("13", dat$EncID.new, sep = "")
  print(c(files[i], "EncID.new"%in%names(dat)))
  micro <- c(micro, dat$EncID.new)
  print(c(length(unique(micro)), length(micro)))
}
dat <- fread(files[3])
setwd("R:/GEMINI/_RESTORE/UHN/Micro/TGH")
files <- list.files()
for(i in 1:length(files)){
  dat <- fread(files[i])
  print(c(files[i], "EncID.new"%in%names(dat)))
  dat$EncID.new <- paste("13", dat$EncID.new, sep = "")
  micro <- c(micro, dat$EncID.new)
  print(c(length(unique(micro)), length(micro)))
}

dat <- fread(files[8])
uhn.dad <- readg(uhn, dad)
uhn.dad$micro <- uhn.dad$EncID.new %in% micro

ggplot(uhn.dad, aes(x = ymd(Discharge.Date), fill = micro)) + 
  geom_histogram(binwidth = 5) + ggtitle("uhn micro missingness")

range(uhn.dad[micro==TRUE, ymd(Discharge.Date)])








#--------------- Feb 6 new UHN Micro quality check ----------------------------
rm(list = ls())
library(gemini)
lib.pa()
micro <- NULL
setwd("R:/GEMINI/_RESTORE/UHN/Micro/TW")
files <- list.files()
for(i in 1:length(files)){
  dat <- fread(files[i])
  dat$EncID.new <- paste("13", dat$EncID.new, sep = "")
  print(c(files[i], "EncID.new"%in%names(dat), "admdate"%in%names(dat)))
  micro <- rbind(micro, dat[,.(EncID.new, admdate, DIS.DT, CDATE, CTIME)])
}
setwd("R:/GEMINI/_RESTORE/UHN/Micro/TGH")
files <- list.files()

dat <- fread(files[1])
for(i in 1:length(files)){
  dat <- fread(files[i])
  dat$EncID.new <- paste("13", dat$EncID.new, sep = "")
  dat$DIS.DT <- dat$X.DIS.DT.
  print(c(files[i], "EncID.new"%in%names(dat), "admdate"%in%names(dat)))
  micro <- rbind(micro, dat[,.(EncID.new, admdate, DIS.DT)])
}
length(unique(micro$EncID.new))

dad <- readg(uhn, dad)
micro <- merge(micro, dad[,.(EncID.new, Admit.Date, Discharge.Date)], by = "EncID.new", 
               all.x = T, all.y = F)

# see how many unique encounters are there
uhn.adm <- readg(uhn, adm)
micro <- merge(micro, uhn.adm[,.(Hash, EncID.new)], by = "EncID.new",
               all.x = T, all.y = F)

unique(micro[,.(Hash, admdate)]) %>% dim



#find those that do not have a matched admitting date

no.match <- micro[!EncID.new%in%micro[ymd(admdate) == ymd(Admit.Date), EncID.new]]







check2 <- micro[ymd(admdate)<=ymd(Admit.Date)+3 & ymd(admdate)>=ymd(Admit.Date)-3]
check1 <- micro[ymd(admdate)==ymd(Admit.Date)]
length(unique(check2$EncID.new))
length(unique(check1$EncID.new))

dad$micro <- dad$EncID.new%in%overlapped.micro$EncID.new
ggplot(dad, aes(x = ymd(Discharge.Date), fill = micro)) + 
  geom_histogram(binwidth = 5) + ggtitle("uhn micro missingness")


#check overlapped hospitalization time
micro[ymd(admdate)>= ymd(Admit.Date)&ymd(admdate)<= ymd(Discharge.Date)|
        ymd(DIS.DT)>= ymd(Admit.Date)&ymd(DIS.DT)<= ymd(Discharge.Date)|
        ymd(admdate)<=ymd(Admit.Date)&ymd(DIS.DT)>=ymd(Discharge.Date)]  -> overlapped.micro
length(unique(overlapped.micro$EncID.new))

micro[!EncID.new%in%overlapped.micro$EncID.new] -> check
check2 <- check[!Hash%in%overlapped.micro$Hash]



# -------------------- new sbk micro pos data ----------------------------------
# ------------feb 23 2017, collection time added -------------------------------
library(gemini)
lib.pa()
sbk.pos <- fread("R:/GEMINI/_RESTORE/SBK/Micro/sbk.micro_pos.nophi-2.csv")
warnings()
sbk.pos$EncID.new <- paste("12", sbk.pos$EncID.new, sep = "")
fwrite(sbk.pos, "H:/GEMINI/Data/SBK/Micro/sbk.micro_pos.csv")





# ---------------------- feb 28 2017 -------------------------------------------
# ---------------------- UHN Micro clean ---------------------------------------
er <- readg(uhn, uhn.er.nophi)
dad <- readg(uhn, dad)
uhntime <- merge(er[,.(Triage.Date, Triage.Time, EncID.new)],
                 dad[,.(Admit.Date, Admit.Time, Discharge.Date,
                        Discharge.Time, EncID.new)], 
                 by = "EncID.new", all.y = T)
sum(duplicated(er))
sum(duplicated(uhntime))
er[EncID.new%in%er$EncID.new[duplicated(er$EncID.new)]]
uhntime[EncID.new%in%uhntime$EncID.new[duplicated(uhntime$EncID.new)]]
# duplicated encounter id in uhn er is caused by different blood transfer component

uhntime <- unique(uhntime)
uhntime[is.na(Triage.Date), ':='(
  Triage.Date = Admit.Date,
  Triage.Time = Admit.Time
)]
uhntime$EncID.new <- as.character(uhntime$EncID.new)
setwd("R:/GEMINI/_RESTORE/UHN/Micro/TW")
files <- list.files()
for(i in files){
  dat <- fread(i)
  print(nrow(dat))
  dat$EncID.new <- paste("13", dat$EncID.new, sep = "")
  dat <- merge(dat, uhntime, by = "EncID.new", all.x = T)
  dat <-dat[ymd(CDATE)>=ymd(Triage.Date)&
            ymd(CDATE)<=ymd(Discharge.Date)]
  print(nrow(dat))
  #fwrite(dat, paste("H:/GEMINI/Data/UHN/Micro/TW/", i, sep = ""))
}

dat[,.(CDATE, CTIME, Triage.Date, Triage.Time, Discharge.Date,
       Discharge.Time, EncID.new)] -> check



setwd("R:/GEMINI/_RESTORE/UHN/Micro/TGH")
files <- list.files()
for(i in files){
  print(i)
  dat <- fread(i)
  print(nrow(dat))
  names(dat)[1:12] <- c("ORDER", "Sex", "BIL", "WARD",
                        "ROOM", "BED","ADMDATE", "DISDATE",
                        "CDATE", "CTIME", "AREA", "CurLoc")
  dat$EncID.new <- paste("13", dat$EncID.new, sep = "")
  dat <- merge(dat, uhntime, by = "EncID.new", all.x = T)
  dat <-dat[ymd(CDATE)>=ymd(Triage.Date)&
              ymd(CDATE)<=ymd(Discharge.Date)]
  print(nrow(dat))
  fwrite(dat, paste("H:/GEMINI/Data/UHN/Micro/TGH/", i, sep = ""))
}
