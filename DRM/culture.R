
#-----------------------  available for SMH, SBK  ------------------------------
library(gemini)
lib.pa()
rm(list = ls())

smh <- readg(smh, micro)
sbkneg <- readg(sbk, micro_neg.csv)
sbkpos <- readg(sbk, micro_pos.csv)


smh$Test_ID %>% table %>% data.table %>% fwrite("H:/GEMINI/Results/DRM/smh.culture.csv")
c(sbkneg$culture_test_cd, sbkpos$culture_test_cd) %>% table %>%
  data.table %>% fwrite("H:/GEMINI/Results/DRM/sbk.culture.csv")

micro <- NULL
setwd("R:/GEMINI/_RESTORE/UHN/Micro/TW")
files <- list.files()
for(i in 1:length(files)){
  dat <- fread(files[i])
  dat$EncID.new <- paste("13", dat$EncID.new, sep = "")
  print(c(files[i], "EncID.new"%in%names(dat), "admdate"%in%names(dat)))
  micro <- rbind(micro, dat[,.(EncID.new, admdate, DIS.DT, TEST, SRC, SITE)])
}
micro.tw <- micro
micro <- NULL
setwd("R:/GEMINI/_RESTORE/UHN/Micro/TGH")
files <- list.files()
for(i in 1:length(files)){
  dat <- fread(files[i])
  dat$EncID.new <- paste("13", dat$EncID.new, sep = "")
  dat$DIS.DT <- dat$X.DIS.DT.
  if(!"SRC"%in%names(dat)){
    dat$SRC <- dat$X.SRC..
  }
  if(!"SITE"%in%names(dat)){
    dat$SITE <- dat$X..SITE..........................
  }
  if(!"TEST"%in%names(dat)){
    dat$TEST <- dat$X..TEST.
  }
  print(c(files[i], "EncID.new"%in%names(dat), "admdate"%in%names(dat)))
  micro <- rbind(micro, dat[,.(EncID.new, admdate, DIS.DT, TEST, SRC, SITE)])
}

micro.tg <- micro

table(smh$Isolate_num)
smh[Isolate_num%in%c("A","B","C","D","E","F","G")] %>%
  fwrite("R:/GEMINI/Check/smh.micro.pos.check.csv")


smh.cul <- table(smh[,.(Test_ID, Source)]) %>% data.table
smh.cul <- smh.cul[N!=0]
test.names <- readxl::read_excel("H:/GEMINI/Results/DRM/TESTS.xls")
source.names <- readxl::read_excel("H:/GEMINI/Results/DRM/source.xls")
smh.cul <- merge(smh.cul, test.names[,c(1,2)], by.x = "Test_ID", by.y = "ID", all.x = T, all.y = F)
smh.cul <- merge(smh.cul, source.names[,c(2,3)], by.x = "Source", by.y = "SOURCE_ID", all.x = T, all.y = F)
names(smh.cul) <- c("Source", "Test_ID", "N", "Test.Name", "Source.Name")
fwrite(smh.cul[,c(2, 4, 1, 5, 3), with = F], "H:/GEMINI/Results/DRM/smh.culture.csv")

sbk.cul <- table(sbkpos[,.(culture_test_cd, description, specimen_source)]) %>% data.table
sbk.cul <- sbk.cul[N!=0]
fwrite(sbk.cul, "H:/GEMINI/Results/DRM/sbk.culture.csv")

tgh.cul <- table(micro.tg[,.(TEST, SRC, SITE)]) %>% data.table
tgh.cul <- tgh.cul[N!=0]
fwrite(tgh.cul, "H:/GEMINI/Results/DRM/tgh.culture.csv")

twh.cul <- table(micro.tw[,.(TEST, SRC, SITE)]) %>% data.table
twh.cul <- twh.cul[N!=0]
fwrite(twh.cul, "H:/GEMINI/Results/DRM/twh.culture.csv")


# ------------------ SMH Unknown culture sample --------------------------------
library(gemini)
lib.pa()
rm(list = ls())
smh <- readg(smh, micro)
culture.marked <- readxl::read_excel("H:/GEMINI/Results/DRM/CultureClassification_Feb9.xlsx")
cul.unknown <- culture.marked[culture.marked$unknown==1, ]
cul.unknown.paste <- paste(cul.unknown$Test_ID, cul.unknown$Source, sep = "")
cul.unknown.data <- smh[paste(Test_ID, Source, sep = "")%in%cul.unknown.paste] 

apply(cul.unknown.data, MARGIN = 2, FUN = function(x) sum(is.na(x)))

fwrite(cul.unknown.data, "H:/GEMINI/Results/DRM/smh.unknown.culture.csv")


# ----------------- UHN Unknown Culture Sample ---------------------------------
tgh.unknown <- readxl::read_excel("H:/GEMINI/Results/DRM/CultureClassification_Feb9_DM.xlsx",
                                  sheet = 3)%>% data.table
twh.unknown <- readxl::read_excel("H:/GEMINI/Results/DRM/CultureClassification_Feb9_DM.xlsx",
                                  sheet = 4)%>%data.table
tgh.unknown <- tgh.unknown[unknown==1, ]
tgh.unknown$TEST <- trimws(tgh.unknown$TEST)
tgh.unknown$SRC <- trimws(tgh.unknown$SRC)
tgh.unknown$SITE <- trimws(tgh.unknown$SITE)
tgh.paste <- paste(tgh.unknown$TEST, tgh.unknown$SRC, tgh.unknown$SITE, sep = "")


twh.unknown <- twh.unknown[unknown==1, ]
twh.unknown$TEST <- trimws(twh.unknown$TEST)
twh.unknown$SRC <- trimws(twh.unknown$SRC)
twh.unknown$SITE <- trimws(twh.unknown$SITE)
twh.paste <- paste(twh.unknown$TEST, twh.unknown$SRC, twh.unknown$SITE, sep = "")
sum(tgh.unknown$N[tgh.unknown$unknown==1], na.rm = T)
sum(twh.unknown$N[twh.unknown$unknown==1], na.rm = T)

setwd("R:/GEMINI/_RESTORE/UHN/Micro/TW")
files <- list.files()
n.unknown <- 0
for(i in 1:length(files)){
  dat <- fread(files[i])
  dat$EncID.new <- paste("13", dat$EncID.new, sep = "")
  dat$TEST <- trimws(dat$TEST)
  dat$SRC <- trimws(dat$SRC)
  dat$SITE <- trimws(dat$SITE)
  dat.unknown <- dat[paste(TEST, SRC, SITE, sep = "")%in%twh.paste]
  fwrite(dat.unknown, paste("H:/GEMINI/Results/DRM/uhn.culture.unknown/TWH/", str_sub(files[i], 1, 10),
           "unknown culture.csv", sep = ""))
  n.unknown <- n.unknown + nrow(dat.unknown)
}
n.unknown

setwd("R:/GEMINI/_RESTORE/UHN/Micro/TGH")
files <- list.files()
n.unknown <- 0
for(i in 1:length(files)){
  dat <- fread(files[i])
  dat$EncID.new <- paste("13", dat$EncID.new, sep = "")
  dat$DIS.DT <- dat$X.DIS.DT.
  if(!"SRC"%in%names(dat)){
    dat$SRC <- dat$X.SRC..
  }
  if(!"SITE"%in%names(dat)){
    dat$SITE <- dat$X..SITE..........................
  }
  if(!"TEST"%in%names(dat)){
    dat$TEST <- dat$X..TEST.
  }
  dat$TEST <- trimws(dat$TEST)
  dat$SRC <- trimws(dat$SRC)
  dat$SITE <- trimws(dat$SITE)
  unknown <- paste(dat$TEST, dat$SRC, dat$SITE, sep = "")
  dat.unknown <- dat[unknown%in%tgh.paste]
  if(nrow(dat)!=0){
    fwrite(dat.unknown, paste("H:/GEMINI/Results/DRM/uhn.culture.unknown/TGH/", str_sub(files[i], 1, 20),
                            "unknown culture.csv", sep = ""))
    print(c(files[i], nrow(dat.unknown)))
  }
  n.unknown <- n.unknown + nrow(dat.unknown)
}
n.unknown

i = 1
dat[, cul:= paste(TEST, SRC, SITE, collapse = "")]
dat$cul
