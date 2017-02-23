setwd("R:/GEMINI/_RESTORE/UHN/Pharmacy")
library(gemini)
lib.pa()
rm(list=ls())
new.phar <- fread("uhn.phar.nophi.csv", na.strings = c(NA, NULL, " "),
                  strip.white = T)
phar.ip <- fread("uhn.ip_pharm.nophi.csv", na.strings = c(NA, NULL, " "),
                 strip.white = T)
phar.er <- fread("uhn.ed_pharm.nophi.csv", na.strings = c(NA, NULL, " "),
                 strip.white = T)


old.with.phar <- union(phar.er$EncID.new, phar.ip$EncID.new)
missing.in.new <- old.with.phar[!old.with.phar%in%new.phar$EncID.new]

write.csv(missing.in.new, "H:/GEMINI/Results/NoLabMedRad/uhn.phar.missing.csv", 
          row.names = F)

length(unique(new.phar$EncID.new))
length(unique(phar.ip$EncID.new))
length(unique(phar.er$EncID.new))

names(new.phar)
names(phar.ip)

head(new.phar)

sum(unique(new.phar$EncID.new)%in% unique(phar.ip$EncID.new))


#check whether there are any strange obs in enc ids that is caused by free text
head(sort(new.phar$EncID.new, decreasing = T))
sum(is.na(new.phar$EncID.new))
sum(is.na(phar.ip$EncID.new))
sum(as.numeric(new.phar$Order_No)%in% phar.ip$Order_No)
sum(is.na(new.phar$Order_No))
sum(is.na(phar.ip$Order_No))
sum(is.na(new.phar$Frequency))


dad <- readg(uhn, dad)
dad$phar_ip <- dad$EncID.new%in% phar.ip$EncID.new
dad$new.phar <- dad$EncID.new %in% new.phar$EncID.new

ggplot(dad, aes(x = ymd(Discharge.Date), fill = phar_ip)) + 
  geom_histogram(alpha = 0.5, binwidth = 20) +
  ggtitle("UHN: IP_Pharmacy vs Discharge Date")

ggplot(dad, aes(x = ymd(Discharge.Date), fill = new.phar)) + 
  geom_histogram(alpha = 0.5, binwidth = 20)+
  ggtitle("UHN: New Pharmacy vs Discharge Date")



sort(table(phar.ip$EncID.new), decreasing = T)[2]

try <- new.phar[as.numeric(Order_No) == 14768469]
try2 <- phar.ip[as.numeric(Order_No) == 14768469]
length(unique(new.phar$Order_No)%in% unique(phar.ip$Order_No))

sort(table(new.phar$Order_No), decreasing = T)[1]


