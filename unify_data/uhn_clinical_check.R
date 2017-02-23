# --------------------- UHN Clinical Data Check --------------------------------
# --------------------- Feb 03 2017---------------------------------------------

library(gemini)
lib.pa()
dad <- readg(uhn, dad)
echo <- readg(uhn, echo)
# transfusion medicine
transip <- readg(uhn, txm_ip)
transer <- readg(uhn, txm_er)
trans <- rbind(transip, transer)
dad$trans <- dad$EncID.new%in%trans$EncID.new
ggplot(dad, aes(ymd(Discharge.Date), fill = trans)) + 
  geom_histogram(binwidth = 10) + ggtitle("transfusion medicine")

#echo
echo <- readg(uhn, echo)
dad$echo <- dad$EncID.new%in%echo$EncID.new
ggplot(dad, aes(ymd(Discharge.Date), fill = echo)) + 
  geom_histogram(binwidth = 10) + ggtitle("echo")

#rad
radip <- readg(uhn, rad_ip)
rader <- readg(uhn, rad_er)
rad <- rbind(radip, rader)
dad$rad <- dad$EncID.new%in%rad$EncID.new
ggplot(dad, aes(ymd(Discharge.Date), fill = rad)) +
  geom_histogram(binwidth = 10) + ggtitle("rad")
