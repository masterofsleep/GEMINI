#============================ Echo =============================================
#------------------  available for SMH, SBK, UHN  ------------------------------
library(gemini)
lib.pa()
rm(list = ls())
smh <- readg(smh, echo)
sbk <- readg(sbk, echo)
uhn <- readg(uhn, echo)

names(smh)
names(sbk)
names(uhn)

sum(duplicated(smh))
sum(duplicated(sbk))
sum(duplicated(uhn))
uhn <- uhn[!duplicated(uhn)]
write.csv(uhn, "H:/GEMINI/Data/UHN/Echo/uhn.echo.csv", row.names = F, na = "")
apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))




# ---------------------- validate cihi vs echo ---------------------------------
# ----------------------     march 03 2017     ---------------------------------
ip.int <- readg(gim, ip_int)
er.int <- readg(gim, er_int)

cci.echo <- c(ip.int[startwith.any(Intervention.Code, c("3IP30", "3HZ30")), EncID.new],
              er.int[startwith.any(Occurrence.Type, c("3IP30", "3HZ30")), EncID.new])
smh.cci.echo <- cci.echo[startsWith(cci.echo, "11")] %>% unique
sbk.cci.echo <- cci.echo[startsWith(cci.echo, "12")] %>% unique

smh.echo <- readg(smh, echo)
smh.echo$EncID.new <- as.character(smh.echo$EncID.new)
sbk.echo <- readg(sbk, echo)

intersect(smh.echo$EncID.new, smh.cci.echo) %>% length
intersect(sbk.echo$EncID.new, sbk.cci.echo) %>% length
setdiff(sbk.echo$EncID.new, sbk.cci.echo) %>% unique %>% length

compare.sets(smh.cci.echo, smh.echo$EncID.new)
compare.sets(sbk.cci.echo, sbk.echo$EncID.new)

# --------------------- march 06 find other possible echo cci ------------------
cci.names <- fread("H:/GEMINI/Coding/CCI_Code_Eng_Desc_2014_V1_0.csv")
er.int$Intervention.Code <- er.int$Occurrence.Type
int.all <- rbind(er.int[,.(Intervention.Code, EncID.new)],
                 ip.int[,.(Intervention.Code, EncID.new)])
smh.int.freq <- int.all[EncID.new%in%smh.echo$EncID.new, .N, by = Intervention.Code] %>%
  arrange(desc(N)) %>% data.table
smh.int.freq[!Intervention.Code%in%cci.names$code, ]
smh.int.freq <- merge(smh.int.freq, cci.names,
                      by.x = "Intervention.Code", by.y = "code",
                      all.x = T, sort = F)

sbk.int.freq <- int.all[EncID.new%in%sbk.echo$EncID.new, .N, by = Intervention.Code] %>%
  arrange(desc(N)) %>% data.table
sbk.int.freq[!Intervention.Code%in%cci.names$code, ]
sbk.int.freq <- merge(sbk.int.freq, cci.names,
                      by.x = "Intervention.Code", by.y = "code",
                      all.x = T, sort = F)


fwrite(smh.int.freq, "H:/GEMINI/Results/Check/echo/smh.echo.cci.freq.csv")
fwrite(sbk.int.freq, "H:/GEMINI/Results/Check/echo/sbk.echo.cci.freq.csv")




# --------------- march 8 2017 new uhn echo data -------------------------------
uhn.echo <- fread("R:/GEMINI/_RESTORE/UHN/Echo/uhn.echo.version2.csv")
uhn.echo$EncID.new <- paste("13", uhn.echo$EncID.new, sep = "")
uhn.echo[,.N, by = Procedure] %>% fwrite("H:/GEMINI/Results/Check/uhn.echo.freq.csv")
fwrite(uhn.echo, "H:/GEMINI/Data/UHN/Echo/uhn.echo.csv")

uhn.echo.old <- fread("R:/GEMINI/_RESTORE/UHN/Echo/old/uhn.echo.csv") %>% unique
uhn.echo.old$EncID.new <- paste("13", uhn.echo.old$EncID.new, sep = "")
setdiff(uhn.echo.old$EncID.new, uhn.echo$EncID.new)

table(uhn.echo.old$Test_Name)
sum(duplicated(uhn.echo))

dad <- readg(uhn, dad)

dat <- str_split(uhn.echo$Discharge_Date_Time, " ") %>% unlist %>% matrix(ncol = 2, byrow = T)
uhn.echo$discharge.date <- dat

ggplot(uhn.echo[Procedure=="2D Echocardiogram"], 
       aes(mdy(discharge.date))) + 
  geom_histogram(binwidth = 5) + ggtitle("2D Echocardiogram")

ggplot(uhn.echo[Procedure=="TEE Echocardiogram"], 
       aes(mdy(discharge.date))) + 
  geom_histogram(binwidth = 10) + ggtitle("TEE Echocardiogram")
