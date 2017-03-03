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
sbk.echo <- readg(sbk, echo)

intersect(smh.echo$EncID.new, smh.cci.echo) %>% length
intersect(sbk.echo$EncID.new, sbk.cci.echo) %>% length
setdiff(sbk.echo$EncID.new, sbk.cci.echo) %>% unique %>% length

compare.sets(smh.cci.echo, smh.echo$EncID.new)
compare.sets(sbk.cci.echo, sbk.echo$EncID.new)

