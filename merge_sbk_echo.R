################################################################################
####                  merge the echo data of SKB                       #########
################################################################################

rm(list=ls())
library(gemini)
lib.pa()

swdr("SBK/Echo")
list.files()

er1 <- fread("echo_er(1).csv", na.strings = c("", NA, NULL))
er2 <- fread("echo_er(2).csv", na.strings = c("", NA, NULL))
ip <- fread("echo_ip.csv", na.strings = c("", NA, NULL))
merged <- rbind(er1, er2, ip)
sum(duplicated(merged))

write.csv(merged, "sbk.echo.csv", row.names = F)

int <- fread("sbk.echo_int.csv", na.strings = c("", NA, NULL))


length(unique(int$SID))
length(unique(merged$SID))

length(intersect(merged$SID, int$SID))

merged[!merged$SID%in%int$SID, ]
