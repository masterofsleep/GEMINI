library(gemini)
lib.pa()

swdr("SBK/CIHI/backup")
int1 <- fread("sbk.ip_int.nophi.csv", na.string = c("", NULL, NA, " "))
int2 <- fread("sbk.ip_int2.nophi.csv", na.string = c("", NULL, NA, " "))

int1 <- arrange(int1, EncID.new)
int2 <- arrange(int2, EncID.new)

sum(int1==int2, na.rm = T)

apply(int1, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(int2, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(me, MARGIN = 2, FUN = function(x)sum(is.na(x)))
int1 <- int1[!(is.na(int1$EncID.new)&is.na(int1$IntervOccurrence)),]
int2 <- int2[!(is.na(int2$EncID.new)&is.na(int2$IntervOccurrence)),]
me <- me[!(is.na(me$EncID.new)&is.na(me$Intervention.Occurrence)),]
length(unique(int1$EncID.new))
length(unique(int2$EncID.new))

length(unique(intersect(int1$EncID.new, int2$EncID.new)))



me <- readg(sbk, ip_int)
sum(duplicated(me))
