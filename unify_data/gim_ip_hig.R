library(gemini)
lib.pa()
#===================GIM_IP_HIG ========================================
#------------available for SMH, SBK, UHN, THP, MSh -----------------------------
rm(list = ls())
smh <- readg(smh, ip_hig)
sbk <- readg(sbk, ip_hig)
uhn <- readg(uhn, ip_hig)
msh <- readg(msh, ip_hig)
thp <- readg(thp, ip_hig)

names(smh)
names(sbk)
names(uhn)
names(msh)
names(thp)

hig.names <- str_replace_all(names(smh), "_", ".")
hig.names[12:13] <- c("FI.Invasive.Ventilation.GE96.Flag",
                      "FI.Invasive.Ventilation.LT96.Flag")
hig.names
names(smh) <- hig.names
names(sbk) <- hig.names
names(uhn) <- hig.names[c(1:14,16:22)]
names(msh)[c(1:13, 16:24)] <- hig.names
names(msh)[14:15] <- c("FI.Mechanical.Ventilation.GE96.Flag",
                       "FI.Mechanical.Ventilation.LT96.Flag")
names(thp) <- hig.names



write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.ip_hig.nophi.csv",
          row.names = F, na = "")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_hig.nophi.csv",
          row.names = F, na = "")
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.ip_hig.nophi.csv",
          row.names = F, na = "")
write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.ip_hig.nophi.csv",
          row.names = F, na = "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.ip_hig.nophi.csv",
          row.names = F, na = "")
names(smh)
names(sbk)
names(uhn)
names(msh)
names(thp)


apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(thp, MARGIN = 2, FUN = function(x)sum(is.na(x)))

smh.check <- smh[is.na(Methodology.Year)]
sbk <- sbk[!is.na(EncID.new)]
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.ip_hig.nophi.csv",
          row.names = F, na = "")

# -------------------------- Check HIG 15 --------------------------------------
smh <- readg(smh, ip_hig)
sbk <- readg(sbk, ip_hig)
uhn <- readg(uhn, ip_hig)
msh <- readg(msh, ip_hig)
thp <- readg(thp, ip_hig)
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")

hig <- rbind(smh[,.(EncID.new, HIG.Weight.15)],
             sbk[,.(EncID.new, HIG.Weight.15)],
             uhn[,.(EncID.new, HIG.Weight.15)],
             msh[,.(EncID.new, HIG.Weight.15)],
             thp[,.(EncID.new, HIG.Weight.15)])[EncID.new%in%dad$EncID.new]

dad[!EncID.new%in%hig$EncID.new] -> check
sum(is.na(hig$HIG.Weight.15))

ggplot(dad[!EncID.new%in%hig$EncID.new],
       aes(x = ymd(Discharge.Date))) + geom_histogram(binwidth = 5)

compare.sets(dad[is.na(RIW.15), EncID.new], hig[is.na(HIG.Weight.15), EncID.new])


