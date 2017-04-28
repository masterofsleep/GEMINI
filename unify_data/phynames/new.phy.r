# -------------------- find all new physicians ----------------------------------
# ------------------------ 2017-04-07 ------------------------------------------
library(gemini)
lib.pa()
smh <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/smh.link.csv")
sbk <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/sbk.link.csv")
uhn <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/uhn.link.csv")
msh <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/msh.link.csv")
thp <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/thp.link.csv")
exclude <- readg(gim, notgim)
all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.csv")

# add the information for two encounters that are missing in the physician list
# encounters 13303103, 13177105
uhn.adm <- readg(uhn, adm)
uhn.dad <- readg(uhn, dad)
find.phy <- function(x){
  adm.code = uhn[EncID.new%in%uhn.adm[
    Admitting.Code==uhn.adm[EncID.new==x, Admitting.Code], EncID.new], adm.code][1]
  dis.code = uhn[EncID.new%in%uhn.adm[
    Discharging.Code==uhn.adm[EncID.new==x, Discharging.Code], EncID.new], dis.code][1]
  mrp.code = uhn[EncID.new%in%uhn.dad[
    MostResponsible.DocterCode==uhn.dad[EncID.new==x, 
                                                             MostResponsible.DocterCode], 
                         EncID.new], mrp.code][1]
  return(data.table(EncID.new = x,
                    adm.code,
                    dis.code,
                    mrp.code))
}

uhn <- rbind(uhn, 
             find.phy("13303103"),
             find.phy("13177105"))

find.new.code <- function(df, code.type.adm, code.type.mrp){
  df$adm.code = as.character(df$adm.code)
  df$dis.code = as.character(df$dis.code)
  df$mrp.code = as.character(df$mrp.code)
  df <- merge(df, all.name[code.type == code.type.adm, .(Code, code.new)], 
              by.x = "adm.code",by.y = "Code",
              all.x = T, all.y = F)
  df <- merge(df, all.name[code.type == code.type.adm, .(Code, code.new)], 
              by.x = "dis.code",by.y = "Code",
              all.x = T, all.y = F)
  df <- merge(df, all.name[code.type == code.type.mrp, .(Code, code.new, GIM)], 
              by.x = "mrp.code",by.y = "Code",
              all.x = T, all.y = F)
  names(df)[5:7] <- c("adm.code.new", "dis.code.new", "mrp.code.new")
  df
}
smh <- find.new.code(smh, "smh", "smh")
sbk <- find.new.code(sbk, "sbk", "sbk")
uhn <- find.new.code(uhn, "uhn.adm", "uhn.mrp")
msh <- find.new.code(msh, "msh", "msh")
thp <- find.new.code(thp, "thp", "thp")
sum(thp$mrp.code.new==thp$dis.code.new)
cohort <- rbind(smh, sbk, uhn, msh, thp, fill = T)[!EncID.new%in%exclude$EncID.new] %>% unique



dad <- fread("H:/GEMINI/DataBackup/Data170214/UHN/CIHI/uhn.ip_dad.nophi.csv")
extra.enc <- dad[mdy(Discharge.Date)>ymd("20150331"), EncID.new]
cohort <- cohort[!EncID.new%in%extra.enc]



fwrite(cohort, "H:/GEMINI/Data/GEMINI/gim.all.phy.csv")
