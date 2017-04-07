# --------------------- to adm transfusion part --------------------------------
library(gemini)
lib.pa()
cohort <- fread("C:/Users/guoyi/Desktop/to.adm/cohort.csv", colClasses = list(character = "EncID.new"))
all.name <- fread("C:/Users/guoyi/Desktop/to.adm/all.name.csv")
cohort <- cohort[physician!="thp-m-708"]
cohort[Institution.Number=="sbk", Institution.Number:="SHSC"]
cohort[Institution.Number=="msh", Institution.Number:="SHS"]
cohort[, Institution.Number := toupper(Institution.Number)]

hgb <- readg(lab, hgb)
sod <- readg(lab, sodium)

hgb[is.na(as.numeric(Result.Value))]
sod[is.na(as.numeric(Result.Value))]

n.blood.test <- data.table(table(c(hgb$EncID.new, sod$EncID.new)))
names(n.blood.test) <- c("EncID.new", "n.bloodtest")
cohort <- merge(cohort, n.blood.test, by = "EncID.new", all.x = T)
cohort[is.na(n.bloodtest), n.bloodtest:=0]
ave.bloodtest <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = sum(x$n.bloodtest)/sum(x$LOS.without.ALC))
}
png("C:/Users/guoyi/Desktop/to.adm/figures/n.bloodtest_overall.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort[str_sub(EncID.new, 1, 2)%in%c("11","12","13", "14")], 100, "Overall", 
         ylab = "Number of CBC and Electrolyte Tests \n per Patient Day per Doctor", ave.fun = ave.bloodtest)
dev.off()


smh.bb <- readg(smh, bb)
sbk.bb <- readg(sbk, bb)
uhn.bb <- rbind(readg(uhn, txm_er),
                readg(uhn, txm_ip))
msh.bb <- readg(msh, bb)

table(smh.bb$Selected_product_code) # RCB
table(sbk.bb$Product.Group.Code) #RBC
table(uhn.bb$Blood_Component) #RBC
table(msh.bb$POPROD) # Red Blood Cells Concentrate

# fix several with time in hh:mm:ss
uhn.bb[nchar(Time_Component_Issued_from_Lab)==8, Time_Component_Issued_from_Lab:=str_sub(
  Time_Component_Issued_from_Lab, 1,5
)]
rbc.trans <- 
      rbind(smh.bb[Selected_product_code=="RCB", .(Trans.Dt = mdy_hm(UseDtTm),
                                             EncID.new)],
      sbk.bb[Product.Group.Code=="RBC", .(Trans.Dt = ymd_hms(paste(Issue.Date, Issue.Time)),
                                          EncID.new)],
      uhn.bb[Blood_Component == "RBC", .(Trans.Dt = mdy_hm(paste(Date_Component_Issued_from_Lab, 
                                                                 Time_Component_Issued_from_Lab)),
                                         EncID.new)],
      msh.bb[POPROD=="Red Blood Cells Concentrate", 
             .(Trans.Dt = ymd_hm(paste(DATE, TIME.new)),EncID.new)])
table(rbc.trans[,str_sub(EncID.new, 1, 2)])
hgb$Collection.DtTm <- ymd_hms(hgb$Collection.DtTm)
hgb <- hgb %>% arrange(EncID.new, Collection.DtTm) %>% data.table


hgb <- hgb[EncID.new%in%cohort$EncID.new]
rbc.trans <- rbc.trans[EncID.new%in%cohort$EncID.new]
with.pre.trans.hgb <- function(x){
  pre.hgb <- hgb[EncID.new%in%x$EncID.new]
  pre.hgb <- pre.hgb[Collection.DtTm>=(x$Trans.Dt- 36*3600)]
  pre.hgb <- pre.hgb[Collection.DtTm<=x$Trans.Dt]
  data.frame(with.pre.hgb = nrow(pre.hgb)>0,
             pre.hgb = ifelse(nrow(pre.hgb)>0, tail(pre.hgb$Result.Value, 1), NA))
}


rbc.trans[str_sub(EncID.new, 1, 2)=="14"]

res <- adply(rbc.trans[7310:7968], 1,  with.pre.trans.hgb)
res <- NULL
for(i in 1:nrow(rbc.trans)){
  res <- rbind(res,
               cbind(rbc.trans[i],
               with.pre.trans.hgb(rbc.trans[i])))
  print(i)
}

hgb <- hgb[str_sub(EncID.new, 1, 2)=="14"]
res <- NULL
for(i in 7310:7968){
  res <- rbind(res,
               cbind(rbc.trans[i],
                     with.pre.trans.hgb(rbc.trans[i])))
  print(i)
}
res
res[is.na(pre.hgb), with.pre.hgb:=FALSE]
fwrite(res, "H:/GEMINI/Results/to.administrator/rbc.trans.with.pre.hgb.csv")


rbc.trans <- fread("H:/GEMINI/Results/to.administrator/rbc.trans.with.pre.hgb.csv")
rbc.trans$Trans.Dt <- ymd_hms(rbc.trans$Trans.Dt)
rbc.trans <- rbind(rbc.trans, res)
fwrite(rbc.trans, "H:/GEMINI/Results/to.administrator/rbc.trans.with.pre.hgb.csv")



rbc.trans.80 <- rbc.trans[with.pre.hgb==T&pre.hgb>80]
n.with.pre.trans.80 <- rbc.trans.80[,.N, by = EncID.new]
n.with.pre.trans.80$EncID.new <- as.character(n.with.pre.trans.80$EncID.new)
names(n.with.pre.trans.80)[2] <- "N.pre.tran.hgb.gt80"
cohort <- merge(cohort, n.with.pre.trans.80, by = "EncID.new",
                all.x = T, all.y = F)
cohort[is.na(N.pre.tran.hgb.gt80), N.pre.tran.hgb.gt80 := 0]

num.pre.trans.hgb80 <- function(x){
  data.frame(N = nrow(x),
             site = x$Institution.Number[1],
             ave = sum(x$N.pre.tran.hgb.gt80)/nrow(x)*1000)
}
ddply(cohort, ~Institution.Number, 
      num.pre.trans.hgb80)

cohort[N.pre.tran.hgb.gt80>0]
png("C:/Users/guoyi/Desktop/to.adm/figures/number.of.rbc.trans.with.prehbg.gt80.png", res = 250, width = 2000, height = 1200)
plot.phy(cohort[str_sub(EncID.new, 1, 2)%in%c("11","12","13", "14")], 100, "Overall", 
         ylab = "Number of RBC Transfusions \n with pre-Transfusion Hgb > 80 \n per 1000 Patient per Doctor", 
         ave.fun = num.pre.trans.hgb80, xstart = -3)
dev.off()
