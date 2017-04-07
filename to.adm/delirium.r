# -------------------- to adm delirium -----------------------------------------
library(gemini)
lib.pa()

## find the 1000 encounters
# setwd("R:/GEMINI-DREAM/DELIRIUM Charts")
# files <- list.files();files
# enc <- NULL
# for(i in c(1:4, 10)){
#   dat <- fread(files[i])
#   enc <- c(enc, dat$EncID.new)
# }
# length(unique(enc))

# only 905 charts available, so only do for those 905 patients

# chart review results
setwd("R:/GEMINI/DELIRIUM")
files <- list.files();files
dat <- NULL
for(i in 2:9){
  dat <- rbind(dat, readxl::read_excel(files[i])[,c(2,13)])
}
ovl <- readxl::read_excel("overlapping results.xls", skip = 1)
ovl$subject_id <- as.integer(ovl$subject_id)

chart.res <- rbind(ovl, dat) %>% data.table
names(chart.res)[1] <- "EncID.new"

chart.res <- chart.res[!duplicated(EncID.new)]
chart.res[del_present=="Yes", del_present := "1"]
chart.res[del_present=="No", del_present := "2"]
chart.res[del_present=="Uncertain", del_present := "3"]
chart.res[, EncID.new := paste("11", EncID.new, sep = "")]

# by ICD10 
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad <- dad[EncID.new%in%chart.res$EncID.new]

ip.diag <- readg(gim, ip_diag)
icd.del <- ip.diag[startsWith(Diagnosis.Code, "F05"), EncID.new]
# by CAM
#vit <- readg(smh, vitals)
#head(vit)
#vit.finding.freq <- data.table(table(vit$Finding.Name))
#cam <- vit[Finding.Name=="CAM Results"]

#cam <- cam[EncID.new%in%chart.res$EncID.new]


dad[, ':='(del.icd = EncID.new%in%icd.del,
           #del.cam = EncID.new%in%cam[Value=="Positive", EncID.new],
           del.chart = EncID.new%in%chart.res[del_present=="1", EncID.new])]
dad[EncID.new%in%chart.res[del_present=="3", EncID.new], 
    del.chart := NA]

table(dad[,.(del.icd, del.chart)])
round1 <- function(x){
  sprintf("%.1f", round(x, 1))
}

round0 <- function(x){
  sprintf("%d", round(x, 0))
}


me.iqr <- function(x){
  paste(round1(median(x, na.rm = T)), " (", round1(quantile(x, na.rm = T)[2]), ", ",
        round1(quantile(x, na.rm = T)[4]), ")", sep = "")
}

me.iqr0 <- function(x){
  paste(round0(median(x)), " (", round0(quantile(x)[2]), ", ",
        round0(quantile(x)[4]), ")", sep = "")
}

cat.prop <- function(x){
  cbind(table(x), 
        round1(table(x)/length(x)*100))
}

cat1.prop <- function(x, cat){
  paste(sum(x==cat, na.rm = T), " (", 
        round1(sum(x==cat, na.rm = T)/length(!is.na(x))*100), ")", sep = "")
}

del.sum <- function(x){
  data.table(
    prev = paste(nrow(x), " (", round1(nrow(x)/905*100), ")", sep = ""),
    ICU = cat1.prop(x$SCU.adm, TRUE),
    Acute.LOS = me.iqr(x$LOS.without.ALC),
    Total.LOS = me.iqr(x$LoS),
    Death = cat1.prop(x$Discharge.Disposition, "7"),
    read.in.30 = cat1.prop(x$read.in.30, TRUE),
    Cost = paste(round1(mean(x$Cost, na.rm = T)), " (", round1(sd(x$Cost, na.rm = T)),
                 ")\n", me.iqr(x$Cost), sep = "")
  )
}
rbind(del.sum(dad[del.icd==T]),
      #del.sum(dad[del.cam==T]),
      del.sum(dad[del.chart==T]),
      del.sum(dad[del.chart==T&del.icd==F]),
      del.sum(dad[del.chart==F])
) %>% t %>% data.frame %>% fwrite("H:/GEMINI/Results/to.administrator/delirium.csv")

dad[del.chart==F] -> check
x <- check$read.in.30
sum(x, na.rm = T)/length(!is.na(x))*100
