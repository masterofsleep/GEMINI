# ---------------------------- Cluster Study -----------------------------------
# -------------------------------- Zahra ---------------------------------------
library(gemini)
lib.pa()
phy <- readg(gim, all.phy)
cohort <- phy[adm.GIM%in%c("y", "GP-GIM")|dis.GIM%in%c("y", "GP-GIM")]
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")

cluster.cohort <- dad[EncID.new%in%cohort$EncID.new&
                        Hash!=""]
find_g1 <- function(){
  return(intersect(cluster.cohort[, .N, by = Hash][N>=3, Hash],
                   cluster.cohort[, .(total.LoS = sum(Acute.LoS)), 
                                  by = Hash][total.LoS>=30, Hash]))
}

g1 <- find_g1()

find_g2 <- function(){
  cohort.after2011 <- cluster.cohort[ymd(Admit.Date)>=ymd("2011-04-01")]
  cohort.before2011 <- cluster.cohort[ymd(Admit.Date)<ymd("2011-04-01")]
  g2 <- intersect(cohort.after2011[, .N, by = Hash][N>=3, Hash],
            cohort.after2011[, .(total.LoS = sum(Acute.LoS)), 
                           by = Hash][total.LoS>=30, Hash])
  return(g2[!g2%in%cohort.before2011$Hash])
}
g2 <- find_g2()

find_g3 <- function(){
  cohort.after2011 <- cluster.cohort[ymd(Admit.Date)>=ymd("2012-04-01")]
  cohort.before2011 <- cluster.cohort[ymd(Admit.Date)<ymd("2012-04-01")]
  g3 <- intersect(cohort.after2011[, .N, by = Hash][N>=3, Hash],
                  cohort.after2011[, .(total.LoS = sum(Acute.LoS)), 
                                   by = Hash][total.LoS>=30, Hash])
  return(g3[!g3%in%cohort.before2011$Hash])
}

g3 <- find_g3()

cluster.cohort <- cluster.cohort[Hash%in%c(g1, g2, g3)]
cluster.cohort <- cluster.cohort%>% arrange(Hash, ymd(Admit.Date)) %>% data.table

time.to.next.visit <- as.numeric(ymd_hm(cluster.cohort[2:nrow(cluster.cohort), 
                                            paste(Admit.Date, Admit.Time)]) - 
  ymd_hm(cluster.cohort[1:(nrow(cluster.cohort)-1), 
                        paste(Discharge.Date, Discharge.Time)]) )/3600/24

cluster.cohort$time.to.next.visit <- c(time.to.next.visit, NA)
cluster.cohort[!duplicated(Hash, fromLast = T), time.to.next.visit:=NA]
check <- cluster.cohort[, .(Hash, time.to.next.admission, time.to.next.visit)]

check[is.na(time.to.next.admission)&!is.na(time.to.next.visit)]
check[!is.na(time.to.next.admission)&
        is.na(time.to.next.visit)]



find_summary <- function(data){
  uni_patient <- data[!duplicated(Hash)]
  data.frame(sample_size = nrow(uni_patient),
             )
  
}