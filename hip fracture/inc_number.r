# ----------------------------- Hip Fracture -----------------------------------
library(gemini)
lib.pa()

dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")
ip.diag <- readg(gim, ip_diag)[EncID.new%in%dad$EncID.new]
ip.int <- readg(gim, ip_int)[EncID.new%in%dad$EncID.new]
inca_code <- c("S720", "S721", "S722")
incc_code <- c("1VA74", "1VA53", "1VC74", "1SQ53")
sum(dad$EncID.new%in%ip.diag$EncID.new)
sum(dad$EncID.new%in%ip.int$EncID.new)

inc_a <- ip.diag[Diagnosis.Type=="M"&startwith.any(Diagnosis.Code, inca_code)]
inc_b <- ip.diag[Diagnosis.Type%in%c("1", "W", "X", "Y")&startwith.any(Diagnosis.Code, inca_code)]
inc_c <- ip.int[startwith.any(Intervention.Code, incc_code), 
                .(EncID.new, Intervention.Code = str_sub(Intervention.Code, 1, 5))] %>% unique

ndup <- function(x){
  sum(duplicated(x))
}
ndup(inc_a$EncID.new)
ndup(inc_b$EncID.new)
ndup(inc_c$EncID.new)

inc_b[duplicated(EncID.new)|duplicated(EncID.new, fromLast = T)] -> dupb
inc_c[duplicated(EncID.new)|duplicated(EncID.new, fromLast = T)] -> dupc

inca_sum <- inc_a[, .N, by = .(str_sub(EncID.new, 1, 2),str_sub(Diagnosis.Code, 1, 4))]
names(inca_sum)[c(1,2)] <- c("site", "Diagnosis.Code")
inca_sum[, ':='(Inclusion.Criteria = "A",
                Diagnosis.Type = "M")]

incb_sum <- inc_b[!EncID.new%in%dupb$EncID.new, 
                  .N, by = .(str_sub(EncID.new, 1, 2),str_sub(Diagnosis.Code, 1, 4),
                               Diagnosis.Type)]
names(incb_sum)[c(1,2)] <- c("site", "Diagnosis.Code")
incb_sum[, ':='(Inclusion.Criteria = "B")]

change_site <- function(df){
  df$site[df$site=="11"] <- "SMH"
  df$site[df$site=="12"] <- "SBK"
  df$site[df$site=="13"] <- "UHN"
  df$site[df$site=="14"] <- "MSH"
  df$site[df$site=="15"] <- "THP"
  return(df)
}
change_site(inca_sum)
change_site(incb_sum)

incab_sum <- rbind(change_site(inca_sum), change_site(incb_sum)) %>% select(Inclusion.Criteria, site, Diagnosis.Type, Diagnosis.Code, N)
incab_sum <- rbind(incab_sum,
                   data.frame(Inclusion.Criteria= "B",
                              site= c("SBK", "THP"),
                              Diagnosis.Type = c("1, W"),
                              Diagnosis.Code = c("S721"),
                              N = 1))

fwrite(incab_sum, "H:/GEMINI/Results/Hip fracture/inc_ab.csv")


incc_sum <- inc_c[!EncID.new%in%dupc, .N, by = .(str_sub(EncID.new, 1, 2),str_sub(Intervention.Code, 1, 5))]
names(incc_sum)[c(1,2)] <- c("site", "Intervention.Code")
incc_sum[, ':='(Inclusion.Criteria = "c")]

incc_sum <- rbind(change_site(incc_sum),
                  data.frame(Inclusion.Criteria = "C",
                             site = c("UHN", "MSH", "THP"),
                             Intervention.Code = c("1VA74, 1VC74",
                                                   "1VA53, 1VC74",
                                                   "1VA53, 1VC74"),
                             N = c(1,2,1))) %>%
  select(Inclusion.Criteria, site, Intervention.Code, N)


fwrite(incc_sum, "H:/GEMINI/Results/Hip fracture/inc_c.csv")

n_by_site <- function(x){
  num <- str_sub(unique(x), 1, 2)
  num[num=="11"] <- "SMH"
  num[num=="12"] <- "SBK"
  num[num=="13"] <- "UHN"
  num[num=="14"] <- "MSH"
  num[num=="15"] <- "THP"
  return(table(num))
}

n_by_site(inc_a$EncID.new)
n_by_site(inc_b$EncID.new)
n_by_site(inc_c$EncID.new)
