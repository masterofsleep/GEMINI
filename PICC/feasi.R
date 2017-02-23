# ------------------------ PICC Feasibility Check ------------------------------
rm(list = ls())
library(gemini)
lib.pa()

ip.int <- readg(gim, ip_int)
er.int <- readg(gim, er_int)
names(er.int)[3] <- "Intervention.Code"
interv <- rbind(ip.int[,.(EncID.new, Intervention.Code)], 
                er.int[,.(EncID.new, Intervention.Code)])
interv <- interv[str_sub(EncID.new, 1, 2)%in%c("11", "12", "13")]
picc.im <- interv[startsWith(Intervention.Code, "1IS53GRLF")]
picc.rm <- interv[startsWith(Intervention.Code, "1IS55GRKA")]

smh.rad <- readg(smh, rad)
sbk.rad <- readg(sbk.rad, rad.csv)
uhn.radip <- readg(uhn, rad_ip)
uhn.rader <- readg(uhn, rad_er)
uhn.rad <- rbind(uhn.radip, uhn.rader)

picc.names1 <- c("Angiography Line PICC CCM", "Angiography Line PICC Insertion",
                "PIC line insertion-1 lumen",
                "PIC line insertion-2 lumen", "PIC line insertion-3 lumen",
                "PIC line check", "PICC Line Single Lumen-Nursing Unit",
                "PICC Line Double Lumen-Nursing Unit", 
                "PICC INSERTION US GUIDED (Z456)",
                "PICC INSERT VENOGRAM", "PICC INSERT SINGLE LUMEN",
                "PICC INSERT DOUBLE LUMEN", "PICC EXCHANGE (Z456,Z457)", "PICC EXCH")
picc.names1%in%c(smh.rad$proc_desc_long, sbk.rad$Test.Name, uhn.rad$ProcedureName)



dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
dad <- dad[ymd(Discharge.Date) < ymd("2015-04-01")]
dad$Institution.Number[dad$Institution.Number=="54265"] <- "uhn-general"
dad$Institution.Number[dad$Institution.Number=="54266"] <- "uhn-western"
dad$Institution.Number[dad$Institution.Number=="M"] <- "thp-m"
dad$Institution.Number[dad$Institution.Number=="C"] <- "thp-c"


any.picc.enc <- c(
  smh.rad[proc_desc_long%in%picc.names1, EncID.new],
  sbk.rad[Test.Name%in%picc.names1, EncID.new],
  uhn.rad[ProcedureName%in%picc.names1, EncID.new]
)
multi.picc.enc <- any.picc.enc[duplicated(any.picc.enc)]

any.picc <- dad[EncID.new%in%any.picc.enc]
table(any.picc$Institution.Number)

multi.picc <- dad[EncID.new%in%multi.picc.enc]
table(multi.picc$Institution.Number)


picc.insert.names <- c("Angiography Line PICC Insertion",
                       "PIC line insertion-1 lumen",
                       "PIC line insertion-2 lumen", "PIC line insertion-3 lumen",
                       "PICC Line Single Lumen-Nursing Unit",
                       "PICC Line Double Lumen-Nursing Unit", 
                       "PICC INSERTION US GUIDED (Z456)",
                       "PICC INSERT VENOGRAM", "PICC INSERT SINGLE LUMEN",
                       "PICC INSERT DOUBLE LUMEN")


smh.insert <- smh.rad[proc_desc_long%in%picc.insert.names, 
                      .(picc.dt= ymd_h(proc_dtime), proc_desc_long, EncID.new)] %>%
  arrange(EncID.new, picc.dt) %>% 
  filter(!duplicated(EncID.new)) %>% data.table
names(smh.insert) <- c("picc.dt", "test.name", "EncID.new")

sbk.insert <- sbk.rad[Test.Name%in%picc.insert.names, 
                      .(picc.dt= ymd_hms(Performed.DtTm), Test.Name, EncID.new)] %>%
  arrange(EncID.new, picc.dt) %>% 
  filter(!duplicated(EncID.new)) %>% data.table
names(sbk.insert) <- c("picc.dt", "test.name", "EncID.new")

uhn.insert <- uhn.rad[ProcedureName%in%picc.insert.names, 
                      .(picc.dt= mdy_hm(ScanStartDateTime), ProcedureName, EncID.new)] %>%
  arrange(EncID.new, picc.dt) %>% 
  filter(!duplicated(EncID.new)) %>% data.table
names(uhn.insert) <- c("picc.dt", "test.name", "EncID.new")


picc.insert <- rbind(smh.insert, sbk.insert, uhn.insert)
dad$EncID.new <- as.character(dad$EncID.new)
picc.insert <- merge(picc.insert, 
                     dad[,.(EncID.new, Discharge.Date, Discharge.Time,
                            Institution.Number)], by = "EncID.new",
                     all.x = F, all.y = F)

picc.insert[, post.picc.time := 
              as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time))-
                           ymd_hms(picc.dt))/3600]
ddply(picc.insert, ~Institution.Number, summarise,
      g5 = sum(post.picc.time>24*5))
library(plyr)
ddply(picc.insert, ~Institution.Number, summarize,
      min = round(min(post.picc.time), 1),
      max = round(max(post.picc.time), 1),
      median = round(median(post.picc.time), 1),
      iqr = round(IQR(post.picc.time), 1))


picc.insert[post.picc.time<0]



picc.names.all <- c("Angiography Line PICC CCM", "Angiography Line PICC Insertion",
                    "Angiography Line PICC Removal",
                "PIC line insertion-1 lumen",
                "PIC line insertion-2 lumen", "PIC line insertion-3 lumen",
                "PIC line check", "PICC Line Single Lumen-Nursing Unit",
                "PICC Line Double Lumen-Nursing Unit", 
                "PICC INSERTION US GUIDED (Z456)",
                "PICC INSERT VENOGRAM", "PICC INSERT SINGLE LUMEN",
                "PICC INSERT DOUBLE LUMEN", "PICC EXCHANGE (Z456,Z457)", "PICC EXCH")
picc.names.all%in%c(smh.rad$proc_desc_long, sbk.rad$Test.Name, uhn.rad$ProcedureName)

any.picc.enc <- c(
  smh.rad[proc_desc_long%in%picc.names1, EncID.new],
  sbk.rad[Test.Name%in%picc.names1, EncID.new],
  uhn.rad[ProcedureName%in%picc.names1, EncID.new]
)
int.any.picc <- c(picc.im$EncID.new, picc.rm$EncID.new)



dad$picc.rad <- dad$EncID.new%in%any.picc.enc& !dad$EncID.new%in%picc.im$EncID.new
dad$picc.int <- dad$EncID.new%in%picc.im$EncID.new & !dad$EncID.new%in%any.picc.enc
dad$picc.both <- dad$EncID.new%in%any.picc.enc&dad$EncID.new%in%picc.im$EncID.new

library(plyr)
ddply(dad, ~Institution.Number, summarize, 
      picc.both = sum(picc.both),
      picc.rad = sum(picc.rad),
      picc.int = sum(picc.int))

ddply(dad, ~Institution.Number, summarize, 
      picc.int = sum(picc.int==TRUE&picc.rad==FALSE))
