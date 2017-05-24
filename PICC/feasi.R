# ------------------------ PICC Feasibility Check ------------------------------
rm(list = ls())
library(gemini)
lib.pa()

ip.int <- readg(gim, ip_int)
er.int <- readg(gim, er_int)
names(er.int)[3] <- "Intervention.Code"
interv <- rbind(ip.int[,.(EncID.new, Intervention.Code)], 
                er.int[,.(EncID.new, Intervention.Code)])
interv <- interv[str_sub(EncID.new, 1, 2)%in%c("11", "12", "13", "14")]
picc.im <- interv[startsWith(Intervention.Code, "1IS53GRLF")]
picc.rm <- interv[startsWith(Intervention.Code, "1IS55GRKA")]

smh.rad <- readg(smh, rad)
sbk.rad <- readg(sbk.rad, rad.csv)
uhn.radip <- readg(uhn, rad_ip)
uhn.rader <- readg(uhn, rad_er)
uhn.rad <- rbind(uhn.radip, uhn.rader)
msh.rad <- rbind(readg(msh, rad_er),
                 readg(msh, rad_ip))
fwrite(msh.rad[,.N, by = ProcedureName], "H:/GEMINI/Results/DataSummary/clinical freq tables/rad.msh.csv")

picc.names1 <- c("Angiography Line PICC CCM", "Angiography Line PICC Insertion",
                "PIC line insertion-1 lumen",
                "PIC line insertion-2 lumen", "PIC line insertion-3 lumen",
                "PIC line check", "PICC Line Single Lumen-Nursing Unit",
                "PICC Line Double Lumen-Nursing Unit", 
                "PICC INSERTION US GUIDED (Z456)",
                "PICC INSERT VENOGRAM", "PICC INSERT SINGLE LUMEN",
                "PICC INSERT DOUBLE LUMEN", "PICC EXCHANGE (Z456,Z457)", "PICC EXCH")
picc.names1%in%c(smh.rad$proc_desc_long, sbk.rad$Test.Name, uhn.rad$ProcedureName)
sum(msh.rad$ProcedureName%in%picc.names1)


dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")


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

picc.insert[, post.picc.time := as.numeric(ymd_hm(paste(Discharge.Date, Discharge.Time))-
                           picc.dt)/3600]
sum(picc.insert$EncID.new%in%dad$EncID.new)

ddply(picc.insert, ~Institution.Number, summarise,
      g5 = sum(post.picc.time>24*5))
library(plyr)
ddply(picc.insert, ~Institution.Number, summarize,
      min = round(min(post.picc.time), 1),
      max = round(max(post.picc.time), 1),
      median = round(median(post.picc.time), 1),
      iqr = round(IQR(post.picc.time), 1))

# Number of physicians with at least ** PICC lines and discharge > 5 days
phy.all <- readg(gim, all.phy)
phy.all$EncID.new <- as.character(phy.all$EncID.new)
picc.insert <- merge(picc.insert, phy.all[,.(EncID.new, mrp.code.new)],
                     all.x = T, all.y = F)
picc.insert[, mrp.code.new := paste(str_sub(EncID.new, 1, 2), mrp.code.new, sep = "-")]
phy.picc.insert <- picc.insert[post.picc.time>24*5,.N, by = .(mrp.code.new, Institution.Number)]
table(phy.picc.insert[N>=20, Institution.Number])
table(phy.picc.insert[N>=10, Institution.Number])

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
picc.names.all%in%c(smh.rad$proc_desc_long, sbk.rad$Test.Name, uhn.rad$ProcedureName,
                    msh.rad$ProcedureName)

any.picc.enc <- c(
  smh.rad[proc_desc_long%in%picc.names1, EncID.new],
  sbk.rad[Test.Name%in%picc.names1, EncID.new],
  uhn.rad[ProcedureName%in%picc.names1, EncID.new]
)
int.any.picc <- c(picc.im$EncID.new, picc.rm$EncID.new)



dad$picc.rad <- dad$EncID.new%in%any.picc.enc& !dad$EncID.new%in%picc.im$EncID.new
dad$picc.int <- dad$EncID.new%in%picc.im$EncID.new & !dad$EncID.new%in%any.picc.enc
dad$picc.both <- dad$EncID.new%in%any.picc.enc&dad$EncID.new%in%picc.im$EncID.new

dad$picc <- ifelse(dad$picc.both, "picc.both", ifelse(dad$picc.int, "int.only",
                                                 ifelse(dad$picc.rad, "rad.only", "No picc")))
ggplot(dad[Institution.Number%in%c("UHN-TW", "UHN-TG")&picc!="No picc"], 
       aes(x = ymd(Admit.Date), fill = picc)) + geom_histogram(binwidth = 10)

ggplot(dad[Institution.Number%in%c("UHN-TW", "UHN-TG")&picc.int], 
       aes(x = ymd(Admit.Date))) + geom_histogram(binwidth = 10)

library(plyr)
ddply(dad, ~Institution.Number, summarize, 
      picc.both = sum(picc.both),
      picc.rad = sum(picc.rad),
      picc.int = sum(picc.int))

ddply(dad, ~Institution.Number, summarize, 
      picc.int = sum(picc.int==TRUE&picc.rad==FALSE))




# ------------------------- Feb 27 2017 ----------------------------------------
# --------------- MRN for records with PICC in rad or intervention only --------

dad[picc.rad == T&picc.int == F&Institution.Number=="smh", .(EncID.new, Institution.Number)] %>% 
  fwrite("H:/GEMINI/Results/PICC/smh.picc.rad.only.csv")
dad[picc.int ==T&picc.rad ==F&Institution.Number=="smh", .(EncID.new, Institution.Number)] %>% 
  fwrite("H:/GEMINI/Results/PICC/smh.picc.int.only.csv")

# ------------------------- 2017 05 24 -----------------------------------------
# sample result for unknown test names
uhn.rad <- rbind(readg(UHN, rad_ip),
                 readg(UHN, rad_er))
uhn_ccm <- uhn.rad[ProcedureName=="Angiography Line PICC CCM"]
fwrite(uhn_ccm, "H:/GEMINI/Results/PICC/check/uhn_angiography_line_picc_ccm.csv")

msh.rad <- rbind(readg(msh, rad_er),
                 readg(msh, rad_ip))
msh_insertion <- msh.rad[ProcedureName=="Angiography Body Line Insertion"]
fwrite(msh_insertion, "H:/GEMINI/Results/PICC/check/msh_angiography_body_line_insertion.csv")



# find frequency table for those in Intervention but not in Radiology files
picc.insert.names <- c("Angiography Line PICC CCM",
                       "Angiography Line PICC Insertion",
                       "PIC line insertion-1 lumen",
                       "PIC line insertion-2 lumen",
                       "PIC line insertion-3 lumen",
                       "PICC Line Single Lumen-Nursing Unit",
                       "PICC Line Double Lumen-Nursing Unit",
                       "PICC INSERTION US GUIDED",
                       "PICC INSERT VENOGRAM",
                       "PICC INSERT SINGLE LUMEN",
                       "PICC INSERT DOUBLE LUMEN",
                       "Angiography Peripheral Line Insertion",
                       "Angiography Body Line Insertion")
length(unique(ip.int$EncID.new))
