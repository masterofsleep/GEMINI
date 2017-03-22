# ==============================================================================
# =====================  msh physician validate  ===============================
# =======================    March 1 2017   ====================================
library(gemini)
lib.pa()
rm(list = ls())

setwd("R:/GEMINI/_RESTORE/MSH/Physician Names")
files <- list.files()
files
gim.names <- fread(files[5], select = c("Last", "First"))
adm <- fread(files[1])
adm[!(paste(ADMITTING_PHYS_FIRST_NAME, ADMITTING_PHYS_LAST_NAME)%in%
      paste(gim.names$First, gim.names$Last)|
      paste(DISCHARGE_ATTENDING_FIRST, DISCHARGE_ATTENDING_LAST)%in%
      paste(gim.names$First, gim.names$Last))] %>% dim
adm.only <- readxl::read_excel(files[2]) %>% data.table
adm.only[!(paste(ADMITTING_PHYS_FIRST_NAME, ADMITTING_PHYS_LAST_NAME)%in%
                 paste(gim.names$First, gim.names$Last)|
                 paste(DISCHARGE_ATTENDING_FIRST, DISCHARGE_ATTENDING_LAST)%in%
                 paste(gim.names$First, gim.names$Last))] %>% dim
dad <- fread(files[3])
dad.only <- readxl::read_excel(files[4]) %>% data.table
dad[!(tolower(paste(Most.Responsible.Physician.First.Name, Most.Responsible.Physician.Last.Name))%in%
      tolower(paste(gim.names$First, gim.names$Last)))] %>% dim
dad.only[!(tolower(paste(Most.Responsible.Physician.First.Name, Most.Responsible.Physician.Last.Name))%in%
           tolower(paste(gim.names$First, gim.names$Last)))]
adm.phy <- adm[, .(ADMITTING_PHYS_FIRST_NAME, ADMITTING_PHYS_LAST_NAME, 
                   paste(ADMITTING_PHYS_FIRST_NAME, ADMITTING_PHYS_LAST_NAME))]

adm.only.adm <- adm.only[, .(ADMITTING_PHYS_FIRST_NAME, ADMITTING_PHYS_LAST_NAME, 
                             paste(ADMITTING_PHYS_FIRST_NAME, ADMITTING_PHYS_LAST_NAME))]
adm.only.dis <- adm.only[, .(DISCHARGE_ATTENDING_FIRST, DISCHARGE_ATTENDING_LAST,
                     paste(DISCHARGE_ATTENDING_FIRST, DISCHARGE_ATTENDING_LAST))]
dis.phy <- adm[, .(DISCHARGE_ATTENDING_FIRST, DISCHARGE_ATTENDING_LAST,
                   paste(DISCHARGE_ATTENDING_FIRST, DISCHARGE_ATTENDING_LAST))]
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

mrp <- dad[, .(Most.Responsible.Physician.First.Name, Most.Responsible.Physician.Last.Name,
               sapply(tolower(paste(Most.Responsible.Physician.First.Name, Most.Responsible.Physician.Last.Name)), 
                     simpleCap))]
dad.only.mrp <- dad.only[, .(Most.Responsible.Physician.First.Name, Most.Responsible.Physician.Last.Name,
                             sapply(tolower(paste(Most.Responsible.Physician.First.Name, Most.Responsible.Physician.Last.Name)), 
                                    simpleCap))]
msh.phy.name <- c(adm.phy, dis.phy, mrp) %>% unique
msh.phy.name[!msh.phy.name%in%paste(gim.names$First, gim.names$Last)] %>%
  write.csv("H:/GEMINI/Results/DataSummary/physician_names/msh.physician.names.csv")

gim.names <- fread(files[5], select = c("Last", "First"))


# ------------------------------- march 27 -------------------------------------
msh.phy.marked <- fread("H:/GEMINI/Coding/physician names/msh physician names_JK.csv")[!is.na(V1)]
msh.phy.marked <- msh.phy.marked[order(V1)]
msh.phy.marked$physician.name[214] <- paste(" ", msh.phy.marked$physician.name[214], sep = "")
msh.phy.marked <- rbind(msh.phy.marked, 
                        data.table(physician.name = paste(gim.names$First, gim.names$Last),
                                   "GIM?" = "y"), fill = T)

names(adm.phy)[1:2] <- c("First", "Last")
names(dis.phy)[1:2] <- c("First", "Last")
names(mrp)[1:2] <- c("First", "Last")

msh.all.phy <- rbind(adm.phy, dis.phy, mrp) %>% unique
msh.all.phy$V3%in%msh.phy.marked$physician.name
msh.all.phy[!V3%in%msh.phy.marked$physician.name]
msh.all.phy <- merge(msh.all.phy, msh.phy.marked[,.(physician.name, `GIM?`)],
                     by.x = "V3", by.y = "physician.name", all.x = T, all.y = F)
names(msh.all.phy)[1] <- "full name"
msh.all.phy$`GIM?`[1] <- "don't know"

adm.phy <- merge(adm.phy, msh.phy.marked[,.(physician.name, `GIM?`)],
                 by.x = "V3", by.y = "physician.name", all.x = T, all.y = F)
table(adm.phy$`GIM?`)
dis.phy <- merge(dis.phy, msh.phy.marked[,.(physician.name, `GIM?`)],
                 by.x = "V3", by.y = "physician.name", all.x = T, all.y = F)
table(dis.phy$`GIM?`)
table(adm.phy$`GIM?`, dis.phy$`GIM?`)
mrp <- merge(mrp, msh.phy.marked[,.(physician.name, `GIM?`)],
             by.x = "V3", by.y = "physician.name", all.x = T, all.y = F)
table(mrp$`GIM?`)


# check those adm only ones
adm.only.adm <- merge(adm.only.adm, msh.phy.marked[,.(physician.name, `GIM?`)],
                      by.x = "V3", by.y = "physician.name", all.x = T, all.y = F)
adm.only.dis <- merge(adm.only.dis, msh.phy.marked[,.(physician.name, `GIM?`)],
                      by.x = "V3", by.y = "physician.name", all.x = T, all.y = F)
table(adm.only.adm$`GIM?`, adm.only.dis$`GIM?`, useNA = "ifany")
dad.only.mrp$V3 <- trimws(dad.only.mrp$V3)
dad.only.mrp <- merge(dad.only.mrp, msh.all.phy[,.(`full name`, `GIM?`)],
                      by.x = "V3", by.y = "full name", all.x = T, all.y = F)
table(dad.only.mrp$`GIM?`)


fwrite(msh.all.phy[order(Last)], "H:/GEMINI/Results/DataSummary/physician_names/msh.phy.names.marked.csv")






# --------------------------- list of unknown/not gim --------------------------
adm.full <- fread("R:/GEMINI/_RESTORE/MSH/Physician Names/adm.full.csv")
dad.full <- fread("R:/GEMINI/_RESTORE/MSH/Physician Names/dad.full.csv")
overlap <- adm.full[EncID.new%in%dad.full$EncID.new, -"V1"]

msh.phy.marked <- fread("C:/Users/guoyi/Desktop/marked_names/names.code.link.csv")
msh.phy.marked <- msh.phy.marked[!duplicated(msh.phy.marked$code)]
overlap <- merge(overlap, msh.phy.marked[,.(adm.phy.name = full.name, `GIM.`, code)],
                  by.x = "adm.code", by.y = "code", all.x = T, all.y = F)
overlap <- merge(overlap, msh.phy.marked[,.(dis.phy.name = full.name, `GIM.`, code)],
                 by.x = "dis.code", by.y = "code", all.x = T, all.y = F)
table(overlap$GIM..x, useNA = "ifany")
table(overlap$GIM..y, useNA = "ifany")
overlap$GIM..y[overlap$GIM..y=="maybe"] <- "don't know"

overlap[, GIM := ifelse(GIM..x=="y"|GIM..y=="y", "y", 
                        ifelse(GIM..x=="n"&GIM..y=="n", "n", "u"))]
overlap[GIM=="u"]
table(overlap$GIM)
names(overlap)[c(5, 7)] <- c("adm.phy.gim", "dis.phy.gim")
fwrite(overlap[GIM=="u", .(EncID.new, adm.phy.name, adm.phy.gim, dis.phy.name, dis.phy.gim,
                           GIM)],
       "R:/GEMINI/_RESTORE/MSH/Physician Names/overlap.unknown.csv")

fwrite(overlap[GIM=="n", .(EncID.new, adm.phy.name, adm.phy.gim, dis.phy.name, dis.phy.gim,
                           GIM)],
       "R:/GEMINI/_RESTORE/MSH/Physician Names/overlap.notgim.csv")


adm.only <- adm.full[!EncID.new%in%dad.full$EncID.new, -"V1"]
adm.only <- merge(adm.only, msh.phy.marked[,.(adm.phy.name = full.name,GIM. ,code)],
                 by.x = "adm.code", by.y = "code", all.x = T, all.y = F)
adm.only <- merge(adm.only, msh.phy.marked[,.(dis.phy.name = full.name, GIM.,code)],
                 by.x = "dis.code", by.y = "code", all.x = T, all.y = F)
table(adm.only $GIM..x, useNA = "ifany")
table(adm.only $GIM..y, useNA = "ifany")
adm.only $GIM..y[adm.only $GIM..y=="maybe"] <- "don't know"

adm.only [, GIM := ifelse(GIM..x=="y"|GIM..y=="y", "y", 
                        ifelse(GIM..x=="n"&GIM..y=="n", "n", "u"))]
adm.only [GIM=="u"]
names(adm.only)[c(5, 7)] <- c("adm.phy.gim", "dis.phy.gim")
table(adm.only$GIM)
fwrite(adm.only[GIM=="u", .(EncID.new, adm.phy.name, adm.phy.gim, dis.phy.name, dis.phy.gim,
                           GIM)],
       "R:/GEMINI/_RESTORE/MSH/Physician Names/adm.only.unknown.csv")

fwrite(adm.only[GIM=="y", .(EncID.new, adm.phy.name, adm.phy.gim, dis.phy.name, dis.phy.gim,
                           GIM)],
       "R:/GEMINI/_RESTORE/MSH/Physician Names/adm.only.gim.csv")

dad.only <- dad.full[!EncID.new%in%adm.full$EncID.new, -"V1"]
table(dad.only$GIM.)
dad.only <- merge(dad.only, msh.phy.marked[,.(mrp = full.name,GIM. ,code)],
                  by.x = "mrp.code", by.y = "code", all.x = T, all.y = F)
table(dad.only$GIM.)
fwrite(dad.only[GIM.=="y"], "R:/GEMINI/_RESTORE/MSH/Physician Names/dad.only.gim.csv")
fwrite(dad.only[!GIM.%in%c("y", "n")], "R:/GEMINI/_RESTORE/MSH/Physician Names/dad.only.unknown.csv")



# ----------------------- check fiscal number ----------------------------------
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.csv")
table(dad$Institution.Number)
msh.adm <- readg(msh, adm)
msh.dad <- readg(msh, dad)
over <- intersect(msh.adm$EncID.new, msh.dad$EncID.new)
sum(over%in%dad$EncID.new)
msh.overlap <- dad[EncID.new%in%overlap[GIM=="y", paste("14", EncID.new, sep = "")]]
table(msh.overlap$fiscal.year)
msh.overlap[ymd(Admit.Date)>=ymd("2010-04-01")&ymd(Admit.Date)<ymd("2011-04-01"),
    fiscal.year1 := "2010"]
msh.overlap[ymd(Admit.Date)>=ymd("2011-04-01")&ymd(Admit.Date)<ymd("2012-04-01"),
    fiscal.year1 := "2011"]
msh.overlap[ymd(Admit.Date)>=ymd("2012-04-01")&ymd(Admit.Date)<ymd("2013-04-01"),
    fiscal.year1 := "2012"]
msh.overlap[ymd(Admit.Date)>=ymd("2013-04-01")&ymd(Admit.Date)<ymd("2014-04-01"),
    fiscal.year1 := "2013"]
msh.overlap[ymd(Admit.Date)>=ymd("2014-04-01")&ymd(Admit.Date)<ymd("2015-04-01"),
    fiscal.year1 := "2014"]
msh.overlap[ymd(Admit.Date)>=ymd("2015-04-01")&ymd(Admit.Date)<ymd("2016-04-01"),
    fiscal.year1 := "2015"]
table(msh.overlap$fiscal.year1)
