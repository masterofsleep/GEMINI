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
