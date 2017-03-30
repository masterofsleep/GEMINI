# ------------ create a complete physician name for Fahad to mark duplicates ---
# ---------------------------- 2017-03-23 --------------------------------------
library(gemini)
lib.pa()

phy.names <- rbind(fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/smh.names.csv"),
                   fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/sbk.names.csv"),
                   fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/msh.names.csv"),
                   fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/uhn.names.csv"),
                   fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/thp.names.csv"),
                   fill = T)

simpleCap <- function(x)gsub("(^|[[:space:]]|'|-)([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)

phy.names$first.name <- simpleCap(tolower(phy.names$first.name))
phy.names$last.name <- simpleCap(tolower(phy.names$last.name))

phy.names <- unique(phy.names[order(last.name, first.name)])

table(phy.names$GIM, useNA = "ifany")

fwrite(phy.names, "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.all.physician.csv")


# --------------------- link new codes back to patients ------------------------
smh <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/smh.link.csv")
sbk <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/sbk.link.csv")
uhn <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/uhn.link.csv")
msh <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/msh.link.csv")
thp <- fread("H:/GEMINI/Results/DataSummary/physician_names/link/thp.link.csv")
exclude <- readg(gim, notgim)
all.names <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.all.physician.csv")

all.enc <- rbind(smh, sbk, uhn, msh, thp)[!EncID.new%in%exclude$EncID.new]
what <- phy.names[Code%in%c(all.enc$adm.code, all.enc$dis.code, all.enc$mrp.code)]
fwrite(what, "C:/Users/guoyi/Desktop/marked_names/all_to_be_marked.csv")
table(need.to.be.coded$GIM)
find.new.code <- function(df, code.type.adm, code.type.mrp){
  df <- merge(df, all.name[code.type = code.type.adm], 
              by.x = "adm.code",by.y = "Code",
              all.x = T, all.y = F)
  df <- merge(df, all.name[code.type = code.type.adm], 
              by.x = "dis.code",by.y = "Code",
              all.x = T, all.y = F)
  df <- merge(df, all.name[code.type = code.type.mrp], 
              by.x = "mrp.code",by.y = "Code",
              all.x = T, all.y = F)
  names(df[5:7]) <- c("adm.code.new", "dis.code.new", "mrp.code.new")
  df
}




list.fr <- readxl::read_excel("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/complete.name.list_FR2.xlsx")
list.all <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.all.physician.csv")

list.fr <- merge(list.fr, list.all[,.(Code, GIM, code.type)],by = c("Code", "code.type"), all.x = T, all.y = F)
list.fr$GIM <- list.fr$GIM.y
list.fr <- data.table(list.fr)
list.fr[,':='(GIM.x = NULL,
              GIM.y = NULL)]
list.fr[Code=="3015"]
list.check <- rbind(list.fr, list.all[GIM=="n"], fill = T)

fwrite(list.check, "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.all.physician.check.csv")
