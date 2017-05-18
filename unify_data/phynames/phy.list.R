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


# --------------------------- combine marked by terence ------------------------
phy.list <- readxl::read_excel("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.all.physician.check_FR.xlsx")%>%
  data.table
tt.mark <- fread("C:/Users/guoyi/Desktop/marked_names/thp/thp.names_classified.csv")

tt.mark[`GIM - Internist`=="X", GIM:="y"]
tt.mark[`Family MD`=="X", GIM:="GP-GIM"]
tt.mark[`Internist (not necessarily core group, may be subspecialists or locums)`=="X", GIM:="y"]
tt.mark[`Not internist, not family MD`=="X", GIM:="n"]
tt.mark[is.na(GIM), GIM:="u"]

table(tt.mark$GIM)

phy.list <- merge(phy.list, tt.mark[,.(Code, GIM)], by = "Code", all.x = T, all.y = F)
check <- phy.list[code.type=="thp"&GIM.y!="u"], GIM.x:=GIM.y
check[GIM.y=="n"&(!is.na(`Same Name (Definite)`)|!is.na(`Same Name (Possible)`))]
phy.list[code.type=="thp"&GIM.y!="u", GIM.x:=GIM.y]
phy.list[code.type=="thp"&is.na(`Same Name (Definite)`)&
           is.na(`Same Name (Possible)`), GIM.x:= "u"]

phy.list[, GIM.y:=NULL]

phy.list <- phy.list[!duplicated(phy.list[,.(Code, code.type, GIM.x)])]
names(phy.list)[8] <- "GIM"

phy.list[!is.na(`Same Name (Definite)`), code.new := `Same Name (Definite)`]
phy.list[!is.na(`Same Name (Possible)`), code.new := `Same Name (Possible)`]
phy.list[`Same Name (Definite)`!=`Same Name (Possible)`]
range(phy.list$code.new, na.rm = T)
table(phy.list$code.new)
phy.list[code.new==1500, code.new:=214]
sum(is.na(phy.list$code.new))
phy.list[is.na(code.new), code.new:= 215: (215+1595)]
all.name <- phy.list[,.(Code, code.type, last.name, first.name, code.new, GIM)] %>% unique

all.name[str_detect(first.name, "Temp")|
         str_detect(first.name, "Resident")|
         str_detect(first.name, "Doctor"), GIM := "n"]




fwrite(all.name, 
       "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.csv")


all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.csv")
gemini.phy <- readg(gim, all.phy)
apply(gemini.phy, 2, function(x)sum(is.na(x)))
gemini.phy[is.na(dis.code.new)]

all.name[code.new%in%c(gemini.phy$adm.code.new, gemini.phy$dis.code.new, gemini.phy$mrp.code.new)]

table(all.name$GIM)
unknown <- all.name[GIM=="u"]




# ------------------------- new categorization ---------------------------------
# ---------------------------- 2017-05-15 --------------------------------------
phy.list <- readxl::read_excel("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.all.physician.check_FR.xlsx")%>%
  data.table
phy.list[!is.na(GIM2), GIM:=GIM2]
tt.mark <- fread("C:/Users/guoyi/Desktop/marked_names/thp/thp.names_classified.csv")

tt.mark[`GIM - Internist`=="X", GIM:="y"]
tt.mark[`Family MD`=="X", GIM:="GP-GIM"]
tt.mark[`Internist (not necessarily core group, may be subspecialists or locums)`=="X", GIM:="y"]
tt.mark[`Not internist, not family MD`=="X", GIM:="n"]
tt.mark[is.na(GIM), GIM:="u"]

table(tt.mark$GIM)

phy.list <- merge(phy.list, tt.mark[,.(Code, GIM)], by = "Code", all.x = T, all.y = F)
check <- phy.list[code.type=="thp"&GIM.y!="u"]
check[GIM.y=="n"&(!is.na(`Same Name (Definite)`)|!is.na(`Same Name (Possible)`))]
phy.list[code.type=="thp"&GIM.y!="u", GIM.x:=GIM.y]
phy.list[code.type=="thp"&is.na(`Same Name (Definite)`)&
           is.na(`Same Name (Possible)`)&GIM.y=="u", GIM.x:= "u"]
table(phy.list$GIM)
phy.list[, GIM.y:=NULL]

phy.list <- phy.list[!duplicated(phy.list[,.(Code, code.type, GIM.x)])]
names(phy.list)[8] <- "GIM"

phy.list[!is.na(`Same Name (Definite)`), code.new := `Same Name (Definite)`]
phy.list[!is.na(`Same Name (Possible)`), code.new := `Same Name (Possible)`]
phy.list[`Same Name (Definite)`!=`Same Name (Possible)`]
range(phy.list$code.new, na.rm = T)
table(phy.list$code.new)
phy.list[code.new==1500, code.new:=214]
sum(is.na(phy.list$code.new))
phy.list[is.na(code.new), code.new:= 216: (216+1590)]
all.name <- phy.list[,.(Code, N, code.type, last.name, first.name, code.new, GIM)] %>% unique

all.name[str_detect(first.name, "Temp")|
           str_detect(first.name, "Resident")|
           str_detect(first.name, "Doctor"), GIM := "n"]





table(all.name$GIM)





# smh geriatrics to "not gim"
all.name[GIM=="Geriatrics", GIM := "n"]
# fix two unknown at sbk
all.name[code.type=="sbk"&GIM=="u"]
# all.name[code.type=="sbk"&last.name=="Lowe", GIM:="y"]
# all.name[code.type=="sbk"&last.name=="Medicine", GIM:="n"]
# uhn
uhn.all <- fread("C:/Users/guoyi/Desktop/marked_names/uhn//uhn.all.names.csv")
all.name[Code%in%
           uhn.all[`MD Code`==2]$Code
         &startsWith(code.type, "uhn"), GIM:= "GP-GIM"]
all.name[startsWith(code.type, "uhn")&GIM%in%c("u")] -> check

# msh
all.name[code.type=="msh"&GIM=="u"]


# thp
table(all.name$GIM)
phy.ncat <- ddply(all.name, ~code.new, summarize,
      n.cat = length(unique(GIM)))
all.name[code.new%in%phy.ncat[phy.ncat$n.cat>1, "code.new"]][order(code.new)]
table(all.name[code.type=="thp", GIM])

#all.name[GIM=="u", GIM:="n"]
all.name[GIM=="n"&N>50] -> check


fwrite(all.name, 
       "H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.new.csv")

