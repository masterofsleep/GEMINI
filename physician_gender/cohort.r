library(gemini)
lib.pa()

phy.sum <- fread("C:/Users/guoyi/Desktop/to.adm/phy.summary.csv")
fwrite(phy.sum[,.(code.new, site, first.name, last.name)],
       "H:/GEMINI/Results/phy_gender/phy100.csv")

phy <- readg(gim, all.phy)
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.new.csv")

phy20.code <- c(phy[,.N, by = "mrp.code.new"][N>=20, mrp.code.new],
phy[,.N, by = "adm.code.new"][N>=20, adm.code.new],
phy[,.N, by = "dis.code.new"][N>=20, dis.code.new])
all.name <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.csv")

phy20 <- all.name[code.new%in%phy20.code]
table(phy20$GIM)
fwrite(phy20[GIM=="u"],
       "H:/GEMINI/Results/phy_gender/phy20.csv")



# connect complete physician information to gemini data
library(gemini)
lib.pa()

marked_names <- readxl::read_excel("H:/GEMINI/Results/phy_gender/Physician Data CPSO.xlsx")
all_phy <- fread("H:/GEMINI/Results/DataSummary/physician_names/complete.name.list/gemini.phy.list.new2.csv")

marked_names <- merge(marked_names, all_phy[, .(Code, Code.type = code.type, code.new, GIM)],
                      by = c("Code", "Code.type"))

marked_names$Gender[marked_names$Gender=="Male "] <- "Male"
marked_names <- marked_names[marked_names$Gender%in%c("Male", "Female"),]
phy <- readg(gim, all.phy)
phy[mrp.code.new%in%marked_names$code.new]
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")

find_er <- function(){
  smh.er <- readg(smh.er, .er.nophi)
  sbk.er <- readg(sbk.er, .er.nophi,
                  colClasses = list(character = c("NACRSRegistrationNumber",
                                                  "EncID.new")))
  uhn.er <- readg(uhn.er, .er.nophi,
                  colClasses = list(character = c("NACRSRegistrationNumber",
                                                  "EncID.new")))
  msh.er <- readg(msh, er.nophi)
  thp.er <- readg(thp, er.nophi)
  from.er <- c(smh.er$EncID.new, sbk.er$EncID.new, uhn.er$EncID.new,
               msh.er$EncID.new, thp.er$EncID.new)
  return(from.er)
}

er_enc <- find_er()

dad[, from.ER := EncID.new%in%er_enc]
cohort <- dad[EncID.new%in%phy[mrp.code.new%in%marked_names$code.new, EncID.new]&
                from.ER==T,
              .(EncID.new, Gender, Age, Admit.Date, Discharge.Date, 
                Charlson.Comorbidity.Index, Year.of.Admission = str_sub(Admit.Date, 1, 4))]
setwd("H:/GEMINI/Results/to.administrator")
copd <- fread("qbp.copd.csv")
cap <- fread("qbp.cap.csv")
uti <- fread("qbp.uti.csv")
stroke <- fread("qbp.stroke.csv")
chf <- fread("qbp.chf.csv")
cohort[,':='(copd = EncID.new%in%copd$EncID.new,
             cap = EncID.new%in%cap$EncID.new,
             chf = EncID.new%in%chf$EncID.new,
             stroke = EncID.new%in%stroke$EncID.new,
             uti = EncID.new%in%uti$EncID.new)]
inc <- fread("C:/Users/guoyi/Desktop/to.adm/kdigo.csv")
cohort$aki <- cohort$EncID.new%in%inc[KDIGO%in%c("2", "3"), EncID.new]
marked_names <- data.table(marked_names)
cohort <- merge(cohort, phy[,.(EncID.new, mrp.code.new)], by = "EncID.new")
cohort <- merge(cohort, 
                marked_names[,. (code.new, Physician.Gender = Gender,
                                 Language,Medical.School = `Medical School`,
                                 Specialty.Issue.Year = `Specialty Issue Year `,
                                 Specialty)], 
                by.x = "mrp.code.new", by.y = "code.new", all.x = T)

cohort[Charlson.Comorbidity.Index>=2, Charlson.Comorbidity.Index:=2]
library(tableone)

cohort[, Years.of.Experience := as.numeric(Year.of.Admission) - 
         as.numeric(Specialty.Issue.Year)]
all_vars <- names(cohort)
CreateTableOne(vars = all_vars[c(4, 7, 3, 8)],
               factorVars = all_vars[c(7,8)], strata = "Physician.Gender",
               data = cohort)

CreateTableOne(vars = all_vars[c(20, 17, 16, 19)],
               factorVars = all_vars[c(17, 16, 19)], strata = "Physician.Gender",
               data = cohort[!duplicated(mrp.code.new)])


find_odds <- function(varname, cat = F){
  cohort <- data.frame(cohort)
  if(cat==F){
    data.frame(male = mean(cohort[cohort$Gender=="M", varname],na.rm = T),
               female = mean(cohort[cohort$Gender=="F", varname], na.rm = T),
               test.of.sig = "T.Test",
               p.value = t.test(cohort[cohort$Gender=="M", varname],
                                cohort[cohort$Gender=="F", varname])$p.value)
  }
}