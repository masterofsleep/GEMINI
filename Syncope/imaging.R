library(gemini)
lib.pa()
#combine CTPA data
ctpa.smh <- fread("H:/GEMINI/Results/Syncope/imaging/ctpa.smh.csv")
ctpa.sbk <- fread("H:/GEMINI/Results/Syncope/imaging/cpta.sbk.final.csv")
ctpa.uhn <- fread("H:/GEMINI/Results/Syncope/imaging/ctpa.uhn.csv")

names(ctpa.smh)
names(ctpa.sbk)
names(ctpa.uhn)

ctpa <- rbind(ctpa.smh[,.(EncID.new, Contrast_Study, CTPA_Test, Positive_PE)],
              ctpa.sbk[,.(EncID.new, Contrast_Study, CTPA_Test, Positive_PE)],
              ctpa.uhn[,.(EncID.new, Contrast_Study, CTPA_Test, Positive_PE)])
sum(duplicated(ctpa$EncID.new))
ctpa[EncID.new%in%ctpa[duplicated(EncID.new), EncID.new]]
write.csv(ctpa, "H:/GEMINI/Results/Syncope/imaging/ctpa.csv", row.names = F,
          na = "")


vq.smh <- fread("H:/GEMINI/Results/Syncope/imaging/vq.smh.csv")
vq.sbk <- fread("H:/GEMINI/Results/Syncope/imaging/vq.sbk.final.csv")
vq.uhn <- fread("H:/GEMINI/Results/Syncope/imaging/vq.uhn.csv")

names(vq.smh)
names(vq.sbk)
names(vq.uhn)
vq <- rbind(vq.smh[,.(EncID.new, Positive_PE)],
              vq.sbk[,.(EncID.new, Positive_PE)],
              vq.uhn[,.(EncID.new, Positive_PE)])
sum(duplicated(vq$EncID.new))
write.csv(vq, "H:/GEMINI/Results/Syncope/imaging/vq.csv", row.names = F,
          na = "")

#du
du.smh <- fread("H:/GEMINI/Results/Syncope/imaging/du.smh.csv")
du.sbk <- fread("H:/GEMINI/Results/Syncope/imaging/du.sbk.final.csv")
du.uhn <- fread("H:/GEMINI/Results/Syncope/imaging/du.uhn.csv")
du.uhn <- du.uhn[EncID.new%in%cohort$EncID.new]
du.sbk <- du.sbk[EncID.new%in%cohort$EncID.new]
du.smh <- du.smh[EncID.new%in%cohort$EncID.new]

names(du.smh)
names(du.sbk)
names(du.uhn)
du <- rbind(du.smh[,.(EncID.new, DVT_Test, Positive_DVT, Superficial_Thrombophlebitis)],
            du.sbk[,.(EncID.new, DVT_Test, Positive_DVT, Superficial_Thrombophlebitis)],
            du.uhn[,.(EncID.new, DVT_Test, Positive_DVT, Superficial_Thrombophlebitis)])
sum(duplicated(du$EncID.new))
write.csv(du, "H:/GEMINI/Results/Syncope/imaging/du.csv", row.names = F,
          na = "")






#------------------------- Dec 19 2016 -----------------------------------------
# ------------------- creating final tables ------------------------------------


cohort <- fread("H:/GEMINI/Results/Syncope/cohort.with.var.csv")
ctpa <- fread("H:/GEMINI/Results/Syncope/imaging/ctpa.csv")
vq <- fread("H:/GEMINI/Results/Syncope/imaging/vq.csv")
du <- fread("H:/GEMINI/Results/Syncope/imaging/du.csv")
ddm <- fread("H:/GEMINI/Results/Syncope/ddm.csv")



# categorize ddm results by hospital reference ranges
ddm[str_sub(EncID.new,1,2)%in%c("11","12"),abnormal:= as.numeric(Result.Value)>=230|Result.Value==">5000"]
ddm[Result.Value=="<230", abnormal:=FALSE]
ddm[startsWith(EncID.new,"13"), abnormal:= as.numeric(Result.Value)>400]


cohort$invest.vte <- cohort$EncID.new%in%c(
  ctpa[CTPA_Test=="y", EncID.new],
  du[DVT_Test=="y", EncID.new],
  vq[,EncID.new],
  ddm[,EncID.new])

cohort$ddm <- cohort$EncID.new%in%ddm$EncID.new
cohort$ddm.abn <- cohort$EncID.new%in%ddm[abnormal==TRUE, EncID.new]
cohort$ctpa <- cohort$EncID.new%in%ctpa[CTPA_Test=="y", EncID.new]
cohort$ctpa.pos <- cohort$EncID.new%in%ctpa[Positive_PE=="y", EncID.new]
cohort$du <- cohort$EncID.new%in%du[DVT_Test=="y", EncID.new]
cohort$du.pos <- cohort$EncID.new%in%du[Positive_DVT=="y", EncID.new]
cohort$vq <- cohort$EncID.new%in%vq$EncID.new
cohort$vq.pos <- cohort$EncID.new%in%vq[Positive_PE=="y", EncID.new]
fwrite(cohort, "H:/GEMINI/Results/Syncope/cohort.full.csv")

cohort$thorax.cont <- cohort$EncID.new%in%ctpa[Contrast_Study=="y"&
                                                 CTPA_Test=="n", EncID.new]
cohort$thorax.cont.pos <- cohort$EncID.new%in%ctpa[Contrast_Study=="y"&
                                                 CTPA_Test=="n"&
                                                   Positive_PE=="y", EncID.new]

cohort <- fread("H:/GEMINI/Results/Syncope/cohort.full.csv")

#table 8.2
dim(cohort)[1]
paste("Investigated for VTE ",
      sum(cohort$invest.vte), "(", 
                         format(sum(cohort$invest.vte)*100/dim(cohort)[1], digits = 4), 
                     "%)", sep = "")

paste("CTPA or VQ: ",
      sum(cohort$ctpa|cohort$vq), "(", 
      format(sum(cohort$ctpa|cohort$vq)*100/dim(cohort)[1], digits = 4), 
      "%)", sep = "")



#table 8.3
apply(cohort[,c(11:18), with = F], MARGIN = 2, FUN = sum) %>% 
  matrix(byrow = T, ncol=2) %>%data.table -> table3

table3 <- cbind(c("D-dimer","CTPA", "Doppler Extremity",
                  "V/Q Scan"), table3)

names(table3) <- c("Test", "Number of Patients who had Test",
                   "Number of Patients with Positive Result")
table3 <- rbind(table3, data.table(Test = "Total",
                           `Number of Patients who had Test`= 
                             sum(cohort$ddm|cohort$ctpa|cohort$vq|cohort$du),
                           `Number of Patients with Positive Result`=
                             sum(cohort$ddm.abn|cohort$ctpa.pos|cohort$vq.pos|cohort$du.pos)))


#either ctpa or doppler or v/q
data.table(Test = "Total",
           `Number of Patients who had Test`= 
             sum(cohort$ctpa|cohort$vq|cohort$du),
           `Number of Patients with Positive Result`=
             sum(cohort$ctpa.pos|cohort$vq.pos|cohort$du.pos))


#table 8.4
cohort$dvt.pos <- cohort$du.pos
cohort$pe.pos <- cohort$ctpa.pos|cohort$vq.pos



xtabs(~ dad.DVT +dvt.pos, data = cohort[du==TRUE])
xtabs(~ dad.PE + pe.pos, data = cohort[ctpa==TRUE|vq==TRUE])

xtabs(~ dad.DVT +dvt.pos, data = cohort)
xtabs(~ dad.PE + pe.pos, data = cohort)

xtabs(~ dad.DVT +dvt.pos, data = cohort)
ip.diag[EncID.new%in%check.enc2$EncID.new&str_sub(Diagnosis.Code,1,3) =="I80"]
xtabs(~ dad.PE + pe.pos, data = cohort)
ip.diag[EncID.new%in%check.enc2$EncID.new&str_sub(Diagnosis.Code,1,3) =="I26"]


cohort[dad.DVT==TRUE&dvt.pos==FALSE] -> check.enc
fwrite(check.enc, "H:/GEMINI/Results/Syncope/Check/falpos_DVT.csv")
ip.diag[EncID.new%in%check.enc]
du[EncID.new%in%check.enc]

cohort[dad.PE==TRUE&pe.pos==FALSE] -> check.enc2 
fwrite(check.enc2, "H:/GEMINI/Results/Syncope/Check/falpos_PE.csv")
ip.diag[EncID.new%in%check.enc2]
ctpa[EncID.new%in%check.enc2]
vq[EncID.new%in%check.enc2]

sbkrad <- readg(sbk.rad, rad.csv)
check <- sbkrad[EncID.new%in%check.enc]
check1 <- uhn.radip[EncID.new%in%check.enc]
check2 <- uhn.rader[EncID.new%in%check.enc]
#table 8.1
vars <- c("Age", "Gender", "previous.PE", "previous.DVT", "atrial.fib.or.flutter",
          "Charlson.Comorbidity.Index")

cohort$Gender <- factor(cohort$Gender, levels = c("M", "F"))
library(tableone)
tab <- CreateTableOne(vars = vars, data = cohort)
print(tab, catDigits = 4)
tableonebygroup <- CreateTableOne(vars = vars, data = cohort, strata = "invest.vte")
print(tableonebygroup, catDigits = 4)


#---------------------------- Jan 27 2017 --------------------------------------
# further check 2 numbers 
cohort <- fread("H:/GEMINI/Results/Syncope/cohort.full.csv")
# how many patients with abnormal D-dimer had either CTPA, Compression Ultrasound, or VQ scan? 
# Of these, how many had a positive imaging test?
cohort[ddm.abn==TRUE]
cohort[,imaging := ctpa|vq|du]
cohort[,imaging.pos := ctpa.pos|vq.pos|du.pos]
sum(cohort[ddm.abn==TRUE, imaging])
sum(cohort[ddm.abn==TRUE, imaging.pos])

sum(cohort[, imaging])
sum(cohort[, imaging.pos])



# ------------------------- Feb 9 ----------------------------------------------
# further more numbers
sum(cohort$ddm==F)
table(cohort[ddm==FALSE&ctpa==TRUE, ctpa.pos])
table(cohort[ddm==FALSE&vq==TRUE, vq.pos])
table(cohort[ddm==FALSE&du==TRUE, du.pos])
table(cohort[ddm==F, imaging])
apply(cohort[ddm==F, c(13:22), with = F], MARGIN = 2, FUN = sum)


sum(cohort$ddm==T)
table(cohort[ddm==TRUE&ddm.abn==F&ctpa==TRUE, ctpa.pos])
table(cohort[ddm==TRUE&ddm.abn==F&vq==TRUE, vq.pos])
table(cohort[ddm==TRUE&ddm.abn==F&du==TRUE, du.pos])
apply(cohort[ddm==T&ddm.abn==F, c(13:22), with = F], MARGIN = 2, FUN = sum)


table(cohort[ddm==TRUE&ddm.abn==T&ctpa==TRUE, ctpa.pos])
table(cohort[ddm==TRUE&ddm.abn==T&vq==TRUE, vq.pos])
table(cohort[ddm==TRUE&ddm.abn==T&du==TRUE, du.pos])
apply(cohort[ddm==T&ddm.abn==T, c(13:22), with = F], MARGIN = 2, FUN = sum)


apply(cohort[dvt.pos==T|pe.pos==T, c(13:20), with = F], MARGIN = 2, FUN = sum)
cohort[dvt.pos==T|pe.pos==T, c(13:22), with = F] -> check

sum(cohort$ctpa|cohort$du|cohort$vq)
