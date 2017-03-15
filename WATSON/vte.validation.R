# ------------------------------------------------------------------------------
# ---------------------------- VTE Validation ----------------------------------
# ------------------------------ 2017-03-15 ------------------------------------

library(gemini)
lib.pa()
rm(list = ls())
setwd("H:/GEMINI/Results/WATSON")
all.enc <- fread("watson.enc.csv", select = "x")
all.enc <- all.enc$x
ip.diag <- readg(gim, ip_diag)[EncID.new%in%all.enc]

all.pe <- ip.diag[startwith.any(Diagnosis.Code, "I26"), EncID.new] %>% unique
all.dvt <- ip.diag[startwith.any(Diagnosis.Code, c("I80", "I821", "I828", "I829")), 
                   EncID.new] %>% unique
setwd("R:/GEMINI-SYNCOPE/watson")
files <- list.files()
files <- files[str_sub(files, -4, -1)=="xlsx"]
library(readxl)
smh.ctpa <- read_excel("smh.ctpa.xlsx")
sbk.ctpa <- read_excel("sbk.ctpa.xlsx")
uhn.ctpa <- read_excel("uhn.ctpa.xlsx")
ctpa <- rbind(smh.ctpa[1:178, c(1, 5:12)],
              sbk.ctpa[1:165, c(1, 4:11)],
              uhn.ctpa[1:221, c(1, 4:11)]) %>% data.table
ctpa[`CTPA Study (y, n)`=="y"&is.na(`Acute PE (y, n)`),
     `Acute PE (y, n)`:= "n"]

smh.du <- read_excel("smh.du.xlsx")
sbk.du <- read_excel("sbk.du.xlsx")
uhn.du <- read_excel("uhn.du.xlsx")
du <- rbind(smh.du[1:100, c(1, 5:11)],
            sbk.du[1:102, c(1, 4:10)],
            uhn.du[1:142, c(1, 4:10)]) %>% data.table
du[is.na(`Positive Study (y,n,u)`), `Upper or Lower Extremity (u,l)`] %>% table

smh.vq <- read_excel("smh.vq.xlsx")
sbk.vq <- read_excel("sbk.vq.xlsx")
uhn.vq <- read_excel("uhn.vq.xlsx")
vq <- rbind(smh.vq[1:9, c(1, 5)],
            sbk.vq[1:3, c(1, 4)],
            uhn.vq[1:3, c(1, 4)]) %>% data.table

table(ctpa$`CTPA Study (y, n)`, useNA = "ifany")
table(ctpa$`Acute PE (y, n)`, useNA = "ifany")

table(du$`Upper or Lower Extremity (u,l)`)
table(du$`Positive Study (y,n,u)`, useNA = "ifany")


table(vq$`Probability of PE (l,m,h,u)`, useNA = "ifany")

all.enc <- data.table(EncID.new = all.enc)
all.enc[,':='(diag.pe = EncID.new%in%all.pe,
              diag.dvt = EncID.new%in%all.dvt,
              ctpa.test = EncID.new%in%ctpa[ctpa$`CTPA Study (y, n)`=="y", EncID.new],
              vq.test = EncID.new%in%vq$EncID.new,
              du.test = EncID.new%in%du[
                du$`Upper or Lower Extremity (u,l)`%in%c("l", "L", "u"), EncID.new]
              )]

all.enc[ctpa.test==T, ctpa.pos := EncID.new%in%ctpa[`Acute PE (y, n)`== "y", EncID.new]]
all.enc[vq.test==T, vq.pos := EncID.new%in%vq[`Probability of PE (l,m,h,u)`=="h", EncID.new]]
all.enc[dvt.test==T, dvt.pos := EncID.new%in%du[`Positive Study (y,n,u)`%in%c("y", "Y"), EncID.new]]

all.enc[, ':='(pe.test = ctpa.test== TRUE | vq.test==TRUE,
               pe.pos = ctpa.pos==T|vq.pos==T)]

# test results for pe
apply(all.enc[,.(ctpa.test, ctpa.pos, vq.test, vq.pos)], MARGIN = 2, FUN = function(x)sum(x, na.rm = T))
# accuracy of Diagnosis of pe

table(all.enc[, .(diag.pe, pe.pos)], useNA = "ifany")

# test results for dvt
apply(all.enc[,.(dvt.test, dvt.pos)], MARGIN = 2, FUN = function(x)sum(x, na.rm = T))

#accuracy of diagnosis of dvt
table(all.enc[, .(diag.dvt, dvt.pos)], useNA = "ifany")
