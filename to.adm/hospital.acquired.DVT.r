# ------------------ hospital acquired VTE -------------------------------------
setwd("H:/GEMINI/Results/WATSON")
all.enc <- fread("watson.enc.csv", select = "x")

smh.rad <- readg(smh, rad, dt = T)
sbk.rad <- readg(sbk, rad.csv, dt = T)
uhn.radip <- readg(uhn, rad_ip, dt = T)
uhn.rader <- readg(uhn, rad_er, dt = T)
uhn.rad <- rbind(uhn.rader, uhn.radip)

smh.time <- smh.rad[EncID.new%in%all.enc$x, 
                    .(EncID.new, result, impression, order.time = ymd_h(ord_for_dtime), Admit.Date, Admit.Time)]
sbk.time <- sbk.rad[EncID.new%in%all.enc$x,
                    .(EncID.new, Results, order.time = ymd_hms(Ordered.DtTm))]
uhn.time <- uhn.rad[EncID.new%in%all.enc$x,
                    .(EncID.new, ReportText, order.time = mdy_hm(OrderDateTime))]
setwd("R:/GEMINI-SYNCOPE/watson")
smh.ctpa <- read_excel("smh.ctpa.xlsx")[1:178,]
sbk.ctpa <- read_excel("sbk.ctpa.xlsx")[1:165,]
uhn.ctpa <- read_excel("uhn.ctpa.xlsx")[1:221,]
smh.du <- read_excel("smh.du.xlsx")[1:100,]
sbk.du <- read_excel("sbk.du.xlsx")[1:102,]
uhn.du <- read_excel("uhn.du.xlsx")[1:142,]
smh.vq <- read_excel("smh.vq.xlsx")[1:9,]
sbk.vq <- read_excel("sbk.vq.xlsx")[1:3,]
uhn.vq <- read_excel("uhn.vq.xlsx")[1:3,]

smh.ctpa.t <- fread("H:/GEMINI/Results/WATSON/smh.ctpa.csv")
smh.du.t <- fread("H:/GEMINI/Results/WATSON/smh.du.csv")
smh.vq.t <- fread("H:/GEMINI/Results/WATSON/smh.vq.csv")

sbk.ctpa.t <- fread("H:/GEMINI/Results/WATSON/sbk.ctpa.csv")
sbk.du.t <- fread("H:/GEMINI/Results/WATSON/sbk.du.csv")
sbk.vq.t <- fread("H:/GEMINI/Results/WATSON/sbk.vq.csv")

uhn.ctpa.t <- fread("H:/GEMINI/Results/WATSON/uhn.ctpa.csv")
uhn.du.t <- fread("H:/GEMINI/Results/WATSON/uhn.du.csv")
uhn.vq.t <- fread("H:/GEMINI/Results/WATSON/uhn.vq.csv")

smh.ctpa <- smh.ctpa %>% arrange(EncID.new, result)
smh.ctpa.t <- smh.ctpa.t %>% arrange(EncID.new, result)
sum(smh.ctpa$EncID.new==smh.ctpa.t$EncID.new)
smh.ctpa$after72 <- smh.ctpa.t$after72

smh.vq <- smh.vq %>% arrange(EncID.new, result)
smh.vq.t <- smh.vq.t %>% arrange(EncID.new, result)
sum(smh.vq$EncID.new==smh.vq.t$EncID.new)
smh.vq$after72 <- smh.vq.t$after72

smh.du <- smh.du %>% arrange(EncID.new, result)
smh.du.t <- smh.du.t %>% arrange(EncID.new, result)
sum(smh.du$EncID.new==smh.du.t$EncID.new)
smh.du$after72 <- smh.du.t$after72
## sbk 
sbk.ctpa <- sbk.ctpa %>% arrange(EncID.new, Results)
sbk.ctpa.t <- sbk.ctpa.t %>% arrange(EncID.new, Results)
sum(sbk.ctpa$EncID.new==sbk.ctpa.t$EncID.new)
sbk.ctpa$after72 <- sbk.ctpa.t$after72

sbk.vq <- sbk.vq %>% arrange(EncID.new, Results)
sbk.vq.t <- sbk.vq.t %>% arrange(EncID.new, Results)
sum(sbk.vq$EncID.new==sbk.vq.t$EncID.new)
sbk.vq$after72 <- sbk.vq.t$after72

sbk.du <- sbk.du %>% arrange(EncID.new, Results)
sbk.du.t <- sbk.du.t %>% arrange(EncID.new, Results)
sum(sbk.du$EncID.new==sbk.du.t$EncID.new)
sbk.du$after72 <- sbk.du.t$after72

## uhn
uhn.ctpa <- uhn.ctpa %>% arrange(EncID.new, ReportText)
uhn.ctpa.t <- uhn.ctpa.t %>% arrange(EncID.new, ReportText)
sum(uhn.ctpa$EncID.new==uhn.ctpa.t$EncID.new)
uhn.ctpa$after72 <- uhn.ctpa.t$after72

uhn.vq <- uhn.vq %>% arrange(EncID.new, ReportText)
uhn.vq.t <- uhn.vq.t %>% arrange(EncID.new, ReportText)
sum(uhn.vq$EncID.new==uhn.vq.t$EncID.new)
uhn.vq$after72 <- uhn.vq.t$after72

uhn.du <- uhn.du %>% arrange(EncID.new, ReportText)
uhn.du.t <- uhn.du.t %>% arrange(EncID.new, ReportText)
sum(uhn.du$EncID.new==uhn.du.t$EncID.new)
uhn.du$after72 <- uhn.du.t$after72

ctpa <- rbind(smh.ctpa[, c(1, 5:13)],
              sbk.ctpa[, c(1, 4:12)],
              uhn.ctpa[, c(1, 4:12)]) %>% data.table
ctpa[`CTPA Study (y, n)`=="y"&is.na(`Acute PE (y, n)`),
     `Acute PE (y, n)`:= "n"]

du <- rbind(smh.du[, c(1, 5:12)],
            sbk.du[, c(1, 4:11)],
            uhn.du[, c(1, 4:11)]) %>% data.table
du[is.na(`Positive Study (y,n,u)`), `Upper or Lower Extremity (u,l)`] %>% table
vq <- rbind(smh.vq[, c(1, 5,6)],
            sbk.vq[, c(1, 4,5)],
            uhn.vq[, c(1, 4,5)]) %>% data.table


setwd("H:/GEMINI/Results/WATSON")
all.enc <- fread("watson.enc.csv", select = "x")
all.enc <- all.enc$x
all.enc <- data.table(EncID.new = all.enc)
all.enc[,':='(ctpa.test = EncID.new%in%ctpa[ctpa$`CTPA Study (y, n)`=="y", EncID.new],
              vq.test = EncID.new%in%vq$EncID.new,
              du.test = EncID.new%in%du[
                du$`Upper or Lower Extremity (u,l)`%in%c("l", "L", "u"), EncID.new]
)]


ctpa[!`Acute PE (y, n)`== "y"|(is.na(`Acute PE (y, n)`)), after72:=NA]
vq[!`Probability of PE (l,m,h,u)`=="h"|is.na(`Probability of PE (l,m,h,u)`), after72 := NA]
du[!`Positive Study (y,n,u)`%in%c("y", "Y")|is.na(`Positive Study (y,n,u)`), after72:=NA]
pos.after72 <- c(ctpa[after72==T, EncID.new], du[after72==T, EncID.new],
                 vq[after72==T, EncID.new])

all.enc[ctpa.test==T, ctpa.pos := EncID.new%in%ctpa[`Acute PE (y, n)`== "y", EncID.new]]
all.enc[vq.test==T, vq.pos := EncID.new%in%vq[`Probability of PE (l,m,h,u)`=="h", EncID.new]]
all.enc[du.test==T, du.pos := EncID.new%in%du[`Positive Study (y,n,u)`%in%c("y", "Y"), EncID.new]]
all.enc[EncID.new%in%pos.after72, pos.after72:= T]

all.enc[pos.after72==T] -> check
table(all.enc[,.(str_sub(EncID.new, 1, 2), pos.after72)])
