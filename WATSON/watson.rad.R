# ================= Create training data for WATSON ============================
# ======================= Feb 15 2017 ==========================================
rm(list = ls())
library(gemini)
lib.pa()
# ------------------- St.Michael's ---------------------------------------------
setwd("R:/GEMINI-DREAM/DELIRIUM Charts")
EncID.new <- NULL
files <- list.files()
dat1 <- fread(files[1])[1:200]
dat2 <- fread(files[2])
dat3 <- fread(files[3])
dat4 <- fread(files[4])
dat5 <- fread(files[10])
EncID.new <- unique(c(dat1$EncID.new, dat2$EncID.new, dat3$EncID.new, dat4$EncID.new,
               dat5$EncID.new))
EncID.new <- paste("11", EncID.new, sep = "")
watson.enc.smh <- EncID.new[1:1000]
smh.ct <- readg(smh, ct, dt = T)
smh.ct[,after72 := ymd_h(ord_for_dtime)> ymd_hm(paste(Admit.Date, Admit.Time))+hours(72)]
testnames <- unique(smh.ct$proc_desc_long)
smh.ctpe.names <- c("THORACIC ANEUR - CONT",
                    "THORACIC ANEUR CTA",
                    "THORACO-ABDO ANEUR - CONT",
                    "THORACO-ABDO ANEUR -/CTA CONT",
                    "THORACO-ABDO ANEUR CTA",
                    "THORACO-ABDO ANEUR POST TEVAR",
                    "THORACO-ABDO DISSECT -/CTA CONT",
                    "THORACO-ABDO DISSECT -/CTA/+ CON",
                    "THORAX - CONT",
                    "THORAX -/+ CONT",
                    "THORAX + CONT",
                    "THORAX HHT",
                    "THORAX HHT LOW DOSE",
                    "THORAX HI-RES",
                    "THORAX HI-RES LO-RES",
                    "THORAX HI-RES LOW-RES CONT",
                    "THORAX LOW DOSE",
                    "THORAX PE",
                    "THORAX PE/ABDOMEN/PELVIS + CONT",
                    "THORAX/ABDOMEN - CONT",
                    "THORAX/ABDOMEN + CONT",
                    "THORAX/ABDOMEN/PELVIS - CONT",
                    "THORAX/ABDOMEN/PELVIS -/+ CONT",
                    "THORAX/ABDOMEN/PELVIS + CONT" )
smh.ctpe.names %in% testnames
smh.ctpa <- smh.ct[proc_desc_long%in%smh.ctpe.names, 
                   .(EncID.new, proc_desc_long, result, impression, after72)]
smh.ctpa.watson <- smh.ctpa[EncID.new%in%watson.enc.smh]
fwrite(smh.ctpa.watson, "H:/GEMINI/Results/WATSON/smh.ctpa.csv")

# VQ
smh.nuc <- readg(smh, nuc, dt = T)
smh.nuc[,after72 := ymd_h(ord_for_dtime)> ymd_hm(paste(Admit.Date, Admit.Time))+hours(72)]
smh.vq <- smh.nuc[startsWith(proc_desc_long, "LUNG")]
smh.vq.watson <- smh.vq[EncID.new%in%watson.enc.smh,
                        .(EncID.new, proc_desc_long, result, impression, after72)]
fwrite(smh.vq.watson, "H:/GEMINI/Results/WATSON/smh.vq.csv")

#Doppler
smh.us <- readg(smh, us, dt = T)
smh.us[,after72 := ymd_h(ord_for_dtime)> ymd_hm(paste(Admit.Date, Admit.Time))+hours(72)]
smh.du.names <- c("DOP LEG VEIN/EXTREMITY BILAT",
                  "DOP LEG VEIN/EXTREMITY UNILAT",
                  "DOP VEN EXTREMITY BILAT",
                  "DOP VEN EXTREMITY UNIL",
                  "DOP ILIAC VESSELS BILAT",
                  "EXTREMITY BILAT",
                  "EXTREMITY UNILAT",
                  "EXTREMITY LEFT")

smh.du.watson <- smh.us[proc_desc_long%in%smh.du.names&
                          EncID.new%in%watson.enc.smh, 
                        .(EncID.new, proc_desc_long, result, impression, after72)]
fwrite(smh.du.watson, "H:/GEMINI/Results/WATSON/smh.du.csv")




# ---------------------------- UHN ---------------------------------------------
uhn.enc <- fread("R:/GEMINI/Sub-study Cohort 1/UHN.csv")
uhn.enc$EncID.new <- paste("13", uhn.enc$EncID.new, sep = "")
set.seed(1200)
watson.enc.uhn <- uhn.enc$EncID.new[sample(nrow(uhn.enc), 1000)]
uhn.ctpe.names <- c("Angiography Body Diagnostic",
                    "Angiography Body Angiogram Venous Subclavian",
                    "Angiography Body Angiogram Thoracic/Abdominal/Pelvic",
                    "CT Angiography Pulmonary Arteries",
                    "CT Angiography Dissection Chest",
                    "CT Chest")

uhn.vq.names <- c("NM Perfusion Lung Scan",
                  "NM Quantitative Perfusion Lung Scan",
                  "NM Quantitative Ventilation Perfusion Lung Scan",
                  "NM V/Q Ventilation Perfusion Lung Scan")


uhn.du.names <- c("US Vascular Peripheral Vein Doppler",
                  "US Vascular Peripheral Vein Doppler Lower Extremity",
                  "US Vascular Peripheral Vein Doppler Upper Extremity",
                  "US Calf",
                  "US Extremity",
                  "US Thigh",
                  "US Vascular Jugular Vein Doppler")

uhn.radip <- readg(uhn, rad_ip, dt = T)
uhn.rader <- readg(uhn, rad_er, dt = T)
uhn.rad <- rbind(uhn.rader, uhn.radip)
uhn.rad[,after72 := mdy_hm(OrderDateTime)>ymd_hm(paste(Admit.Date, Admit.Time))+hours(72)]
uhn.ctpa <- uhn.rad[ProcedureName%in%uhn.ctpe.names&EncID.new%in%watson.enc.uhn,
                    .(EncID.new, ProcedureName, ReportText, after72)]
uhn.du <- uhn.rad[ProcedureName%in%uhn.du.names&EncID.new%in%watson.enc.uhn,
                    .(EncID.new, ProcedureName, ReportText,  after72)]
uhn.vq <- uhn.rad[ProcedureName%in%uhn.vq.names&EncID.new%in%watson.enc.uhn,
                    .(EncID.new, ProcedureName, ReportText, after72)]

fwrite(uhn.ctpa, "H:/GEMINI/Results/WATSON/uhn.ctpa.csv")
fwrite(uhn.du, "H:/GEMINI/Results/WATSON/uhn.du.csv")
fwrite(uhn.vq, "H:/GEMINI/Results/WATSON/uhn.vq.csv")




# -------------------------- SBK -----------------------------------------------
sbk.ctpe.names <- c("CT Angio chest for pulmonary emboli",
                    "CT Angiogram chest",
                    "CT Angiogram chest, abdomen, pelvis",
                    "CT Chest",
                    "CT chest",
                    "CT chest, abdomen, pelvis")
sbk.vq.names <- c("Emerg-Lung Scan V/Q(Aero+PERF",
                  "Lung Perfusion Scan",
                  "Lung Scan Vent / Perf")
sbk.du.names <- c("Abdomen Ltd.+Bil leg doppler",
                  "Abdomen Ltd.+LT leg doppler",
                  "Abdomen Ltd.+RT leg doppler",
                  "Abdomen US + Right Leg Doppler",
                  "Abdomen US + Left Leg Doppler",
                  "Abdomen+Leg Doppler: Bil US",
                  "Arm Venous C Doppler Bil US",
                  "Arm Venous C Doppler Left US",
                  "Arm Venous C Doppler Right US",
                  "Leg Doppler Bilateral",
                  "Leg Doppler Left",
                  "Leg Doppler Right",
                  "Subclavian/Jug Doppler US: RT",
                  "Subclavian/Jug Doppler US:Left",
                  "Subclavian/Jug Doppler US:BIL",
                  "Venous Doppler & US:  Left leg",
                  "Venous Doppler & US: Bil leg",
                  "Venous Doppler & US: Right leg",
                  "WB: Arm Venous C Doppler Left US",
                  "WB: Leg Venous C Doppler Bilateral US",
                  "WB: Leg Venous C Doppler Left US",
                  "WB: Leg Venous C Doppler Right US",
                  "Abdomen + pelvic + leg: bil US",
                  "Groin doppler Bil: US")
sbk.enc <- fread("R:/GEMINI/Sub-study Cohort 1/SBK.csv")
sbk.enc$EncID.new <- paste("12", sbk.enc$EncID.new, sep = "")
set.seed(1200)
watson.enc.sbk <- sbk.enc$EncID.new[sample(nrow(sbk.enc), 1000)]

sbk.rad <- readg(sbk, rad.csv, dt = T)
sbk.rad[,after72 := ymd_hms(Ordered.DtTm)>ymd_hm(paste(Admit.Date, Admit.Time))+hours(72)]
sbk.ctpa <- sbk.rad[Test.Name%in%sbk.ctpe.names&EncID.new%in%watson.enc.sbk,
                    .(EncID.new, Test.Name, Results, after72)]
sbk.du <- sbk.rad[Test.Name%in%sbk.du.names&EncID.new%in%watson.enc.sbk,
                  .(EncID.new, Test.Name, Results, after72)]
sbk.vq <- sbk.rad[Test.Name%in%sbk.vq.names&EncID.new%in%watson.enc.sbk,
                  .(EncID.new, Test.Name, Results, after72)]
fwrite(sbk.ctpa, "H:/GEMINI/Results/WATSON/sbk.ctpa.csv")
fwrite(sbk.du, "H:/GEMINI/Results/WATSON/sbk.du.csv")
fwrite(sbk.vq, "H:/GEMINI/Results/WATSON/sbk.vq.csv")
write.csv(c(watson.enc.sbk, watson.enc.smh, watson.enc.uhn), 
          "H:/GEMINI/Results/WATSON/watson.enc.csv")

# ---------------------------- MSH ---------------------------------------------
msh <- readg(msh, adm)
set.seed(100)
watson.enc.msh <- sample(msh$EncID.new, 1000)
write.csv(watson.enc.msh, "H:/GEMINI/Results/delirium/msh.enc1000.csv")


msh.ctpe.names <- c("Angiography Body Diagnostic",
                    "Angiography Body Angiogram Venous Subclavian",
                    "Angiography Body Angiogram Thoracic/Abdominal/Pelvic",
                    "CT Angiography Pulmonary Arteries",
                    "CT Angiography Dissection Chest",
                    "CT Chest")

msh.vq.names <- c("NM Perfusion Lung Scan",
                  "NM Quantitative Perfusion Lung Scan",
                  "NM Quantitative Ventilation Perfusion Lung Scan",
                  "NM V/Q Ventilation Perfusion Lung Scan")


msh.du.names <- c("US Vascular Peripheral Vein Doppler",
                  "US Vascular Peripheral Vein Doppler Lower Extremity",
                  "US Vascular Peripheral Vein Doppler Upper Extremity",
                  "US Calf",
                  "US Extremity",
                  "US Thigh",
                  "US Vascular Jugular Vein Doppler")

msh.radip <- readg(msh, rad_ip, dt = T)
msh.rader <- readg(msh, rad_er, dt = T)
msh.rad <- rbind(msh.rader, msh.radip)
msh.rad[,after72 := ymd_hm(OrderDateTime)>ymd_hm(paste(Admit.Date, Admit.Time))+hours(72)]
msh.ctpa <- msh.rad[ProcedureName%in%msh.ctpe.names&EncID.new%in%watson.enc.msh,
                    .(EncID.new, ProcedureName, ReportText, after72)]
msh.du <- msh.rad[ProcedureName%in%msh.du.names&EncID.new%in%watson.enc.msh,
                  .(EncID.new, ProcedureName, ReportText,  after72)]
msh.vq <- msh.rad[ProcedureName%in%msh.vq.names&EncID.new%in%watson.enc.msh,
                  .(EncID.new, ProcedureName, ReportText, after72)]

fwrite(msh.ctpa, "H:/GEMINI/Results/WATSON/msh.ctpa.csv")
fwrite(msh.du, "H:/GEMINI/Results/WATSON/msh.du.csv")
fwrite(msh.vq, "H:/GEMINI/Results/WATSON/msh.vq.csv")

