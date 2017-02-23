# ================= Create training data for WATSON ============================
# ======================= Feb 15 2017 ==========================================
rm(list = ls())

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
smh.ct <- readg(smh, ct)
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
                   .(EncID.new, proc_desc_long, result, impression)]
smh.ctpa.watson <- smh.ctpa[EncID.new%in%watson.enc.smh]
fwrite(smh.ctpa.watson, "H:/GEMINI/Results/WATSON/smh.ctpa.csv")

# VQ
smh.nuc <- readg(smh, nuc)
smh.vq <- smh.nuc[startsWith(proc_desc_long, "LUNG")]
smh.vq.watson <- smh.vq[EncID.new%in%watson.enc.smh,
                        .(EncID.new, proc_desc_long, result, impression)]
fwrite(smh.vq.watson, "H:/GEMINI/Results/WATSON/smh.vq.csv")

#Doppler
smh.us <- readg(smh, us)
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
                        .(EncID.new, proc_desc_long, result, impression)]
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

uhn.radip <- readg(uhn, rad_ip)
uhn.rader <- readg(uhn, rad_er)
uhn.rad <- rbind(uhn.rader, uhn.radip)

uhn.ctpa <- uhn.rad[ProcedureName%in%uhn.ctpe.names&EncID.new%in%watson.enc.uhn,
                    .(EncID.new, ProcedureName, ReportText)]
uhn.du <- uhn.rad[ProcedureName%in%uhn.du.names&EncID.new%in%watson.enc.uhn,
                    .(EncID.new, ProcedureName, ReportText)]
uhn.vq <- uhn.rad[ProcedureName%in%uhn.vq.names&EncID.new%in%watson.enc.uhn,
                    .(EncID.new, ProcedureName, ReportText)]

fwrite(uhn.ctpa, "H:/GEMINI/Results/WATSON/uhn.ctpa.csv")
fwrite(uhn.du, "H:/GEMINI/Results/WATSON/uhn.du.csv")
fwrite(uhn.vq, "H:/GEMINI/Results/WATSON/uhn.vq.csv")
