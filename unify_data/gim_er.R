library(gemini)
lib.pa()
#===================GIM_ER_2010_2015 ========================================
#------------available for SMH, SBK, UHN, THP, MSh -----------------------------
rm(list = ls())
smh <- readg(smh.er, .er.nophi)
sbk <- readg(sbk.er, .er.nophi,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn <- readg(uhn.er, .er.nophi,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh <- readg(msh, .er.nophi)
thp <- readg(thp, .er.nophi)

er.name <- c("NACRSRegistrationNumber",
             "Institution.Number",
             "Institution.From",
             "Institution.To",
             "Admit.via.Ambulance",
             "Ambulance.Arrival.Date",
             "Ambulance.Arrival.Time",
             "Presenting.Complaint.List",
             "Triage.Date",
             "Triage.Time",
             "Triage.Level",
             "Registration.Date",
             "Registration.Time",
             "Date.of.Physician.Initial.Assessment",
             "Time.of.Physician.Initial.Assessment",
             "Disposition.Date",
             "Disposition.Time",
             "Date.Left.ER",
             "Time.Left.ER",
             "Visit.Disposition",
             "Blood.Transfusion.in.ED",
             "Blood.Components.ED",
             "Units.of.Blood.ED",
             "RBC.Y.N",
             "Plt.Y.N",
             "Plasma.Y.N",
             "Albumin.Y.N",
             "Other.Y.N",
             "Auto.Transfusion.Y.N",
             "Chief.Complaint",
             "Presenting.Complaint",
             "Presenting.Complaint.Occurr",
             "Presenting.Complaint.Desc",
             "Start.Time.Stamp",
             "Discharge.Time.Stamp",
             "Institution.From.Type",
             "Institution.To.Type",
             "RBC.Units",
             "Plt.Units",
             "Plasma.Units",
             "Albumin.Units",
             "Other.Units",
             "Auto.Transfusion.Units",
             "Readmit.Code",
             "EncID.new")

names(smh) <-c("NACRSRegistrationNumber",
               "Institution.Number",
               "Institution.From",
               "Institution.To",
               "Admit.via.Ambulance",
               "Ambulance.Arrival.Date",
               "Ambulance.Arrival.Time",
               "Presenting.Complaint.List",
               "Triage.Date",
               "Triage.Time",
               "Triage.Level",
               "Registration.Date",
               "Registration.Time",
               "Date.of.Physician.Initial.Assessment",
               "Time.of.Physician.Initial.Assessment",
               "Disposition.Date",
               "Disposition.Time",
               "Date.Left.ER",
               "Time.Left.ER",
               "Visit.Disposition",
               "Blood.Transfusion.in.ED",
               "RBC.Y.N",
               "Plt.Y.N",
               "Plasma.Y.N",
               "Albumin.Y.N",
               "Other.Y.N",
               "Auto.Transfusion.Y.N",
               "EncID.new")
sbk <- sbk[,-2, with = F]
names(sbk)[1:24] <- c("NACRSRegistrationNumber",
             "Institution.Number",
             "Institution.From",
             "Institution.To",
             "Admit.via.Ambulance",
             "Ambulance.Arrival.Date",
             "Ambulance.Arrival.Time",
             "Chief.Complaint",
             "Presenting.Complaint.List",
             "Presenting.Complaint.Occurr",
             "Presenting.Complaint.Desc",
             "Triage.Date",
             "Triage.Time",
             "Triage.Level",
             "Registration.Date",
             "Registration.Time",
             "Date.of.Physician.Initial.Assessment",
             "Time.of.Physician.Initial.Assessment",
             "Disposition.Date",
             "Disposition.Time",
             "Date.Left.ER",
             "Time.Left.ER",
             "Visit.Disposition",
             "EncID.new")

names(uhn) <- c("NACRSRegistrationNumber",
                "Start.Time.Stamp",
                "Discharge.Time.Stamp",
                "Institution.Number",
                "Institution.From",
                "Institution.From.Type",
                "Institution.To",
                "Admit.via.Ambulance",
                "Ambulance.Arrival.Date",
                "Ambulance.Arrival.Time",
                "Presenting.Complaint.List",
                "Triage.Date",
                "Triage.Time",
                "Triage.Level",
                "Registration.Date",
                "Registration.Time",
                "Date.of.Physician.Initial.Assessment",
                "Time.of.Physician.Initial.Assessment",
                "Disposition.Date",
                "Disposition.Time",
                "Date.Left.ER",
                "Time.Left.ER",
                "Visit.Disposition",
                "Blood.Transfusion.in.ED",
                "Blood.Components.ED",
                "Units.of.Blood.ED",
                "EncID.new")
names(msh) <- c("NACRSRegistrationNumber",
                "Institution.Number",
                "Institution.From",
                "Institution.From.Type",
                "Institution.To",
                "Institution.To.Type",
                "Readmit.Code",
                "Admit.via.Ambulance",
                "Ambulance.Arrival.Date",
                "Ambulance.Arrival.Time",
                "Chief.Complaint",
                "Triage.Date",
                "Triage.Time",
                "Triage.Level",
                "Registration.Date",
                "Registration.Time",
                "Date.of.Physician.Initial.Assessment",
                "Time.of.Physician.Initial.Assessment",
                "Disposition.Date",
                "Disposition.Time",
                "Date.Left.ER",
                "Time.Left.ER",
                "Visit.Disposition",
                "Blood.Transfusion.in.ED",
                "RBC.Y.N",
                "RBC.Units",
                "Plt.Y.N",
                "Plt.Units",
                "Plasma.Y.N",
                "Plasma.Units",
                "Albumin.Y.N",
                "Albumin.Units",
                "Other.Y.N",
                "Other.Units",
                "Auto.Transfusion.Y.N",
                "EncID.new")


names(thp) <- c("NACRSRegistrationNumber",
                "Institution.Number",
                "Institution.From",
                "Institution.To",
                "Admit.via.Ambulance",
                "Ambulance.Arrival.Date",
                "Ambulance.Arrival.Time",
                "Presenting.Complaint.List",
                "Triage.Date",
                "Triage.Time",
                "Triage.Level",
                "Registration.Date",
                "Registration.Time",
                "Date.of.Physician.Initial.Assessment",
                "Time.of.Physician.Initial.Assessment",
                "Disposition.Date",
                "Disposition.Time",
                "Date.Left.ER",
                "Time.Left.ER",
                "Visit.Disposition",
                "Blood.Transfusion.in.ED",
                "RBC.Y.N",
                "RBC.Units",
                "Plt.Y.N",
                "Plt.Units",
                "Plasma.Y.N",
                "Plasma.Units",
                "Albumin.Y.N",
                "Albumin.Units",
                "Other.Y.N",
                "Other.Units",
                "Auto.Transfusion.Y.N",
                "Auto.Transfusion.Units",
                "EncID.new")




sbkbld <- readg(sbk, blood,
                colClasses = list(character = c("ERRegNumber",
                                                "EncID.new")))
apply(sbkbld, MARGIN = 2, FUN = function(x)sum(!is.na(x)))
check <- sbkbld[!EncID.new%in% sbk$EncID.new, ]
sbk.new <- merge(sbk, sbkbld[,c(2:8,16), with = F], 
                 by = "EncID.new", all.x = T, all.y = F)
names(sbk.new)[25:31] <- c("Blood.Transfusion.in.ED","RBC.Y.N","Plt.Y.N",
                           "Plasma.Y.N", "Albumin.Y.N", 
                           "Other.Y.N", "Auto.Transfusion.Y.N")


uhn <- uhn[!is.na(NACRSRegistrationNumber), ]


msh <- msh[!duplicated(msh)]
write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.er.nophi.csv",
          row.names = F, na = "")
write.csv(sbk.new, "H:/GEMINI/Data/SBK/CIHI/sbk.er.nophi.csv",
          row.names = F, na = "")
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.er.nophi.csv",
          row.names = F, na = "")
write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.er.nophi.csv",
          row.names = F, na = "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.er.nophi.csv",
          row.names = F, na = "")
names(smh)
names(sbk)
names(uhn)
names(msh)
names(thp)


apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(thp, MARGIN = 2, FUN = function(x)sum(is.na(x)))
thp.check <- thp[!is.na(Other.Units)]


names(sbk)[10] <- "Presenting.Complaint.List"
names(sbk)[25:31] <- names(smh)[21:27]
sbk <- sbk[,c(2:21, 1), with = F]
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.er.nophi.csv",
          row.names = F, na = "")




#--------------------------- Dec 09 2016 ---------------------------------------

smh <- readg(smh.er, .er.nophi)
sbk <- readg(sbk.er, .er.nophi,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn <- readg(uhn.er, .er.nophi,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh <- readg(msh, .er.nophi)
thp <- readg(thp, .er.nophi)




sbktime <- function(x){
  dat <- x
  dat[!is.na(x)] <- paste(x[!is.na(x)],":00",sep = "")
  return(dat)
}
sbk[,`:=`(Ambulance.Arrival.Date = mdy(Ambulance.Arrival.Date),
          Triage.Date = mdy(Triage.Date),
          Registration.Date = mdy(Registration.Date),
          Date.of.Physician.Initial.Assessment = mdy(Date.of.Physician.Initial.Assessment),
          Disposition.Date = mdy(Disposition.Date),
          Date.Left.ER = myd(Date.Left.ER)
          Ambulance.Arrival.Time = sbktime(Ambulance.Arrival.Time),
          Triage.Time = sbktime(Triage.Time),
          Registration.Time = sbktime(Registration.Time),
          Time.of.Physician.Initial.Assessment = sbktime(Time.of.Physician.Initial.Assessment),
          Disposition.Time = sbktime(Disposition.Time),
          Time.Left.ER = sbktime(Time.Left.ER))]
sbk[,`:=`(Date.Left.ER = mdy(Date.Left.ER),
          Time.Left.ER = sbktime(Time.Left.ER))]

uhn$Admit.via.Ambulance[uhn$Admit.via.Ambulance=="No Ambulance"] <- "N" 

msh[,`:=`(Ambulance.Arrival.Date = ymd(Ambulance.Arrival.Date),
          Triage.Date = ymd(Triage.Date),
          Registration.Date = ymd(Registration.Date),
          Date.of.Physician.Initial.Assessment = ymd(Date.of.Physician.Initial.Assessment),
          Disposition.Date = ymd(Disposition.Date),
          Ambulance.Arrival.Time = sbktime(Ambulance.Arrival.Time),
          Triage.Time = sbktime(Triage.Time),
          Registration.Time = sbktime(Registration.Time),
          Time.of.Physician.Initial.Assessment = sbktime(Time.of.Physician.Initial.Assessment),
          Disposition.Time = sbktime(Disposition.Time))]
msh[,`:=`(Date.Left.ER = ymd(Date.Left.ER),
          Time.Left.ER = sbktime(Time.Left.ER))]

thptime <- function(x){
  dat <- x
  dat[!is.na(x)] <- paste("000", x[!is.na(x)],sep = "")
  dat[!is.na(x)] <- paste(str_sub(dat[!is.na(x)], -4,-3),":",
                          str_sub(dat[!is.na(x)], -2,-1),sep = "")
  return(dat)
}
thptime(thp$Ambulance.Arrival.Time)


thp[,`:=`(Ambulance.Arrival.Date = ymd(Ambulance.Arrival.Date),
          Triage.Date = ymd(Triage.Date),
          Registration.Date = ymd(Registration.Date),
          Date.of.Physician.Initial.Assessment = ymd(Date.of.Physician.Initial.Assessment),
          Disposition.Date = ymd(Disposition.Date),
          Ambulance.Arrival.Time = thptime(Ambulance.Arrival.Time),
          Triage.Time = thptime(Triage.Time),
          Registration.Time = thptime(Registration.Time),
          Time.of.Physician.Initial.Assessment = thptime(Time.of.Physician.Initial.Assessment),
          Disposition.Time = thptime(Disposition.Time))]
thp[,`:=`(Date.Left.ER = ymd(Date.Left.ER),
          Time.Left.ER = thptime(Time.Left.ER))]
smh[,`:=`(Ambulance.Arrival.Date = ymd(Ambulance.Arrival.Date),
          Triage.Date = ymd(Triage.Date),
          Registration.Date = ymd(Registration.Date),
          Date.of.Physician.Initial.Assessment = ymd(Date.of.Physician.Initial.Assessment),
          Disposition.Date = ymd(Disposition.Date),
          Ambulance.Arrival.Time = thptime(Ambulance.Arrival.Time),
          Triage.Time = thptime(Triage.Time),
          Registration.Time = thptime(Registration.Time),
          Time.of.Physician.Initial.Assessment = thptime(Time.of.Physician.Initial.Assessment),
          Disposition.Time = thptime(Disposition.Time))]
smh[,`:=`(Date.Left.ER = ymd(Date.Left.ER),
          Time.Left.ER = thptime(Time.Left.ER))]
write.csv(smh, "H:/GEMINI/Data/SMH/CIHI/smh.er.nophi.csv",
          row.names = F, na = "")
write.csv(sbk, "H:/GEMINI/Data/SBK/CIHI/sbk.er.nophi.csv",
          row.names = F, na = "")
write.csv(uhn, "H:/GEMINI/Data/UHN/CIHI/uhn.er.nophi.csv",
          row.names = F, na = "")
write.csv(msh, "H:/GEMINI/Data/MSH/CIHI/msh.er.nophi.csv",
          row.names = F, na = "")
write.csv(thp, "H:/GEMINI/Data/THP/CIHI/thp.er.nophi.csv",
          row.names = F, na = "")


ymd_hm(paste(check$Date.of.Physician.Initial.Assessment, 
              check$Time.of.Physician.Initial.Assessment, sep = " ")) - 
  ymd_hm(paste(check$Triage.Date, 
                check$Triage.Time, sep = " "))




smh <- readg(smh.er, .er.nophi)
sbk <- readg(sbk.er, .er.nophi,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
uhn <- readg(uhn.er, .er.nophi,
             colClasses = list(character = c("NACRSRegistrationNumber",
                                             "EncID.new")))
msh <- readg(msh, .er.nophi)
thp <- readg(thp, .er.nophi)


apply(smh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(sbk, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(uhn, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(msh, MARGIN = 2, FUN = function(x)sum(is.na(x)))
apply(thp, MARGIN = 2, FUN = function(x)sum(is.na(x)))
