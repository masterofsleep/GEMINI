# ------------------------- Abdomen Xray Feasibility ---------------------------
# ------------------------------- 2017-05-24 -----------------------------------
library(gemini)
lib.pa()
find_abdomen_xr <- function(){
  smh.xr <- readg(smh, xray)
  smh.abx <- smh.xr[body_part_mne=="ABDOMEN"&!proc_desc_long%in%c("ABDOMEN 1V",
                                                       "ABDOMEN 2+V")]
  sbk.rad <- readg(sbk, rad.csv)
  sbk.abx.code <- c("AB2C1X", "AB2C2X", "AB3C1X", "AB3C2X", 
                    "ABDO2X", "ABDO3X", "ABDODX", "ABDOFX",
                    "ABDOOX", "ABDOUX", "CX+A2X", "CX2AAX",
                    "CXA+AO", "CXA+AX", "CXAA1X", "CXAA2X",
                    "CXRA1X")
  sbk.abx <- sbk.rad[Test.Code%in%sbk.abx.code]
  
  uhn.rad <- rbind(readg(UHN, rad_ip),
                   readg(UHN, rad_er))
  uhn.abx <- uhn.rad[startsWith(ProcedureName, "XR Abdomen")]
  msh.rad <- rbind(readg(msh, rad_er),
                   readg(msh, rad_ip))
  msh.abx <- msh.rad[startsWith(ProcedureName, "XR Abdomen")]
  names(smh.abx)
  names(sbk.abx)
  names(uhn.abx)
  names(msh.abx)
  
  abdomen.xr <- rbind(smh.abx[,.(EncID.new, Test.Name = proc_desc_long,
                                 Ordered.DtTm = ymd_h(ord_for_dtime),
                                 Performed.DtTm = ymd_h(proc_dtime),
                                 Result = result,
                                 Impression = impression,
                                 Site = "SMH"
                                 )],
                      sbk.abx[,.(EncID.new, Test.Name,
                                 Ordered.DtTm,
                                 Performed.DtTm,
                                 Result = Results,
                                 Site = "SBK")],
                      uhn.abx[,.(EncID.new, Test.Name = ProcedureName,
                                 Ordered.DtTm = mdy_hm(OrderDateTime),
                                 Performed.DtTm = mdy_hm(ScanStartDateTime),
                                 Result = ReportText,
                                 Site = "UHN")],
                      msh.abx[,.(EncID.new, Test.Name = ProcedureName,
                                 Ordered.DtTm = ymd_hm(OrderDateTime),
                                 Performed.DtTm = ymd_hm(ScanStartDateTime),
                                 Result = ReportText,
                                 Site = "MSH")], fill = T)
  return(abdomen.xr)
}

abdomen.xr <- find_abdomen_xr()
dad <- fread("H:/GEMINI/Results/DesignPaper/design.paper.dad.v4.csv")
dad[, EncID.new:=as.character(EncID.new)]
abdomen.xr <- merge(abdomen.xr, dad[, .(EncID.new, Admit.Date, Admit.Time, 
                                        SCU.adm)],
                    by = "EncID.new")
abdomen.xr.post.adm <- abdomen.xr[Ordered.DtTm>=ymd_hm(paste(Admit.Date, Admit.Time))&
                                    SCU.adm==F]

find_all_ct <- function(){
  smh.ct <- readg(smh, ct)
  sbk.rad <- readg(sbk, rad.csv)
  map.sbk <- readxl::read_excel("H:/GEMINI/Results/DesignPaper/rad.freq.table.new_AV.xlsx", sheet = 1)
  sbk.rad <- merge(sbk.rad, 
                   map.sbk[,c("Test.Name", "Test.Type", 
                              "Interventional Procedure")], 
                   by = "Test.Name",
                   all.x = T, all.y = F)
  sbk.ct <- sbk.rad[Test.Type==3]
  uhn.rad <- rbind(readg(UHN, rad_ip),
                   readg(UHN, rad_er))
  
  uhn.ct <- uhn.rad[startsWith(ProcedureName, "CT")]
  msh.rad <- rbind(readg(msh, rad_er),
                   readg(msh, rad_ip))
  msh.ct <- msh.rad[startsWith(ProcedureName, "CT")]
  names(smh.ct)
  names(sbk.ct)
  names(uhn.ct)
  names(msh.ct)
  
  ct <- rbind(smh.ct[,.(EncID.new, Test.Name = proc_desc_long,
                                 Ordered.DtTm = ymd_h(ord_for_dtime),
                                 Performed.DtTm = ymd_h(proc_dtime),
                                 Result = result,
                                 Impression = impression,
                                 Site = "SMH"
  )],
  sbk.ct[,.(EncID.new, Test.Name,
             Ordered.DtTm,
             Performed.DtTm,
             Result = Results,
             Site = "SBK")],
  uhn.ct[,.(EncID.new, Test.Name = ProcedureName,
             Ordered.DtTm = mdy_hm(OrderDateTime),
             Performed.DtTm = mdy_hm(ScanStartDateTime),
             Result = ReportText,
             Site = "UHN")],
  msh.ct[,.(EncID.new, Test.Name = ProcedureName,
             Ordered.DtTm = ymd_hm(OrderDateTime),
             Performed.DtTm = ymd_hm(ScanStartDateTime),
             Result = ReportText,
             Site = "MSH")], fill = T)
  return(ct)
}
ct <- find_all_ct()
names(ct)[2:6] <- paste("CT.", names(ct)[2:6], sep = "")

res <- NULL
for(i in 1:nrow(abdomen.xr.post.adm)){
  first.ct <- ct[EncID.new==abdomen.xr.post.adm[i, EncID.new]&CT.Ordered.DtTm>=
                   abdomen.xr.post.adm[i, Ordered.DtTm]][order(CT.Ordered.DtTm)][1]
  df <- cbind(abdomen.xr.post.adm[i], first.ct[, .(CT.Test.Name,
                                                   CT.Ordered.DtTm, 
                                                   CT.Performed.DtTm,
                                                   CT.Result,
                                                   CT.Impression)])
  res <- rbind(res, df, fill = T)
}

fwrite(res, "H:/GEMINI/Results/abdomen_xray/abx_ct.csv")
