# -------------------------------- Radiology -----------------------------------

smh.ct <- readg(smh, ct)
smh.mri <- readg(smh, mri)
smh.us <- readg(smh.us, us)
smh.xray <- readg(smh, xray)
smh.ir <- fread("H:/GEMINI/Results/DesignPaper/smh.rad.freq.csv")
smh.xray <- smh.xray[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
smh.ct<- smh.ct[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
smh.mri <- smh.mri[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
smh.us <- smh.us[!proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
smh.ir <- smh.rad[proc_desc_long%in%smh.ir[Interventional==1, Test.Name], EncID.new]
map.sbk <- readxl::read_excel("H:/GEMINI/Results/DesignPaper/rad.freq.table.new_AV.xlsx", sheet = 1)
sbk.rad <- readg(sbk.rad, rad.csv)

sum(sbk.rad$Test.Name%in%map.sbk$Test.Name)
sbk.rad <- merge(sbk.rad, 
                 map.sbk[,c("Test.Name", "Test.Type", 
                            "Interventional Procedure")], 
                 by = "Test.Name",
                 all.x = T, all.y = F)
sbk.us <- sbk.rad[Test.Type==2&is.na(`Interventional Procedure`), EncID.new]
sbk.xray <- sbk.rad[Test.Type==1&is.na(`Interventional Procedure`), EncID.new]
sbk.ct <- sbk.rad[Test.Type==3&is.na(`Interventional Procedure`), EncID.new]
sbk.mri <- sbk.rad[Test.Type==4&is.na(`Interventional Procedure`), EncID.new]
sbk.ir <- sbk.rad[`Interventional Procedure`==1, EncID.new]
uhn.radip <- readg(uhn, rad_ip)
uhn.rader <- readg(uhn, rad_er)
uhn.rad <- rbind(uhn.radip, uhn.rader)
map.uhn <- 
  readxl::read_excel("H:/GEMINI/Results/DesignPaper/rad.freq.table.new_AV.xlsx", sheet = 2)%>%
  data.table
uhn.ir.names <- map.uhn[Interventional==1,Test.Name]
uhn.ct <- uhn.rad[str_sub(ProcedureName,1,2) =="CT"&
                    !ProcedureName%in%uhn.ir.names, EncID.new]
uhn.us <- uhn.rad[str_sub(ProcedureName,1,2) =="US"&
                    !ProcedureName%in%uhn.ir.names, EncID.new]
uhn.xray <- uhn.rad[str_sub(ProcedureName,1,2) =="XR"&
                      !ProcedureName%in%uhn.ir.names, EncID.new]
uhn.mri <- uhn.rad[str_sub(ProcedureName,1,3) =="MRI"&
                     !ProcedureName%in%uhn.ir.names, EncID.new]
uhn.ir <- uhn.rad[ProcedureName%in%uhn.ir.names, EncID.new]
msh.rader <- readg(msh, rad_er)
msh.radip <- readg(msh, rad_ip)
msh.rad <- rbind(msh.rader, msh.radip)
msh.ct <- msh.rad[str_sub(ProcedureName,1,2) =="CT"&
                    !ProcedureName%in%uhn.ir.names, EncID.new]
msh.us <- msh.rad[str_sub(ProcedureName,1,2) =="US"&
                    !ProcedureName%in%uhn.ir.names, EncID.new]
msh.xray <- msh.rad[str_sub(ProcedureName,1,2) =="XR"&
                      !ProcedureName%in%uhn.ir.names, EncID.new]
msh.mri <- msh.rad[str_sub(ProcedureName,1,3) =="MRI"&
                     !ProcedureName%in%uhn.ir.names, EncID.new]
msh.ir <- msh.rad[ProcedureName%in%uhn.ir.names, EncID.new]
ct.enc <- c(smh.ct, sbk.ct, uhn.ct, msh.ct)
us.enc <- c(smh.us, sbk.us, uhn.us, msh.us)
xray.enc <- c(smh.xray, sbk.xray, uhn.xray, msh.xray)
mri.enc <- c(smh.mri, sbk.mri, uhn.mri, msh.mri)
ir.enc <- c(smh.ir, sbk.ir, uhn.ir, msh.ir)


ctmrius <- table(c(ct.enc, us.enc, mri.enc)) %>% data.table
names(ctmrius) <- c("EncID.new", "N.rad")
fwrite(ctmrius, "C:/Users/guoyi/Desktop/to.adm/n.ctmrius.csv")