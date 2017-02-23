smh.phar <- readg(smh, phar)
sbk.phar <- readg(sbk, phar)
uhn.phar <- readg(uhn, phar.nophi)
drm.din <- readxl::read_excel("H:/GEMINI/Feasibility/DRM/FINALDINLIST.xlsx")
table(smh.phar$route)
table(smh.phar$doseUnit)
smh.phar[route=="34U", 
         `:=`(doseAmount = paste(doseAmount, doseUnit, route, sep = " "),
              doseUnit = CHECK
         )]
shift.column <- function(x, index1, index2){
  x <- data.frame(x)
  x[,index1] <- x[,index2]
  x[,index2[-1]] <- NA
  return(data.table(x))
}

#see which are the names with shiftting problem
smh.phar[route=="34U"]
shift.column(smh.phar[route=="34U"], c(17:23), c(19:25))
smh.phar[route=="34U"]
smh.phar[route =="CHECK"]
smh.phar[route=="mg"]
smh.phar[route=="unit"]
# THESE ARE ALL THAT CAUSED BY COMMA IN DOSE AMOUNT
# 34U, CHECK, mg, unit
smh.phar[doseUnit=="0"]

smh.phar[str_detect(str_sub(doseUnit,1,1), "[0-9]")] -> check

smh.antibio <- smh.phar[din%in%drm.din$`FINAL DINS`]
table(smh.antibio$route)
smh.antibio.route <- data.table(table(smh.antibio[!route%in%c("34U", "CHECK", "mg", "unit"), route]))
fwrite(smh.antibio.route, "H:/GEMINI/Results/DRM/smh.antibio.route.csv")

#sbk
table(sbk.phar$route)
sbk.antibio <- sbk.phar[ndc_din%in%drm.din$`FINAL DINS`]
sbk.antibio.route <- data.table(table(sbk.antibio$route))
fwrite(sbk.antibio.route, "H:/GEMINI/Results/DRM/sbk.antibio.route.csv")

#uhn
uhn.antibio <- uhn.phar[DIN%in%drm.din$`FINAL DINS`]
uhn.antibio.route <- data.table(table(uhn.antibio$Route_Code))
fwrite(uhn.antibio.route, "H:/GEMINI/Results/DRM/uhn.antibio.route.csv")


#msh
msh.phar <- fread("R:/GEMINI/_RESTORE/MSH/Phar/msh.phar.nophi.csv")
msh.antibio <- msh.phar[DIN%in%drm.din$`FINAL DINS`]
msh.antibio.route <- data.table(table(msh.antibio$ROUTE))
fwrite(msh.antibio.route, "H:/GEMINI/Results/DRM/msh.antibio.route.csv")