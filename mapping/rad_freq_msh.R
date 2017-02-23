# =========== Frequency table for Radiology test names =========================
library(gemini)
lib.pa()

msh.er <- readg(msh, rad_er)
msh.ip <- readg(msh, rad_ip)
msh <- rbind(msh.er, msh.ip)


msh.freq <- data.table(table(msh$ProcedureName))
fwrite(msh.freq, "H:/GEMINI/Results/Shortadm/rad.msh.freq.csv", showProgress = T)
