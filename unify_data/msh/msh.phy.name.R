# ==============================================================================
# =====================  msh physician validate  ===============================
# =======================    March 1 2017   ====================================
library(gemini)
lib.pa()

setwd("R:/GEMINI/_RESTORE/MSH/Physician Names")
files <- list.files()
files
gim.names <- fread(files[5], select = c("Last", "First"))
adm <- fread(files[1])
adm[(paste(ADMITTING_PHYS_FIRST_NAME, ADMITTING_PHYS_LAST_NAME)%in%
      paste(gim.names$First, gim.names$Last)|
      paste(DISCHARGE_ATTENDING_FIRST, DISCHARGE_ATTENDING_LAST)%in%
      paste(gim.names$First, gim.names$Last))]
adm.only <- readxl::read_excel(files[2]) %>% data.table
adm.only[(paste(ADMITTING_PHYS_FIRST_NAME, ADMITTING_PHYS_LAST_NAME)%in%
                 paste(gim.names$First, gim.names$Last)|
                 paste(DISCHARGE_ATTENDING_FIRST, DISCHARGE_ATTENDING_LAST)%in%
                 paste(gim.names$First, gim.names$Last))]
dad <- fread(files[3])
dad.only <- readxl::read_excel(files[4]) %>% data.table
dad[tolower(paste(Most.Responsible.Physician.First.Name, Most.Responsible.Physician.Last.Name))%in%
      tolower(paste(gim.names$First, gim.names$Last))]
dad.only[tolower(paste(Most.Responsible.Physician.First.Name, Most.Responsible.Physician.Last.Name))%in%
           tolower(paste(gim.names$First, gim.names$Last))]
adm.phy <- adm[, paste(ADMITTING_PHYS_FIRST_NAME, ADMITTING_PHYS_LAST_NAME)]
dis.phy <- adm[, paste(DISCHARGE_ATTENDING_FIRST, DISCHARGE_ATTENDING_LAST)]
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

mrp <- dad[, sapply(tolower(paste(Most.Responsible.Physician.First.Name, Most.Responsible.Physician.Last.Name)), 
                     simpleCap)]
msh.phy.name <- c(adm.phy, dis.phy, mrp) %>% unique
msh.phy.name[!msh.phy.name%in%paste(gim.names$First, gim.names$Last)]
