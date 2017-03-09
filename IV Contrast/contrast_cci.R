# ---------------------- validate CCI code for Contrast CT ---------------------
# --------------------------  march 9 2017 -------------------------------------
library(gemini)
lib.pa()
rm(list = ls())

# find all the contrast ct in syncope cohort
setwd("R:/GEMINI-SYNCOPE/imaging")
files <- list.files()
files
smh <- fread(files[2])
sbk <- fread(files[1])
uhn <- fread(files[3])
contrast.enc <- c(smh[Contrast_Study=="y", EncID.new],
                  sbk[Contrast_Study=="y", EncID.new],
                  uhn[Contrast_Study=="y", EncID.new])
# find all the contrast ct in watson cohort
setwd("R:/GEMINI-SYNCOPE/watson")
files <- list.files()
files
library(readxl)
smh.new <- read_excel(files[10]) %>% data.table
sbk.new <- read_excel(files[4]) %>% data.table
uhn.new <- read_excel(files[15]) %>% data.table

contrast.enc.new <- c(smh.new[`Contrast (y, n)`=="y", EncID.new], 
                      sbk.new[`Contrast (y, n)`=="y", EncID.new], 
                      uhn.new[`Contrast (y, n)`=="y", EncID.new])
contrast.enc.all <- c(contrast.enc, contrast.enc.new)


ip.int <- readg(gim, ip.int, 
                colClasses = list(character = "EncID.new"))[
                  startwith.any(EncID.new, c("11", "12", "13"))]
er.int <- readg(gim, er.int, 
                colClasses = list(character = "EncID.new"))[
                  startwith.any(EncID.new, c("11", "12", "13"))]

cci.code <- readxl::read_excel("H:/GEMINI/Feasibility/Contrast/Contrast CCI Codes.xlsx", sheet = 1)
cci.code$Description <- str_sub(cci.code$`CCI Code`, 9, -1)
cci.code$`CCI Code` <- str_sub(cci.code$`CCI Code`, 1, 7)
ip.cec <- ip.int[Intervention.Code%in%cci.code$`CCI Code`, EncID.new]
er.cec <- er.int[Occurrence.Type%in%cci.code$`CCI Code`, EncID.new]
contrast.enhanced <- c(ip.cec, er.cec)

compare.sets(contrast.enc.all, contrast.enhanced)

contrast.in.int <- contrast.enc.all%in%contrast.enhanced
conpare <- data.table(EncID.new = contrast.enc.all, contrast.in.int) %>% unique %>%
  data.table
table(conpare$contrast.in.int, str_sub(conpare$EncID.new, 1, 2))[2, ]/
  table(str_sub(conpare$EncID.new, 1, 2))
