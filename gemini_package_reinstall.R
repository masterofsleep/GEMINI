#compile gemini library and reinstall it
library(roxygen2)
library(devtools)
setwd("H:/GEMINI/Code/gemini")
document()
setwd("..")
install("gemini")
library(gemini)
