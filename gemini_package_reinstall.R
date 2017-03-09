#compile gemini library and reinstall it
library(roxygen2)
library(devtools)
setwd("C:/Users/guoyi/Documents/Code/gemini/gemini")
document()
setwd("..")
install("gemini")
library(gemini)

