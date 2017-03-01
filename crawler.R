library(rvest)
library(XML)
url <- "http://www.recruitingsite.com/csbsites/stmichaels/JobDescription.asp?CategoryCode=15121&JobNumber=798094&JobTitle=PatientSafetySpecialist"
web <- read_html(url)
web %>% html_nodes("p:nth-child(2) .WSTM_JD_Value") %>%
  html_text()
