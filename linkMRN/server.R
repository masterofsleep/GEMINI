library(shiny)
library(readxl)
library(data.table)
options(shiny.maxRequestSize=100*1024^2)
shinyServer(function(input, output) {
  datasetInput <- reactive({
    link <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/SMH.LINKLIST_NEWHASH.csv")
    link$EncID.new <- paste(input$site, link$EncID.new, sep = "")
    dad <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/smh.ip_dad.nophi.csv")
    dad$EncID.new <- paste(input$site, dad$EncID.new, sep = "")
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    if (input$filetype1=="xlsx"){
      file.rename(inFile1$datapath,
                  paste(inFile1$datapath, ".xlsx", sep=""))
      dat <- read_excel(paste(inFile1$datapath, ".xlsx", sep=""), sheet = as.numeric(input$sheetindex))
      dat$EncID.new <- as.character(dat$EncID.new)
    } else {
      dat <- fread(inFile1$datapath, colClasses = list(character = c("EncID.new")))
    }
    if(nchar(dat$EncID.new)==6){
      dat$EncID.new <- paste(input$site, dat$EncID.new, sep = "")
    }
    res1 <- merge(dat, link[,.(EncID.new, MRN)], by = "EncID.new",
                    all.x = T, all.y = F)
    merge(res1, dad[,.(EncID.new, ADMITDATE, DISCHARGEDATE)], by = "EncID.new",
          all.x = T, all.y = F)
  })
  output$table <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  output$downloadData <- downloadHandler(
    filename = function() {
    paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = F)
    }
  )

})