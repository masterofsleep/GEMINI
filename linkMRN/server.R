library(shiny)
library(readxl)
library(data.table)
options(shiny.maxRequestSize=100*1024^2)
shinyServer(function(input, output) {
  datasetInput <- reactive({
    
    # read in the link file
    # Please edit the path below, if EncID.new is not in the file,
    # change the second link$EncID.new to link$`column name of encounter ID`
    link <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/SMH.LINKLIST_NEWHASH.csv")
    link$EncID.new <- paste(input$site, link$EncID.new, sep = "")
    # read in the dad file for Admiting&Discharging Date Time
    # Please edit the path below, if EncID.new is not in the file,
    # change the second dad$EncID.new to dad$`column name of encounter ID`
    dad <- fread("R:/GEMINI/_RESTORE/SMH/CIHI/smh.ip_dad.nophi.csv")
    dad$EncID.new <- paste(input$site, dad$EncID.new, sep = "")
    
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    ext = tools::file_ext(inFile1$name)
    if (ext=="csv"){
      dat <- fread(inFile1$datapath, colClasses = list(character = c("EncID.new")))
    } else {
      file.rename(inFile1$datapath,
                  paste(inFile1$datapath, ".xlsx", sep=""))
      dat <- read_excel(paste(inFile1$datapath, ".xlsx", sep=""), sheet = as.numeric(input$sheetindex))
      dat$EncID.new <- as.character(dat$EncID.new)
    }
    if(nchar(dat$EncID.new)[1]==6){
      dat$EncID.new <- paste(input$site, dat$EncID.new, sep = "")
    }
    res1 <- 
      merge(link[,.(EncID.new, MRN)], dat,
            # edit the column names below to be the column names for MRN and EncID.new
            # in the link file
            by = "EncID.new",
            all.x = F, all.y = T)
    if(input$vartoadd=="mrndt"){
      res2 <- merge(res1,
          # edit the column names in the parenthesis to be the column names for 
          # admit and discharge date in the dad file
          dad[,.(EncID.new, ADMITDATE, DISCHARGEDATE)], by = "EncID.new",
          all.x = T, all.y = F)
    }else if(input$vartoadd=="mrn"){
      res2 <- res1
    }
    res2
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