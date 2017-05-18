library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Add MRN to GEMINI data"),
  sidebarPanel(
    radioButtons('site', 'Select Site',
                 c(SMH='11',
                   SBK='12',
                   UHN='13',
                   MSH='14',
                   THP='15'),
                 ''),
    fileInput('file1', 'Choose File to be processed (adding MRN)',
              accept=c('text/csv', '.xlsx', '.csv')),
    # radioButtons('filetype1', 'Select File Type',
    #             c(csv='csv',
    #               excel='xlsx'),
    #             'csv'),
    radioButtons('sheetindex', 'Select Excel Sheet Index',
                 c(sheet1=1,
                   sheet2=2,
                   sheet3=3,
                   sheet4=4),
                 1),

    tags$hr(),
    numericInput("obs", "Number of rows in preview of merged file:", 10)
  ),

  
  mainPanel(
    tableOutput('table'),
    downloadButton('downloadData', 'Download')
  )
))