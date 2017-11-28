library(shiny)
library(DT)

fluidPage(titlePanel("DaKaMon Importer"),
          
          tabsetPanel(
            tabPanel("FoI generation",
                     sidebarLayout(
                       sidebarPanel(
                         fileInput("csvFileFoI", "Select a FoI file for upload."),
                         checkboxInput("headerFoI", "Header", TRUE),
                         fluidRow(column(
                           6,
                           textInput(
                             "sepFoI",
                             "Column separator:",
                             value = ";",
                             width = "80%"
                           )
                         ),
                         column(
                           6,
                           textInput(
                             "decFoI",
                             "Decimal separator:",
                             value = ".",
                             width = "80%"
                           )
                         )),
                         textInput("exclRowFoI", "Exclude rows:"),
                         textInput("exclColFoI", "Exclude columns:"),
                         actionButton("goFoI", "store in DB"),
                         width = 2
                       ),
                       mainPanel(dataTableOutput('tableFoI'))
                     )),
            
            tabPanel("Data upload",
                     sidebarLayout(
                       sidebarPanel(
                         fileInput("csvFileData", "Select a data file for upload."),
                         checkboxInput("headerData", "Header", TRUE),
                         fluidRow(column(
                           6,
                           textInput(
                             "sepData",
                             "Column separator:",
                             value = ";",
                             width = "80%"
                           )
                         ),
                         column(
                           6,
                           textInput(
                             "decData",
                             "Decimal separator:",
                             value = ".",
                             width = "80%"
                           )
                         )),
                         textInput("exclRowData", "Exclude rows:"),
                         textInput("exclColData", "Exclude columns:"),
                         actionButton("goData", "store in DB"),
                         width = 2
                       ),
                       mainPanel(dataTableOutput('tableData'))
                     ))
          ))