library(shiny)
library(DT)

function(input, output) {
  ## FoI logic
  # toAdd: validate FoIs IDs?
  # toAdd: restrict hierarchie?
  # toAdd: validate UoM?
  # add Button "check DB consistency" -> list known vs new variables; matching vs non-matching uom ( -> hottable to fix?) -> manually update csv file
  inCSVFoI <- reactive({
    inFile <- input$csvFileFoI
    if (is.null(inFile))
      return(NULL)
    read.csv(
      inFile$datapath,
      header = input$headerFoI,
      sep = input$sepFoI,
      dec = input$decFoI
    )
  })
  
  inclRowFoI <- reactive({
    if (is.null(inCSVFoI()))
      return(numeric())
    exclText <- input$exclRowFoI
    if (is.null(exclText))
      return(1:ncol(inCSVFoI()))
    exclNum <-
      as.numeric(strsplit(exclText, fixed = T, split = ",")[[1]])
    ! c(1:nrow(inCSVFoI())) %in% exclNum[!is.na(exclNum)]
  })
  
  inclColFoI <- reactive({
    if (is.null(inCSVFoI()))
      return(numeric())
    exclText <- input$exclColFoI
    if (is.null(exclText))
      return(1:ncol(inCSVFoI()))
    exclNum <-
      as.numeric(strsplit(exclText, fixed = T, split = ",")[[1]])
    ! c(1:ncol(inCSVFoI())) %in% exclNum[!is.na(exclNum)]
  })
  
  output$tableFoI <-
    DT::renderDataTable(inCSVFoI()[inclRowFoI(), inclColFoI(), drop = F],
                        filter = "top",
                        options = list(paging = FALSE,
                                       dom = 'Bfrtip'))
  
  # data tab logic:
  # toAdd: validate UoM?
  # add Button "check DB consistency" -> list known vs new variables; matching vs non-matching uom ( -> hottable to fix?) -> manually update csv file
  inCSVData <- reactive({
    inFile <- input$csvFileData
    if (is.null(inFile))
      return(NULL)
    read.csv(
      inFile$datapath,
      header = input$headerData,
      sep = input$sepData,
      dec = input$decData
    )
  })
  
  inclRowData <- reactive({
    if (is.null(inCSVData()))
      return(numeric())
    exclText <- input$exclRowData
    if (is.null(exclText))
      return(1:ncol(inCSVData()))
    exclNum <-
      as.numeric(strsplit(exclText, fixed = T, split = ",")[[1]])
    ! c(1:nrow(inCSVData())) %in% exclNum[!is.na(exclNum)]
  })
  
  inclColData <- reactive({
    if (is.null(inCSVData()))
      return(numeric())
    exclText <- input$exclColData
    if (is.null(exclText))
      return(1:ncol(inCSVData()))
    exclNum <-
      as.numeric(strsplit(exclText, fixed = T, split = ",")[[1]])
    ! c(1:ncol(inCSVData())) %in% exclNum[!is.na(exclNum)]
  })
  
  output$tableData <-
    DT::renderDataTable(inCSVData()[inclRowData(), inclColData(), drop = F],
                        filter = "top",
                        options = list(paging = FALSE))
}