#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(httr)
library(XML)
library(rgdal)
library(htmltools)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  stDf <- reactiveValues()
  
  m <- leaflet()  %>%
    addProviderTiles('OpenStreetMap.BlackAndWhite',
                     options = providerTileOptions(noWrap = TRUE))
  output$map <- renderLeaflet({ 
  
    # input <- NULL
    # input$time <- 2013
    # input$stat <- "45412-01-02-4"
    
    if(is.null(stDf[[input$time]])) {
      # "<wps:Execute xmlns:wps=\"http://www.opengis.net/wps/2.0\" xmlns:ows=\"http://www.opengis.net/ows/2.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.opengis.net/wps/2.0 http://schemas.opengis.net/wps/2.0/wps.xsd\" service=\"WPS\" version=\"2.0.0\" response=\"raw\" mode=\"sync\">  <ows:Identifier>org.n52.wps.extension.FusionAlgorithm</ows:Identifier>  <wps:Input id=\"data\">    <wps:Reference xlink:href=\"http://sg.geodatenzentrum.de/wfs_vg250?request=GetFeature&amp;service=wfs&amp;version=1.0.0&amp;TypeName=vg250:vg250_krs&amp;maxFeatures=15\" mimeType=\"text/xml\" schema=\"http://schemas.opengis.net/gml/2.1.2/feature.xsd\" />  </wps:Input>  <wps:Input id=\"year\">    <wps:Data>      <wps:LiteralValue>",
      time <- Sys.time()
      xmlBody <- paste0("<wps:Execute xmlns:wps=\"http://www.opengis.net/wps/2.0\" xmlns:ows=\"http://www.opengis.net/ows/2.0\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.opengis.net/wps/2.0 http://schemas.opengis.net/wps/2.0/wps.xsd\" service=\"WPS\" version=\"2.0.0\" response=\"raw\" mode=\"sync\">  <ows:Identifier>org.n52.wps.extension.FusionAlgorithm</ows:Identifier>  <wps:Input id=\"data\">    <wps:Reference xlink:href=\"http://sg.geodatenzentrum.de/wfs_vg250?request=GetFeature&amp;service=wfs&amp;version=1.0.0&amp;TypeName=vg250:vg250_krs\" mimeType=\"text/xml\" schema=\"http://schemas.opengis.net/gml/2.1.2/feature.xsd\" />  </wps:Input>  <wps:Input id=\"year\">    <wps:Data>      <wps:LiteralValue>",
                        input$time,
                        "</wps:LiteralValue>    </wps:Data>  </wps:Input>  <wps:Input id=\"code\">    <wps:Data>      <wps:LiteralValue>",
                        "45412-01-02-4",
                        "</wps:LiteralValue>    </wps:Data>  </wps:Input>  <wps:Output id=\"result\" transmission=\"value\" mimeType=\"text/xml\" schema=\"http://schemas.opengis.net/gml/2.1.2/feature.xsd\"  /></wps:Execute>")
      
      res <- POST("http://tamis.dev.52north.org/wps/WebProcessingService?service=WPS&request=DescribeProcess&version=2.0.0&identifier=org.n52.wps.extension.FusionAlgorithm", 
                  body = xmlBody)
      
      cat("WPS:", Sys.time()-time, "\n") # 33 sec
      time <- Sys.time()
      
      tf <- tempfile(fileext = ".gml")
      write(rawToChar(res$content), file = tf)
      
      cat("Write file:", Sys.time()-time, "\n") # 0.6 sec
      time <- Sys.time()
      
      spDf <- readOGR(tf)
      
      cat("Read OGR:", Sys.time()-time, "\n") # 21 sec
      
      spDf@proj4string <- CRS("+init=epsg:25832")
      spDf <- spTransform(spDf, CRS("+init=epsg:4326"))
      
      spDf@data$nspskm <- log(1+spDf@data$Gaesteuebernachtungen/(spDf@data$Shape_Area/1e6))
      spDf@data$Gaesteuebernachtungen <- spDf@data$Gaesteuebernachtungen/1e3
      spDf@data$Angebotene_Gaestebetten <- spDf@data$Angebotene_Gaestebetten/1e3
      
      
      stDf[[input$time]] <- spDf
    }
  
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = stDf[[input$time]]@data[[input$stat]]
    )

    uom <- switch(input$stat,
                  Gaesteuebernachtungen = "k nights spent",
                  Angebotene_Gaestebetten = "k beds",
                  nspskm = "nights spent / sqkm")
    if (input$stat != "nspskm") {
      lab <- sapply(round(stDf[[input$time]]@data[[input$stat]], 0),
                    function(x) HTML(paste(x, uom)))
    } else {
      lab <- sapply(round(expm1(stDf[[input$time]]@data[[input$stat]]), 0),
                    function(x) HTML(paste(x, uom)))
    }
    
    
    m <- addPolygons(m, stroke=F, opacity = 0.9, fillOpacity = 0.8,
                     col = ~pal(stDf[[input$time]]@data[[input$stat]]),
                     data = stDf[[input$time]], label = lab,
                     highlight = highlightOptions(
                       stroke=TRUE, 
                       weight = 5,
                       color = "#666",
                       dashArray = "",
                       fillOpacity = 0.7,
                       bringToFront = TRUE))
    if (input$stat != "nspskm") {
      m <- addLegend(m, "bottomright", pal = pal, values = stDf[[input$time]]@data[[input$stat]],
                     title = uom, opacity = 1)
    } else { 
      ##
      values = stDf[[input$time]]@data[[input$stat]]
      labFormat <- labelFormat()
      cuts = pretty(values, n = 7)
      n = length(cuts)
      r = range(values, na.rm = TRUE)
      cuts = cuts[cuts >= r[1] & cuts <= r[2]]
      n = length(cuts)
      p = (cuts - r[1])/(r[2] - r[1])
      extra = list(p_1 = p[1], p_n = p[n])
      p = c("", paste0(100 * p, "%"), "")
      colors = pal(c(r[1], cuts, r[2]))
      # colors = paste(colors, p, sep = " ", collapse = ", ")
      labels = labFormat(type = "numeric", round(expm1(c(r[1], cuts, r[2])), 0))
      cat(length(colors), "\n")
      cat(length(labels), "\n")
      ##
      
      m <- addLegend(m, "bottomright", colors = colors,
                     labels = labels,
                     title = uom, opacity = 1)
    }
  
    m
  })
  

  
})
