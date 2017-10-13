#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(sf)
library(shiny)
library(leaflet)
library(httr)
library(XML)
library(rgdal)
library(htmltools)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  stDf <- reactiveValues()
  
  output$map <- renderLeaflet({ 
  
    # input <- NULL
    # input$time <- 2013
    # input$stat <- "Gaesteuebernachtungen"
    
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
      
      
      sfdf <- read_sf(tf)
      
      sfdf <- st_set_crs(sfdf, 25832)
      
      sfdf <- st_transform(sfdf, crs = 4326)
      
      sfdf$nspskm <- log(1+sfdf$Gaesteuebernachtungen/(sfdf$Shape_Area/1e6))
      sfdf$Gaesteuebernachtungen <- sfdf$Gaesteuebernachtungen/1e3
      sfdf$Angebotene_Gaestebetten <- sfdf$Angebotene_Gaestebetten/1e3
      
      
      stDf[[input$time]] <- sfdf
    }
  
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = stDf[[input$time]][[input$stat]]
    )

    uom <- switch(input$stat,
                  Gaesteuebernachtungen = "k nights spent",
                  Angebotene_Gaestebetten = "k beds",
                  nspskm = "nights spent / sqkm")
    if (input$stat != "nspskm") {
      lab <- sapply(round(stDf[[input$time]][[input$stat]], 0),
                    function(x) HTML(paste(x, uom)))
    } else {
      lab <- sapply(round(expm1(stDf[[input$time]][[input$stat]]), 0),
                    function(x) HTML(paste(x, uom)))
    }
    
    m <- leaflet(stDf[[input$time]])  %>%
      addProviderTiles('OpenStreetMap.BlackAndWhite',
                       options = providerTileOptions(noWrap = TRUE)) %>% addPolygons(stroke=F, opacity = 0.9, fillOpacity = 0.8,
                     col = ~pal(stDf[[input$time]][[input$stat]]),
                     label = lab,
                     highlight = highlightOptions(
                       # stroke=TRUE, 
                       weight = 5,
                       color = "#666",
                       dashArray = "",
                       fillOpacity = 0.7,
                       bringToFront = TRUE))
    if (input$stat != "nspskm") {
      m <- addLegend(m, "bottomright", pal = pal, values = stDf[[input$time]][[input$stat]],
                     title = uom, opacity = 1)
    } else { 
      ##
      values = stDf[[input$time]][[input$stat]]
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
