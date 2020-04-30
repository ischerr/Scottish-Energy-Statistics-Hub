require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

NonGasGridOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Proportion of households not on the gas grid by local authority (estimates)", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('NonGasGridSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('NonGasGrid.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("NonGasGridPlot")),
    leafletOutput(ns("GasGridMap"), height = "800px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("NonGasGridTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISNonGasGrid"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISNonGasGrid")
        
      )
    )
  )
}




###### Server ######
NonGasGrid <- function(input, output, session) {

  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("NonGasGrid.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$NonGasGridSubtitle <- renderText({
    
    paste("Scotland, 2018")
  
    })
  
  output$NonGasGridPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
   
     writePNG(readPNG("Structure/5 - Consumers/NonGasGridOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$NonGasGridTable = renderDataTable({
    
    NonGasGrid <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-gas grid by LA",
      skip = 13
    )[c(2,5)]

    datatable(
      NonGasGrid,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Proportion of households not on the gas grid by local authority (estimates)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of households not on the gas grid by local authority (estimates)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of households not on the gas grid by local authority (estimates)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2, 1)
  })
  
  output$NonGasGridTimeSeriesTable = renderDataTable({
    
    NonGasGrid <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier elec",
      skip = 13
    )
    
    names(NonGasGrid)[1] <- "Quarter"#
    
    NonGasGrid <- NonGasGrid[complete.cases(NonGasGrid),]
    
    NonGasGrid$Quarter <- as.Date(as.numeric(NonGasGrid$Quarter), origin = "1899-12-30")
    
    NonGasGrid$Quarter <- as.character(as.yearqtr(NonGasGrid$Quarter))
    
    
    datatable(
      NonGasGrid,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        title = "Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:9, 1)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/NonGasGrid.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("NonGasGridTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("NonGasGridTimeSeriesTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$NonGasGrid.png <- downloadHandler(
    filename = "NonGasGrid.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/NonGasGridChart.png"), file) 
    }
  )
  
  output$GasGridMap <- renderLeaflet({
    
    ### Load Packages
    library(readr)
    library("maptools")
    library(tmaptools)
    library(tmap)
    library("sf")
    library("leaflet")
    library("rgeos")
    library(readxl)
    library(ggplot2)
    
    print("MapsandGasGrid")
    
    # This is unlikely to change from 2012
    yearstart <- 2012
    
    ### Set the final year for the loop as the current year ###
    yearend <- format(Sys.Date(), "%Y")
    
    
    ### Add Simplified shape back to the Shapefile
    LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")
    
    LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
    ############ RENEWABLE ELECTRICITY ################################################
    
    GasGridMap <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-gas grid by LA",
      skip = 13
    )
    
    GasGridMap <- GasGridMap[c(2,ncol(GasGridMap),5)]
    
    names(GasGridMap) <- c("LocalAuthority", "CODE", "Meters")
    
    GasGridMap <- GasGridMap[which(substr(GasGridMap$CODE, 1,3)== "S12"),]
    
    GasGridMap$Content <- paste0("<b>",GasGridMap$LocalAuthority, "</b><br/>Proportion of Economy 7 Meters:<br/><em>", percent(GasGridMap$Meters, 0.1),"</em>" )
    
    GasGridMap$Hover <- paste0(GasGridMap$LocalAuthority, " - ", percent(GasGridMap$Meters, 0.1))
    
    GasGridMap$Meters <- GasGridMap$Meters*100
    
    
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    GasGridMap <- GasGridMap[order(GasGridMap$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      append_data(LA, GasGridMap, key.shp = "CODE", key.data = "CODE")
    
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = c(0,100))
    
    leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(Meters),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = c(0,50,100),
                         title = "Proportion of<br/>Economy 7 Meters",
                         labFormat = labelFormat(suffix = "%"),
                         opacity = 1
      ) 
    
  }) 
}
