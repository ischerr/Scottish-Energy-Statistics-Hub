require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ChargingPointsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Charging Points",
    fluidRow(column(8,
                    h3("Total electric vehicle charging points by local authority", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('ChargingPointSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ChargingPointStatic'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("ULEVsPlot")),
    leafletOutput(ns("ChargingPointMap"), height = "700px")%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Charging Events",
             fluidRow(column(8,
                             h3("Total electric vehicle charging events by local authority", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('ChargingEventsSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ChargingEventsStatic'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("ULEVsPlot")),
             leafletOutput(ns("ChargingEventsMap"), height = "700px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Charge Provided",
             fluidRow(column(8,
                             h3("Total electric vehicle charge drawn by local authority", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('ChargeProvidedSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ChargeProvidedStatic'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("ULEVsPlot")),
             leafletOutput(ns("ChargeProvidedMap"), height = "700px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
      fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Charging Points",
    fluidRow(
    column(10, h3("Data - Charging Points", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ChargingPointTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Charging Events",
             fluidRow(
               uiOutput(ns("ChargingEventsDataSubtitle")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ChargingEventsTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Charge Provided",
             fluidRow(
               uiOutput(ns("ChargeProvidedDataSubtitle")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ChargeProvidedTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("DFTLicenced", "DFTULEVs"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("DFTLicenced"),
        SourceLookup("DFTULEVs")
        
      )
    )
  )
}




###### Server ######
ChargingPoints <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ULEVs.R")

  
  output$ChargingPointSubtitle <- renderText({
    
    AverageBillMap <- read_delim("Processed Data/Output/Charging Points/Points.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    paste("Scotland,", names(AverageBillMap)[ncol(AverageBillMap)])
  })
  
  output$ChargingPointMap <- renderLeaflet({
    
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
    
    ### Add Simplified shape back to the Shapefile
    LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")
    
    LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
    ############ RENEWABLE ELECTRICITY ################################################
    
    AverageBillMap <- read_delim("Processed Data/Output/Charging Points/Points.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    AverageBillMap <- AverageBillMap[c(1,2,ncol(AverageBillMap))]
    
    names(AverageBillMap) <- c("LocalAuthority", "CODE", "Points")
    
    AverageBillMap <- AverageBillMap[which(substr(AverageBillMap$CODE, 1,3)== "S12"),]
    
    AverageBillMap$Content <- paste0("<b>",AverageBillMap$LocalAuthority, "</b><br/>Charging Points:<br/><em>", round(AverageBillMap$Points, digits = 0),"</em>" )
    
    AverageBillMap$Hover <- paste0(AverageBillMap$LocalAuthority, " - ", round(AverageBillMap$Points, digits = 2))
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    AverageBillMap <- AverageBillMap[order(AverageBillMap$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      append_data(LA, AverageBillMap, key.shp = "CODE", key.data = "CODE")
    
    
    pal <- colorNumeric(
      palette = "Greens",
      domain = LAMap$Points)
    
    l <-leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(Points),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~Points,
                         title = "Charging Points",
                         opacity = 1
      ) 
    
  l
      
  })
  
  output$ChargingEventsSubtitle <- renderText({
    
    AverageBillMap <- read_delim("Processed Data/Output/Charging Points/Points.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    paste("Scotland,", names(AverageBillMap)[ncol(AverageBillMap)])
  })
  
  output$ChargingEventsMap <- renderLeaflet({
    
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
    
    ### Add Simplified shape back to the Shapefile
    LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")
    
    LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
    ############ RENEWABLE ELECTRICITY ################################################
    
    AverageBillMap <- read_delim("Processed Data/Output/Charging Points/Events.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    AverageBillMap <- AverageBillMap[c(1,2,ncol(AverageBillMap))]
    
    names(AverageBillMap) <- c("LocalAuthority", "CODE", "Events")
    
    AverageBillMap <- AverageBillMap[which(substr(AverageBillMap$CODE, 1,3)== "S12"),]
    
    AverageBillMap$Content <- paste0("<b>",AverageBillMap$LocalAuthority, "</b><br/>Charging Events:<br/><em>", format(round(AverageBillMap$Events, digits = 0), big.mark = ","),"</em>" )
    
    AverageBillMap$Hover <- paste0(AverageBillMap$LocalAuthority, " - ", format(round(AverageBillMap$Events, digits = 2), big.mark = ","))
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    AverageBillMap <- AverageBillMap[order(AverageBillMap$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      append_data(LA, AverageBillMap, key.shp = "CODE", key.data = "CODE")
    
    
    pal <- colorNumeric(
      palette = "Greens",
      domain = LAMap$Events)
    
    l <-leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(Events),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~Events,
                         title = "Charging Events",
                         opacity = 1
      ) 
    
    l
    
  })
  
  output$ChargeProvidedSubtitle <- renderText({
    
    AverageBillMap <- read_delim("Processed Data/Output/Charging Points/Points.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    paste("Scotland,", names(AverageBillMap)[ncol(AverageBillMap)])
  })
  
  output$ChargeProvidedMap <- renderLeaflet({
    
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
    
    ### Add Simplified shape back to the Shapefile
    LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")
    
    LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
    ############ RENEWABLE ELECTRICITY ################################################
    
    AverageBillMap <- read_delim("Processed Data/Output/Charging Points/AmountCharged.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    AverageBillMap <- AverageBillMap[c(1,2,ncol(AverageBillMap))]
    
    names(AverageBillMap) <- c("LocalAuthority", "CODE", "AmountCharged")
    
    AverageBillMap$AmountCharged <- AverageBillMap$AmountCharged /1000
    
    AverageBillMap <- AverageBillMap[which(substr(AverageBillMap$CODE, 1,3)== "S12"),]
    
    AverageBillMap$Content <- paste0("<b>",AverageBillMap$LocalAuthority, "</b><br/>Amount Charged:<br/><em>", format(round(AverageBillMap$AmountCharged, digits = 0), big.mark = ",")," GWh</em>" )
    
    AverageBillMap$Hover <- paste0(AverageBillMap$LocalAuthority, " - ", format(round(AverageBillMap$AmountCharged, digits = 2), big.mark = ","), " GWh")
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    AverageBillMap <- AverageBillMap[order(AverageBillMap$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      append_data(LA, AverageBillMap, key.shp = "CODE", key.data = "CODE")
    
    
    pal <- colorNumeric(
      palette = "Greens",
      domain = LAMap$AmountCharged)
    
    l <-leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(AmountCharged),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~AmountCharged,
                         title = "Amount<br/>Charged<br/>(GWh)",
                         opacity = 1
      ) 
    
    l
    
  })
  

  output$ChargingPointTable = renderDataTable({
    
    ChargingPoint <- read_delim("Processed Data/Output/Charging Points/Points.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(ChargingPoint)[1:2] <- c("Local Authority", "LA Code")
    
    datatable(
      ChargingPoint,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Total electric vehicle charging points by local authority",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Total electric vehicle charging points by local authority",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Total electric vehicle charging points by local authority")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(3:5), 0) 
  })
  
  
  TableYear <- {AverageBillMap <- read_delim("Processed Data/Output/Charging Points/Points.txt", 
                                             "\t", escape_double = FALSE, trim_ws = TRUE)
  
  paste(names(AverageBillMap)[ncol(AverageBillMap)])}
  
  output$ChargingEventsDataSubtitle <- renderUI({
    
    column(10, h3(paste("Data - Charging Points,", TableYear) , style = "color: #39ab2c;  font-weight:bold"))
    
  })
  
  output$ChargingEventsTable = renderDataTable({
    
    ChargingPoint <- read_delim("Processed Data/Output/Charging Points/Events.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ChargingPoint <- ChargingPoint[c(1,2,6)]
    
    names(ChargingPoint) <- c("Local Authority", "LA Code", "Charging Events")
    
    datatable(
      ChargingPoint,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = paste("Total electric vehicle charging events by local authority,",TableYear),
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = paste("Total electric vehicle charging events by local authority,",TableYear),
            header = TRUE
          ),
          list(extend = 'csv',
               title = paste("Total electric vehicle charging events by local authority,",TableYear))
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(3:5), 0) 
  })
  
  output$ChargeProvidedDataSubtitle <- renderUI({
    
    column(10, h3(paste("Data - Charge Provided (GWh),", TableYear) , style = "color: #39ab2c;  font-weight:bold"))
    
  })
  
  output$ChargeProvidedTable = renderDataTable({
    
    ChargingPoint <- read_delim("Processed Data/Output/Charging Points/AmountCharged.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ChargingPoint$Total <- ChargingPoint$Total / 1000
    
    ChargingPoint <- ChargingPoint[c(1,2,6)]
    
    names(ChargingPoint) <- c("Local Authority", "LA Code", "Charge Provided (GWh)")
    
    datatable(
      ChargingPoint,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = paste("Total electric vehicle charge provided (GWh) by local authority,",TableYear),
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = paste("Total electric vehicle charging events by local authority,",TableYear),
            header = TRUE
          ),
          list(extend = 'csv',
               title = paste("Total electric vehicle charging events by local authority,",TableYear))
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(3:5), 0) 
  })
  
  output$ChargingPointStatic <- downloadHandler(
    filename = "ChargePointMap.png",
    content = function(file) {
      writePNG(readPNG("Structure/2 - Renewables/Transport/ChargePointMap.png"), file) 
    }
  )
  
  
  output$ChargingEventsStatic <- downloadHandler(
    filename = "ChargeEventsMap.png",
    content = function(file) {
      writePNG(readPNG("Structure/2 - Renewables/Transport/ChargeEventsMap.png"), file) 
    }
  )
  
  
  output$ChargeProvidedStatic <- downloadHandler(
    filename = "ChargeProvidedMap.png",
    content = function(file) {
      writePNG(readPNG("Structure/2 - Renewables/Transport/ChargeChargedMap.png"), file) 
    }
  )
  
}
  
    