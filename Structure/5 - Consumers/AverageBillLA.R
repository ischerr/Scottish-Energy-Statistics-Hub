require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



AverageBillLAOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Energy",
    fluidRow(column(8,
                    h3("Average annual energy bill prices by local authority", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('AverageBillLASubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('AverageBillLA.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("AverageBillLAPlot")),
    leafletOutput(ns("AverageBillLAPlot"), height = "700px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Electricity",
             fluidRow(column(8,
                             h3("Average annual electricity bill prices by local authority", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('AverageBillLAElecSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('AverageBillLAElec.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("AverageBillLAPlot")),
             leafletOutput(ns("AverageBillLAElecPlot"), height = "700px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas",
             fluidRow(column(8,
                             h3("Average annual gas bill prices by local authority", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('AverageBillLAGasSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('AverageBillLAGas.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("AverageBillLAPlot")),
             leafletOutput(ns("AverageBillLAGasPlot"), height = "700px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    tabsetPanel(
      tabPanel("LA Breakdown",
    fluidRow(
    column(10, h3("Data", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("AverageBillLATable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Electicity cost breakdown",
             fluidRow(
               column(10, h3("Data - Average variable unit costs and standing charges for standard electricity in 2019", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ElecUnitTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas cost breakdown",
             fluidRow(
               column(10, h3("Data - Average variable unit costs and standing charges for standard gas in 2019", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("GasUnitTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("BEISUnitElec",
                           "BEISSubNatElec",
                           "BEISUnitGas",
                           "BEISSubNatGas"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("BEISUnitElec",
                        "BEISSubNatElec",
                        "BEISUnitGas",
                        "BEISSubNatGas"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("BEISUnitElec"),
        SourceLookup("BEISSubNatElec"),
        SourceLookup("BEISUnitGas"),
        SourceLookup("BEISSubNatGas")

        
      )
    )
  )
}




###### Server ######
AverageBillLA <- function(input, output, session) {

  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("AverageBillLA.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$AverageBillLASubtitle <- renderText({
    
    paste("Scotland, 2019")
  
    })
  
  output$AverageBillLAPlot <- renderLeaflet({
    
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
    
    AverageBillMap <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Average Bill",
      skip = 22
    )[c(2,1,4,5,6)]
    
    names(AverageBillMap) <- c("LocalAuthority", "CODE", "Average Electricity Bill", "Average Gas Bill", "Total")
    
    AverageBillMap <- AverageBillMap[which(substr(AverageBillMap$CODE, 1,3)== "S12"),]
    
    AverageBillMap$Content <- paste0("<b>",AverageBillMap$LocalAuthority, "</b><br/>Average total bill:<br/><em>\u00A3", round(AverageBillMap$Total, digits = 0),"</em>" )
    
    AverageBillMap$Hover <- paste0(AverageBillMap$LocalAuthority, " - \u00A3", round(AverageBillMap$Total, digits = 0))
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    AverageBillMap <- AverageBillMap[order(AverageBillMap$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      merge(LA, AverageBillMap)
    
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = LAMap$Total)
    
    leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(Total),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~Total,
                         title = "Average total<br/>bill",
                         labFormat = labelFormat(prefix = "\u00A3"),
                         opacity = 1
      ) 
    
  }) 
  
  output$AverageBillLATable = renderDataTable({
    
    AverageBillLA <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Average Bill",
      skip = 22
    )[c(2,1,4,5,6)]

    names(AverageBillLA) <- c("Local Authority", "Code", "Average Electricity Bill", "Average Gas Bill", "Average Total Bill")
    
    datatable(
      AverageBillLA,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Average annual energy bill prices (\u00A3)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average annual energy bill prices (\u00A3)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average annual energy bill prices (\u00A3)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2, 1) %>% 
      formatCurrency(3:5, currency = "\u00A3", digits = 0)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/AverageBillLA.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("AverageBillLATable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ElecUnitTable")
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("GasUnitTable")
  })
  
  output$ElecUnitTable = renderDataTable({
    

    ElecUnitCost <- read_csv("Processed Data/Output/Energy Bills/ElecUnitCost.csv",locale = locale(encoding = "WINDOWS-1252"))

    
    names(ElecUnitCost) <- c("Region", "Credit - Average variable unit price (\u00A3/kWh)", "Credit - Average fixed cost (\u00A3/year)" ," Direct debit - Average variable unit price (\u00A3/kWh)", "Direct debit - Average fixed cost (\u00A3/year)" , "Prepayment - Average variable unit price (\u00A3/kWh)" , "Prepayment - Average fixed cost (\u00A3/year)" , "Total - Average variable unit price (\u00A3/kWh)" , "Total - Average fixed cost (\u00A3/year)")

    datatable(
      ElecUnitCost,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Average variable unit costs and standing charges for standard electricity in 2019",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average variable unit costs and standing charges for standard electricity in 2019',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average variable unit costs and standing charges for standard electricity in 2019')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:9, 2) %>% 
      formatRound(c(2,4,6,8), 3)
  })
  
  output$GasUnitTable = renderDataTable({
    


    GasUnitCost <- read_csv("Processed Data/Output/Energy Bills/GasUnitCost.csv",locale = locale(encoding = "WINDOWS-1252"))

    
    names(GasUnitCost) <- c("Region", "Credit - Average variable unit price (\u00A3/kWh)", "Credit - Average fixed cost (\u00A3/year)" ," Direct debit - Average variable unit price (\u00A3/kWh)", "Direct debit - Average fixed cost (\u00A3/year)" , "Prepayment - Average variable unit price (\u00A3/kWh)" , "Prepayment - Average fixed cost (\u00A3/year)" , "Total - Average variable unit price (\u00A3/kWh)" , "Total - Average fixed cost (\u00A3/year)")
    
    datatable(
      GasUnitCost,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Average variable unit costs and standing charges for standard gas in 2019",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average variable unit costs and standing charges for standard gas in 2019',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average variable unit costs and standing charges for standard gas in 2019')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:9, 2) %>% 
      formatRound(c(2,4,6,8), 3)
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$AverageBillLA.png <- downloadHandler(
    filename = "AverageBillLA.png",
    content = function(file) {
      file.copy("Structure/5 - Consumers/AverageBillLAChart.png", file) 
    }
  )

  output$AverageBillLAElecSubtitle <- renderText({
    
    paste("Scotland, 2019")
    
  })
  
  output$AverageBillLAElecPlot <- renderLeaflet({
    
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
    
    AverageBillMap <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Average Bill",
      skip = 22
    )[c(2,1,4,5,6)]
    
    names(AverageBillMap) <- c("LocalAuthority", "CODE", "Average Electricity Bill", "Average Gas Bill", "Total")
    
    AverageBillMap <- AverageBillMap[which(substr(AverageBillMap$CODE, 1,3)== "S12"),]
    
    AverageBillMap$Content <- paste0("<b>",AverageBillMap$LocalAuthority, "</b><br/>Average electricity bill:<br/><em>\u00A3", round(AverageBillMap$`Average Electricity Bill`, digits = 0),"</em>" )
    
    AverageBillMap$Hover <- paste0(AverageBillMap$LocalAuthority, " - \u00A3", round(AverageBillMap$`Average Electricity Bill`, digits = 0))
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    AverageBillMap <- AverageBillMap[order(AverageBillMap$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      merge(LA, AverageBillMap)
    
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = LAMap$`Average Electricity Bill`)
    
    leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(`Average Electricity Bill`),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~`Average Electricity Bill`,
                         title = "Average electricity<br/>bill",
                         labFormat = labelFormat(prefix = "\u00A3"),
                         opacity = 1
      ) 
    
  }) 
  
  output$AverageBillLAElec.png <- downloadHandler(
    filename = "AverageBillElecLA.png",
    content = function(file) {
      file.copy("Structure/5 - Consumers/AverageBillLAElecChart.png", file) 
    }
  )
  
  output$AverageBillLAGasSubtitle <- renderText({
    
    paste("Scotland, 2019")
    
  })
  
  output$AverageBillLAGasPlot <- renderLeaflet({
    
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
    
    AverageBillMap <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Average Bill",
      skip = 22
    )[c(2,1,4,5,6)]
    
    names(AverageBillMap) <- c("LocalAuthority", "CODE", "Average Electricity Bill", "Average Gas Bill", "Total")
    
    AverageBillMap <- AverageBillMap[which(substr(AverageBillMap$CODE, 1,3)== "S12"),]
    
    AverageBillMap <- AverageBillMap[which(AverageBillMap$`Average Gas Bill` > 0),]
    
    AverageBillMap$Content <- paste0("<b>",AverageBillMap$LocalAuthority, "</b><br/>Average gas bill:<br/><em>\u00A3", round(AverageBillMap$`Average Gas Bill`, digits = 0),"</em>" )
    
    AverageBillMap$Hover <- paste0(AverageBillMap$LocalAuthority, " - \u00A3", round(AverageBillMap$`Average Gas Bill`, digits = 0))
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    AverageBillMap <- AverageBillMap[order(AverageBillMap$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      merge(LA, AverageBillMap)
    
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = LAMap$`Average Gas Bill`)
    
    palWithoutNA <- colorNumeric(
      palette = "Blues",
      domain = LAMap$`Average Gas Bill`,
      na.color=rgb(0,0,0,0))
    
    leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(`Average Gas Bill`),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = palWithoutNA, values = ~`Average Gas Bill`,
                         title = "Average gas<br/>bill",
                         labFormat = labelFormat(prefix = "\u00A3"),
                         opacity = 1
      ) 
    
  }) 
  
  output$AverageBillLAGas.png <- downloadHandler(
    filename = "AverageBillGasLA.png",
    content = function(file) {
      file.copy("Structure/5 - Consumers/AverageBillLAGasChart.png", file) 
    }
  )
  
}
