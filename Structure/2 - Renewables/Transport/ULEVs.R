require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



ULEVsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Vehicles licenced",
    fluidRow(column(8,
                    h3("Number of ultra low emission vehicles licenced", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('ULEVsSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ULEVs.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("ULEVsPlot")),
    plotlyOutput(ns("ULEVsPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Vehicles licenced by LA",
             fluidRow(column(8,
                             h3("Number of ultra low emission vehicles licenced by Local Authority", style = "color: #39ab2c;  font-weight:bold"),
                             
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ULEVbyLAmap.png'), 'Download Graph', style="float:right")
             )),
             fluidRow(column(6,selectInput(ns("YearSelect"), "Year:", c(unique(ULEVbyLA$Quarter)), selected = max(ULEVbyLA$Quarter), multiple = FALSE,
                                           selectize = TRUE, width = NULL, size = NULL) ),
                      column(6, align = 'right', selectInput(ns("TechSelect"), "Vehicle Type:", unique(ULEVbyLA$variable), selected = "Total ULEVs", multiple = FALSE,
                                                             selectize = TRUE, width = "300px", size = NULL))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("ElecGenFuelPlot")),
             leafletOutput(ns("ULEVbyLAmap"), height = "675px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    
    tabPanel("First time registrations",
             fluidRow(column(8,
                             h3("Proportion of ULEVs registered for the first time", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('ULEVRegOutputSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ULEVRegOutput.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("ULEVsPlot")),
             plotlyOutput(ns("ULEVRegOutputPlot"))%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    
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
                             h3("Total electric vehicle charging events by local authority - ChargePlace Scotland Network", style = "color: #39ab2c;  font-weight:bold"),
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
                             h3("Total electric vehicle charge drawn by local authority - ChargePlace Scotland Network", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('ChargeProvidedSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ChargeProvidedStatic'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("ULEVsPlot")),
             leafletOutput(ns("ChargeProvidedMap"), height = "700px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Vehicles licenced",
    fluidRow(
    column(10, h3("Data - Vehicles licenced", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ULEVsTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Local Authority",
             fluidRow(
               column(10, h3("Data - Renewable electricity generation at Local Authority Level (GWh)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12,selectInput(ns("YearSelect2"), "Year:", c(unique(ULEVbyLA$Quarter)), selected = max(ULEVbyLA$Quarter), multiple = FALSE,
                                     selectize = TRUE, width = "200px", size = NULL) )
             ),
             fluidRow(
               column(12, dataTableOutput(ns("LAGenTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("First Time Registrations",
      fluidRow(
        column(10, h3("Data - All ULEVs registered for the first time in Scotland", style = "color: #39ab2c;  font-weight:bold")),
        column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
      ),
      fluidRow(
        column(12, dataTableOutput(ns("ULEVRegOutputTable"))%>% withSpinner(color="#39ab2c"))),
      tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Charging Points",
             fluidRow(
               column(10, h3("Data - Charging Points", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ChargingPointTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Charging Events",
             fluidRow(
               uiOutput(ns("ChargingEventsDataSubtitle")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable5"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ChargingEventsTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Charge Provided",
             fluidRow(
               uiOutput(ns("ChargeProvidedDataSubtitle")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable6"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ChargeProvidedTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("DFTLicenced", "DFTULEVs", "ChargePlace", "TransportScotland13", "DFTCharging"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("DFTLicenced"),
        SourceLookup("DFTULEVs"),
        SourceLookup("DFTCharging"),
        SourceLookup("ChargePlace"),
        SourceLookup("TransportScotland13")
        
      )
    )
  )
}




###### Server ######
ULEVs <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ULEVs.R")

  
  output$ULEVsSubtitle <- renderText({
    
    Data <- read_delim("Processed Data/Output/Vehicles/ULEV.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    names(Data) <- c("Year", 'Battery Electric Vehicles',"Hybrid Electric Vehicles","All ULEVs",'Other ULEVs')
    
    Data <- Data[which(nchar(Data$Year)>4),]
    
    Data$Year <- as.yearqtr(Data$Year)
    
    paste("Scotland,", min(Data$Year), "-", max(Data$Year))
  })
  
  output$ULEVsPlot <- renderPlotly  ({
    
    Data <- read_delim("Processed Data/Output/Vehicles/ULEV.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    names(Data) <- c("Year", 'Battery Electric Vehicles',"Hybrid Electric Vehicles","All ULEVs",'Other ULEVs')
    
    Data <- Data[which(nchar(Data$Year)>4),]
    
    Data$Year <- as.yearqtr(Data$Year)
    
    Data2 <- rbind(head(Data,1), tail(Data,1))
    
    ChartColours <- c("#005a32", "#41ab5d", "#addd8e")
    
    p <-  plot_ly(Data, 
                  x = ~Year, 
                  y = ~ `Battery Electric Vehicles`, 
                  name = 'Battery Electric Vehicles',
                  type = 'scatter',
                  mode = 'none',
                  stackgroup = 'one',
                  fillcolor = ChartColours[1],
                  hoverinfo = "text",
                  text = paste0("Battery Electric Vehicles: ", Data$`Battery Electric Vehicles`, "\nAs Of: ", Data$Year)
    )%>%
      add_trace(
        y = ~ `Hybrid Electric Vehicles`, 
        name = 'Hybrid Electric Vehicles',
        fillcolor = ChartColours[2],
        hoverinfo = "text",
        text = paste0("Hybrid Electric Vehicles: ", Data$`Hybrid Electric Vehicles`, "\nAs Of: ", Data$Year)
      ) %>% 
      add_trace(
        y = ~ `Other ULEVs`, 
        name = 'Other ULEVs',
        fillcolor = ChartColours[3],
        hoverinfo = "text",
        text = paste0("Other ULEVs: ", Data$`Other ULEVs`, "\nAs Of: ", Data$Year)
      ) %>% 
      add_trace(
        x = Data2$Year,
        y = Data2$`All ULEVs` +  1500,
        name = "All ULEVs",
        mode = 'text',
        text = paste("<b>Total:\n", format(Data2$`All ULEVs`, big.mark = ","),"</b>"),
        stackgroup = 'two',
        showlegend = FALSE,
        fillcolor = 'rgba(26,150,65,0)'
      ) %>% 
      layout(
        barmode = 'group',
        bargap = 0.25,
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'skip',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     showgrid = FALSE,
                     range = c(min(Data$Year)-.25, max(Data$Year)+.25)
                     
        ),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          zeroline = FALSE,
          rangemode = "tozero",
          range = c(0, max(Data$`All ULEVs`)+3000)
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
    
    
  })
  
  output$ULEVRegOutputSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", 
        skip = 17)
    
    Data <- Data[c(1,3,4,2)]
    
    Data$Year <- as.yearqtr(Data$Year)
    
    paste("Scotland,", min(Data$Year), "-", max(Data$Year))
  })
  
  output$ULEVRegOutputPlot <- renderPlotly  ({
    
    
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", col_names = TRUE, 
        skip = 18)
    
    Data <- Data[c(1,12)]
    
    names(Data) <- c("Year", "Proportion")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    LineColours <- c("#39ab2c","#ef3b2c","#fb6a4a","#fc9272","#fcbba1")
    
    ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
    
    ULEVRegOutput <- Data
    
    p <-  plot_ly(ULEVRegOutput, x = ~ Year ) %>%  
      add_trace(y = ~ `Proportion`,
                name = "Proportion",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Proportion: ",
                  percent(ULEVRegOutput$Proportion, accuracy = 0.1),
                  "\nYear: ",
                  format(ULEVRegOutput$Year, "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ULEVRegOutput[which(ULEVRegOutput$`Proportion` != 0),], 1),
        x = ~ Year,
        y = ~ `Proportion`,
        name = "Proportion",
        legendgroup = "1",
        text = paste0(
          "Proportion: ",
          percent(tail(ULEVRegOutput[which(ULEVRegOutput$`Proportion` != 0),], 1)$`Proportion`, accuracy = 0.1),
          "\nYear: ",
          format(tail(ULEVRegOutput[which(ULEVRegOutput$`Proportion` != 0),], 1)$Year, "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>%
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE),
        yaxis = list(
          title = "",
          tickformat = ".1%",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
    
    
  })
  
  
  output$ULEVsTable = renderDataTable({
    
    ULEV <- read_delim("Processed Data/Output/Vehicles/ULEV.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    AllVehicles <- read_delim("Processed Data/Output/Vehicles/AllVehicles.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    AllVehicles <- AllVehicles[c(1,9)]
    
    AllVehicles$Total <- AllVehicles$Total * 1000
    
    #AllVehicles <- rbind(read_csv("Processed Data/Output/Vehicles/VehicleTotal2014.csv"), AllVehicles)
    
    names(AllVehicles) <- c("Quarter", "All Vehicles")
    
    AllVehicles <- merge(ULEV, AllVehicles)
    
    names(AllVehicles)[4] <- "All ULEVs"
    
    AllVehicles$`Proportion of Vehicles which are ULEVs` <- AllVehicles$`All ULEVs`/ AllVehicles$`All Vehicles`
    
    AllVehicles <- AllVehicles[c(1, 4,3,2,5,6,7)]
    
    AllVehicles <- AllVehicles[which(nchar(AllVehicles$Quarter)>4),]
    
    datatable(
      AllVehicles,
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
        title = "Number of Ultra Low Emission Vehicles (ULEVs) licenced at end of year/quarter",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Number of Ultra Low Emission Vehicles (ULEVs) licenced at end of year/quarter",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Number of Ultra Low Emission Vehicles (ULEVs) licenced at end of year/quarter")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:6), 0) %>% 
      formatPercentage(7,2)
  })
  
  output$ULEVRegOutputTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", col_names = TRUE, 
        skip = 17)
    
    Data <- Data[c(1,10:12)]
    
    ULEVRegOutputTech <- Data
    
    datatable(
      ULEVRegOutputTech,
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
        title = "All ULEVs registered for the first time in Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "All ULEVs registered for the first time in Scotland",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "All ULEVs registered for the first time in Scotland")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:4), 0) %>% 
      formatPercentage(4,2)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Transport/ULEVs.txt")[2])
                     
                   )))
  })
  
 observeEvent(input$ToggleTable1, {
    toggle("ULEVsTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("LAGenTable")
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("ULEVRegOutputTable")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("ChargingPointTable")
  })
  
  observeEvent(input$ToggleTable5, {
    toggle("ChargingEventsTable")
  })
  
  observeEvent(input$ToggleTable6, {
    toggle("ChargeProvidedTable")
  })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ULEVs.png <- downloadHandler(
    filename = "ULEVs.png",
    content = function(file) {

      ElecVehicles <- read_delim("Processed Data/Output/Vehicles/ULEV.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE)
      
      
      names(ElecVehicles) <- c("Year", 'Battery Electric Vehicles',"Hybrid Electric Vehicles","All ULEVs",'Other ULEVs')
      
      ElecVehicles$`All ULEVs` <- NULL
      
      ElecVehicles <- ElecVehicles[which(nchar(ElecVehicles$Year)>4),]
      
      ElecVehicles$Year <-
        as.yearqtr(ElecVehicles$Year, format = "%Y Q%q")
      
      
      ElecVehiclesMin <- head(ElecVehicles, 1)
      ElecVehiclesMax <- tail(ElecVehicles, 1)
      
      ElecVehicles <- melt(ElecVehicles, id.vars = "Year")
      
      ElecVehicles <- ElecVehicles %>% mutate(variable = factor(variable),
                                              variable = factor(variable, levels = rev(levels(variable))))
      
      
      ### variables
      ChartColours <- c("#39ab2c", "#005a32", "#41ab5d", "#addd8e")
      sourcecaption = "Source: DfT"
      plottitle = "Number of ultra low emission vehicles\nlicenced"
      
      #ElecVehicles$CavityPercentage <- PercentLabel(ElecVehicles$Cavity)
      
      
      ElecVehiclesChart <- ElecVehicles %>%
        ggplot(aes(
          x = Year,
          y = value,
          group = variable,
          fill = variable
        )) +
        scale_fill_manual(
          "variable",
          values = c(
            "Battery Electric Vehicles" = ChartColours[2],
            "Hybrid Electric Vehicles" = ChartColours[3],
            "Other ULEVs" = ChartColours[4]
          )
        ) +
        geom_area(posistion = "fill") +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              format(Year, format = "%Y Q%q"),
              ""
            ),
            hjust = ifelse(Year == min(Year), 0, 1),
            vjust = 1.5,
            colour = ChartColours[2],
            fontface = 2,
            family = "Century Gothic"
          )
        ) +
        annotate(
          "text",
          x = ElecVehiclesMin$Year,
          y = ElecVehiclesMin$`Battery Electric Vehicles` * 0.5,
          label = format(ElecVehiclesMin$`Battery Electric Vehicles`,big.mark = ","),
          hjust = -.1,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecVehiclesMin$Year,
          y = (ElecVehiclesMin$`Hybrid Electric Vehicles` * 0.5) + ElecVehiclesMin$`Battery Electric Vehicles`,
          label = format(ElecVehiclesMin$`Hybrid Electric Vehicles`,big.mark = ","),
          hjust = -.1,
          vjust = -1,
          colour = ChartColours[4],
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecVehiclesMax$Year,
          y = ElecVehiclesMax$`Battery Electric Vehicles` * 0.5,
          label = format(ElecVehiclesMax$`Battery Electric Vehicles`, big.mark = ","),
          hjust = 1.1,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecVehiclesMax$Year,
          y = (ElecVehiclesMax$`Hybrid Electric Vehicles` * 0.5) + ElecVehiclesMax$`Battery Electric Vehicles`,
          label = format(ElecVehiclesMax$`Hybrid Electric Vehicles`, big.mark = ","),
          hjust = 1.1,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecVehicles$Year),
          y = (
            ElecVehiclesMax$`Battery Electric Vehicles` + ElecVehiclesMin$`Battery Electric Vehicles`
          ) * .25,
          label = "Battery Electric",
          hjust = .5,
          vjust = 3.5,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecVehicles$Year),
          y = ((ElecVehiclesMax$`Hybrid Electric Vehicles` + ElecVehiclesMin$`Hybrid Electric Vehicles`) *
                 .25
          ) + ((
            ElecVehiclesMax$`Battery Electric Vehicles` + ElecVehiclesMin$`Battery Electric Vehicles`
          ) * .5
          ),
          label = "Hybrid Electric",
          hjust = .5,
          vjust = 8.1,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecVehicles$Year),
          y = ((ElecVehiclesMax$`Other ULEVs` + ElecVehiclesMin$`Other ULEVs`) *
                 .25
          ) +
            ((ElecVehiclesMax$`Hybrid Electric Vehicles` + ElecVehiclesMin$`Hybrid Electric Vehicles`) *
                 .5
          ) + ((
            ElecVehiclesMax$`Battery Electric Vehicles` + ElecVehiclesMin$`Battery Electric Vehicles`
          ) * .5
          ),
          label = "Other ULEVs",
          hjust = .5,
          vjust = 2,
          colour = ChartColours[4],
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecVehiclesMin$Year,
          y = ElecVehiclesMin$`Battery Electric Vehicles` + ElecVehiclesMin$`Hybrid Electric Vehicles` + ElecVehiclesMin$`Other ULEVs`,
          label = paste0(
            "Total: ",
            format(ElecVehiclesMin$`Battery Electric Vehicles` + ElecVehiclesMin$`Hybrid Electric Vehicles` + ElecVehiclesMin$`Other ULEVs`, big.mark = ",")
          ),
          hjust = 0,
          vjust = -3,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecVehiclesMax$Year,
          y = ElecVehiclesMax$`Battery Electric Vehicles` + ElecVehiclesMax$`Hybrid Electric Vehicles` + ElecVehiclesMax$`Other ULEVs`,
          label = paste0(
            "Total: ",
            format(ElecVehiclesMax$`Battery Electric Vehicles` + ElecVehiclesMax$`Hybrid Electric Vehicles` + ElecVehiclesMax$`Other ULEVs`,big.mark = ",")
          ),
          hjust = 1.3,
          vjust = 1,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        )
      
      ElecVehiclesChart
      
      
      ElecVehiclesChart <-
        StackedArea(ElecVehiclesChart,
                    ElecVehicles,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      
      ElecVehiclesChart <- ElecVehiclesChart +
        ylim(-100, max(ElecVehiclesMax$`Battery Electric Vehicles`)+max(ElecVehiclesMax$`Other ULEVs`)+max(ElecVehiclesMax$`Hybrid Electric Vehicles`))
      


      
      ggsave(
        file,
        plot =  ElecVehiclesChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )



output$ULEVRegOutput.png <- downloadHandler(
  filename = "ULEVRegOutput.png",
  content = function(file) {
    

    ElecVehiclesRegistrations <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "ULEVs", col_names = TRUE, 
        skip = 18)[c(1,10,12)]
    
    names(ElecVehiclesRegistrations) <- c("Year", "Registrations", "Proportion")
    
    ElecVehiclesRegistrations$Year <-
      as.yearqtr(ElecVehiclesRegistrations$Year, format = "%Y Q%q")
    
        ElecVehiclesProportion <- ElecVehiclesRegistrations
        
        ElecVehiclesProportion$Year <-
          as.yearqtr(ElecVehiclesProportion$Year, format = "%Y Q%q")
        
        
        ### variables
        ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
        LineColours <- c("#39ab2c", "#238b45", "#a1d99b")
        sourcecaption = "Source: DfT"
        plottitle = "Proportion of ULEVs registered for\nthe first time"
        
        #ElecVehiclesProportion$CavityPercentage <- PercentLabel(ElecVehiclesProportion$Cavity)
        
        
        ElecVehiclesProportionChart <- ElecVehiclesProportion %>%
          ggplot(aes(x = Year,
                     y = Proportion)) +
          geom_line(aes(),
                    size = 1.5,
                    colour = LineColours[1],
                    family = "Century Gothic") +
          geom_point(
            data = tail(ElecVehiclesProportion, 1),
            aes(x = Year,
                y = Proportion),
            size = 4,
            colour = LineColours[1],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              label = ifelse(Year == min(Year), percent(Proportion, accuracy =  .1), ""),
              show_guide = FALSE
            ),
            fontface = 2,
            vjust = 2,
            colour = LineColours[1],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = Year,
              label = ifelse(Year == max(Year), percent(Proportion, accuracy =  .1), ""),
              show_guide = FALSE
            ),
            hjust = -.4,
            fontface = 2,
            colour = LineColours[1],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              y = 0,
              label = ifelse(Year == min(Year) |
                               Year == max(Year), format(Year, format = "%Y Q%q"), ""),
              hjust = 0.5,
              vjust = 1.5,
              colour = ChartColours[1],
              fontface = 2
            ),
            family = "Century Gothic"
          )
        
        
        ElecVehiclesProportionChart
        
        
        ElecVehiclesProportionChart <-
          StackedArea(ElecVehiclesProportionChart,
                      ElecVehiclesProportion,
                      plottitle,
                      sourcecaption,
                      ChartColours)
        
        
        ElecVehiclesProportionChart <- ElecVehiclesProportionChart +
          xlim(min(as.numeric(ElecVehiclesProportion$Year)-.15),max(as.numeric(ElecVehiclesProportion$Year)+.3))
        
        ElecVehiclesProportionChart
        
        
        ggsave(
          file,
          plot =  ElecVehiclesProportionChart,
          width = 14,
          height = 16,
          units = "cm",
          dpi = 300
        )
        
    
  })
  
  
  output$ChargingPointSubtitle <- renderText({
    
    AverageBillMap <- read_csv("Processed Data/Output/Charging Points/Points.csv")
    
    paste("Scotland,", AverageBillMap$Year[1])
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
    
    AverageBillMap <- read_csv("Processed Data/Output/Charging Points/Points.csv")[1:4]
    
    names(AverageBillMap) <- c("CODE", "LocalAuthority", "Points", "Rapid Points")
    
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
      merge(LA, AverageBillMap)
    
    
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
      merge(LA, AverageBillMap)
    
    
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
    
    AverageBillMap$Content <- paste0("<b>",AverageBillMap$LocalAuthority, "</b><br/>Amount Charged:<br/><em>", format(round(AverageBillMap$AmountCharged, digits = 0), big.mark = ",")," MWh</em>" )
    
    AverageBillMap$Hover <- paste0(AverageBillMap$LocalAuthority, " - ", format(round(AverageBillMap$AmountCharged, digits = 2), big.mark = ","), " MWh")
    
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
                         title = "Amount<br/>Charged<br/>(MWh)",
                         opacity = 1
      ) 
    
    l
    
  })
  
  
  output$ChargingPointTable = renderDataTable({
    
    ChargingPoint <- read_csv("Processed Data/Output/Charging Points/Points.csv")
    
    ChargingPoint <- ChargingPoint[c(2:33,1),]
    
    datatable(
      ChargingPoint[c(2,1,3,4)],
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
      formatRound(c(3:4), 0) 
  })
  
  
  TableYear <- {AverageBillMap <- read_delim("Processed Data/Output/Charging Points/Points.txt", 
                                             "\t", escape_double = FALSE, trim_ws = TRUE)
  
  paste(names(AverageBillMap)[ncol(AverageBillMap)])}
  
  output$ChargingEventsDataSubtitle <- renderUI({
    
    column(10, h3(paste("Data - Charging Events,", TableYear, " - ChargePlace Scotland Network") , style = "color: #39ab2c;  font-weight:bold"))
    
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
      formatRound(c(3), 0) 
  })
  
  output$ChargeProvidedDataSubtitle <- renderUI({
    
    column(10, h3(paste("Data - Charge Provided (MWh),", TableYear, " - ChargePlace Scotland Network") , style = "color: #39ab2c;  font-weight:bold"))
    
  })
  
  output$ChargeProvidedTable = renderDataTable({
    
    ChargingPoint <- read_delim("Processed Data/Output/Charging Points/AmountCharged.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ChargingPoint$Total <- ChargingPoint$Total / 1000
    
    ChargingPoint <- ChargingPoint[c(1,2,6)]
    
    names(ChargingPoint) <- c("Local Authority", "LA Code", "Charge Provided (MWh)")
    
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
        title = paste("Total electric vehicle charge provided (MWh) by local authority,",TableYear),
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = paste("Total electric vehicle charge provided (MWh) by local authority,",TableYear),
            header = TRUE
          ),
          list(extend = 'csv',
               title = paste("Total electric vehicle charge provided (MWh) by local authority,",TableYear))
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(3), 0) 
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
  
  output$ULEVbyLAmap <- renderLeaflet({
    
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
    
    Year =  input$YearSelect
    
    Tech =  as.character(input$TechSelect)
    
    ULEVbyLA <- read_delim("Processed Data/Output/Vehicles/ULEVbyLA.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(ULEVbyLA)[3] <- "CODE"
    
    ULEVbyLA <- ULEVbyLA[which(ULEVbyLA$Quarter == Year),]
    
    ULEVbyLA <- ULEVbyLA[which(substr(ULEVbyLA$CODE,1,3) == "S12"),]
    
    ULEVbyLA <- ULEVbyLA[which(ULEVbyLA$variable == Tech),]
    
    ULEVbyLA$Content <- paste0("<b>",ULEVbyLA$LAName, "</b><br/>", ULEVbyLA$variable[1], " licenced:<br/><em>", round(ULEVbyLA$value, digits = 1),"</em>" )
    
    ULEVbyLA$Hover <- paste0(ULEVbyLA$LAName, " - ", round(ULEVbyLA$value, digits = 1), "")
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    ULEVbyLA <- ULEVbyLA[order(ULEVbyLA$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      merge(LA, ULEVbyLA)
    
    
    pal <- colorNumeric(
      palette = "Greens",
      domain = LAMap$value)
    
    l <-leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(value),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~value,
                         title = paste0(ULEVbyLA$variable[1], " licenced"),
                         opacity = 1
      ) 
    
    l
    
  })
  
  output$LAGenTable = renderDataTable({
    
    LARenGen <- read_delim("Processed Data/Output/Vehicles/ULEVbyLA.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)
    
    Year2 = input$YearSelect2
    
    LARenGen <- LARenGen[which(LARenGen$Quarter == Year2),]
    
    LARenGen <- dcast(LARenGen, Quarter + LAName + LACode ~ variable, value.var = "value")
    
    datatable(
      LARenGen,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Number of ultra low emission vehicles licenced by Local Authority",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Number of ultra low emission vehicles licenced by Local Authority',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Number of ultra low emission vehicles licenced by Local Authority')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(4:ncol(LARenGen), 0) %>% 
      formatStyle(ncol(LARenGen), fontWeight = "bold")
  })
  
  output$ULEVbyLAmap.png <- downloadHandler(
    filename = "ULEVbyLAmap.png",
    content = function(file) {
      writePNG(readPNG("Structure/2 - Renewables/Transport/ULEVsLA.png"), file) 
    }
  )

}
    
    