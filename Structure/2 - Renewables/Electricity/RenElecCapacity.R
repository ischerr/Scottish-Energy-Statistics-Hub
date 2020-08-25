require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



RenElecCapacityOutput <- function(id) {
  ns <- NS(id)
  tagList( 
    tabsetPanel(
      tabPanel("Quarterly operational capacity",
               fluidRow(column(8,
                               h3("Operational renewable capacity", style = "color: #39ab2c;  font-weight:bold"),
                               h4(textOutput(ns('RenElecCapacitySubtitle')), style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('RenElecOperational.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecCapacityPlot")),
               plotlyOutput(ns("RenElecCapacityPlot"))%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Annual operational capacity by fuel",
               fluidRow(column(8,
                               h3("Annual operational capacity of sites generating electricity from renewable sources", style = "color: #39ab2c;  font-weight:bold"),
                               h4(textOutput(ns('RenElecFuelSubtitle')), style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('RenElecFuel.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               plotlyOutput(ns("RenElecFuelPlot"), height = "900px")%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Operational capacity by Local Authority",
               fluidRow(column(8,
                               h3("Renewable electricity capacity at Local Authority Level", style = "color: #39ab2c;  font-weight:bold"),
                               
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('LACapMap.png'), 'Download Graph', style="float:right")
               )),
               fluidRow(column(6,selectInput(ns("YearSelect"), "Year:", c(max(LARenCap$Year):min(LARenCap$Year)), selected = max(LARenCap$Year), multiple = FALSE,
                                             selectize = TRUE, width = NULL, size = NULL) ),
                        column(6, align = 'right', selectInput(ns("TechSelect"), "Tech:", c(unique(names(LARenCap[4:10]))), selected = "Total Renewable", multiple = FALSE,
                                                               selectize = TRUE, width = "300px", size = NULL))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("ElecGenFuelPlot")),
               leafletOutput(ns("LACapMap"), height = "675px")%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Operational capacity tech",
             fluidRow(column(8,
                             h3("Operational renewable capacity by technology", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenElecBreakdownCapSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecBreakdownCap.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenElecCapacityPlot")),
             plotlyOutput(ns("RenElecBreakdownCapPlot"), height = "600px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),

    tabPanel("Operational capacity by installation size",
             fluidRow(column(8,
                             h3("Operational renewable capacity by installation size", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenElecOperationalSizeSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecOperationalSize.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenElecCapacityPlot")),

             plotlyOutput(ns("RenElecOperationalSizePlot"), height = "600px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(

      tabPanel("Quarterly Capacity by technology",
               fluidRow(
                 column(10, h3("Data - Quarterly operational renewable capacity by technology (MW)", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("RenElecBreakdownCapTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Annual Capacity by technology",
               fluidRow(
                 column(10, h3("Data - Annual operational renewable capacity by technology (MW)", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("AnnualRenElecBreakdownCapTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Renewable sites",
               fluidRow(
                 column(10, h3("Data - Annual operational number of sites generating electricity from renewable sources", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("RenSitesTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),

      tabPanel("Capacity by installation size",
               fluidRow(
                 column(10, h3("Data - Operational renewable capacity by installation size (MW)", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("RenElecOperationalSizeTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Local Authority",
               fluidRow(
                 column(10, h3("Data - Renewable electricity capacity at Local Authority Level (MW)", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable5"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12,selectInput(ns("YearSelect2"), "Year:", c(max(LARenCap$Year):min(LARenCap$Year)), selected = max(LARenCap$Year), multiple = FALSE,
                                       selectize = TRUE, width = "200px", size = NULL) )
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("LACapTable"))%>% withSpinner(color="#39ab2c"))),
               HTML("<blockquote><p>*The sum of local authorities will not add up to overall Scottish capacity because some sites have not been allocated a local authority.</p></blockquote>"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),

  
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISRenElec", "BEISREPD"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISREPD"),
        SourceLookup("BEISRenElec")
        
      )
    )
  )
}

###### Server ######
RenElecCapacity <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecCapacity.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$RenElecCapacitySubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable elec capacity", col_names = TRUE,
        skip = 15)
    
    Data <- Data[c(2,4)]
    
    names(Data) <- c("Year", "Capacity")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    paste(min(Data$Year),"-", max(Data$Year))
  })

  output$RenElecCapacityPlot <- renderPlotly  ({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable elec capacity", col_names = TRUE,
        skip = 15)
    
    Data <- Data[c(2,4)]
    
    names(Data) <- c("Year", "Capacity")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    Data$Capacity <- Data$Capacity*1000
    
    ElecCapOperational <- Data[order(Data$Year),]
    
    ### variables
    ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
    LineColours <- c("#39ab2c", "#238b45", "#a1d99b")
    sourcecaption = "Source: BEIS"
    plottitle = "Operational renewable capacity"
    
    
    
    p <-  plot_ly(data = ElecCapOperational,
                  x = ~ Year ) %>% 
      add_trace(y = ~ `Capacity`,
                name = "Prepayment",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Capacity: ",
                  format(round(ElecCapOperational$`Capacity`, digits = 0),big.mark = ","),
                  " MW\nAs of: ",
                  format(ElecCapOperational$Year, "%Y Q%q")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecCapOperational[which(ElecCapOperational$`Capacity` > 0 | ElecCapOperational$`Capacity` < 0),], 1),
        x = ~ Year,
        y = ~ `Capacity`,
        name = "Capacity",
        text = paste0(
          "Capacity: ",
          format(round(ElecCapOperational[which(ElecCapOperational$`Capacity` > 0 | ElecCapOperational$`Capacity` < 0),][-1,]$`Capacity`, digits = 0), big.mark = ","),
          " MW\nAs of: ",
          format(ElecCapOperational[which(ElecCapOperational$`Capacity` > 0 | ElecCapOperational$`Capacity` < 0),][-1,]$Year, "%Y Q%q")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(ElecCapOperational$Year)-1
                               , max(ElecCapOperational$Year)+1)),
        yaxis = list(
          title = "MW",
          tickformat = "",
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
  
  output$RenElecBreakdownCapTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "R - QTRCapacity", col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- c("Date", "Wind Onshore", "Wind Offshore", "Shoreline wave / tidal", "Solar P.V.", "Small Hydro", "Large Hydro", "Landfill Gas", "Sewage", "Waste", "Animal Biomass", "Anaerobic Digestion", "Plant", "Total")
    
    Data <- Data[-1,]
    
    Data$Date <- paste0(substr(Data$Date,1,4), " Q", substr(Data$Date, 8,8))
    
    Data[2:14]%<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    Data$Biomass <- Data$`Animal Biomass` + Data$Plant
    
    Data$`Animal Biomass` <- NULL
    
    Data$Plant <- NULL
    
    Data$`Anaerobic Digestion` <- Data$`Anaerobic Digestion` + Data$Sewage
    
    Data$Sewage <- NULL
    
    names(Data)[1] <- "Quarter"
    
    datatable(
      Data[c(1:10,12,11)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Operational renewable capacity by technology (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Operational renewable capacity by technology (MW)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Operational renewable capacity by technology (MW)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(Data), 0) %>% 
      formatStyle(12, fontWeight = "bold")
  })
  
  output$AnnualRenElecBreakdownCapTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    
    
    RenElecCapFuel <- as_tibble(Data)
    
    RenElecCapFuel[is.na(RenElecCapFuel)] <- 0
    
    RenElecCapFuel <- arrange(RenElecCapFuel,-row_number())
    
    RenElecCapFuel$Total <- RenElecCapFuel$`Other bioenergy` + RenElecCapFuel$`Sewage gas` + RenElecCapFuel$`Wave and tidal` + RenElecCapFuel$`Landfill gas` + RenElecCapFuel$`Solar PV` + RenElecCapFuel$Hydro + RenElecCapFuel$`Offshore Wind` + RenElecCapFuel$`Onshore Wind`
    
    RenElecCapFuel<-RenElecCapFuel[seq(dim(RenElecCapFuel)[1],1),]
    
    datatable(
      RenElecCapFuel[],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Operational renewable capacity by technology (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Operational renewable capacity by technology (MW)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Operational renewable capacity by technology (MW)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecCapFuel), 0) %>% 
      formatStyle(10, fontWeight = "bold")
  })
  
 output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecCapacity.txt")[2])
                    
                  )))
 })
 
  
  observeEvent(input$ToggleTable1, {
    toggle("RenElecBreakdownCapTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("AnnualRenElecBreakdownCapTable")
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("RenSitesTable")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("RenElecOperationalSizeTable")
  })
  
  observeEvent(input$ToggleTable5, {
    toggle("LACapTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenElecOperational.png <- downloadHandler(
    filename = "RenElecOperational.png",
    content = function(file) {
      
      
      RenElecOperational <- read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable elec capacity",
        col_names = TRUE,
        skip = 15
      )
      
      
      RenElecOperational <- RenElecOperational[c(2, 4)]
      
      names(RenElecOperational) <- c("Year", "Total")
      
      RenElecOperational$Total <-
        as.numeric(RenElecOperational$Total)
      
      RenElecOperational$Year <-
        as.yearqtr(RenElecOperational$Year, format = "%Y Q%q")
      
      RenElecOperational <-
        RenElecOperational[order(RenElecOperational$Year), ]
      
      RenElecOperational <-
        RenElecOperational[complete.cases(RenElecOperational), ]
      ### variables
      ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
      LineColours <- c("#39ab2c", "#238b45", "#a1d99b")
      sourcecaption = "Source: BEIS"
      plottitle = "Operational renewable capacity"
      
      #RenElecOperational$CavityPercentage <- PercentLabel(RenElecOperational$Cavity)
      
      
      RenElecOperationalChart <- RenElecOperational %>%
        ggplot(aes(x = Year,
                   y = Total)) +
        geom_line(aes(),
                  size = 1.5,
                  colour = LineColours[1],
                  family = "Century Gothic") +
        geom_point(
          data = tail(RenElecOperational, 1),
          aes(x = Year,
              y = Total),
          size = 4,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            label = ifelse(Year == min(Year), paste(round(Total, digits = 1), "MW"), ""),
            show_guide = FALSE
          ),
          fontface = 2,
          hjust = 1.1,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .3,
            label = ifelse(Year == max(Year), paste(round(Total, digits = 1), "MW"), ""),
            show_guide = FALSE
          ),
          fontface = 2,
          hjust = -0.1,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
      geom_text(
        aes(
          y = 0,
          label = ifelse(
            Year == min(Year) |
              Year == max(Year),
            format(Year, format = "%Y Q%q"),
            ""
          ),
          hjust = 0.5,
          vjust = 1.5,
          colour = ChartColours[1],
          fontface = 2
        ),
        family = "Century Gothic"
      )
      
      
      RenElecOperationalChart
      
      
      RenElecOperationalChart <-
        StackedArea(
          RenElecOperationalChart,
          RenElecOperational,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      
      RenElecOperationalChart <- RenElecOperationalChart +
        xlim(min(as.numeric(RenElecOperational$Year) - 2), max(as.numeric(RenElecOperational$Year) + 3))
      
      RenElecOperationalChart
      
      
      ggsave(
        file,
        plot = RenElecOperationalChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenElecBreakdownCapPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "R - QTRCapacity", col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- c("Date", "Wind Onshore", "Wind Offshore", "Shoreline wave / tidal", "Solar Photovoltaics", "Small Hydro", "Large Hydro", "Landfill Gas", "Sewage", "Energy from waste", "Animal Biomass", "Anaerobic Digestion", "Plant", "Total")
    
    Data <- Data[2,]
    
    Data[2:14]%<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    Data$Biomass <- Data$`Animal Biomass` + Data$Plant
    
    Data$`Animal Biomass` <- NULL
    
    Data$Plant <- NULL
    
    Data$`Anaerobic Digestion` <- Data$`Anaerobic Digestion` + Data$Sewage
    
    Data$Sewage <- NULL
    
    Data$Total <- NULL
    
    Data <- melt(Data)
    
    names(Data) <-  c("Time", "Type", "Renewables")
    
    RenElecPipeline <- as_tibble(Data)
    
    
    # 
    # Data <- as.data.frame(t(Data))
    # 
    # names(Data) <-  as.character(unlist(Data[1,]))
    # names(Data)[1] <- "Type"
    # Data <- tail(Data,-1)
    # Data %<>% lapply(function(x) as.numeric(as.character(x)))
    # Data <- as.data.frame(Data)
    
    RenElecPipeline <- RenElecPipeline[which(RenElecPipeline$Renewables > 0),]
    
    RenElecPipeline <- arrange(RenElecPipeline, RenElecPipeline$Renewables)
    
    RenElecPipeline$Type <- paste0("<b>",RenElecPipeline$Type, "</b>")
    
    rownames(RenElecPipeline) <- NULL
    
    #RenElecPipeline$Type <- as.numeric(rownames(RenElecPipeline))
    
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#31a354",
        "#0868ac",
        "#43a2ca",
        "#7bccc4",
        "#a6bddb",
        "#d0d1e6",
        "#bdbdbd",
        "#969696"
      )
    
    
    p <- plot_ly(data = RenElecPipeline, y = ~ Type) %>%
      
      add_trace(
        data = RenElecPipeline,
        x = ~ `Renewables`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Renewables",
        text = paste0("Renewables: ", format(round(RenElecPipeline$`Renewables`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = RenElecPipeline,
        y = ~ Type,
        x = ~ (RenElecPipeline$`Renewables`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste("<b>",format(round((RenElecPipeline$`Renewables`), digits = 0), big.mark = ","),"MW</b>"),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(
          title = "",
          showgrid = FALSE,
          # ticktext = list( "Landfill Gas",          
          #                  "Large Hydro",          
          #                  "Anaerobic Digestion",   
          #                  "Small Hydro",     
          #                  "Energy from waste"  ,   
          #                  "Biomass (co-firing)" ,  
          #                  "Solar Photovoltaics" ,  
          #                  "Shoreline wave / tidal",
          #                  "Wind Offshore",       
          #                  "Wind Onshore"),
          tickvals = list(0,1,2,3,4,5,6,7,8,9, 10),
          tickmode = "array"
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = FALSE,
          showticklabels = FALSE,
          range = c(0,9900),
          zeroline = FALSE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F)
    
    p
  })
  
  output$RenElecBreakdownCapSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "R - QTRCapacity", col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- c("Date", "Wind Onshore", "Wind Offshore", "Shoreline wave / tidal", "Solar Photovoltaics", "Small Hydro", "Large Hydro", "Landfill Gas", "Sewage", "Waste", "Animal Biomass", "Anaerobic Digestion", "Plant", "Total")
    
    Data <- Data[2,]
    
    Data$Date <- paste0(substr(Data$Date,1,4), " Q", substr(Data$Date, 8,8))
    
        paste("Scotland,", Data$Date)
  })
  
  output$RenElecBreakdownCap.png <- downloadHandler(
    filename = "RenElecBreakdownCap.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "R - QTRCapacity", col_names = FALSE)
      
      Data <- as_tibble(t(Data))
      
      names(Data) <- c("Date", "Wind Onshore", "Wind Offshore", "Shoreline wave / tidal", "Solar Photovoltaics", "Small Hydro", "Large Hydro", "Landfill Gas", "Sewage", "Energy from waste", "Animal Biomass", "Anaerobic Digestion", "Plant", "Total")
      
      Data <- Data[2,]
      
      Data$Date <- paste0(substr(Data$Date,1,4), " Q", substr(Data$Date, 8,8))
      
      Data[2:14]%<>% lapply(function(x)
        as.numeric(as.character(x)))
      
      Data$Biomass <- Data$`Animal Biomass` + Data$Plant
      
      Data$`Animal Biomass` <- NULL
      
      Data$Plant <- NULL
      
      Data$`Anaerobic Digestion` <- Data$`Anaerobic Digestion` + Data$Sewage
      
      Data$Sewage <- NULL
      
      Data$Total <- NULL
      
      PipelineTotal <- as_tibble(Data)
      
      PipelineTotal <- melt(PipelineTotal, id.vars = "Date")
      
      PipelineTotal <- PipelineTotal[order(-PipelineTotal$value),]
      
      PipelineTotal$variable <-
        factor(PipelineTotal$variable,
               levels = rev(unique(PipelineTotal$variable)),
               ordered = TRUE)
      
      PipelineTotal$value <- as.numeric(PipelineTotal$value)
      
      PipelineTotal <- PipelineTotal %>%
        group_by(Date) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = max(value))
      
      plottitle <-
        "Operational renewable capacity by technology"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c(
          "#31a354",
          "#0868ac",
          "#43a2ca",
          "#7bccc4",
          "#a6bddb",
          "#d0d1e6",
          "#bdbdbd",
          "#969696"
        )
      
      
      PipelineTotalChart <- PipelineTotal %>%
        ggplot(aes(x = variable, y = value), family = "Century Gothic") +
        geom_bar(stat = "identity", width = .4, fill = ChartColours[1]) +
        geom_text(
          aes(
            x = variable,
            y = -200,
            label = variable,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 1
        ) +
        geom_text(
          aes(
            x = variable,
            y = value+1000  ,
            label = paste0(format(round(value, digits = 0), big.mark = ","), " MW"),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 1.7,
            y = (3.5/4) * 15,
            label = " ",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic"
        )
      
      
      
      PipelineTotalChart
      
      
      PipelineTotalChart <-
        StackedBars(PipelineTotalChart,
                    PipelineTotal,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      PipelineTotalChart <-
        PipelineTotalChart +
        labs(subtitle = paste("Scotland,", PipelineTotal$Date)) +
        ylim(-3500, max(PipelineTotal$top)+1700)+
        coord_flip()
      
      PipelineTotalChart
      
      ggsave(
        file,
        plot = PipelineTotalChart,
        width = 17.5,
        height = 10,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$RenElecFuelSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecCapFuel <- as_tibble(Data)
    
    paste(min(RenElecCapFuel$Year),"-", max(RenElecCapFuel$Year))
  })
  
  output$RenElecFuelPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    
    
    RenElecCapFuel <- as_tibble(Data)
    
    RenElecCapFuel[is.na(RenElecCapFuel)] <- 0
    
    RenElecCapFuel <- RenElecCapFuel[c(1, (ncol(RenElecCapFuel) - 1):2)]
    
    RenElecCapFuel <- arrange(RenElecCapFuel,-row_number())
    
    RenElecCapFuel$Total <- RenElecCapFuel$`Other bioenergy` + RenElecCapFuel$`Sewage gas` + RenElecCapFuel$`Wave and tidal` + RenElecCapFuel$`Landfill gas` + RenElecCapFuel$`Solar PV` + RenElecCapFuel$Hydro + RenElecCapFuel$`Offshore Wind` + RenElecCapFuel$`Onshore Wind`
    
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#004529",
        "#006837",
        "#238443",
        "#41ab5d",
        "#78c679",
        "#7bccc4",
        "#4eb3d3",
        "#2b8cbe"
      )
    
    RenElecCapFuel$Year <-
      paste0("01/01/", RenElecCapFuel$Year)
    
    RenElecCapFuel$Year <- dmy(RenElecCapFuel$Year)
    
    p <- plot_ly(
      data = RenElecCapFuel,
      x = ~Year,
      y = ~`Onshore Wind`,
      legendgroup = 1,
      stackgroup = 1,
      text = paste0(
        "Onshore Wind: ",
        format(round(RenElecCapFuel$`Onshore Wind`, digits = 0),big.mark = ","),
        " MW\nYear: ",
        format(RenElecCapFuel$Year, "%Y")
      ),
      name = "Onshore Wind",
      type = "scatter",
      mode = "none",
      hoverinfo = "text",
      fillcolor = (BarColours[1])
    ) %>% 
      add_trace(
        data = RenElecCapFuel,
        x = ~Year,
        y = ~`Offshore Wind`,
        legendgroup = 2,
        text = paste0(
          "Offshore Wind: ",
          format(round(RenElecCapFuel$`Offshore Wind`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          format(RenElecCapFuel$Year, "%Y")
        ),
        name = "Offshore Wind",
        type = "scatter",
        hoverinfo = "text",
        fillcolor = (BarColours[2])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        x = ~Year,
        y = ~`Hydro`,
        legendgroup = 3,
        text = paste0(
          "Hydro: ",
          format(round(RenElecCapFuel$`Hydro`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          format(RenElecCapFuel$Year, "%Y")
        ),
        name = "Hydro",
        type = "scatter",
        hoverinfo = "text",
        fillcolor = (BarColours[3])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        x = ~Year,
        y = ~`Solar PV`,
        legendgroup = 4,
        text = paste0(
          "Solar PV: ",
          format(round(RenElecCapFuel$`Solar PV`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          format(RenElecCapFuel$Year, "%Y")
        ),
        name = "Solar PV",
        type = "scatter",
        hoverinfo = "text",
        fillcolor = (BarColours[4])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        x = ~Year,
        y = ~`Landfill gas`,
        legendgroup = 5,
        text = paste0(
          "Landfill gas: ",
          format(round(RenElecCapFuel$`Landfill gas`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          format(RenElecCapFuel$Year, "%Y")
        ),
        name = "Landfill gas",
        type = "scatter",
        hoverinfo = "text",
        fillcolor = (BarColours[5])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        x = ~Year,
        y = ~`Wave and tidal`,
        legendgroup = 6,
        text = paste0(
          "Wave and tidal: ",
          format(round(RenElecCapFuel$`Wave and tidal`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          format(RenElecCapFuel$Year, "%Y")
        ),
        name = "Wave and tidal",
        type = "scatter",
        hoverinfo = "text",
        fillcolor = (BarColours[6])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        x = ~Year,
        y = ~`Sewage gas`,
        legendgroup = 7,
        text = paste0(
          "Sewage gas: ",
          format(round(RenElecCapFuel$`Sewage gas`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          format(RenElecCapFuel$Year, "%Y")
        ),
        name = "Sewage gas",
        type = "scatter",
        hoverinfo = "text",
        fillcolor = (BarColours[7])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        x = ~Year,
        y = ~`Other bioenergy`,
        legendgroup = 8,
        text = paste0(
          "Other bioenergy: ",
          format(round(RenElecCapFuel$`Other bioenergy`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          format(RenElecCapFuel$Year, "%Y")
        ),
        name = "Other bioenergy",
        type = "scatter",
        hoverinfo = "text",
        fillcolor = (BarColours[8])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(RenElecCapFuel$Year)-100, max(RenElecCapFuel$Year)+100)),
        yaxis = list(
          title = "MW",
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
  
  
  output$RenElecFuelCapTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecCapFuel <- as_tibble(Data)
    
    RenElecCapFuel <- RenElecCapFuel[c(1, (ncol(RenElecCapFuel) - 1):2)]
    
    RenElecCapFuel <- arrange(RenElecCapFuel,-row_number())
    
    RenElecCapFuel$Total <- RenElecCapFuel$`Other bioenergy` + RenElecCapFuel$`Sewage gas` + RenElecCapFuel$`Wave and tidal` + RenElecCapFuel$`Landfill gas` + RenElecCapFuel$`Solar PV` + RenElecCapFuel$Hydro + RenElecCapFuel$`Offshore Wind` + RenElecCapFuel$`Onshore Wind`
    
    
    datatable(
      RenElecCapFuel[c(1,9:2,10)],
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
        title = "Renewable Electricity - Installed Capacity (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Electricity - Installed Capacity (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Electricity - Installed Capacity (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecCapFuel), 0)
  })
  

  output$LACapacitySubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "R - QTRCapacity", col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- c("Date", "Wind Onshore", "Wind Offshore", "Shoreline wave / tidal", "Solar Photovoltaics", "Small Hydro", "Large Hydro", "Landfill Gas", "Sewage", "Waste", "Animal Biomass", "Anaerobic Digestion", "Plant", "Total")
    
    Data <- Data[2,]
    
    Data$Date <- paste0(substr(Data$Date,1,4), " Q", substr(Data$Date, 8,8))
    
    paste("Scotland,", Data$Date)
  })
  
  
  output$LACapacityMap <- renderLeaflet({
    
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
    
    LARenCapPipeline <- read_delim("Processed Data/Output/Renewable Capacity/LARenCap.txt", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,2,7)]
    
    LARenCapPipeline <- LARenCapPipeline[c(1,2,ncol(LARenCapPipeline))]
    
    names(LARenCapPipeline) <- c("LocalAuthority", "CODE", "Capacity")
    
    LARenCapPipeline <- LARenCapPipeline[which(substr(LARenCapPipeline$CODE, 1,3)== "S12"),]
    
    LARenCapPipeline$Content <- paste0("<b>",LARenCapPipeline$LocalAuthority, "</b><br/>Renewable Electricity<br/>Capacity:<br/><em>", round(LARenCapPipeline$Capacity, digits = 0)," MW</em>" )
    
    LARenCapPipeline$Hover <- paste0(LARenCapPipeline$LocalAuthority, " - ", round(LARenCapPipeline$Capacity, digits = 2), " MW")
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    LARenCapPipeline <- LARenCapPipeline[order(LARenCapPipeline$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      append_data(LA, LARenCapPipeline, key.shp = "CODE", key.data = "CODE")
    
    
    pal <- colorNumeric(
      palette = "Greens",
      domain = LAMap$Capacity)
    
    palWithoutNA <- colorNumeric(
      palette = "Greens",
      domain = LAMap$Capacity,
      na.color=rgb(0,0,0,0))
    
    l <-leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas", ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(Capacity),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal =  palWithoutNA, values = ~Capacity,
                         title = "Renewable  Electricity<br/>Capacity (MW)",
                         opacity = 1
      ) 
    
    l
    
  })
  
  output$RenElecFuel.png <- downloadHandler(
    filename = "RenElecFuel.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx",
                         sheet = "Renewable elec by fuel",
                         col_names = TRUE,
                         skip = 12
      )
      
      Data<- Data[complete.cases(Data),]
      
      Data <- distinct(Data, Year, .keep_all = TRUE)
      
      Data <- head(Data, -1)
      
      Data %<>% lapply(function(x)
        as.numeric(as.character(x)))
      
      RenElecCapFuel <- as_tibble(Data)
      
      RenElecCapFuel[is.na(RenElecCapFuel)] <- 0
      
      RenElecCapFuel <- RenElecCapFuel[c(1, (ncol(RenElecCapFuel) - 1):2)]
      
      RenElecCapFuel <- arrange(RenElecCapFuel,-row_number())
      
      
      RenElecCapFuel <- melt(RenElecCapFuel, id.vars = "Year")
      
      
      RenElecCapFuel$variable <-
        factor(RenElecCapFuel$variable,
               levels = unique(RenElecCapFuel$variable),
               ordered = TRUE)
      
      RenElecCapFuel <- RenElecCapFuel %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Installed capacity of sites generating electricity from renewable sources"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c(
          "#004529",
          "#006837",
          "#238443",
          "#41ab5d",
          "#78c679",
          "#7bccc4",
          "#4eb3d3",
          "#2b8cbe"
        )
      
      width <- max(RenElecCapFuel$Year) - min(RenElecCapFuel$Year)
      
      RenElecCapFuelChart <- RenElecCapFuel %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        annotate(
          "segment",
          x = min(RenElecCapFuel$Year)-10,
          xend = max(RenElecCapFuel$Year)+10,
          y = 2500,
          yend = 2500,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year),
            y = 2500,
            label = "2500 MW",
            fontface = 2
          ),
          colour = "grey",
          hjust = 0,
          vjust = -.3,
          family = "Century Gothic",
          size = 3
        ) +
        annotate(
          "segment",
          x = min(RenElecCapFuel$Year)-10,
          xend = max(RenElecCapFuel$Year)+10,
          y = 5000,
          yend = 5000,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year),
            y = 5000,
            label = "5000 MW",
            fontface = 2
          ),
          colour = "grey",
          hjust = 0,
          vjust = -.3,
          family = "Century Gothic",
          size = 3
        ) +
        annotate(
          "segment",
          x = min(RenElecCapFuel$Year)-10,
          xend = max(RenElecCapFuel$Year)+10,
          y = 7500,
          yend = 7500,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year),
            y = 7500,
            label = "7500 MW",
            fontface = 2
          ),
          colour = "grey",
          hjust = 0,
          vjust = -.3,
          family = "Century Gothic",
          size = 3
        ) +
        annotate(
          "segment",
          x = min(RenElecCapFuel$Year)-10,
          xend = max(RenElecCapFuel$Year)+10,
          y = 10000,
          yend = 10000,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year),
            y = 10000,
            label = "10000 MW",
            fontface = 2
          ),
          colour = "grey",
          hjust = 0,
          vjust = -.3,
          family = "Century Gothic",
          size = 3
        ) +
        annotate(
          "segment",
          x = min(RenElecCapFuel$Year)-10,
          xend = max(RenElecCapFuel$Year)+10,
          y = 12500,
          yend = 12500,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year),
            y = 12500,
            label = "12500 MW",
            fontface = 2
          ),
          colour = "grey",
          hjust = 0,
          vjust = -.3,
          family = "Century Gothic",
          size = 3
        ) +
        scale_fill_manual(
          "variable",
          values = c(
            "Onshore Wind" = BarColours[1],
            "Offshore Wind" = BarColours[2],
            "Hydro" = BarColours[3],
            "Solar PV" = BarColours[4],
            "Landfill gas" = BarColours[5],
            "Wave and tidal" = BarColours[6],
            "Sewage gas" = BarColours[7],
            "Other bioenergy" = BarColours[8],
            "Total" = "White"
          )
        ) +
        geom_area(posistion = "fill") +
        annotate(
          "text",
          x = RenElecCapFuel$Year,
          y = -250,
          label = ifelse(RenElecCapFuel$Year == "z", "", str_wrap(RenElecCapFuel$Year, width = 20)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1],
          size = 3
        ) +
        geom_text(
          aes(
            x = max(RenElecCapFuel$Year)+.1,
            y = 4000,
            label = "Onshore Wind"
          ),
          fontface = 2,
          colour =  BarColours[1],
          family = "Century Gothic",
          hjust = 0,
          size = 3
        ) +
        geom_text(
          aes(
            x = max(RenElecCapFuel$Year)+.1,
            y = 8800,
            label = "Offshore Wind"
          ),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic",
          hjust = 0,
          size = 3
        ) +
        geom_text(
          aes(
            x = max(RenElecCapFuel$Year)+.1,
            y = 9900,
            label = "Hydro"
          ),
          fontface = 2,
          colour =  BarColours[3],
          family = "Century Gothic",
          hjust = 0,
          size = 3
        ) +
        geom_text(
          aes(
            x = max(RenElecCapFuel$Year)+.1,
            y = 11000,
            label = "Solar PV"
          ),
          fontface = 2,
          colour =  BarColours[4],
          family = "Century Gothic",
          hjust = 0,
          size = 3
        ) +
        geom_text(
          aes(
            x = max(RenElecCapFuel$Year)+.1,
            y = 11350,
            label = "Landfill gas"
          ),
          fontface = 2,
          colour =  BarColours[5],
          family = "Century Gothic",
          hjust = 0,
          size = 3
        ) +
        geom_text(
          aes(
            x = max(RenElecCapFuel$Year)+.1,
            y = 11700,
            label = "Wave and Tidal"
          ),
          fontface = 2,
          colour =  BarColours[6],
          family = "Century Gothic",
          hjust = 0,
          size = 3
        ) +
        geom_text(
          aes(
            x = max(RenElecCapFuel$Year)+.1,
            y = 12000,
            label = "Sewage Gas"
          ),
          fontface = 2,
          colour =  BarColours[7],
          family = "Century Gothic",
          hjust = 0,
          size = 3
        ) +
        geom_text(
          aes(
            x = max(RenElecCapFuel$Year)+.1,
            y = 12330,
            label = "Other Bioenergy"
          ),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0,
          size = 3
        ) +
        geom_text(
          aes(x = max(RenElecCapFuel$Year)+.1,
              y = (max(RenElecCapFuel$top)*1.1) * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = (max(RenElecCapFuel$top)*1.1) * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = -100,
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) 
      RenElecCapFuelChart
      
      
      RenElecCapFuelChart <-
        StackedArea(RenElecCapFuelChart,
                    RenElecCapFuel,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      RenElecCapFuelChart <-
        RenElecCapFuelChart +
        labs(subtitle = paste("Scotland,", min(RenElecCapFuel$Year), "-", max(RenElecCapFuel$Year))) +
        ylim(-(max(RenElecCapFuel$top)*0.025), (max(RenElecCapFuel$top)*1.08)) +
        coord_cartesian(xlim = c(min(RenElecCapFuel$Year),max(RenElecCapFuel$Year+2)))
      
      RenElecCapFuelChart
      
      ggsave(
        file,
        plot = RenElecCapFuelChart,
        width = 24,
        height = 18,
        units = "cm",
        dpi = 300
      )
    }
  )

  ######
  Time <- read_excel("Structure/CurrentWorking.xlsx", 
                     sheet = "Renewable elec pipeline", col_names = TRUE,
                     skip = 26, n_max = 1)
  Quarter <- substr(Time[1,1], 8,8)
  
  Quarter <- as.numeric(Quarter)*3
  
  Year <- substr(Time[1,1], 1,4)
  
  Subtitle <- paste("Scotland,", month.name[Quarter], Year)
  
  
  ######
  
  observeEvent(input$ToggleTable5, {
    toggle("RenElecPipelineTimeTable")
  })
  
  output$RenElecOperational.png <- downloadHandler(
    filename = "RenElecOperational.png",
    content = function(file) {
      
      
      RenElecOperational <- read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable elec capacity",
        col_names = TRUE,
        skip = 13
      )
      
      
      RenElecOperational <- RenElecOperational[c(2, 4)]
      
      names(RenElecOperational) <- c("Year", "Total")
      
      RenElecOperational$Total <-
        as.numeric(RenElecOperational$Total)
      
      RenElecOperational$Year <-
        as.yearqtr(RenElecOperational$Year, format = "%Y Q%q")
      
      RenElecOperational <-
        RenElecOperational[order(RenElecOperational$Year), ]
      
      RenElecOperational <-
        RenElecOperational[complete.cases(RenElecOperational), ]
      ### variables
      ChartColours <- c("#39ab2c", "#238b45", "#a1d99b")
      LineColours <- c("#39ab2c", "#238b45", "#a1d99b")
      sourcecaption = "Source: BEIS"
      plottitle = "Operational renewable capacity"
      
      #RenElecOperational$CavityPercentage <- PercentLabel(RenElecOperational$Cavity)
      
      
      RenElecOperationalChart <- RenElecOperational %>%
        ggplot(aes(x = Year,
                   y = Total)) +
        geom_line(aes(),
                  size = 1.5,
                  colour = LineColours[1],
                  family = "Century Gothic") +
        geom_point(
          data = tail(RenElecOperational, 1),
          aes(x = Year,
              y = Total),
          size = 4,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            label = ifelse(Year == min(Year), paste(round(Total, digits = 1), "MW"), ""),
            show_guide = FALSE
          ),
          fontface = 2,
          hjust = 1.1,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .3,
            label = ifelse(Year == max(Year), paste(round(Total, digits = 1), "MW"), ""),
            show_guide = FALSE
          ),
          fontface = 2,
          hjust = -0.1,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            y = 0,
            label = ifelse(
              Year == min(Year) |
                Year == max(Year),
              format(Year, format = "%Y Q%q"),
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      RenElecOperationalChart
      
      
      RenElecOperationalChart <-
        StackedArea(
          RenElecOperationalChart,
          RenElecOperational,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      
      RenElecOperationalChart <- RenElecOperationalChart +
        xlim(min(as.numeric(RenElecOperational$Year) - 2), max(as.numeric(RenElecOperational$Year) + 3))
      
      RenElecOperationalChart
      
      
      ggsave(
        file,
        plot = RenElecOperationalChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenElecOperationalSizeTable = renderDataTable({
    
    CapacitySizeTech <- read_delim("Processed Data/Output/Capacity by Size/CapacitySizeTech.txt", 
                                   "\t", escape_double = FALSE, col_names = FALSE, 
                                   trim_ws = TRUE)
    
    
    names(CapacitySizeTech) <- unlist(CapacitySizeTech[1,])
    
    
    CapacitySizeTech <- CapacitySizeTech[-1,]
    
    datatable(
      CapacitySizeTech[c(1,3,2,4:7),],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        columnDefs = list(list(className = 'dt-right', targets = 1:5)),
        autoWidth = TRUE,

        title = "Operational renewable capacity by installation size (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Operational renewable capacity by installation size (MW)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Operational renewable capacity by installation size (MW)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(CapacitySizeTech), 0)
  })
  
  output$RenElecOperationalSizeSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "R - QTRCapacity", col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- c("Date", "Wind Onshore", "Wind Offshore", "Shoreline wave / tidal", "Solar Photovoltaics", "Small Hydro", "Large Hydro", "Landfill Gas", "Sewage", "Waste", "Animal Biomass", "Anaerobic Digestion", "Plant", "Total")
    
    Data <- Data[2,]
    
    Data$Date <- paste0(substr(Data$Date,1,4), " Q", substr(Data$Date, 8,8))
    
    paste("Scotland,", Data$Date)
  })
  
  output$RenElecOperationalSizePlot <- renderPlotly  ({
    
    OperationalSize <- read_delim("Processed Data/Output/Capacity by Size/CapacitySizeTech.txt", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(OperationalSize) <- c("Technology Type", "Tiny", "Small", "Medium", "Large", "Total")
    
    OperationalSize <- OperationalSize[order(-OperationalSize$Total),]
       
    OperationalSize$Year <- paste("<b>", OperationalSize$`Technology Type`, "</b>")
    
    ChartColours <- c("#39ab2c", "#FF8500")
    
    BarColours <- c("#addd8e",
                    "#78c679",
                    "#41ab5d",
                    "#238443")
    
    p <- plot_ly(data = OperationalSize, y = ~ Year) %>%
      
      add_trace(
        data = OperationalSize,
        x = ~ `Tiny`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Installations below 5MW",
        text = paste0("Total capacity of installations below 5 MW: ", format(round(OperationalSize$`Tiny`, digits = 0.1), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = OperationalSize,
        x = ~ `Small`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Installations between 5 - 10MW",
        text = paste0("Total capacity of installations between 5 - 10 MW: ", format(round(OperationalSize$`Small`, digits = 0.1), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = OperationalSize,
        x = ~ `Medium`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Installations between 10 - 50MW",
        text = paste0("Total capacity of installations between 10 - 50 MW: ", format(round(OperationalSize$`Medium`, digits = 0.1), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = OperationalSize,
        x = ~ `Large`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Installations above 50 MW",
        text = paste0("Total capacity of installations above 50 MW: ", format(round(OperationalSize$`Large`, digits = 0.1), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(OperationalSize$`Year`),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
  })
  
  
  output$LARenCapPipelineTable = renderDataTable({
    
    LARenCapPipeline <- read_delim("Processed Data/Output/Renewable Capacity/LARenCap.txt", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,2,7)]
    
    
    names(LARenCapPipeline) <- c("Local Authority", "LA Code", "Operational capacity (MW)")
    
    datatable(
      LARenCapPipeline,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'asc')),
        title = "Operational renewable capacity by Local Authority (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Operational renewable capacity by Local Authority (MW)",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Operational renewable capacity by Local Authority (MW)")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(3, 1) 
  })
  
  
  output$RenElecOperationalSize.png <- downloadHandler(
    filename = "RenElecOperationalSize.png",
    content = function(file) {
      
      
      OperationalSize <- read_delim("Processed Data/Output/Capacity by Size/CapacitySizeTech.txt", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)
      
      names(OperationalSize) <- c("Type", "Tiny", "Small", "Medium", "Large", "Total")
      
      OperationalSize <- OperationalSize[order(OperationalSize$Total),]
      
      OperationalSize <- OperationalSize[which(OperationalSize$Total > 250),]
      
      
      
      
      ChartColours <- c("#39ab2c", "#FF8500")
      
      BarColours <- c("#addd8e",
                      "#78c679",
                      "#41ab5d",
                      "#238443")
      
      
      OperationalSize<- OperationalSize[1:5]
      
  
      
      OperationalSize <- melt(OperationalSize, id.vars = "Type")
      
      OperationalSize$Type <- str_wrap(OperationalSize$Type, 26)
      
          OperationalSize$Type <-
        factor(OperationalSize$Type,
               levels = unique(OperationalSize$Type),
               ordered = TRUE)
      
      OperationalSize$variable <-
        factor(OperationalSize$variable,
               levels = rev(unique(OperationalSize$variable)),
               ordered = TRUE)
      
      OperationalSize <- OperationalSize %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      
      
      plottitle <-
        "Operational renewable capacity by installation size"
      sourcecaption <- "Source: BEIS"
      
      
      
      OperationalSizeChart <- OperationalSize %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Tiny" = BarColours[1],
            "Small" = BarColours[2],
            "Medium" = BarColours[3],
            "Large" = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            y = pos,
            label = ifelse(OperationalSize$value > 800 & OperationalSize$Type == "All Technologies", paste0(format(round(value, 0), big.mark = ",", trim = TRUE),"\nMW"), ""),
            
            fontface = 2
          ),
          colour = "white",
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Type,
            y = -100,
            label = Type,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 1
        ) +
        geom_text(
          aes(
            x = 6.95,
            y = max(OperationalSize$top)*0.05,
            label = "Installations\nbelow\n5 MW",
            fontface = 2
          ),
          colour = BarColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 6.95,
            y = max(OperationalSize$top)*0.33,
            label = "Installations\nbetween\n5 MW - 10 MW",
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 6.95,
            y = max(OperationalSize$top)*0.63,
            label = "Installations\nbetween\n10 MW - 50 MW",
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        )+
        
        geom_text(
          aes(
            x = 6.95,
            y = max(OperationalSize$top)*0.9,
            label = "Installations\nabove\n50 MW",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = 6.95,
            y = max(OperationalSize$top)*1.08,
            label = "Total\nCapacity",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = 7.45,
            y = .5,
            label = "",
            fontface = 2
          ),
          colour = "black",
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Type,
            y = top + 630,
            label = paste0(format(round(OperationalSize$top,0), big.mark = ",", trim = TRUE), "\nMW"),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5
        )
      
      
      
      OperationalSizeChart
      
      
      OperationalSizeChart <-
        StackedBars(OperationalSizeChart,
                    OperationalSize,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      OperationalSizeChart <-
        OperationalSizeChart +
        labs(subtitle = Subtitle) +
        ylim(-max(OperationalSize$top)*0.30,max(OperationalSize$top)*1.1)+
        coord_flip()
      
      OperationalSizeChart
      
      ggsave(
        file,
        plot = OperationalSizeChart,
        width = 20,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenSitesSubtitle <- renderText({
    
    Data <- read_delim("Processed Data/Output/Renewable Generation/RenewableSites.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    paste(min(Data$Year),"-", max(Data$Year))
  })
  
  output$RenSitesPlot <- renderPlotly  ({
    
    Data <- read_delim("Processed Data/Output/Renewable Generation/RenewableSites.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    RenSites <- as_tibble(Data)
    
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#004529",
        "#006837",
        "#238443",
        "#41ab5d",
        "#78c679",
        "#7bccc4",
        "#4eb3d3",
        "#2b8cbe"
      )
    
    RenSites$Year <-
      paste0("01/01/", RenSites$Year)
    
    RenSites$Year <- dmy(RenSites$Year)
    
    p <- plot_ly(
      data = RenSites,
      x = ~Year,
      y = ~`Onshore Wind`,
      legendgroup = 1,
      stackgroup = 1,
      text = paste0(
        "Onshore Wind: ",
        format(round(RenSites$`Onshore Wind`, digits = 0),big.mark = ","),
        "\nYear: ",
        format(RenSites$Year, "%Y")
      ),
      name = "Onshore Wind",
      type = "scatter",
      mode = "none",
      hoverinfo = "text",
      fillcolor = ( BarColours[1])
    ) %>% 
      add_trace(
        data = RenSites,
        x = ~Year,
        y = ~`Offshore Wind`,
        legendgroup = 2,
        text = paste0(
          "Offshore Wind: ",
          format(round(RenSites$`Offshore Wind`, digits = 0),big.mark = ","),
          "\nYear: ",
          format(RenSites$Year, "%Y")
        ),
        name = "Offshore Wind",
        type = "scatter",
        mode = "none",
        hoverinfo = "text",
        fillcolor = ( BarColours[2])
      )  %>% 
      add_trace(
        data = RenSites,
        x = ~Year,
        y = ~`Hydro`,
        legendgroup = 3,
        text = paste0(
          "Hydro: ",
          format(round(RenSites$`Hydro`, digits = 0),big.mark = ","),
          "\nYear: ",
          format(RenSites$Year, "%Y")
        ),
        name = "Hydro",
        type = "scatter",
        mode = "none",
        hoverinfo = "text",
        fillcolor = ( BarColours[3])
      )  %>% 
      add_trace(
        data = RenSites,
        x = ~Year,
        y = ~`Solar PV`,
        legendgroup = 4,
        text = paste0(
          "Solar PV: ",
          format(round(RenSites$`Solar PV`, digits = 0),big.mark = ","),
          "\nYear: ",
          format(RenSites$Year, "%Y")
        ),
        name = "Solar PV",
        type = "scatter",
        mode = "none",
        hoverinfo = "text",
        fillcolor = ( BarColours[4])
      )  %>% 
      add_trace(
        data = RenSites,
        x = ~Year,
        y = ~`Landfill gas`,
        legendgroup = 5,
        text = paste0(
          "Landfill gas: ",
          format(round(RenSites$`Landfill gas`, digits = 0),big.mark = ","),
          "\nYear: ",
          format(RenSites$Year, "%Y")
        ),
        name = "Landfill gas",
        type = "scatter",
        mode = "none",
        hoverinfo = "text",
        fillcolor = ( BarColours[5])
      )  %>% 
      add_trace(
        data = RenSites,
        x = ~Year,
        y = ~`Wave and tidal`,
        legendgroup = 6,
        text = paste0(
          "Wave and tidal: ",
          format(round(RenSites$`Wave and tidal`, digits = 0),big.mark = ","),
          "\nYear: ",
          format(RenSites$Year, "%Y")
        ),
        name = "Wave and tidal",
        type = "scatter",
        mode = "none",
        hoverinfo = "text",
        fillcolor = ( BarColours[6])
      )  %>% 
      add_trace(
        data = RenSites,
        x = ~Year,
        y = ~`Sewage gas`,
        legendgroup = 7,
        text = paste0(
          "Sewage gas: ",
          format(round(RenSites$`Sewage gas`, digits = 0),big.mark = ","),
          "\nYear: ",
          format(RenSites$Year, "%Y")
        ),
        name = "Sewage gas",
        type = "scatter",
        mode = "none",
        hoverinfo = "text",
        fillcolor = ( BarColours[7])
      )  %>% 
      add_trace(
        data = RenSites,
        x = ~Year,
        y = ~`Other Bioenergy`,
        legendgroup = 8,
        text = paste0(
          "Other Bioenergy: ",
          format(round(RenSites$`Other Bioenergy`, digits = 0),big.mark = ","),
          "\nYear: ",
          format(RenSites$Year, "%Y")
        ),
        name = "Other Bioenergy",
        type = "scatter",
        mode = "none",
        hoverinfo = "text",
        fillcolor = ( BarColours[8])
      )  %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(RenSites$Year)-100, max(RenSites$Year)+100)),
        yaxis = list(
          title = " ",
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
  
  
  output$RenSitesTable = renderDataTable({
    
    Data <- read_delim("Processed Data/Output/Renewable Generation/RenewableSites.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    RenSites <- as_tibble(Data)
    
    
    datatable(
      RenSites,
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
        title = "Renewable Electricity - Renewable Sites",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Electricity - Renewable Sites',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Electricity - Renewable Sites')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenSites), 0)
  })
  
  
  output$RenSites.png <- downloadHandler(
    filename = "RenSites.png",
    content = function(file) {
      
      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      Data <- read_delim("Processed Data/Output/Renewable Generation/RenewableSites.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE)
      
      
      
      RenSites <- as_tibble(Data[1:9])
      
      
      RenSites <- melt(RenSites, id.vars = "Year")
      
      
      RenSites$variable <-
        factor(RenSites$variable,
               levels = unique(RenSites$variable),
               ordered = TRUE)
      
      RenSites <- RenSites %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Number of sites generating electricity\nfrom renewable sources"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c(
          "#004529",
          "#006837",
          "#238443",
          "#41ab5d",
          "#78c679",
          "#7bccc4",
          "#4eb3d3",
          "#2b8cbe"
        )
      
      
      RenSitesChart <- RenSites %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Onshore Wind" = BarColours[1],
            "Offshore Wind" = BarColours[2],
            "Hydro" = BarColours[3],
            "Solar PV" = BarColours[4],
            "Landfill gas" = BarColours[5],
            "Wave and tidal" = BarColours[6],
            "Sewage gas" = BarColours[7],
            "Other Bioenergy" = BarColours[8],
            "Total" = "White"
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = RenSites$Year,
          y = -.01,
          label = ifelse(RenSites$Year == "z", "", str_wrap(RenSites$Year, width = 20)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1],
          size = 3,
          hjust = 1.05
        ) +
        geom_text(
          aes(
            x = 1998.5,
            y = (max(RenSites$top)*1.1) * (.5 / 8),
            label = "Onshore\nWind"
          ),
          fontface = 2,
          colour =  BarColours[1],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 1998.5,
            y = (max(RenSites$top)*1.1) * (1.5 / 8),
            label = "Offshore\nWind"
          ),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 1998.5,
            y = (max(RenSites$top)*1.1) * (2.5 / 8),
            label = "Hydro"
          ),
          fontface = 2,
          colour =  BarColours[3],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 1998.5,
            y = (max(RenSites$top)*1.1) * (3.5 / 8),
            label = "Solar PV"
          ),
          fontface = 2,
          colour =  BarColours[4],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 1998.5,
            y = (max(RenSites$top)*1.1) * (4.5 / 8),
            label = "Landfill\ngas"
          ),
          fontface = 2,
          colour =  BarColours[5],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 1998.5,
            y = (max(RenSites$top)*1.1) * (5.5 / 8),
            label = "Wave\nand Tidal"
          ),
          fontface = 2,
          colour =  BarColours[6],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 1998.5,
            y = (max(RenSites$top)*1.1) * (6.5 / 8),
            label = "Sewage\nGas"
          ),
          fontface = 2,
          colour =  BarColours[7],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = 1998.5,
            y = (max(RenSites$top)*1.1) * (7.5 / 8),
            label = "Other\nBioenergy"
          ),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 1997.5,
              y = (max(RenSites$top)*1.1) * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = (max(RenSites$top)*1.1) * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = -100,
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = RenSites$Year ,
            y = RenSites$top,
            label = paste(format(
              round(RenSites$top, digits = 0), big.mark = ","
            ), "")
          ),
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic",
          hjust = -0.1,
          size = 3
        ) 
      RenSitesChart
      
      
      RenSitesChart <-
        StackedBars(RenSitesChart,
                    RenSites,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      RenSitesChart <-
        RenSitesChart +
        labs(subtitle = paste("Scotland,", min(RenSites$Year), "-", max(RenSites$Year))) +
        coord_flip() +
        xlim(max(RenSites$Year+.5),min(RenSites$Year-.5)) +
        ylim(-(max(RenSites$top)*0.03), (max(RenSites$top)*1.05))
      
      RenSitesChart
      
      ggsave(
        file,
        plot = RenSitesChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  
  
  output$LACapMap <- renderLeaflet({
    
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
    
    Year = as.numeric(input$YearSelect)
    
    Tech = as.character(input$TechSelect)
    
    LARenCap <- read_delim("Processed Data/Output/Renewable Capacity/LAOperationalRenCap.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)
    
    LARenCap <-  melt(LARenCap, id.vars = c("LACode", "LAName", "Year"))
    
    names(LARenCap)[1] <- "CODE"
    
    LARenCap <- LARenCap[which(LARenCap$Year == Year),]
    
    LARenCap <- LARenCap[which(substr(LARenCap$CODE,1,3) == "S12"),]
    
    LARenCap <- LARenCap[which(LARenCap$variable == Tech),]
    
    LARenCap$Content <- paste0("<b>",LARenCap$LAName, "</b><br/>", LARenCap$variable[1], " Capacity:<br/><em>", round(LARenCap$value, digits = 1)," MW</em>" )
    
    LARenCap$Hover <- paste0(LARenCap$LAName, " - ", round(LARenCap$value, digits = 1), " MW")
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    LARenCap <- LARenCap[order(LARenCap$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      append_data(LA, LARenCap, key.shp = "CODE", key.data = "CODE")
    
    
    pal <- colorNumeric(
      palette = "Greens",
      domain = LAMap$value)
    
    palWithoutNA <- colorNumeric(
      palette = "Greens",
      domain = LAMap$value,
      na.color=rgb(0,0,0,0))
    
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
      leaflet::addLegend("bottomright", pal = palWithoutNA, values = ~value,
                         title = paste0(LARenCap$variable[1], " Capacity (MW)"),
                         opacity = 1
      ) 
    
    l
    
  })
  
  
  
  
  
  output$LACapTable = renderDataTable({
    
    LARenCap <- read_delim("Processed Data/Output/Renewable Capacity/LAOperationalRenCap.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)
    
    Year2 = as.numeric(input$YearSelect2)
    
    LARenCap <- LARenCap[which(substr(LARenCap$LACode,1,3) == "S12"),]
    
    LARenCap <- LARenCap[which(LARenCap$Year == Year2),]
    
    LARenCap <- LARenCap[order(-LARenCap$Year, LARenCap$LAName),]
    
    datatable(
      LARenCap,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Renewable electricity generation at Local Authority Level (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable electricity generation at Local Authority Level (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable electricity generation at Local Authority Level (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(4:ncol(LARenCap), 1) 
  })
  
  
  output$LACapMap.png <- downloadHandler(
    filename = "LACapMap.png",
    content = function(file) {
      file.copy(("Structure/2 - Renewables/Electricity/LARenCap.png"), file) 
    }
  )

  
}
