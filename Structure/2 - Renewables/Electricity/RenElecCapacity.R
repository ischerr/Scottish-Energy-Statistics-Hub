require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

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
    tabPanel("Pipeline renewable capacity",
             fluidRow(column(8,
                             h3("Pipeline renewable capacity by planning stage", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenElecPipelineCapSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecPipelineCap.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenElecCapacityPlot")),
             plotlyOutput(ns("RenElecPipelineCapPlot"), height = "200px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Pipeline capacity tech",
             fluidRow(column(8,
                             h3("Pipeline renewable capacity by technology", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenElecPipelineSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecPipeline.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenElecPipelinePlot")),
             plotlyOutput(ns("RenElecPipelinePlot"), height = "500px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Quarterly Operational Capacity",
               fluidRow(
                 column(10, h3("Data - Operational renewable capacity by technology (MW)", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("RenElecBreakdownCapTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
        tabPanel("Annual Operational Capacity",
                 fluidRow(
                   column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
                   column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
                 ),
                 fluidRow(
                   column(12, dataTableOutput(ns("RenElecFuelCapTable"))%>% withSpinner(color="#39ab2c"))),
                 tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Pipeline Capacity",
             fluidRow(
               column(10, h3("Data - Pipeline Capacity (MW)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecPipelineTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("LA Pipeline Capacity",
             fluidRow(
               column(10, h3("Data - Pipeline Capacity by Local Authority", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable6"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecPipelineLATable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Pipeline Projects",
             fluidRow(
               column(10, h3("Data - Pipeline Projects by Tech", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecPipelineCapTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Pipeline Capacity Time Series",
             fluidRow(
               column(10, h3("Data - Pipeline Capacity Time Series (GW)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable5"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecPipelineTimeTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
      column(1,
             p("Next update:")),
      column(2,
             p("March 2019")),
      column(1, align = "right",
             p("Sources:")),
      column(
        8,
        align = "right",
        SourceLookup("BEISFinalConsump"),
        SourceLookup("ETElecGen"),
        SourceLookup("ESTRenHeat")
        
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
    plottitle = "Opertional renewable capacity"
    
    
    
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
  
 output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecCapacity.txt")[2])
                    
                  )))
 })
 
  
  observeEvent(input$ToggleTable2, {
    toggle("RenElecBreakdownCapTable")
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
    
    names(Data) <- c("Date", "Wind Onshore", "Wind Offshore", "Shoreline wave / tidal", "Solar Photovoltaics", "Small Hydro", "Large Hydro", "Landfill Gas", "Sewage", "Waste", "Animal Biomass", "Anaerobic Digestion", "Plant", "Total")
    
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
      
      names(Data) <- c("Date", "Wind Onshore", "Wind Offshore", "Shoreline wave / tidal", "Solar Photovoltaics", "Small Hydro", "Large Hydro", "Landfill Gas", "Sewage", "Waste", "Animal Biomass", "Anaerobic Digestion", "Plant", "Total")
      
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
        ylim(-3000, max(PipelineTotal$top)+1700)+
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
    
    RenElecCapFuel$Year <- paste0("<b>", RenElecCapFuel$Year, "</b>")
    
    p <- plot_ly(
      data = RenElecCapFuel,
      y = ~Year,
      x = ~`Onshore Wind`,
      legendgroup = 1,
      text = paste0(
        "Onshore Wind: ",
        format(round(RenElecCapFuel$`Onshore Wind`, digits = 0),big.mark = ","),
        " MW\nYear: ",
        RenElecCapFuel$Year
      ),
      name = "Onshore Wind",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Offshore Wind`,
        legendgroup = 2,
        text = paste0(
          "Offshore Wind: ",
          format(round(RenElecCapFuel$`Offshore Wind`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Offshore Wind",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[2])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Hydro`,
        legendgroup = 3,
        text = paste0(
          "Hydro: ",
          format(round(RenElecCapFuel$`Hydro`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Hydro",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[3])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Solar PV`,
        legendgroup = 4,
        text = paste0(
          "Solar PV: ",
          format(round(RenElecCapFuel$`Solar PV`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Solar PV",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[4])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Landfill gas`,
        legendgroup = 5,
        text = paste0(
          "Landfill gas: ",
          format(round(RenElecCapFuel$`Landfill gas`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Landfill gas",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[5])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Wave and tidal`,
        legendgroup = 6,
        text = paste0(
          "Wave and tidal: ",
          format(round(RenElecCapFuel$`Wave and tidal`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Wave and tidal",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[6])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Sewage gas`,
        legendgroup = 7,
        text = paste0(
          "Sewage gas: ",
          format(round(RenElecCapFuel$`Sewage gas`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Sewage gas",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[7])
      )  %>% 
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~`Other bioenergy`,
        legendgroup = 8,
        text = paste0(
          "Other bioenergy: ",
          format(round(RenElecCapFuel$`Other bioenergy`, digits = 0),big.mark = ","),
          " MW\nYear: ",
          RenElecCapFuel$Year
        ),
        name = "Other bioenergy",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[8])
      )  %>%
      add_trace(
        data = RenElecCapFuel,
        y = ~Year,
        x = ~Total + 100,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste("<b>",format(round(RenElecCapFuel$Total, digits = 0), big.mark = ","),"MW</b>"),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      ) %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     autorange = "reversed",
                     dtick = 1),
        xaxis = list(
          title = "",
          tickformat = "",
          range = c(0,13000),
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
  
  observeEvent(input$ToggleTable, {
    toggle("RenElecFuelCapTable")
  })

  output$RenElecFuel.png <- downloadHandler(
    filename = "RenElecFuel.png",
    content = function(file) {
      
      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
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
        "Installed capacity of sites generating\nelectricity from renewable sources"
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
      
      
      RenElecCapFuelChart <- RenElecCapFuel %>%
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
            "Other bioenergy" = BarColours[8],
            "Total" = "White"
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = RenElecCapFuel$Year,
          y = -.01,
          label = ifelse(RenElecCapFuel$Year == "z", "", str_wrap(RenElecCapFuel$Year, width = 20)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1],
          size = 3,
          hjust = 1.05
        ) +
        geom_text(
          aes(
            x = 1999,
            y = 12350 * (.5 / 8),
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
            x = 1999,
            y = 12350 * (1.5 / 8),
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
            x = 1999,
            y = 12350 * (2.5 / 8),
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
            x = 1999,
            y = 12350 * (3.5 / 8),
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
            x = 1999,
            y = 12350 * (4.5 / 8),
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
            x = 1999,
            y = 12350 * (5.5 / 8),
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
            x = 1999,
            y = 12350 * (6.5 / 8),
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
            x = 1999,
            y = 12350 * (7.5 / 8),
            label = "Other\nBioenergy"
          ),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 1998,
              y = 12350 * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = 12350 * (8 / 8),
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
            x = RenElecCapFuel$Year ,
            y = RenElecCapFuel$top,
            label = paste(format(
              round(RenElecCapFuel$top, digits = 0), big.mark = ","
            ), "MW")
          ),
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic",
          hjust = -0.1,
          size = 3
        ) 
      RenElecCapFuelChart
      
      
      RenElecCapFuelChart <-
        StackedBars(RenElecCapFuelChart,
                    RenElecCapFuel,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      RenElecCapFuelChart <-
        RenElecCapFuelChart +
        labs(subtitle = paste("Scotland,", min(RenElecCapFuel$Year), "-", max(RenElecCapFuel$Year))) +
        coord_flip() +
        xlim(max(RenElecCapFuel$Year+.5),min(RenElecCapFuel$Year-1))
      
      RenElecCapFuelChart
      
      ggsave(
        file,
        plot = RenElecCapFuelChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenElecPipelineSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec pipeline", col_names = TRUE,
                       skip = 29, n_max = 1)
    Quarter <- substr(Data[1,1], 8,8)
    
    Quarter <- as.numeric(Quarter)*3
    
    Year <- substr(Data[1,1], 1,4)
    
    paste("Scotland,", month.name[Quarter], Year)
  })
  
  output$RenElecPipelinePlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec pipeline", col_names = TRUE,
                       skip = 16, n_max = 10)[1:6]
    
    Data[2:5]%<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecPipeline <- as_tibble(Data)
    
    RenElecPipeline$Total <- RenElecPipeline$`Under Construction` + RenElecPipeline$`Awaiting Construction` + RenElecPipeline$`In Planning`
    
    names(RenElecPipeline)[1] <- "Type"
    
    RenElecPipeline <- RenElecPipeline[which(RenElecPipeline$Total > 0),]
    
    RenElecPipeline <- arrange(RenElecPipeline, RenElecPipeline$Total)
    
    RenElecPipeline$Type <- paste0("<b>", RenElecPipeline$Type, "</b>")
    
    rownames(RenElecPipeline) <- NULL
    
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
        x = ~ `Under Construction`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Under Construction",
        text = paste0("Under Construction: ", format(round(RenElecPipeline$`Under Construction`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = RenElecPipeline,
        x = ~ `Awaiting Construction`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Awaiting Construction",
        text = paste0("Awaiting Construction: ", format(round(RenElecPipeline$`Awaiting Construction`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      
      add_trace(
        data = RenElecPipeline,
        x = ~ `In Planning`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "In Planning",
        text = paste0("In Planning: ", format(round(RenElecPipeline$`In Planning`, digits = 0), big.mark = ","), " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = RenElecPipeline,
        y = ~ Type,
        x = ~ (RenElecPipeline$`Under Construction` + RenElecPipeline$`Awaiting Construction` + RenElecPipeline$`In Planning`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste("<b>",format(round((RenElecPipeline$`Under Construction` + RenElecPipeline$`Awaiting Construction` + RenElecPipeline$`In Planning`), digits = 0), big.mark = ","),"MW</b>"),
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
          tickvals = list(0,1,2,3,4,5,6,7,8,9),
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
  
  output$RenElecPipelineTable = renderDataTable({
    
    RenElecPipeline <- read_excel("Structure/CurrentWorking.xlsx",
                                  sheet = "Renewable elec pipeline", col_names = TRUE, 
                                  skip = 16,
                                  n_max = 11)
    
    RenElecPipeline <- RenElecPipeline[c(1:5)]
    
    
    
    names(RenElecPipeline)[1] <- c("Type")
    
    datatable(
      RenElecPipeline,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(RenElecPipeline)-1, 'desc')),
        title = "Pipeline renewable capacity by technology",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline renewable capacity by technology',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline renewable capacity by technology')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecPipeline), 0)
  })
  
  output$RenElecPipelineLATable = renderDataTable({
    
    RenElecPipeline <- read_excel("Structure/CurrentWorking.xlsx",
                                  sheet = "Table 2.3", col_names = TRUE, skip = 4
    )[1:4]
    
    names(RenElecPipeline) <- c("Local Authority", "In Planning", "Awaiting Construction", "Under Construction")
    
    RenElecPipeline <- RenElecPipeline[complete.cases(RenElecPipeline),]
    
    LALookup <- read_excel("Structure/LALookup.xlsx", 
                           sheet = "LA to Code")
    
    names(LALookup) <- c("Local Authority", "LA Code")
    
    RenElecPipeline <- merge(RenElecPipeline, LALookup, all.x = TRUE)
    
    RenElecPipeline$Total <- RenElecPipeline$`In Planning` + RenElecPipeline$`Awaiting Construction` + RenElecPipeline$`Under Construction`
    
    RenElecPipeline <- RenElecPipeline[c(1:22,24:31,33,34,23,32),c(1,5,2,3,4,6)]
    
    datatable(
      RenElecPipeline,
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Pipeline renewable capacity by planning stage and local authority (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline renewable capacity by planning stage and local authority (MW)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline renewable capacity by planning stage and local authority (MW)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(3:ncol(RenElecPipeline), 0)
    
  })
  
  output$RenElecPipelineCapTable = renderDataTable({
    
    RenElecPipeline <- read_excel("Structure/CurrentWorking.xlsx",
                                  sheet = "Renewable elec pipeline", col_names = TRUE, 
                                  skip = 16,
                                  n_max = 11)
    
    RenElecPipeline <- RenElecPipeline[c(7:9)]
    
    
    
    names(RenElecPipeline)[1] <- c("Type")
    
    datatable(
      RenElecPipeline,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(RenElecPipeline)-1, 'desc')),
        title = "Pipeline renewable capacity by planning stage",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline renewable capacity by planning stage',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline renewable capacity by planning stage')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecPipeline), 0)
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("RenElecPipelineTable")
  })
  
  observeEvent(input$ToggleTable6, {
    toggle("RenElecPipelineLATable")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("RenElecPipelineCapTable")
  })
  
  
  ######
  Time <- read_excel("Structure/CurrentWorking.xlsx", 
                     sheet = "Renewable elec pipeline", col_names = TRUE,
                     skip = 29, n_max = 1)
  Quarter <- substr(Time[1,1], 8,8)
  
  Quarter <- as.numeric(Quarter)*3
  
  Year <- substr(Time[1,1], 1,4)
  
  Subtitle <- paste("Scotland,", month.name[Quarter], Year)
  
  
  output$RenElecPipeline.png <- downloadHandler(
    filename = "RenElecPipeline.png",
    content = function(file) {
      
      
      
      ### Load Packages and Functions
      Data2 <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable elec pipeline", col_names = TRUE, 
                          skip = 16, n_max = 10)[1:5]
      
      names(Data2)[1] <- "Type"
      
      RenElecCapTech <- Data2[1:4]
      
      
      RenElecCapTech <- arrange(RenElecCapTech,-row_number())
      
      RenElecCapTech$Type <-
        factor(RenElecCapTech$Type,
               levels = unique(RenElecCapTech$Type),
               ordered = TRUE)
      
      RenElecCapTech <- melt(RenElecCapTech, id.vars = "Type")
      
      
      RenElecCapTech$variable <-
        factor(RenElecCapTech$variable,
               levels = rev(unique(RenElecCapTech$variable)),
               ordered = TRUE)
      
      RenElecCapTech <- RenElecCapTech %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Pipeline renewable capacity by technology"
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
      
      
      RenElecCapTechChart <- RenElecCapTech %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Operational" = BarColours[1],
            "Under Construction" = BarColours[2],
            "Awaiting Construction" = BarColours[3],
            "In Planning" = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            x = Type,
            y = -200,
            label = Type,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 1
        ) +
        geom_text(
          aes(
            x = Type,
            y = top+100  ,
            label = paste(format(round(top,digits = 0),big.mark = ","), "MW"),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0
        ) +
        
        geom_text(
          aes(
            x = 11.1,
            y = (2/2) * (max(RenElecCapTech$top) * .8) +(max(RenElecCapTech$top) *.1),
            label = "Under\nConstruction",
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 11.1,
            y = (1/2) * (max(RenElecCapTech$top) * .8) +(max(RenElecCapTech$top) *.1),
            label = "Awaiting\nConstruction",
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 11.1,
            y = (0/2) * (max(RenElecCapTech$top) * .8) +(max(RenElecCapTech$top) *.1),
            label = "In\nPlanning",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 12,
            y = (3.5/4) * 15000,
            label = " ",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic"
        )
      
      
      RenElecCapTechChart
      
      
      RenElecCapTechChart <-
        StackedBars(RenElecCapTechChart,
                    RenElecCapTech,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      RenElecCapTechChart <-
        RenElecCapTechChart +
        labs(subtitle = Subtitle) +
        ylim(-4000, max(RenElecCapTech$top)+1800)+
        coord_flip()
      
      RenElecCapTechChart
      
      ggsave(
        file,
        plot = RenElecCapTechChart,
        width = 17.5,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
  )
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
  
  output$RenElecPipelineCapPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec pipeline", col_names = TRUE,
                       skip = 29, n_max = 1)
    
    names(Data)[1] <- c("Type")
    
    Data[2:5]%<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecPipelineCap <- as_tibble(Data)
    
    RenElecPipelineCap <- arrange(RenElecPipelineCap,-row_number())
    
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
    
    
    RenElecPipelineCap$Type <- paste0("<b>", RenElecPipelineCap$Type, "</b>"  )
    
    p <- plot_ly(data = RenElecPipelineCap, y = ~ Type) %>%
      
      add_trace(
        data = RenElecPipelineCap,
        x = ~ `Under Construction`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Under Construction",
        text = paste0("Under Construction: ", round(RenElecPipelineCap$`Under Construction`, digits = 1), " GW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = RenElecPipelineCap,
        x = ~ `Awaiting Construction`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Awaiting Construction",
        text = paste0("Awaiting Construction: ", round(RenElecPipelineCap$`Awaiting Construction`, digits = 1), " GW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      
      add_trace(
        data = RenElecPipelineCap,
        x = ~ `In Planning`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "In Planning",
        text = paste0("In Planning: ", round(RenElecPipelineCap$`In Planning`, digits = 1), " GW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = RenElecPipelineCap,
        y = ~Type,
        x = ~ (RenElecPipelineCap$`Under Construction` + RenElecPipelineCap$`Awaiting Construction` + RenElecPipelineCap$`In Planning`) + 0.3,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste("<b>",format(round((RenElecPipelineCap$`Under Construction` + RenElecPipelineCap$`Awaiting Construction` + RenElecPipelineCap$`In Planning`), digits = 1), big.mark = ","),"GW</b>"),
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
          tickmode = "array"
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = FALSE,
          showticklabels = FALSE,
          range = c(0,15),
          zeroline = FALSE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F)
    
    p
  })
  
  output$RenElecPipelineCapSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec pipeline", col_names = TRUE,
                       skip = 29, n_max = 1)
    Quarter <- substr(Data[1,1], 8,8)
    
    Quarter <- as.numeric(Quarter)*3
    
    Year <- substr(Data[1,1], 1,4)
    
    paste("Scotland,", month.name[Quarter], Year)
  })
  
  output$RenElecPipelineCap.png <- downloadHandler(
    filename = "RenElecPipelineCap.png",
    content = function(file) {
      
      Data2 <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Renewable elec pipeline", col_names = TRUE,
                          skip = 15, n_max = 13)
      
      Data2 <- Data2[c(1,4,3,2)]
      
      Data2 <- Data2[complete.cases(Data2),]
      
      names(Data2) <- c("Type", "Under Construction", "Awaiting Construction", "In Planning")
      
      PipelineTotal <- tail(Data2, 1)
      
      PipelineTotal <- arrange(PipelineTotal,-row_number())
      
      PipelineTotal$Type <-
        factor(PipelineTotal$Type,
               levels = unique(PipelineTotal$Type),
               ordered = TRUE)
      
      PipelineTotal <- melt(PipelineTotal, id.vars = "Type")
      
      
      PipelineTotal$variable <-
        factor(PipelineTotal$variable,
               levels = rev(unique(PipelineTotal$variable)),
               ordered = TRUE)
      
      PipelineTotal$value <- as.numeric(PipelineTotal$value) /1000
      
      PipelineTotal <- PipelineTotal %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Pipeline renewable capacity by planning stage"
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
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Under Construction" = BarColours[2],
            "Awaiting Construction" = BarColours[3],
            "In Planning" = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .4) +
        geom_text(
          aes(
            x = Type,
            y = -200,
            label = Type,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 1
        ) +
        geom_text(
          aes(
            x = Type,
            y = top+1.600  ,
            label = paste0(format(sprintf("%.1f", round(top,digits = 1)),big.mark = ","), "\nGW"),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Type,
            y = pos  ,
            label = paste0(format(round(value,digits = 1),big.mark = ",", trim = TRUE), "\nGW"),
            fontface = 2
          ),
          colour = "white",
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = 1.4,
            y = PipelineTotal$pos[which(PipelineTotal$variable == "Under Construction")],
            label = "Under\nConstruction",
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic",
          size = 3.5
        ) +
        geom_text(
          aes(
            x = 1.4,
            y = PipelineTotal$pos[which(PipelineTotal$variable == "Awaiting Construction")],
            label = "Awaiting\nConstruction",
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic",
          size = 3.5
        ) +
        geom_text(
          aes(
            x = 1.4,
            y = PipelineTotal$pos[which(PipelineTotal$variable == "In Planning")],
            label = "In\nPlanning",
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic",
          size = 3.5
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
        labs(subtitle = Subtitle) +
        ylim(-2, max(PipelineTotal$top)+1.700)+
        coord_flip()
      
      PipelineTotalChart
      
      ggsave(
        file,
        plot = PipelineTotalChart,
        width = 17.5,
        height = 8,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenElecPipelineTimeTable = renderDataTable({
    
    RenElecCapacity <- read_excel("Structure/CurrentWorking.xlsx",
                                  sheet = "Renewable elec pipeline", col_names = TRUE, 
                                  skip = 29,)
    
    names(RenElecCapacity)[1] <- "Quarter"
    
    datatable(
      RenElecCapacity,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Pipeline Capacity Time Series",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Pipeline Capacity Time Series',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Pipeline Capacity Time Series')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecCapacity), 1)
  })
  
}
