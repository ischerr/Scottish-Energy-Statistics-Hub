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
      tabPanel("Installed capacity",
    fluidRow(column(8,
                    h3("Operational renewable capacity by planning stage", style = "color: #39ab2c;  font-weight:bold"),
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
    tabPanel("Technology Breakdown",
             fluidRow(column(8,
                             h3("Pipeline renewable capacity by plannng stage", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenElecBreakdownCapSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecBreakdownCap.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenElecCapacityPlot")),
             plotlyOutput(ns("RenElecBreakdownCapPlot"), height = "600px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             fluidRow(
               column(10, h3("Data - Operational renewable capacity by technology (MW)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecBreakdownCapTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
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
  
  
  output$RenElecCapacityTable = renderDataTable({
    
    RenElecCapacity <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable elec capacity", col_names = TRUE, 
                          skip = 15)
    
    RenElecCapacity <- RenElecCapacity[c(2,4)]
    
    
    
    names(RenElecCapacity)[1] <- c("Quarter")
    
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
        order = list(list(ncol(RenElecCapacity)-2, 'desc')),
        title = "Operational Capacity",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Operational Capacity',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Operational Capacity')
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
 
 
  observeEvent(input$ToggleTable, {
    toggle("RenElecCapacityTable")
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
}
