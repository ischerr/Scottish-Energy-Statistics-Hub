require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

GHGEmissionsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Total emissions",
    fluidRow(column(8,
                    h3("Greenhouse gas emissions", style = "color: #1A5D38;  font-weight:bold"),
                    h4(textOutput(ns('GHGEmissionsPercentageReductionTargetsSubtitle')), style = "color: #1A5D38;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GHGEmissionsPercentageReductionTargets.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    #dygraphOutput(ns("GHGEmissionsPlot")),
    plotlyOutput(ns("GHGEmissionsPercentageReductionTargetsPlot"))%>% withSpinner(color="#1A5D38"),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
    tabPanel("Sector Inventory",
             fluidRow(column(8,
                             h3("Greenhouse gas emissions by source sector", style = "color: #1A5D38;  font-weight:bold"),
                             h4(textOutput(ns('SectorInventorySubtitle')), style = "color: #1A5D38;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('SectorInventory.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("SectorInventoryPlot")),
             plotlyOutput(ns("SectorInventoryPlot"), height = "700px")%>% withSpinner(color="#1A5D38"),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"))
    ,
    tabPanel("Energy related emissions",
             fluidRow(column(8,
                             h3("Energy related greenhouse gas emissions (MtCO2e)", style = "color: #1A5D38;  font-weight:bold"),
                             h4(textOutput(ns('EnSupplyEmissionsSubtitle')), style = "color: #1A5D38;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnSupplyEmissions.png'), 'Download Graph', style="float:right")
             )),

             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("EnSupplyEmissionsPlot")),
             plotlyOutput(ns("EnSupplyEmissionsPlot"))%>% withSpinner(color="#1A5D38"),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")
             )
    ),
    fluidRow(
    column(10,h3("Commentary", style = "color: #1A5D38;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    tabsetPanel(
      tabPanel("Emissions by Sector",
    fluidRow(
      column(10, h3("Data -  Scottish greenhouse gas emissions by source sector (MtCO2e)", style = "color: #1A5D38;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("SectorInventoryTable"))%>% withSpinner(color="#1A5D38"))),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"))
    ,
    tabPanel("Energy related emissions",
             fluidRow(
               column(10, h3("Data -  Energy related greenhouse gas emissions (MtCO2e)", style = "color: #1A5D38;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EnSupplyEmissionsTable"))%>% withSpinner(color="#1A5D38"))),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"))
    )
    ,
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
GHGEmissions <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }

  print("GHGEmissions.R")
  
    output$GHGEmissionsPercentageReductionTargetsSubtitle <- renderText({
      
      GHGEmissions <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                      "\t", escape_double = FALSE, trim_ws = TRUE)
      
      GHGEmissions$Total <- rowSums(GHGEmissions[2:11])
      
      names(GHGEmissions)[1] <- "Year"
      ### variables
      
      paste("Scotland,", min(GHGEmissions$Year),"-", max(GHGEmissions$Year))
    })
  
  output$GHGEmissionsPercentageReductionTargetsPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Adjusted emissions", skip = 12)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    Data <- setDT(Data, keep.rownames = TRUE)[]
    Data[1,1] <- 1986
    Data[2,1] <- 1989
    Data[is.na(Data)] <- 0
    
    Data <- Data %>% unite(Targets,5:7, sep = "", remove = TRUE)
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data[Data == 0] <- NA
    
    Data <- Data[,c(1,3,5,2,4)]
    
    names(Data) <- c("Year", "Renewables", "Tgt", "2008Inventory", "FixedTargets")
    
    Data[which(Data$Year == 2045),]$Tgt <- 0.000000000000000000000000000000000000000001
    AdjustedEmissions <- Data
    
    plottitle <- "Percentage reduction targets - based on adjusted emissions (MtCO2e)"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#1A5D38", "#FF8500")
    LineColours <- c( "#1A5D38","#2b8cbe", "#FF8500", "#addd8e")
    
    AdjustedEmissions$Year <- paste0("01/01/", AdjustedEmissions$Year)
    
    AdjustedEmissions$Year <- dmy(AdjustedEmissions$Year)
    
    AdjustedEmissions$YearDisplay <- as.character(format(AdjustedEmissions$Year, "%Y"))
    
    AdjustedEmissions[1,6] <- "Baseline"
    
    
    p <-  plot_ly(data = AdjustedEmissions,
                  x = ~ Year ) %>% 
      add_trace(data = AdjustedEmissions,
                x = ~ Year,
                y = ~ `Renewables`,
                name = "Greenhouse gas emissions",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Greenhouse gas emissions: ",
                  round(AdjustedEmissions$`Renewables`, digits = 1),
                  " MtCO2e\nYear: ",
                  AdjustedEmissions$YearDisplay
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1986-01-01"), ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))),],
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Total greenhouse gas emissions",
        text = paste0(
          "Greenhouse gas emissions: ",
          round(AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1986-01-01"), ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))),]$`Renewables`, digits = 1),
          " MtCO2e\nYear: ",
          AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1986-01-01"), ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))),]$YearDisplay
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>% 
      add_trace(
        data = AdjustedEmissions[which(AdjustedEmissions$Tgt != 0),],
        x = ~ Year,
        y = ~ `Tgt`,
        name = "Targets",
        text = paste0(
          "Fixed Annual Target: ",
          round(AdjustedEmissions[which(AdjustedEmissions$Tgt != 0),]$`Tgt`, digits = 1),
          " MtCO2e\n(Reduction of ", percent(1 -( AdjustedEmissions[which(AdjustedEmissions$Tgt != 0),]$`Tgt` / AdjustedEmissions[which(AdjustedEmissions$Year == ymd("1990-01-01")),]$Renewables), accuracy = 1),
          
          
          ")\nYear: ",
          format(AdjustedEmissions[which(AdjustedEmissions$Tgt != 0),]$Year, "%Y")
        ),
        hoverinfo = 'text',
        type = "scatter", 
        legendgroup = "2",
        mode = 'markers',
        marker = list(size = 25,
                      symbol = "diamond", 
                      color = LineColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(AdjustedEmissions$Year)-500, max(AdjustedEmissions[which(AdjustedEmissions$Tgt != 0),]$Year)+500)),
        yaxis = list(
          title = "MtCO2e",
          showgrid = TRUE,
          range = c(-5,82),
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
    
    
  })
  
  
  output$GHGEmissionsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Adjusted emissions", skip = 12)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    Data <- setDT(Data, keep.rownames = TRUE)[]
    Data[1,1] <- 1990
    Data[is.na(Data)] <- 0
    
    Data <- Data %>% unite(Targets,5:6, sep = "", remove = TRUE)
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data <- Data[,c(1,3,5,2,4)]
    
    names(Data) <- c("Year", "1990 - 2016 Inventory", "Reduction Target", "1990 - 2008 Inventory", "Fixed Annual Targets")
    
    Data$CheckRow <- Data$`1990 - 2016 Inventory` + Data$`Reduction Target` +Data$`1990 - 2008 Inventory` + Data$`Fixed Annual Targets`
    
    Data <- Data[which(Data$CheckRow > 0),]
    
    Data[Data == 0] <- NA
    
    GHGEmissions <- Data[c(1,4,2,5,3)]
    datatable(
      GHGEmissions,
      extensions = 'Buttons',
     # container = sketch,
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'asc')),
        title = "Scottish Greenhouse Gas Emissions (MtCO2e)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Scottish Greenhouse Gas Emissions (MtCO2e)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Scottish Greenhouse Gas Emissions (MtCO2e)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatRound(2:5, 1)
  })
  
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/1 - Whole System/GHGEmissions.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("GHGEmissionsTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$GHGEmissionsPercentageReductionTargets.png <- downloadHandler(
    filename = "GHGEmissionsPercentageReductionTargets.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Adjusted emissions", skip = 12)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      
      colnames(Data) <- as.character(unlist(Data[1,]))
      Data = Data[-1, ]
      Data <- setDT(Data, keep.rownames = TRUE)[]
      Data[1,1] <- 1986
      Data[2,1] <- 1989
      Data[is.na(Data)] <- 0
      
      Data <- Data %>% unite(Targets,5:7, sep = "", remove = TRUE)
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      Data[Data == 0] <- NA
      
      Data <- Data[,c(1,3,5,2,4)]
      
      names(Data) <- c("Year", "Renewables", "Tgt", "2008Inventory", "FixedTargets")
      
      Data[which(Data$Year == 2045),]$Tgt <- 0.000000000000000000000000000000000000000001
      
      AdjustedEmissions <- Data
      
      plottitle <- "Greenhouse gas emissions and percentage reduction targets - based on\nadjusted emissions (MtCO2e)"
      sourcecaption <- "Source: SG"
      ChartColours <- c("#1A5D38", "#FF8500")
      LineColours <- c( "#1A5D38","#2b8cbe", "#FF8500", "#addd8e")
      
      AdjustedEmissionsChart <-
        AdjustedEmissions %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Renewables,
              
              label = Renewables),
          size = 1.5,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = AdjustedEmissions[which(AdjustedEmissions$Year == max(AdjustedEmissions[which(AdjustedEmissions$Renewables > 0),]$Year)),],
          aes(
            x = Year,
            y = Renewables,
            label = round(`2008Inventory`, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          data = AdjustedEmissions[which(AdjustedEmissions$Year == max(AdjustedEmissions[which(AdjustedEmissions$Renewables > 0),]$Year)),],
          aes(
            x = Year,
            y = Renewables,
            label = round(Renewables, digits = 1),
            show_guide = FALSE
          ),
          fontface = 2,
          vjust = -1,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year %in% c(1986, 1990,1995, 1998), round(Renewables, digits= 1), ""),
            hjust = 0.5,
            vjust = -1,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+0.75,
            y = Renewables,
            label = ifelse(Year == max(AdjustedEmissions$Year[which(AdjustedEmissions$FixedTargets > 0)]), round(Renewables, digits= 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        
        geom_point(
          data = tail(AdjustedEmissions, 1),
          aes(
            x = Year,
            y = Renewables,
            label = round(Renewables, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = AdjustedEmissions[which(AdjustedEmissions$Year %in% c(1986,1990,1995,1998)),],
          aes(
            x = Year,
            y = Renewables,
            label = round(Renewables, digits = 1),
            show_guide = FALSE
          ),
          size = 3,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year %in% c(1990, 1995, 1998, max(AdjustedEmissions[which(AdjustedEmissions$Renewables > 0),]$Year)), Year, ""),
            hjust = 0.5,
            vjust = 1.7,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year %in% AdjustedEmissions$Year[which(AdjustedEmissions$Tgt > 0)], Year, ""),
            hjust = 0.5,
            vjust = 1.7
          ),
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = 1986,
            y = 0,
            label = "Baseline",
            hjust = 0.5,
            vjust = 1.7,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )+ geom_point(
          aes(
            x = Year,
            y = Tgt,
            label = round(`2008Inventory`, digits = 1),
            show_guide = FALSE
          ),
          size = 6,
          shape = 18,
          colour = LineColours[3],
          family = "Century Gothic" ) +
        geom_text(
          aes(
            x = Year,
            y = Tgt,
            label = paste0("Target: ", round(Tgt, digits = 1), "\n(-", percent(1 - ( Tgt / AdjustedEmissions$Renewables[which(AdjustedEmissions$Year == 1986)]), accuracy = 1), ")"),
            show_guide = FALSE
          ),
          colour = LineColours[3],
          family = "Century Gothic",
          vjust = ifelse(AdjustedEmissions$Tgt <= 30, -0.5, 1.5)
        ) 
      
      AdjustedEmissionsChart <-
        LinePercentChart(AdjustedEmissionsChart,
                         AdjustedEmissions,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      AdjustedEmissionsChart
      
      AdjustedEmissionsChart <- AdjustedEmissionsChart +
        labs(subtitle = paste("Scotland, 1990 -", max(AdjustedEmissions$Year[which(AdjustedEmissions$Renewables > 0)]))) +
        xlim(min(AdjustedEmissions$Year), 2046)+
        ylim(0,79)
      
      
      ggsave(
        file,
        plot = AdjustedEmissionsChart,
        width = 30,
        height = 15,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
  output$SectorInventorySubtitle <- renderText({
    
    SectorInventory <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)
    
    paste("Scotland,", max(as.numeric(SectorInventory$refPeriod), na.rm = TRUE))
  })
  
  output$SectorInventoryPlot <- renderPlotly  ({
    
    SectorInventory <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)
    
    SectorInventory <- SectorInventory[nrow(SectorInventory),]
    
    SectorInventory$refPeriod <- NULL
    
    SectorInventory <- melt(SectorInventory)
    
    SectorInventory$variable <- as.character(SectorInventory$variable)
    
    SectorInventory$Type <- "Emissions"
    
    SectorInventory <- rbind(SectorInventory, c("Total", sum(SectorInventory$value), "Total greenhouse gas emissions"))
    
    SectorInventory[which(SectorInventory$value < 0),]$Type <- "Carbon sinks"
    
    SectorInventory <- dcast(SectorInventory, Type ~ variable, value.var = "value")
    
    SectorInventory[is.na(SectorInventory)] <- 0
    
    SectorInventory[2:12] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    SectorInventory <- as_tibble(SectorInventory)
    
    SectorInventory <- SectorInventory[c(3,1,2),]
    
    rownames(SectorInventory) <- NULL
    
    SectorInventoryPlotData <- SectorInventory[c(1,3),]
    
    SectorInventoryPlotData$Type <- as.numeric(rownames(SectorInventoryPlotData))
    
    ChartColours <- c("#1A5D38", "#FF8500")
    BarColours <-
      c(
        "#016c59",
        "#9e0142",
        "#d53e4f",
        "#f46d43",
        "#fdae61",
        "#fee08b",
        "#abdda4",
        "#66c2a5",
        "#3288bd",
        "#5e4fa2"
        
      )
    
    
    p <- plot_ly(data = SectorInventoryPlotData, y = ~ Type) %>%
      add_trace(
        data = SectorInventoryPlotData,
        x = ~ Total,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Total",
        text = paste0("Total\n", round(SectorInventoryPlotData$Total, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = SectorInventoryPlotData,
        x = ~ `transport-excluding-international`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Domestic Transport",
        text = paste0("Domestic Transport\n", round(SectorInventoryPlotData$`transport-excluding-international`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      )  %>%
      add_trace(
        data = SectorInventoryPlotData,
        x = ~ `business`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Business",
        text = paste0("Business\n", round(SectorInventoryPlotData$`business`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      )  %>%
      add_trace(
        data = SectorInventoryPlotData,
        x = ~ `agriculture`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Agriculture",
        text = paste0("Agriculture\n", round(SectorInventoryPlotData$`agriculture`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = SectorInventoryPlotData,
        x = ~ `energy-supply`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Energy Supply",
        text = paste0("Energy Supply\n", round(SectorInventoryPlotData$`energy-supply`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = SectorInventoryPlotData,
        x = ~ `residential`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Residential",
        text = paste0("Residential\n", round(SectorInventoryPlotData$`residential`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = SectorInventoryPlotData,
        x = ~ `international-aviation-and-shipping`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = BarColours[7],
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "International aviation and shipping",
        text = paste0("International aviation and shipping\n", round(SectorInventoryPlotData$`international-aviation-and-shipping`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[7]),
        legendgroup = 7
      ) %>%
      add_trace(
        data = SectorInventoryPlotData,
        x = ~ `waste-management`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = BarColours[8],
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Waste management",
        text = paste0("waste-management\n", round(SectorInventoryPlotData$`waste-management`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[8]),
        legendgroup = 8
      ) %>%
      add_trace(
        data = SectorInventoryPlotData,
        x = ~ `public`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = BarColours[9],
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "public",
        text = paste0("Public\n", round(SectorInventoryPlotData$`public`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[9]),
        legendgroup = 9
      ) %>%
      add_trace(
        data = SectorInventoryPlotData,
        x = ~ `industrial-processes`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = BarColours[10],
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "industrial-processes",
        text = paste0("Industrial processes\n", round(SectorInventoryPlotData$`industrial-processes`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[10]),
        legendgroup = 10
      ) %>%
      add_annotations(
        ax = max(SectorInventory$Total)- min(SectorInventory$`land-use-land-use-change-and-forestry`),
        x = max(SectorInventory$Total),
        ay = 1.5,
        y = 1.5,
        xref = "x", yref = "y",
        axref = "x", ayref = "y",
        showlegend = FALSE ,
        arrowhead = 4,
        arrowsize = 1,
        arrowcolor = BarColours[8],
        hoverinfo = 'name',
        legendgroup = 11,
        text = "",
        name = "Carbon sinks absorb more carbon than they generate",
        line = list(
          arrowhead = 1,
          width = 3,
          color = BarColours[8],
          dash = "none"
        )
      ) %>%
      add_trace(
        mode = 'text',
        x =max(SectorInventory$Total) - (min(SectorInventory$`land-use-land-use-change-and-forestry`)/2),
        y = 1.5,
        xref = "x", yref = "y",
        showlegend = FALSE ,
        hoverinfo = 'name',
        legendgroup = 10,
        text = paste0("Forestry\n\n", round(min(SectorInventory$`land-use-land-use-change-and-forestry`),1)," MtCO2e"),
        name = paste("Carbon sinks"),
        marker = list(
          size = 100,
          opacity = 0
        ),
        showarrow = F,
        textfont = list(
          size = 20,
          color = BarColours[8]
        )
      ) %>%
      layout(
        barmode = 'stack',
        showlegend = FALSE,
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(
          title = "",
          showgrid = FALSE,
          ticktext = list("<b>Total Greenhouse\nGas Emissions</b>", "<b>Emissions</b>"),
          tickvals = list(1, 2),
          tickmode = "array",
          range = c(0.75,2.25)
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F) %>% 
      onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")
    
    
    p
    
    
  })
  
  output$SectorInventory.png <- downloadHandler(
    filename = "SectorInventory.png",
    content = function(file) {
      
      SectorInventory <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)
      
      SectorInventoryYear <- paste("Scotland,", max(as.numeric(SectorInventory$refPeriod), na.rm = TRUE))
      
      SectorInventory <- SectorInventory[nrow(SectorInventory),]
      
      SectorInventory$refPeriod <- NULL
      
      SectorInventory <- melt(SectorInventory)
      
      SectorInventory$variable <- as.character(SectorInventory$variable)
      
      SectorInventory$Type <- "Emissions"
      
      SectorInventory <- rbind(SectorInventory, c("Total", sum(SectorInventory$value), "Total greenhouse gas emissions"))
      
      SectorInventory[which(SectorInventory$value < 0),]$Type <- "Carbon sinks"
      
      SectorInventory <- dcast(SectorInventory, Type ~ variable, value.var = "value")
      
      SectorInventory[is.na(SectorInventory)] <- 0
      
      SectorInventory[2:12] %<>% lapply(function(x) as.numeric(as.character(x)))
      
      SectorInventory <- as_tibble(SectorInventory)
      
      SectorInventory <- SectorInventory[c(3,1,2),]
      
      SectorInventory <- SectorInventory[c(1,10,7,5,8,12,6,9,4,2,3,11)]
      
      ChartColours <- c("#1A5D38", "#FF8500")
      BarColours <-
        c(
          "#016c59",
          "#9e0142",
          "#d53e4f",
          "#f46d43",
          "#fdae61",
          "#fee08b",
          "#abdda4",
          "#66c2a5",
          "#3288bd",
          "#5e4fa2"
          
        )
      
      
      SectorInventory$Type <-
        factor(SectorInventory$Type,
               levels = unique(SectorInventory$Type),
               ordered = TRUE)
      
      SectorInventory <- melt(SectorInventory, id.vars = "Type")
      
      GHGCarbonSink <- min(SectorInventory[which(SectorInventory$variable == "land-use-land-use-change-and-forestry"),]$value)
      
      
      SectorInventory <- subset(SectorInventory, SectorInventory$Type != "Carbon sinks" )
      SectorInventory$variable <-
        factor(
          SectorInventory$variable,
          levels = unique(SectorInventory$variable),
          ordered = TRUE
        )
      
      SectorInventory <- SectorInventory %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Greenhouse gas emissions by source sector"
      sourcecaption <- "Source: SG"
      
      SectorInventoryChart <- SectorInventory %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "transport-excluding-international" = BarColours[2],
            "business" = BarColours[3],
            "agriculture" = BarColours[4],
            "energy-supply" = BarColours[5],
            "residential" = BarColours[6],
            "international-aviation-and-shipping" = BarColours[7],
            "waste-management" = BarColours[8],
            "public" = BarColours[9],
            "industrial-processes" = BarColours[10],
            "land-use-land-use-change-and-forestry" = ChartColours[1],
            "Total" = BarColours[1]
          )
        ) +
        geom_bar(stat = "identity",
                 width = .4) +
        geom_text(
          aes(
            x = Type,
            y = ifelse(top > 0, -4.5,7),
            label = ifelse(SectorInventory$variable != "Total", "", str_wrap(SectorInventory$Type, width = 13))
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        ) + 
        geom_text(
          aes(
            x = Type,
            y = top-pos,
            label = ifelse(value > 1.5 & Type == "Emissions", sprintf("%.1f", round(value, digits = 1)), ""
            )),
          family = "Century Gothic",
          fontface = 2,
          colour =  "white"
        ) + 
        geom_text(
          aes( x = Type,
               y = top + ifelse(top > 0, +1,-1),
               label = ifelse(SectorInventory$variable != "Total", "", paste(round(top, digits = 1), "MtCO2e")),
               hjust = ifelse(top > 0, 0, 1)
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        ) +
        geom_segment(
          x = 1.44,
          xend = 1.44,
          y = SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top,
          yend = SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top + GHGCarbonSink,
          colour =   BarColours[7],
          arrow = arrow(length = unit(0.4, "cm")),
          size = 1
        ) + 
        geom_text(
          aes( x = 1.6,
               y = SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top + (GHGCarbonSink/2),
               label = paste("Forestry: \n", round(GHGCarbonSink, digits = 1), "MtCO2e")
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =   BarColours[7]
        ) +
        geom_text(
          aes( x = 1.3,
               y = SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top + (GHGCarbonSink/2),
               label = "Carbon sinks absorb more\ncarbon than they generate"
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =  BarColours[7],
          size = 3
        ) +
        geom_text(
          aes( x = 2.4,
               y = (SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top) *(0/7) - 3,
               label = "Domestic\nTransport",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[2]
        )  +
        geom_text(
          aes( x = 2.4,
               y = (SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top) *(1/7) - 3,
               label = "Business",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[3]
        )  +
        geom_text(
          aes( x = 2.4,
               y = (SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top) *(2/7) - 3,
               label = "Agriculture",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[4]
        )  +
        geom_text(
          aes( x = 2.4,
               y = (SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top) *(3/7) - 3,
               label = "Energy\nSupply",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[5]
        ) +
        geom_text(
          aes( x = 2.4,
               y = (SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top) *(4/7) - 3,
               label = "Residential",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[6]
        ) +
        geom_text(
          aes( x = 2.4,
               y = (SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top) *(5/7) - 3,
               label = "International\naviation\nand shipping",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[7]
        )+
        geom_text(
          aes( x = 2.4,
               y = (SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top) *(6/7) - 3,
               label = "Waste\nManagement",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[8]
        )+
        geom_text(
          aes( x = 2.4,
               y = (SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top) *(7/7) - 3,
               label = "Public",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[9]
        )+
        geom_text(
          aes( x = 2.4,
               y = (SectorInventory[which(SectorInventory$Type == "Emissions" & SectorInventory$variable == "Total"),]$top) *(8/7) - 3,
               label = "Industrial\nProcesses",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[10]
        )
      
      SectorInventoryChart
      
      
      SectorInventoryChart <-
        StackedBars(SectorInventoryChart,
                    SectorInventory,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      SectorInventoryChart <-
        SectorInventoryChart +
        labs(subtitle = SectorInventoryYear)+
        coord_flip()+ 
        ylim(-7,53)
      
      SectorInventoryChart
      
      
      
      ggsave(
        file,
        plot = SectorInventoryChart,
        width = 27,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$SectorInventoryTable = renderDataTable({
    
    SectorInventory <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(SectorInventory) <- c("Year", "Agriculture", "Business", "Energy Supply", "Industrial Processes", "International Aviation and Shipping", "Forestry (Carbon Sink)", "Public",  "Residential", "Domestic Transport", "Waste Management" )
    
    SectorInventory <- SectorInventory[c(1,10,3,2,4,9,6,11,8,5,7)]
    
    SectorInventory$`Total Emissions` <- rowSums(SectorInventory[2:11])
    
    datatable(
      SectorInventory[c(1,12,2:11)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        scrollX = TRUE,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        title = "Scottish greenhouse gas Emissions by source sector (MtCO2e)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Scottish greenhouse gas Emissions by source sector (MtCO2e)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Scottish greenhouse gas Emissions by source sector (MtCO2e)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:12, 1) %>% 
      formatStyle(12, fontStyle = "italic") %>% 
      formatStyle(2, fontWeight = "bold")
  })
  
  output$EnSupplyEmissionsSubtitle <- renderText({
    
    EnSupplyEmissions <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")
    
    names(EnSupplyEmissions)[1] <- "Year"
    
    EnSupplyEmissions$Year <- as.numeric(EnSupplyEmissions$Year)
    
    EnSupplyEmissions <- as_tibble(EnSupplyEmissions)
    
    names(EnSupplyEmissions)[7] <- "Greenhouse Gas"
    
    EnSupplyEmissions <- EnSupplyEmissions[complete.cases(EnSupplyEmissions),]
    
    paste("Scotland, 1990","-", max(EnSupplyEmissions$Year))
  })
  
  output$EnSupplyEmissionsPlot <- renderPlotly  ({
    
    EnSupplyEmissions <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")
    
    names(EnSupplyEmissions)[1] <- "Year"
    
    EnSupplyEmissions$Year <- as.numeric(EnSupplyEmissions$Year)
    
    EnSupplyEmissions <- as_tibble(EnSupplyEmissions)
    
    names(EnSupplyEmissions)[7] <- "Greenhouse Gas"
    
    EnSupplyEmissions <- EnSupplyEmissions[complete.cases(EnSupplyEmissions),]
    
    
    plottitle <- "Net source greenhouse gas emissions from the energy supply sector (MtCO2e)"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#1A5D38", "#FF8500")
    LineColours <- c( "#39ab2c","#006837", "#41ab5d", "#addd8e")
    
    EnSupplyEmissions$Year <- paste0("01/01/", EnSupplyEmissions$Year)
    
    EnSupplyEmissions$Year <- dmy(EnSupplyEmissions$Year)
    
    EnSupplyEmissions$YearLabel <- as.character(format(EnSupplyEmissions$Year, "%Y"))
    
    
    p <-  plot_ly(data = EnSupplyEmissions,
                  x = ~ Year ) %>% 
      add_trace(data = EnSupplyEmissions,
                x = ~ Year,
                y = ~ `Greenhouse Gas`,
                name = "Total Greenhouse Gas Emissions",
                type = 'scatter',
                mode = 'none',
                legendgroup = "5",
                text = paste0(
                  "Total Greenhouse Gas Emissions: ",
                  round(EnSupplyEmissions$`Greenhouse Gas`, digits = 1),
                  " MtCO2e\nYear: ",
                  EnSupplyEmissions$YearLabel
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "dash")
      ) %>% 
      
      add_trace(data = EnSupplyEmissions,
                x = ~ Year,
                y = ~ `Electricity`,
                name = "Electricity",
                type = 'scatter',
                mode = 'none',
                legendgroup = "1",
                stackgroup = 'one',
                text = paste0(
                  "Electricity: ",
                  round(EnSupplyEmissions$`Electricity`, digits = 1),
                  " MtCO2e\nYear: ",
                  EnSupplyEmissions$YearLabel
                ),
                hoverinfo = 'text',
                fillcolor = LineColours[1]
      ) %>% 
      
      add_trace(data = EnSupplyEmissions,
                x = ~ Year,
                y = ~ `Industry`,
                name = "Industry",
                type = 'scatter',
                mode = 'none',
                legendgroup = "2",
                stackgroup = 'one',
                text = paste0(
                  "Industry: ",
                  round(EnSupplyEmissions$`Industry`, digits = 1),
                  " MtCO2e\nYear: ",
                  EnSupplyEmissions$YearLabel
                ),
                hoverinfo = 'text',
                fillcolor = LineColours[2]
      ) %>% 
      
      add_trace(data = EnSupplyEmissions,
                x = ~ Year,
                y = ~ `Heat`,
                name = "Heat",
                type = 'scatter',
                mode = 'none',
                legendgroup = "3",
                stackgroup = 'one',
                text = paste0(
                  "Heat: ",
                  round(EnSupplyEmissions$`Heat`, digits = 1),
                  " MtCO2e\nYear: ",
                  EnSupplyEmissions$YearLabel
                ),
                hoverinfo = 'text',
                fillcolor = LineColours[3]
      ) %>% 
      
      add_trace(data = EnSupplyEmissions,
                x = ~ Year,
                y = ~ `Transport`,
                name = "Transport",
                type = 'scatter',
                mode = 'none',
                legendgroup = "4",
                stackgroup = "one",
                text = paste0(
                  "Transport: ",
                  round(EnSupplyEmissions$`Transport`, digits = 1),
                  " MtCO2e\nYear: ",
                  EnSupplyEmissions$YearLabel
                ),
                hoverinfo = 'text',
                fillcolor = LineColours[4]
      ) %>% 
      add_trace(
        data = tail(EnSupplyEmissions, 1),
        x = ~ Year,
        y = ~ `Greenhouse Gas`,
        name = "Total Greenhouse Gas Emissions",
        legendgroup = "5",
        text = paste0(
          "Total Greenhouse Gas Emissions: ",
          round(EnSupplyEmissions$`Greenhouse Gas`, digits = 1),
          " MtCO2e\nYear: ",
          EnSupplyEmissions$YearLabel
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      layout(
        barmode = 'stack',
        barmode = 'group',
        bargap = 0.25,
        bargap = 0.66,
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(EnSupplyEmissions$Year)-100, max(EnSupplyEmissions$Year)+100)),
        yaxis = list(
          title = "MtCO2e",
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
  
  
  output$EnSupplyEmissions.png <- downloadHandler(
    filename = "EnSupplyEmissions.png",
    content = function(file) {
      
      EnSupplyEmissions <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")
      
      names(EnSupplyEmissions)[1] <- "Year"
      
      EnSupplyEmissions$Year <- as.numeric(EnSupplyEmissions$Year)
      
      EnSupplyEmissions <- as_tibble(EnSupplyEmissions)
      
      names(EnSupplyEmissions)[7] <- "Greenhouse Gas"
      
      EnSupplyEmissions <- EnSupplyEmissions[complete.cases(EnSupplyEmissions),]
      plottitle <- "Energy related greenhouse gas emissions (MtCO2e)"
      sourcecaption <- "Source: SG"
      ChartColours <- c("#1A5D38", "#FF8500")
      LineColours <- c( "#39ab2c","#006837", "#41ab5d", "#addd8e")
      
      EnSupplyEmissionsTotal <- EnSupplyEmissions[c(1,7)]
      
      EnSupplyEmissions[c(5,7)] <- NULL
      
      EnSupplyEmissions <- melt(EnSupplyEmissions, id.vars = "Year")
      
      EnSupplyEmissions <- EnSupplyEmissions %>% mutate(variable = factor(variable),
                                                        variable = factor(variable, levels = c("Transport","Heat","Industry","Electricity")))
      
      EnSupplyEmissions <- EnSupplyEmissions[order(EnSupplyEmissions$variable),]
      
      
      EnSupplyEmissionsChart <-
        EnSupplyEmissions %>% 
        ggplot(aes(
          x = Year,
          y = value,
          group = variable,
          fill = variable
        )) +
        scale_fill_manual(
          "variable",
          values = c(
            "Electricity" = LineColours[1],
            "Industry" = LineColours[2],
            "Heat" = LineColours[3],
            "Transport" = LineColours[4]
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
              format(Year, format = "%Y"),
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
          "line",
          x = EnSupplyEmissionsTotal$Year,
          y = EnSupplyEmissionsTotal$`Greenhouse Gas`,
          size = 1,
          linetype = 2,
          colour = ChartColours[1]
        )+
        annotate(
          "point",
          x = max(EnSupplyEmissionsTotal$Year),
          y = EnSupplyEmissionsTotal[which(EnSupplyEmissionsTotal$Year == max(EnSupplyEmissionsTotal$Year)),]$`Greenhouse Gas`,
          size = 3,
          colour = ChartColours[1]
        )+
        annotate(
          "text",
          x = min(EnSupplyEmissionsTotal$Year)-2,
          y = EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Electricity"),]$value / 2,
          colour = LineColours[1],
          label = paste0("Electricity\n", round(EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Electricity"),]$value, 1), " MtCO2e"),
          fontface = 2,
          family = "Century Gothic"
        )+
        annotate(
          "text",
          x = min(EnSupplyEmissionsTotal$Year)-2,
          y = EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Electricity"),]$value +
            ( EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Industry"),]$value/ 2),
          colour = LineColours[2],
          label = paste0("Industry\n", round(EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Industry"),]$value, 1), " MtCO2e"),
          fontface = 2,
          family = "Century Gothic"
        )+
        annotate(
          "text",
          x = min(EnSupplyEmissionsTotal$Year)-2,
          y = EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Electricity"),]$value +
            EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Industry"),]$value +
            (EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Heat"),]$value/2),
          colour = LineColours[3],
          label = paste0("Heat\n", round(EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Heat"),]$value, 1), " MtCO2e"),
          fontface = 2,
          family = "Century Gothic"
        )+
        annotate(
          "text",
          x = min(EnSupplyEmissionsTotal$Year)-2,
          y = EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Electricity"),]$value +
            EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Industry"),]$value +
            EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Heat"),]$value +
            (EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Transport"),]$value/2),
          colour = LineColours[4],
          label = paste0("Transport\n", round(EnSupplyEmissions[which(EnSupplyEmissions$Year == min(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Transport"),]$value, 1), " MtCO2e"),
          fontface = 2,
          family = "Century Gothic"
        )+
        annotate(
          "text",
          x = max(EnSupplyEmissionsTotal$Year)+2,
          y = EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Electricity"),]$value / 2,
          colour = LineColours[1],
          label = paste0(round(EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Electricity"),]$value, 1), " MtCO2e"),
          fontface = 2,
          family = "Century Gothic"
        )+
        annotate(
          "text",
          x = max(EnSupplyEmissionsTotal$Year)+2,
          y = EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Electricity"),]$value +
            ( EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Industry"),]$value/ 2),
          colour = LineColours[2],
          label = paste0(round(EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Industry"),]$value, 1), " MtCO2e"),
          fontface = 2,
          family = "Century Gothic"
        )+
        annotate(
          "text",
          x = max(EnSupplyEmissionsTotal$Year)+2,
          y = EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Electricity"),]$value +
            EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Industry"),]$value +
            (EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Heat"),]$value/2),
          colour = LineColours[3],
          label = paste0(round(EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Heat"),]$value, 1), " MtCO2e"),
          fontface = 2,
          family = "Century Gothic"
        )+
        annotate(
          "text",
          x = max(EnSupplyEmissionsTotal$Year)+2,
          y = EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Electricity"),]$value +
            EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Industry"),]$value +
            EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Heat"),]$value +
            (EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Transport"),]$value/2),
          colour = LineColours[4],
          label = paste0(round(EnSupplyEmissions[which(EnSupplyEmissions$Year == max(EnSupplyEmissions$Year) & EnSupplyEmissions$variable == "Transport"),]$value, 1), " MtCO2e"),
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = min(EnSupplyEmissionsTotal$Year)-2,
          y = EnSupplyEmissionsTotal[which(EnSupplyEmissionsTotal$Year == min(EnSupplyEmissionsTotal$Year)),]$`Greenhouse Gas`,
          colour = ChartColours[1],
          label = paste0("Total Greenhouse\nGas Emissions\n", round(EnSupplyEmissionsTotal[which(EnSupplyEmissionsTotal$Year == min(EnSupplyEmissionsTotal$Year)),]$`Greenhouse Gas`, 1), " MtCO2e"),
          fontface = 2,
          family = "Century Gothic"
        )+
        annotate(
          "text",
          x = max(EnSupplyEmissionsTotal$Year)+2,
          y = EnSupplyEmissionsTotal[which(EnSupplyEmissionsTotal$Year == max(EnSupplyEmissionsTotal$Year)),]$`Greenhouse Gas`,
          colour = ChartColours[1],
          label = paste0(round(EnSupplyEmissionsTotal[which(EnSupplyEmissionsTotal$Year == max(EnSupplyEmissionsTotal$Year)),]$`Greenhouse Gas`, 1), " MtCO2e"),
          fontface = 2,
          family = "Century Gothic"
        )
      
      
      
      EnSupplyEmissionsChart <-
        LinePercentChart(EnSupplyEmissionsChart,
                         EnSupplyEmissions,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      EnSupplyEmissionsChart
      
      EnSupplyEmissionsChart <- EnSupplyEmissionsChart +
        labs(subtitle = paste("Scotland, 1990 -", max(EnSupplyEmissions$Year))) +
        xlim(min(EnSupplyEmissions$Year)-3, max(EnSupplyEmissions$Year)+3)+
        ylim(-1.5,80.5)
      
      EnSupplyEmissionsChart
      
      ggsave(
        file,
        plot = EnSupplyEmissionsChart,
        width = 35,
        height = 11,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
  output$EnSupplyEmissionsTable = renderDataTable({
    

    EnSupplyEmissions <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")
    
    names(EnSupplyEmissions)[1] <- "Year"
    
    EnSupplyEmissions$Year <- as.numeric(EnSupplyEmissions$Year)
    
    EnSupplyEmissions <- as_tibble(EnSupplyEmissions)
    
    names(EnSupplyEmissions)[7] <- "Total Greenhouse Gas Emissions"
    
    EnSupplyEmissions <- EnSupplyEmissions[complete.cases(EnSupplyEmissions),]
    
    datatable(
      EnSupplyEmissions[c(1:4,6:7)],
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
        title = "Energy related greenhouse gas emissions (MtCO2e)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Energy related greenhouse gas emissions (MtCO2e)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Energy related greenhouse gas emissions (MtCO2e)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:6, 1) %>% 
      formatStyle(6, fontWeight = "bold")
  })
  
}
