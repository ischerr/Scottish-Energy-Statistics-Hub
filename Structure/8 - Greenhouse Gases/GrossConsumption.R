require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

GrossConsumptionOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Gross Consumption", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('GrossConsumptionSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GrossConsumption.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("GrossConsumptionPlot")),
    plotlyOutput(ns("GrossConsumptionPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("GrossConsumptionTable"))%>% withSpinner(color="#39ab2c"))),
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
        SourceLookup("ESTGrossConsumption")
        
      )
    )
  )
}




###### Server ######
GrossConsumption <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("GrossConsumption.R")
  
  output$GrossConsumptionSubtitle <- renderText({
    
    GrossConsumption <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)
    
    paste("Scotland,", max(as.numeric(GrossConsumption$refPeriod), na.rm = TRUE))
  })
  
  output$GrossConsumptionPlot <- renderPlotly  ({
    
    GrossConsumption <- read_excel("Processed Data/TestConsumption.xlsx")
    
    GrossConsumptionPlotData <- GrossConsumption[c(1,3),]
    
    GrossConsumptionPlotData$Type <- as.numeric(rownames(GrossConsumptionPlotData))
    
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#045a8d",
        "#238b45",
        "#d7301f",
        "#f46d43",
        "#fdae61",
        "#fee08b",
        "#abdda4",
        "#016c59",
        "#3288bd",
        "#5e4fa2"
        
      )
    
    
    p <- plot_ly(data = GrossConsumptionPlotData, y = ~ Type) %>%
      add_trace(
        data = GrossConsumptionPlotData,
        x = ~ Consumption,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Consumption",
        text = paste0("Consumption\n", round(GrossConsumptionPlotData$Consumption, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = GrossConsumptionPlotData,
        x = ~ `Renewables`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Renewables",
        text = paste0("Renewables\n", round(GrossConsumptionPlotData$`Renewables`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      )  %>%
      add_trace(
        data = GrossConsumptionPlotData,
        x = ~ `Non-renewable`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextanchor = "middle",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Non-renewable",
        text = paste0("Non-renewable\n", round(GrossConsumptionPlotData$`Non-renewable`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      )  %>%
      add_annotations(
        ax = max(GrossConsumption$Consumption)- min(GrossConsumption$`Exports`),
        x = max(GrossConsumption$Consumption),
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
        name = "Exports",
        line = list(
          arrowhead = 1,
          width = 3,
          color = BarColours[8],
          dash = "none"
        )
      ) %>%
      add_trace(
        mode = 'text',
        x =max(GrossConsumption$Consumption) - (min(GrossConsumption$`Exports`)/2),
        y = 1.5,
        xref = "x", yref = "y",
        showlegend = FALSE ,
        hoverinfo = 'name',
        legendgroup = 10,
        text = paste0("Exports\n\n", round(min(GrossConsumption$`Exports`),1)," MtCO2e"),
        name = paste("Exports"),
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
          ticktext = list("<b>Consumption</b>", "<b>Generation</b>"),
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
  
  
  output$GrossConsumptionTable = renderDataTable({
    
    GrossConsumption <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(GrossConsumption) <- c("Year", "Agriculture", "Business", "Energy Supply", "Industrial Processes", "International Aviation and Shipping", "Forestry (Carbon Sink)", "Public",  "Residential", "Domestic Transport", "Waste Management" )
    
    GrossConsumption <- GrossConsumption[c(1,10,3,2,4,9,6,11,8,5,7)]
    
    GrossConsumption$`Total Emissions` <- rowSums(GrossConsumption[2:11])
    
    datatable(
      GrossConsumption,
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
        title = "Sources of Scottish Greenhouse Gas Emissions (MtCO2e)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Sources of Scottish Greenhouse Gas Emissions (MtCO2e)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Sources of Scottish Greenhouse Gas Emissions (MtCO2e)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:12, 1) %>% 
      formatStyle(11, fontStyle = "italic")
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/8 - Greenhouse Gases/GrossConsumption.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("GrossConsumptionTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$GrossConsumption.png <- downloadHandler(
    filename = "GrossConsumption.png",
    content = function(file) {

      GrossConsumption <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
      
      GrossConsumptionYear <- paste("Scotland,", max(as.numeric(GrossConsumption$refPeriod), na.rm = TRUE))
      
      GrossConsumption <- GrossConsumption[nrow(GrossConsumption),]
      
      GrossConsumption$refPeriod <- NULL
      
      GrossConsumption <- melt(GrossConsumption)
      
      GrossConsumption$variable <- as.character(GrossConsumption$variable)
      
      GrossConsumption$Type <- "Emissions"
      
      GrossConsumption <- rbind(GrossConsumption, c("Total", sum(GrossConsumption$value), "Total greenhouse gas emissions"))
      
      GrossConsumption[which(GrossConsumption$value < 0),]$Type <- "Carbon sinks"
      
      GrossConsumption <- dcast(GrossConsumption, Type ~ variable, value.var = "value")
      
      GrossConsumption[is.na(GrossConsumption)] <- 0
      
      GrossConsumption[2:12] %<>% lapply(function(x) as.numeric(as.character(x)))
      
      GrossConsumption <- as_tibble(GrossConsumption)
      
      GrossConsumption <- GrossConsumption[c(3,1,2),]
      
      GrossConsumption <- GrossConsumption[c(1,10,7,5,8,12,6,9,4,2,3,11)]
      
      ChartColours <- c("#39ab2c", "#FF8500")
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
      
      
      GrossConsumption$Type <-
        factor(GrossConsumption$Type,
               levels = unique(GrossConsumption$Type),
               ordered = TRUE)
      
      GrossConsumption <- melt(GrossConsumption, id.vars = "Type")
      
      GHGCarbonSink <- min(GrossConsumption[which(GrossConsumption$variable == "land-use-land-use-change-and-forestry"),]$value)
      
      
      GrossConsumption <- subset(GrossConsumption, GrossConsumption$Type != "Carbon sinks" )
      GrossConsumption$variable <-
        factor(
          GrossConsumption$variable,
          levels = unique(GrossConsumption$variable),
          ordered = TRUE
        )
      
      GrossConsumption <- GrossConsumption %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Sources of Scottish greenhouse gas emissions"
      sourcecaption <- "Source: SG"
      
      GrossConsumptionChart <- GrossConsumption %>%
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
            label = ifelse(GrossConsumption$variable != "Total", "", str_wrap(GrossConsumption$Type, width = 13))
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
               label = ifelse(GrossConsumption$variable != "Total", "", paste(round(top, digits = 1), "MtCO2e")),
               hjust = ifelse(top > 0, 0, 1)
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        ) +
        geom_segment(
          x = 1.44,
          xend = 1.44,
          y = GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top,
          yend = GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top + GHGCarbonSink,
          colour =   BarColours[7],
          arrow = arrow(length = unit(0.4, "cm")),
          size = 1
        ) + 
        geom_text(
          aes( x = 1.6,
               y = GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top + (GHGCarbonSink/2),
               label = paste("Forestry: \n", round(GHGCarbonSink, digits = 1), "MtCO2e")
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =   BarColours[7]
        ) +
        geom_text(
          aes( x = 1.3,
               y = GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top + (GHGCarbonSink/2),
               label = "Carbon sinks absorb more\ncarbon than they generate"
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =  BarColours[7],
          size = 3
        ) +
        geom_text(
          aes( x = 2.4,
               y = (GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top) *(0/7) - 3,
               label = "Domestic\nTransport",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[2]
        )  +
        geom_text(
          aes( x = 2.4,
               y = (GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top) *(1/7) - 3,
               label = "Business",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[3]
        )  +
        geom_text(
          aes( x = 2.4,
               y = (GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top) *(2/7) - 3,
               label = "Agriculture",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[4]
        )  +
        geom_text(
          aes( x = 2.4,
               y = (GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top) *(3/7) - 3,
               label = "Energy\nSupply",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[5]
        ) +
        geom_text(
          aes( x = 2.4,
               y = (GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top) *(4/7) - 3,
               label = "Residential",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[6]
        ) +
        geom_text(
          aes( x = 2.4,
               y = (GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top) *(5/7) - 3,
               label = "International\naviation\nand shipping",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[7]
        )+
        geom_text(
          aes( x = 2.4,
               y = (GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top) *(6/7) - 3,
               label = "Waste\nManagement",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[8]
        )+
        geom_text(
          aes( x = 2.4,
               y = (GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top) *(7/7) - 3,
               label = "Public",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[9]
        )+
        geom_text(
          aes( x = 2.4,
               y = (GrossConsumption[which(GrossConsumption$Type == "Emissions" & GrossConsumption$variable == "Total"),]$top) *(8/7) - 3,
               label = "Industrial\nProcesses",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[10]
        )
      
      GrossConsumptionChart
      
      
      GrossConsumptionChart <-
        StackedBars(GrossConsumptionChart,
                    GrossConsumption,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      GrossConsumptionChart <-
        GrossConsumptionChart +
        labs(subtitle = GrossConsumptionYear)+
        coord_flip()+ 
        ylim(-7,53)
      
      GrossConsumptionChart
      
      
      
      ggsave(
        file,
        plot = GrossConsumptionChart,
        width = 27,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
  )
}
