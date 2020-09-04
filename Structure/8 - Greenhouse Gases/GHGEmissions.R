require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

SectorInventoryOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Sources of Scottish greenhouse gas emissions", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('SectorInventorySubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('SectorInventory.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("SectorInventoryPlot")),
    plotlyOutput(ns("SectorInventoryPlot"))%>% withSpinner(color="#39ab2c"),
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
      column(12, dataTableOutput(ns("SectorInventoryTable"))%>% withSpinner(color="#39ab2c"))),
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
        SourceLookup("ESTSectorInventory")
        
      )
    )
  )
}




###### Server ######
SectorInventory <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("SectorInventory.R")
  
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
        "Sources of Scottish greenhouse gas emissions"
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
      SectorInventory,
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
                     paste(readtext("Structure/8 - Greenhouse Gases/SectorInventory.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("SectorInventoryTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  
}
