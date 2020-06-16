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
    fluidRow(column(8,
                    h3("Sources of Scottish greenhouse gas emissions", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('GHGEmissionsSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GHGEmissions.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("GHGEmissionsPlot")),
    plotlyOutput(ns("GHGEmissionsPlot"))%>% withSpinner(color="#39ab2c"),
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
      column(12, dataTableOutput(ns("GHGEmissionsTable"))%>% withSpinner(color="#39ab2c"))),
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
        SourceLookup("ESTGHGEmissions")
        
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
  
  output$GHGEmissionsSubtitle <- renderText({
    
    GHGEmissions <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "GHG emissions", col_names = FALSE, 
                               skip = 12)
    
    GHGEmissions <- t(GHGEmissions)
    
    GHGEmissions <- as_tibble(GHGEmissions)
    
    names(GHGEmissions) <- unlist(GHGEmissions[1,])
    
    names(GHGEmissions)[1] <- "Year"
    
    GHGEmissions %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGEmissions <- as_tibble(GHGEmissions)
    
    GHGEmissions[2,1] <- " Baseline Period"
    
    GHGEmissions <- GHGEmissions[complete.cases(GHGEmissions),]
    
    paste("Scotland,", max(GHGEmissions$Year))
  })
  
  output$GHGEmissionsPlot <- renderPlotly  ({
    
    GHGEmissions <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "GHG emissions", skip = 12, col_names = FALSE)
    
    GHGEmissionsYear <- max(as.numeric(GHGEmissions[1,]), na.rm = TRUE)
    
    GHGEmissions <- GHGEmissions[c(ncol(GHGEmissions)-3):(ncol(GHGEmissions))]
    
    GHGEmissions <- as_tibble(t(GHGEmissions))
    
    names(GHGEmissions) <- as.character(unlist(GHGEmissions[1,]))
    
    GHGEmissions <- GHGEmissions[-1,]
    
    names(GHGEmissions) <- c("Type", "Forestry", "Other", "Waste", "Development", "Residential", "Business/Industrial process", "Agriculture", "Energy Supply", "Transport", "Total" )
    
    GHGEmissions[2:11] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGEmissions[is.na(GHGEmissions)] <- 0
    
    GHGEmissions$Other <- GHGEmissions$Other + GHGEmissions$Waste + GHGEmissions$Development
    
    GHGEmissions$Waste <- NULL
    
    GHGEmissions$Development <- NULL
    
    GHGEmissions <- GHGEmissions[c(1,9,2:8)]
    
    GHGEmissionsPlotData <- GHGEmissions[c(1,3),]
    
    GHGEmissionsPlotData$Type <- as.numeric(rownames(GHGEmissionsPlotData))
    
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#016c59",
        "#662506", 
        "#993404",
        "#cc4c02",
        "#ec7014",
        "#fe9929",
        "#fec44f",
        "#78c679"
      )
    
    
    p <- plot_ly(data = GHGEmissionsPlotData, y = ~ Type) %>%
      add_trace(
        data = GHGEmissionsPlotData,
        x = ~ Total,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Total",
        text = paste0("Total\n", round(GHGEmissionsPlotData$Total, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = GHGEmissionsPlotData,
        x = ~ Transport,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Transport",
        text = paste0("Transport\n", round(GHGEmissionsPlotData$Transport, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      )  %>%
      add_trace(
        data = GHGEmissionsPlotData,
        x = ~ `Energy Supply`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Energy Supply",
        text = paste0("Energy Supply\n", round(GHGEmissionsPlotData$`Energy Supply`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      )  %>%
      add_trace(
        data = GHGEmissionsPlotData,
        x = ~ `Agriculture`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Agriculture",
        text = paste0("Agriculture\n", round(GHGEmissionsPlotData$`Agriculture`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = GHGEmissionsPlotData,
        x = ~ `Business/Industrial process`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Business/Industrial process",
        text = paste0("Business/Industrial process\n", round(GHGEmissionsPlotData$`Business/Industrial process`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = GHGEmissionsPlotData,
        x = ~ `Residential`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Residential",
        text = paste0("Residential\n", round(GHGEmissionsPlotData$`Residential`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = GHGEmissionsPlotData,
        x = ~ `Other`,
        type = 'bar',
        textinfo = 'text',
        textposition = "inside",
        insidetextfont = list(color = "#FFFFFF",
                              font = "bold"),
        width = 0.3,
        orientation = 'h',
        name = "Other",
        text = paste0("Other\n", round(GHGEmissionsPlotData$`Other`, digits = 1), " MtCO2e"),
        hoverinfo = 'text',
        marker = list(color = BarColours[7]),
        legendgroup = 7
      ) %>%
      add_annotations(
        ax = rowSums(GHGEmissionsPlotData[2:9])[2],
        x =rowSums(GHGEmissionsPlotData[2:9])[2] + min(GHGEmissions$Forestry),
        ay = 1.5,
        y = 1.5,
        xref = "x", yref = "y",
        axref = "x", ayref = "y",
        showlegend = FALSE ,
        arrowhead = 4,
        arrowsize = 1,
        arrowcolor = BarColours[8],
        hoverinfo = 'name',
        legendgroup = 10,
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
        x =rowSums(GHGEmissionsPlotData[2:9])[2] + (min(GHGEmissions$Forestry)/2),
        y = 1.5,
        xref = "x", yref = "y",
        showlegend = FALSE ,
        hoverinfo = 'name',
        legendgroup = 10,
        text = paste0("Forestry\n\n", round(min(GHGEmissions$Forestry),1)," MtCO2e"),
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
  
  
  output$GHGEmissionsTable = renderDataTable({
    
    GHGEmissions <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "GHG emissions", col_names = FALSE, 
                          skip = 12)
    
    GHGEmissions <- t(GHGEmissions)
    
    GHGEmissions <- as_tibble(GHGEmissions)
    
    names(GHGEmissions) <- unlist(GHGEmissions[1,])
    
    names(GHGEmissions)[1] <- "Year"
    
    GHGEmissions %<>% lapply(function(x) as.numeric(as.character(x)))
    
   GHGEmissions <- as_tibble(GHGEmissions)
    
   GHGEmissions[2,1] <- " Baseline Period"
    
GHGEmissions <- GHGEmissions[complete.cases(GHGEmissions),]
    
    datatable(
      GHGEmissions,
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
      formatRound(2:11, 1) 
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/8 - Greenhouse Gases/GHGEmissions.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("GHGEmissionsTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$GHGEmissions.png <- downloadHandler(
    filename = "GHGEmissions.png",
    content = function(file) {

      GHGEmissions <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "GHG emissions", skip = 12, col_names = FALSE)
      
      GHGEmissionsYear <- max(as.numeric(GHGEmissions[1,]), na.rm = TRUE)
      
      GHGEmissions <- GHGEmissions[c(ncol(GHGEmissions)-3):(ncol(GHGEmissions))]
      
      GHGEmissions <- as_tibble(t(GHGEmissions))
      
      names(GHGEmissions) <- as.character(unlist(GHGEmissions[1,]))
      
      GHGEmissions <- GHGEmissions[-1,]
      
      names(GHGEmissions) <- c("Type", "Forestry", "Other", "Waste", "Development", "Residential", "Business and Industrial process", "Agriculture and Related Land Use", "Energy Supply", "Transport", "Total" )
      
      GHGEmissions[2:11] %<>% lapply(function(x) as.numeric(as.character(x)))
      
      GHGEmissions[is.na(GHGEmissions)] <- 0
      
      GHGEmissions$Other <- GHGEmissions$Other + GHGEmissions$Waste + GHGEmissions$Development
      
      GHGEmissions$Waste <- NULL
      
      GHGEmissions$Development <- NULL
      
      GHGEmissions <- GHGEmissions[c(1,9,2:8)]
      
      # GHGEmissions <- arrange(GHGEmissions, -row_number())
      
      GHGEmissions$Type <-
        factor(GHGEmissions$Type,
               levels = unique(GHGEmissions$Type),
               ordered = TRUE)
      
      GHGEmissions <- melt(GHGEmissions, id.vars = "Type")
      
      GHGCarbonSink <- min(GHGEmissions[which(GHGEmissions$variable == "Forestry"),]$value)
      
      
      GHGEmissions <- subset(GHGEmissions, GHGEmissions$Type != "Carbon sinks" )
      GHGEmissions$variable <-
        factor(
          GHGEmissions$variable,
          levels = unique(GHGEmissions$variable),
          ordered = TRUE
        )
      
      GHGEmissions <- GHGEmissions %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Sources of Scottish greenhouse gas emissions"
      sourcecaption <- "Source: SG"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c(
          "#662506", 
          "#993404",
          "#cc4c02",
          "#ec7014",
          "#fe9929",
          "#fec44f",
          "#78c679",
          "#016c59"
        )
      
      
      GHGEmissionsChart <- GHGEmissions %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Transport" = BarColours[1],
            "Energy Supply" = BarColours[2],
            "Agriculture and Related Land Use" = BarColours[3],
            "Business and Industrial process" = BarColours[4],
            "Residential" = BarColours[5],
            "Other" = BarColours[6],
            "Forestry" = BarColours[7],
            "Total" = BarColours[8]
          )
        ) +
        geom_bar(stat = "identity",
                 width = .4) +
        geom_text(
          aes(
            x = Type,
            y = ifelse(top > 0, -4.5,7),
            label = ifelse(GHGEmissions$variable != "Total", "", str_wrap(GHGEmissions$Type, width = 13))
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        ) + 
        geom_text(
          aes(
            x = Type,
            y = top-pos,
            label = ifelse(value > 0 & Type == "Emissions", sprintf("%.1f", round(value, digits = 1)), ""
            )),
          family = "Century Gothic",
          fontface = 2,
          colour =  "white"
        ) + 
        geom_text(
          aes( x = Type,
               y = top + ifelse(top > 0, +1,-1),
               label = ifelse(GHGEmissions$variable != "Total", "", paste(round(top, digits = 1), "MtCO2e")),
               hjust = ifelse(top > 0, 0, 1)
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        ) +
        geom_segment(
          x = 1.44,
          xend = 1.44,
          y = GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Total"),]$top,
          yend = GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Total"),]$top + GHGCarbonSink,
          colour =   BarColours[7],
          arrow = arrow(length = unit(0.4, "cm")),
          size = 1
        ) + 
        geom_text(
          aes( x = 1.55,
               y = GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Total"),]$top + (GHGCarbonSink/2),
               label = paste("Forestry: \n", round(GHGCarbonSink, digits = 1), "MtCO2e")
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =   BarColours[7]
        ) +
        geom_text(
          aes( x = 1.28,
               y = GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Total"),]$top + (GHGCarbonSink/2),
               label = "Carbon sinks absorb\nmore carbon than\nthey generate"
          ),
          family = "Century Gothic",
          fontface = 2,
          colour =  BarColours[7],
          size = 3
        ) +
        geom_text(
          aes( x = 2.3,
               y = GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Transport" ),]$top - GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Transport" ),]$pos,
               label = "Transport",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[1]
        )  +
        geom_text(
          aes( x = 1.7,
               y = GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Energy Supply" ),]$top - GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Energy Supply" ),]$pos,
               label = "Energy Supply",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[2]
        )  +
        geom_text(
          aes( x = 2.3,
               y = GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Agriculture and Related Land Use" ),]$top - GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Agriculture and Related Land Use" ),]$pos,
               label = "Agriculture",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[3]
        )  +
        geom_text(
          aes( x = 1.7,
               y = GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Business and Industrial process" ),]$top - GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Business and Industrial process" ),]$pos,
               label = "Business and Industrial",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[4]
        )  +
        geom_text(
          aes( x = 2.3,
               y = GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Residential" ),]$top - GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Residential" ),]$pos,
               label = "Residential",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[5]
        )  +
        geom_text(
          aes( x = 1.7,
               y = GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Other" ),]$top - GHGEmissions[which(GHGEmissions$Type == "Emissions" & GHGEmissions$variable == "Other" ),]$pos,
               label = "Other",
               family = "Century Gothic",
               fontface = 2
          ),
          colour =  BarColours[6]
        ) 
      
      
      GHGEmissionsChart
      
      
      GHGEmissionsChart <-
        StackedBars(GHGEmissionsChart,
                    GHGEmissions,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      GHGEmissionsChart <-
        GHGEmissionsChart +
        labs(subtitle = GHGEmissionsYear)+
        coord_flip()+ 
        ylim(-7,57)
      
      GHGEmissionsChart
      
      
      
      ggsave(
        file,
        plot = GHGEmissionsChart,
        width = 24,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
  )
}
