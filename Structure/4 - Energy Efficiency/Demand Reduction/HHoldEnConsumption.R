require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

HHoldEnConsumptionOutput <- function(id) {
  ns <- NS(id)
  tagList(
   fluidRow(column(8,
                    h3("End use of energy consumption", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('HHoldEnConsumptionSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('HHoldEnConsumption.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("HHoldEnConsumptionPlot")),
    plotlyOutput(ns("HHoldEnConsumptionPlot"), height = "450px")%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
   fluidRow(
    column(10, h3("Data - End use of energy consumption", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("HHoldEnConsumptionTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("SGSHCS", "BEISSubNatEnergy", "BEISUKConsump"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("SGSHCS"),
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("BEISUKConsump")
      )
    )
  )
}




###### Server ######
HHoldEnConsumption <- function(input, output, session) {
  # output$HHoldEnConsumptionPlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/1 - Whole System/HHoldEnConsumption.csv",
  #       header = TRUE,
  #       sep = ",",
  #       na.strings = "-"
  #     )
  #
  #   YearLow <- as.numeric(min(RenEn$Year))
  #   YearHigh <- as.numeric(max(RenEn$Year +1))
  #
  #   dygraph(RenEn, main = "Renewable Energy Target") %>%
  #     dyAxis("y", label = "% Progress", valueRange = c(0,30)) %>%
  #     dyAxis("x", label = "Year", drawGrid = TRUE) %>%
  #     dyOptions(colors =  c("Green","Orange", "Blue")) %>%
  #     dyLegend(width = 170 ,
  #              labelsSeparateLines = TRUE ,
  #              show = "always") %>%
  #     dyOptions(
  #       stackedGraph = TRUE,
  #       axisLineColor = "white",
  #       gridLineColor = "white",
  #       includeZero = TRUE,
  #       fillAlpha = .65
  #     ) %>%
  #     #    dyRangeSelector() %>%
  #     dyCSS("Structure/1 - Whole System/legend.css")
  #
  # })
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("HHoldEnConsumption.R")
  
  output$HHoldEnConsumptionSubtitle <- renderText({
    

      paste("Scotland, 2018")
    
  })
 
  output$HHoldEnConsumptionPlot <- renderPlotly  ({
    
    HHoldEnConsumption <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Household energy consump", col_names = TRUE, 
                               skip = 20)
    
    HHoldEnConsumption <- HHoldEnConsumption[-1,]

    HHoldEnConsumption <- HHoldEnConsumption[1:5]
    
    names(HHoldEnConsumption)[1] <- "Measure"
    
    HHoldEnConsumption <- melt(HHoldEnConsumption)
    
    HHoldEnConsumption$variable <- paste0("<b>", HHoldEnConsumption$variable, "</b>")
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#bd0026", "#f03b20", "#fd8d3c","#feb24c")
    
    
    p <- plot_ly(
      data = HHoldEnConsumption,
      labels = ~variable,
      type = 'pie',
      values = ~value,
      text = paste0(
        HHoldEnConsumption$variable,
        ": ", format(round(HHoldEnConsumption$value, 0), big.mark = ","), " GWh"
      ),
      textposition = 'outside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      marker = list(colors = BarColours,
                    line = list(color = '#FFFFFF', width = 1))
    )  %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "black"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    #orca(p, "StaticCharts/HHoldEnConsumptione.svg")
    
    
    
  })
  
  output$HHoldEnConsumptionTable = renderDataTable({
    
    HHoldEnConsumption <- read_excel("Structure/CurrentWorking.xlsx",
                                             sheet = "Household energy consump", col_names = FALSE, 
                                             skip = 20)
    
    HHoldEnConsumption <- as_tibble(t(HHoldEnConsumption))
    



    names(HHoldEnConsumption) <- unlist(HHoldEnConsumption[1,])
    
    HHoldEnConsumption <- tail(HHoldEnConsumption, -1)
    
    names(HHoldEnConsumption)[1] <- "End Use"
    
        HHoldEnConsumption[2:3]%<>% lapply(function(x)
      as.numeric(as.character(x)))
    
        datatable(
      HHoldEnConsumption,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "End use of energy consumption",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'End use of energy consumption',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'End use of energy consumption')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(HHoldEnConsumption), 0) %>% 
      formatPercentage(2,1)
  })
  
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Demand Reduction/HHoldEnConsumption.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("HHoldEnConsumptionTable")
  })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$HHoldEnConsumption.png <- downloadHandler(
    filename = "HHoldEnConsumption.png",
    content = function(file) {

writePNG(
  readPNG(
  "Structure/4 - Energy Efficiency/Demand Reduction/HHoldEnConsumptionChart.png"
),
file)
    }
  )
}


