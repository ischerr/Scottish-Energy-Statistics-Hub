require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnConsSectorOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Sector Consumption",
    fluidRow(column(8,
                    h3("Total final energy consumption by sector", style = "color: #1A5D38;  font-weight:bold"),
                    h4(textOutput(ns('EnConsSectorSubtitle')), style = "color: #1A5D38;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsSector.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    #dygraphOutput(ns("EnConsSectorPlot")),
    plotlyOutput(ns("EnConsSectorPlot"), height = "450px")%>% withSpinner(color="#1A5D38"),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
    tabPanel("Domestic & Non-domestic",
             fluidRow(column(8,
                             h3("Total final energy consumption domestic and non-domestic", style = "color: #1A5D38;  font-weight:bold"),
                             h4(textOutput(ns('EnConsSectorDomNonDomSubtitle')), style = "color: #1A5D38;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsSectorDomNonDom.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("EnConsSectorPlot")),
             plotlyOutput(ns("EnConsSectorDomNonDomPlot"))%>% withSpinner(color="#1A5D38"),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #1A5D38;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    tabsetPanel(
      tabPanel("Consumption by Sector",
               fluidRow(
    column(10, h3("Data - Sector Consumption", style = "color: #1A5D38;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnConsSectorTable"))%>% withSpinner(color="#1A5D38"))),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
    tabPanel("Domestic and Non-domestic",
             fluidRow(
               column(10, h3("Data - Domestic & Non Domestic", style = "color: #1A5D38;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EnConsSectorDomNonDomTable"))%>% withSpinner(color="#1A5D38"))),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"))),
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
EnConsSector <- function(input, output, session) {
  # output$EnConsSectorPlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/1 - Whole System/EnConsSector.csv",
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
  
  print("EnConsSector.R")
  
  output$EnConsSectorSubtitle <- renderText({
    
    EnConsSector <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Energy consump by sector", col_names = TRUE, 
                          skip = 17)
    
    EnConsSector <- EnConsSector[2:6]
    
    EnConsSector <- EnConsSector[complete.cases(EnConsSector),]
    
    names(EnConsSector) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    paste(max(as.numeric(EnConsSector$Year), na.rm = TRUE))
  })
 
  output$EnConsSectorPlot <- renderPlotly  ({
    
    EnConsSector <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    EnConsSector <- EnConsSector[2:6]
    
    EnConsSector <- EnConsSector[complete.cases(EnConsSector),]
    
    names(EnConsSector) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    ChartYear <- 2017
    
    ChartColours <- c("#fc9272", "#2b8cbe", "#34d1a3", "#02818a")
    
    EnConsSector <- melt(EnConsSector, id = "Year")
    EnConsSector <- EnConsSector[which(EnConsSector$Year == ChartYear),]
    
    p <- plot_ly(
      data = EnConsSector,
      labels = ~variable,
      type = 'pie',
      values = ~value,
      text = paste0(
        EnConsSector$variable,
        ": ", format(round(EnConsSector$value, 0), big.mark = ","), " GWh" 
      ),
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      marker = list(colors = ChartColours,
                    line = list(color = '#FFFFFF', width = 1))
    )  %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
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
    
    
    
    
  })
  
  output$EnConsSectorTable = renderDataTable({
    
    EnConsSector <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    EnConsSector <- EnConsSector[2:6]
    
    EnConsSector <- EnConsSector[complete.cases(EnConsSector),]
    
    names(EnConsSector) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    EnConsSector$Total <- EnConsSector$Heat + EnConsSector$Transport + EnConsSector$Electricity + EnConsSector$Other
    
    datatable(
      EnConsSector,
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
        title = "Total final energy consumption by sector (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Total final energy consumption by sector (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Total final energy consumption by sector (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(EnConsSector), 0)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/1 - Whole System/EnConsSector.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("EnConsSectorTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("EnConsSectorDomNonDomTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnConsSector.png <- downloadHandler(
    filename = "EnConsSector.png",
    content = function(file) {

writePNG(
  readPNG(
  "Structure/1 - Whole System/EnConsumptionSectorChart.png"
),
file)
    }
  )
  
  output$EnConsSectorDomNonDomPlot <- renderPlotly  ({
    
    EnConsSectorDomNonDom <- read_excel("Structure/CurrentWorking.xlsx",
                                        sheet = "Energy consump by sector", col_names = TRUE, 
                                        skip = 17)
    
    EnConsSectorDomNonDom <- EnConsSectorDomNonDom[c(2,14,15) ]
    
    EnConsSectorDomNonDom <- EnConsSectorDomNonDom[complete.cases(EnConsSectorDomNonDom),]
    
    names(EnConsSectorDomNonDom) <- c("Year", "Domestic", "Non-domestic")
    
    ChartYear <- max(EnConsSectorDomNonDom$Year)
    
    ChartColours <- c("#34d1a3", "#2b8cbe")
    
    EnConsSectorDomNonDom <- melt(EnConsSectorDomNonDom, id = "Year")
    EnConsSectorDomNonDom <- EnConsSectorDomNonDom[which(EnConsSectorDomNonDom$Year == ChartYear),]
    
    p <- plot_ly(
      data = EnConsSectorDomNonDom,
      labels = ~variable,
      type = 'pie',
      values = ~value,
      text = paste0(
        EnConsSectorDomNonDom$variable,
        ": ", format(round(EnConsSectorDomNonDom$value,0), big.mark = ","), " GWh" 
      ),
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      marker = list(colors = ChartColours,
                    line = list(color = '#FFFFFF', width = 1))
    )  %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
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

  })
  
  output$EnConsSectorDomNonDom.png <- downloadHandler(
    filename = "EnConsSectorDomNonDom.png",
    content = function(file) {
      
      writePNG(
        readPNG(
          "Structure/1 - Whole System/EnConsumptionSectorDomNonDomChart.png"
        ),
        file)
    }
  )
  
  output$EnConsSectorDomNonDomSubtitle <- renderText({
    
    EnConsSector <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    EnConsSector <- EnConsSector[2:6]
    
    EnConsSector <- EnConsSector[complete.cases(EnConsSector),]
    
    names(EnConsSector) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    paste(max(as.numeric(EnConsSector$Year), na.rm = TRUE))
  })
  
  output$EnConsSectorDomNonDomTable = renderDataTable({
    
    EnConsSectorDomNonDom <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    EnConsSectorDomNonDom <- EnConsSectorDomNonDom[c(2,10:15)]
    
    EnConsSectorDomNonDom <- EnConsSectorDomNonDom[complete.cases(EnConsSectorDomNonDom),]
    
    names(EnConsSectorDomNonDom) <- c("Year", "Heat - Domestic", "Heat - Non-Domestic ", "Electricity - Domestic", "Electricity - Non-Domestic ", "Total - Domestic", "Toal - Non-Domestic ")
    
   datatable(
      EnConsSectorDomNonDom,
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
        title = "Total final energy consumption by sector, domestic and non-domestic (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Total final energy consumption by sector, domestic and non-domestic (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Total final energy consumption by sector, domestic and non-domestic (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(EnConsSectorDomNonDom), 0)
  })
  
}

