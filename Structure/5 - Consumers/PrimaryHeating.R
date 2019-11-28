require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

PrimaryHeatingOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Sector Consumption",
    fluidRow(column(8,
                    h3("Total final energy consumption by sector", style = "color: #1A5D38;  font-weight:bold"),
                    h4(textOutput(ns('PrimaryHeatingSubtitle')), style = "color: #1A5D38;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('PrimaryHeating.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    #dygraphOutput(ns("PrimaryHeatingPlot")),
    plotlyOutput(ns("PrimaryHeatingPlot"), height = "450px")%>% withSpinner(color="#1A5D38"),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
    tabPanel("Domestic & Non-domestic",
             fluidRow(column(8,
                             h3("Total final energy consumption domestic and non-domestic", style = "color: #1A5D38;  font-weight:bold"),
                             h4(textOutput(ns('PrimaryHeatingDomNonDomSubtitle')), style = "color: #1A5D38;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('PrimaryHeatingDomNonDom.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("PrimaryHeatingPlot")),
             plotlyOutput(ns("PrimaryHeatingDomNonDomPlot"))%>% withSpinner(color="#1A5D38"),
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
      column(12, dataTableOutput(ns("PrimaryHeatingTable"))%>% withSpinner(color="#1A5D38"))),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
    tabPanel("Domestic and Non-domestic",
             fluidRow(
               column(10, h3("Data - Domestic & Non Domestic", style = "color: #1A5D38;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("PrimaryHeatingDomNonDomTable"))%>% withSpinner(color="#1A5D38"))),
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
PrimaryHeating <- function(input, output, session) {
  # output$PrimaryHeatingPlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/1 - Whole System/PrimaryHeating.csv",
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
  
  print("PrimaryHeating.R")
  
  output$PrimaryHeatingSubtitle <- renderText({
    
    PrimaryHeating <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Energy consump by sector", col_names = TRUE, 
                          skip = 17)
    
    PrimaryHeating <- PrimaryHeating[2:6]
    
    PrimaryHeating <- PrimaryHeating[complete.cases(PrimaryHeating),]
    
    names(PrimaryHeating) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    paste(max(as.numeric(PrimaryHeating$Year), na.rm = TRUE))
  })
 
  output$PrimaryHeatingPlot <- renderPlotly  ({
    
    PrimaryHeating <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    PrimaryHeating <- PrimaryHeating[2:6]
    
    PrimaryHeating <- PrimaryHeating[complete.cases(PrimaryHeating),]
    
    names(PrimaryHeating) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    ChartYear <- 2017
    
    ChartColours <- c("#fc9272", "#2b8cbe", "#34d1a3", "#02818a")
    
    PrimaryHeating <- melt(PrimaryHeating, id = "Year")
    PrimaryHeating <- PrimaryHeating[which(PrimaryHeating$Year == ChartYear),]
    
    p <- plot_ly(
      data = PrimaryHeating,
      labels = ~variable,
      type = 'pie',
      values = ~value,
      text = paste0(
        PrimaryHeating$variable,
        ": ", format(round(PrimaryHeating$value, 0), big.mark = ","), " GWh" 
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
  
  output$PrimaryHeatingTable = renderDataTable({
    
    PrimaryHeating <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    PrimaryHeating <- PrimaryHeating[2:6]
    
    PrimaryHeating <- PrimaryHeating[complete.cases(PrimaryHeating),]
    
    names(PrimaryHeating) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    PrimaryHeating$Total <- PrimaryHeating$Heat + PrimaryHeating$Transport + PrimaryHeating$Electricity + PrimaryHeating$Other
    
    datatable(
      PrimaryHeating,
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
      formatRound(2:ncol(PrimaryHeating), 0)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/5 - Consumer/PrimaryHeating.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("PrimaryHeatingTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("PrimaryHeatingDomNonDomTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$PrimaryHeating.png <- downloadHandler(
    filename = "PrimaryHeating.png",
    content = function(file) {

writePNG(
  readPNG(
  "Structure/1 - Whole System/PrimaryHeatingSectorChart.png"
),
file)
    }
  )
  
  output$PrimaryHeatingDomNonDomPlot <- renderPlotly  ({
    
    PrimaryHeatingDomNonDom <- read_excel("Structure/CurrentWorking.xlsx",
                                        sheet = "Energy consump by sector", col_names = TRUE, 
                                        skip = 17)
    
    PrimaryHeatingDomNonDom <- PrimaryHeatingDomNonDom[c(2,14,15) ]
    
    PrimaryHeatingDomNonDom <- PrimaryHeatingDomNonDom[complete.cases(PrimaryHeatingDomNonDom),]
    
    names(PrimaryHeatingDomNonDom) <- c("Year", "Domestic", "Non-domestic")
    
    ChartYear <- max(PrimaryHeatingDomNonDom$Year)
    
    ChartColours <- c("#34d1a3", "#2b8cbe")
    
    PrimaryHeatingDomNonDom <- melt(PrimaryHeatingDomNonDom, id = "Year")
    PrimaryHeatingDomNonDom <- PrimaryHeatingDomNonDom[which(PrimaryHeatingDomNonDom$Year == ChartYear),]
    
    p <- plot_ly(
      data = PrimaryHeatingDomNonDom,
      labels = ~variable,
      type = 'pie',
      values = ~value,
      text = paste0(
        PrimaryHeatingDomNonDom$variable,
        ": ", format(round(PrimaryHeatingDomNonDom$value,0), big.mark = ","), " GWh" 
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
  
  output$PrimaryHeatingDomNonDom.png <- downloadHandler(
    filename = "PrimaryHeatingDomNonDom.png",
    content = function(file) {
      
      writePNG(
        readPNG(
          "Structure/5 - Consumer/PrimaryHeatingChart.png"
        ),
        file)
    }
  )
  
  output$PrimaryHeatingDomNonDomSubtitle <- renderText({
    
    PrimaryHeating <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    PrimaryHeating <- PrimaryHeating[2:6]
    
    PrimaryHeating <- PrimaryHeating[complete.cases(PrimaryHeating),]
    
    names(PrimaryHeating) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    paste(max(as.numeric(PrimaryHeating$Year), na.rm = TRUE))
  })
  
  output$PrimaryHeatingDomNonDomTable = renderDataTable({
    
    PrimaryHeatingDomNonDom <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    PrimaryHeatingDomNonDom <- PrimaryHeatingDomNonDom[c(2,10:15)]
    
    PrimaryHeatingDomNonDom <- PrimaryHeatingDomNonDom[complete.cases(PrimaryHeatingDomNonDom),]
    
    names(PrimaryHeatingDomNonDom) <- c("Year", "Heat - Domestic", "Heat - Non-Domestic ", "Electricity - Domestic", "Electricity - Non-Domestic ", "Total - Domestic", "Toal - Non-Domestic ")
    
   datatable(
      PrimaryHeatingDomNonDom,
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
      formatRound(2:ncol(PrimaryHeatingDomNonDom), 0)
  })
  
}

