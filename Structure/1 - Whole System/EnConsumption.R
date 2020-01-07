require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnConsumptionOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Sector Consumption",
    fluidRow(column(8,
                    h3("Total final energy consumption by sector", style = "color: #1A5D38;  font-weight:bold"),
                    h4(textOutput(ns('EnConsumptionSubtitle')), style = "color: #1A5D38;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsumption.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    #dygraphOutput(ns("EnConsumptionPlot")),
    plotlyOutput(ns("EnConsumptionPlot"), height = "450px")%>% withSpinner(color="#1A5D38"),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
    tabPanel("Domestic & Non-domestic",
             fluidRow(column(8,
                             h3("Total final energy consumption domestic and non-domestic", style = "color: #1A5D38;  font-weight:bold"),
                             h4(textOutput(ns('EnConsumptionDomNonDomSubtitle')), style = "color: #1A5D38;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsumptionDomNonDom.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("EnConsumptionPlot")),
             plotlyOutput(ns("EnConsumptionDomNonDomPlot"))%>% withSpinner(color="#1A5D38"),
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
      column(12, dataTableOutput(ns("EnConsumptionTable"))%>% withSpinner(color="#1A5D38"))),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
    tabPanel("Domestic and Non-domestic",
             fluidRow(
               column(10, h3("Data - Domestic & Non Domestic", style = "color: #1A5D38;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EnConsumptionDomNonDomTable"))%>% withSpinner(color="#1A5D38"))),
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
EnConsumption <- function(input, output, session) {
  # output$EnConsumptionPlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/1 - Whole System/EnConsumption.csv",
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
  
  print("EnConsumption.R")
  
  output$EnConsumptionSubtitle <- renderText({
    
    EnConsumption <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Energy consump by sector", col_names = TRUE, 
                          skip = 17)
    
    EnConsumption <- EnConsumption[2:6]
    
    EnConsumption <- EnConsumption[complete.cases(EnConsumption),]
    
    names(EnConsumption) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    paste(max(as.numeric(EnConsumption$Year), na.rm = TRUE))
  })
 
  output$EnConsumptionPlot <- renderPlotly  ({
    
    EnConsumption <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    EnConsumption <- EnConsumption[2:6]
    
    EnConsumption <- EnConsumption[complete.cases(EnConsumption),]
    
    names(EnConsumption) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    ChartYear <- 2017
    
    ChartColours <- c("#fc9272", "#2b8cbe", "#34d1a3", "#02818a")
    
    EnConsumption <- melt(EnConsumption, id = "Year")
    EnConsumption <- EnConsumption[which(EnConsumption$Year == ChartYear),]
    
    EnConsumption$variable <- paste0("<b>", EnConsumption$variable, "</b>")
    
    p <- plot_ly(
      data = EnConsumption,
      labels = ~variable,
      type = 'pie',
      values = ~value,
      text = paste0(
        EnConsumption$variable,
        ": ", format(round(EnConsumption$value, 0), big.mark = ","), " GWh" 
      ),
      textposition = 'outside',
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
  
  output$EnConsumptionTable = renderDataTable({
    
    EnConsumption <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    EnConsumption <- EnConsumption[2:6]
    
    EnConsumption <- EnConsumption[complete.cases(EnConsumption),]
    
    names(EnConsumption) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    EnConsumption$Total <- EnConsumption$Heat + EnConsumption$Transport + EnConsumption$Electricity + EnConsumption$Other
    
    datatable(
      EnConsumption,
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
      formatRound(2:ncol(EnConsumption), 0)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/1 - Whole System/EnConsumption.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("EnConsumptionTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("EnConsumptionDomNonDomTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnConsumption.png <- downloadHandler(
    filename = "EnConsumption.png",
    content = function(file) {

writePNG(
  readPNG(
  "Structure/1 - Whole System/EnConsumptionSectorChart.png"
),
file)
    }
  )
  
  output$EnConsumptionDomNonDomPlot <- renderPlotly  ({
    
    EnConsumptionDomNonDom <- read_excel("Structure/CurrentWorking.xlsx",
                                        sheet = "Energy consump by sector", col_names = TRUE, 
                                        skip = 17)
    
    EnConsumptionDomNonDom <- EnConsumptionDomNonDom[c(2,14,15) ]
    
    EnConsumptionDomNonDom <- EnConsumptionDomNonDom[complete.cases(EnConsumptionDomNonDom),]
    
    names(EnConsumptionDomNonDom) <- c("Year", "Domestic", "Non-domestic")
    
    ChartYear <- max(EnConsumptionDomNonDom$Year)
    
    ChartColours <- c("#34d1a3", "#2b8cbe")
    
    EnConsumptionDomNonDom <- melt(EnConsumptionDomNonDom, id = "Year")
    EnConsumptionDomNonDom <- EnConsumptionDomNonDom[which(EnConsumptionDomNonDom$Year == ChartYear),]
    
    EnConsumptionDomNonDom$variable <- paste0("<b>", EnConsumptionDomNonDom$variable, "</b>")
    
    p <- plot_ly(
      data = EnConsumptionDomNonDom,
      labels = ~variable,
      type = 'pie',
      values = ~value,
      text = paste0(
        EnConsumptionDomNonDom$variable,
        ": ", format(round(EnConsumptionDomNonDom$value,0), big.mark = ","), " GWh" 
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
  
  output$EnConsumptionDomNonDom.png <- downloadHandler(
    filename = "EnConsumptionDomNonDom.png",
    content = function(file) {
      
      writePNG(
        readPNG(
          "Structure/1 - Whole System/EnConsumptionSectorDomNonDomChart.png"
        ),
        file)
    }
  )
  
  output$EnConsumptionDomNonDomSubtitle <- renderText({
    
    EnConsumption <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    EnConsumption <- EnConsumption[2:6]
    
    EnConsumption <- EnConsumption[complete.cases(EnConsumption),]
    
    names(EnConsumption) <- c("Year", "Heat", "Transport", "Electricity", "Other")
    
    paste(max(as.numeric(EnConsumption$Year), na.rm = TRUE))
  })
  
  output$EnConsumptionDomNonDomTable = renderDataTable({
    
    EnConsumptionDomNonDom <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Energy consump by sector", col_names = TRUE, 
                               skip = 17)
    
    EnConsumptionDomNonDom <- EnConsumptionDomNonDom[c(2,10:15)]
    
    EnConsumptionDomNonDom <- EnConsumptionDomNonDom[complete.cases(EnConsumptionDomNonDom),]
    
    names(EnConsumptionDomNonDom) <- c("Year", "Heat - Domestic", "Heat - Non-Domestic ", "Electricity - Domestic", "Electricity - Non-Domestic ", "Total - Domestic", "Toal - Non-Domestic ")
    
   datatable(
      EnConsumptionDomNonDom,
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
      formatRound(2:ncol(EnConsumptionDomNonDom), 0)
  })
  
}

