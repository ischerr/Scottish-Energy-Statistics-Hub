require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnSupplySwitchOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Proportion of customers who have switched energy supplier by local authority", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('EnSupplySwitchSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnSupplySwitch.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("EnSupplySwitchPlot")),
    plotlyOutput(ns("EnSupplySwitchPlot"), height = "500px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data - Proportion of customers who have switched energy supplier by local authority 2018", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnSupplySwitchTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
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
        SourceLookup("CASEnSwitch")
        
      )
    )
  )
}




###### Server ######
EnSupplySwitch <- function(input, output, session) {
  # output$EnSupplySwitchPlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/2 - Renewables/Electricity/EnSupplySwitch.csv",
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
  #     dyCSS("Structure/2 - Renewables/Electricity/legend.css")
  #
  # })
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("EnSupplySwitch.R")
  ###### Renewable Energy ###### ######


  Data <- read_csv("Processed Data/Output/Consumers/MarketSwitching.csv")
  
  names(Data)[1] <- "Year"
  
  MarketSwitch <- Data  
  
  ### From ESD ###
  
  output$EnSupplySwitchSubtitle <- renderText({
    
    paste("Scotland, 2018")
  })
  
  output$EnSupplySwitchPlot <- renderPlotly({
    
    ChartColours <- c("#4292c6", "#7bccc4", "#08519c", "#ef3b2c")
    sourcecaption = "Source: BEIS"
    plottitle = "Proportion of payment methods used\nfor electricity bills"
    
    
    
    p <-  plot_ly(data = MarketSwitch,
                  x = ~ Year ) %>% 
      add_trace(y = ~ `North Scotland`,
                name = "North Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "North Scotland: ",
                  percent(MarketSwitch$`North Scotland`, accuracy = 0.1),
                  "\nYear: ",
                  format(MarketSwitch$Year, "%B %Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "dash")
      ) %>% 
      add_trace(
        data = tail(MarketSwitch[which(MarketSwitch$`North Scotland` > 0 | MarketSwitch$`North Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `North Scotland`,
        name = "North Scotland",
        text = paste0(
          "North Scotland: ",
          percent(MarketSwitch[which(MarketSwitch$`North Scotland` > 0 | MarketSwitch$`North Scotland` < 0),][-1,]$`North Scotland`, accuracy = 0.1),
          "\nYear: ",
          format(MarketSwitch[which(MarketSwitch$`North Scotland` > 0 | MarketSwitch$`North Scotland` < 0),][-1,]$Year, "%B %Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = MarketSwitch,
                x = ~ Year,
                y = ~ `South Scotland`,
                name = "South Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "South Scotland: ",
                  percent(MarketSwitch$`South Scotland`, accuracy = 0.1),
                  "\nYear: ",
                  format(MarketSwitch$Year, "%B %Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "dash")
      ) %>% 
      add_trace(
        data = tail(MarketSwitch[which(MarketSwitch$`South Scotland` > 0 | MarketSwitch$`South Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `South Scotland`,
        name = "South Scotland",
        legendgroup = "2",
        text = paste0(
          "South Scotland: ",
          percent(MarketSwitch[which(MarketSwitch$`South Scotland` > 0 | MarketSwitch$`South Scotland` < 0),][-1,]$`South Scotland`, accuracy = 0.1),
          "\nYear: ",
          format(MarketSwitch[which(MarketSwitch$`South Scotland` > 0 | MarketSwitch$`South Scotland` < 0),][-1,]$Year, "%B %Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      add_trace(data = MarketSwitch,
                x = ~ Year,
                y = ~ `Whole Scotland`,
                name = "Whole Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Whole Scotland: ",
                  percent(MarketSwitch$`Whole Scotland`, accuracy = 0.1),
                  "\nYear: ",
                  format(MarketSwitch$Year, "%B %Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(MarketSwitch[which(MarketSwitch$`Whole Scotland` > 0 | MarketSwitch$`Whole Scotland` < 0),], 1),
        x = ~ Year,
        y = ~ `Whole Scotland`,
        name = "Whole Scotland",
        legendgroup = "3",
        text = paste0(
          "Whole Scotland: ",
          percent(MarketSwitch[which(MarketSwitch$`Whole Scotland` > 0 | MarketSwitch$`Whole Scotland` < 0),][-1,]$`Whole Scotland`, accuracy = 0.1),
          "\nYear: ",
          format(MarketSwitch[which(MarketSwitch$`Whole Scotland` > 0 | MarketSwitch$`Whole Scotland` < 0),][-1,]$Year, "%B %Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(data = MarketSwitch,
                x = ~ Year,
                y = ~ `GB`,
                name = "GB",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "4",
                text = paste0(
                  "GB: ",
                  percent(MarketSwitch$`GB`, accuracy = 0.1),
                  "\nYear: ",
                  format(MarketSwitch$Year, "%B %Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[4], dash = "none")
      ) %>% 
      add_trace(
        data = tail(MarketSwitch[which(MarketSwitch$`GB` > 0 | MarketSwitch$`GB` < 0),], 1),
        x = ~ Year,
        y = ~ `GB`,
        name = "GB",
        legendgroup = "4",
        text = paste0(
          "GB: ",
          percent(MarketSwitch[which(MarketSwitch$`GB` > 0 | MarketSwitch$`GB` < 0),][-1,]$`GB`, accuracy = 0.1),
          "\nYear: ",
          format(MarketSwitch[which(MarketSwitch$`GB` > 0 | MarketSwitch$`GB` < 0),][-1,]$Year, "%B %Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[4])
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
                     showgrid = FALSE
                     ),
        yaxis = list(
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
  
  
  output$EnSupplySwitchTable = renderDataTable({
    
    EnSupplySwitch <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Switching by LA",
      skip = 13
    )[2:4]

    datatable(
      EnSupplySwitch,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Proportion of customers who have switched energy supplier by local authority, 2018",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of customers who have switched energy supplier by local authority, 2018',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of customers who have switched energy supplier by local authority, 2018')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(3), 1) 
  })
  
  output$EnSupplySwitchTimeSeriesTable = renderDataTable({
    
    EnSupplySwitch <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Non-home supplier elec",
      skip = 13
    )
    
    names(EnSupplySwitch)[1] <- "Quarter"#
    
    EnSupplySwitch <- EnSupplySwitch[complete.cases(EnSupplySwitch),]
    
    EnSupplySwitch$Quarter <- as.Date(as.numeric(EnSupplySwitch$Quarter), origin = "1899-12-30")
    
    EnSupplySwitch$Quarter <- as.character(as.yearqtr(EnSupplySwitch$Quarter))
    
    
    datatable(
      EnSupplySwitch,
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
        title = "Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:9, 1)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/EnSupplySwitch.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("EnSupplySwitchTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("EnSupplySwitchTimeSeriesTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnSupplySwitch.png <- downloadHandler(
    filename = "EnSupplySwitch.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/EnSupplySwitchChart.png"), file) 
    }
  )
}
