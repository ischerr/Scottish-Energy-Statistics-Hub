require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



DomEPCsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Housing Stock EPC",
    fluidRow(column(8,
                    h3("Distribution of Scottish housing stock by EPC band", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('StockEPCSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('StockEPC.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("StockEPCPlot")),
    plotlyOutput(ns("StockEPCPlot"))%>% withSpinner(color="#34d1a3"),
    HTML("<blockquote><p>*based on SAP 2012 RdSAP v9.93&nbsp;</p></blockquote>"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Properties above Band C",
             fluidRow(column(8,
                             h3("Proportion of properties rated EPC band C or above", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('EPCProportionsSubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EPCProportions.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("StockEPCPlot")),
             plotlyOutput(ns("EPCProportionsPlot"))%>% withSpinner(color="#34d1a3"),
             HTML("<blockquote><p>*based on SAP 2012 RdSAP v9.92&nbsp;</p></blockquote>"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Housing Tenure EPC",
             fluidRow(column(8,
                             h3("Distribution of Scottish housing stock by EPC band and housing tenure", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('EPCTenureSubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EPCTenure.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("StockEPCPlot")),
             plotlyOutput(ns("EPCTenurePlot"))%>% withSpinner(color="#34d1a3"),
             HTML("<blockquote><p>*based on SAP 2012 RdSAP v9.93&nbsp;</p></blockquote>"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("MedianEER Rating",
             fluidRow(column(8,
                             h3("MedianEER Rating", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('EERProportionsSubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EERProportions.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("StockEPCPlot")),
             plotlyOutput(ns("EERProportionsPlot"))%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    tabsetPanel(
      tabPanel("Housing stock EPC",
    fluidRow(
    column(10, h3("Data - Housing stock by EPC (SAP 2012 RdSAP v9.93)", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("StockEPCTable"))%>% withSpinner(color="#34d1a3"))),
    HTML("<blockquote><p>*based on SAP 2012 RdSAP v9.93&nbsp;</p></blockquote>"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Properties above EPC C",
             fluidRow(
               column(10, h3("Data - Properties above EPC band C", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EPCProportionsTable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Housing tenure EPC",
             fluidRow(
               column(10, h3("Data - Housing tenure by EPC", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EPCTenureTable"))%>% withSpinner(color="#34d1a3"))),
             HTML("<blockquote><p>*based on SAP 2012 RdSAP v9.93&nbsp;</p></blockquote>"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("MedianEER Rating",
             fluidRow(
               column(10, h3("Data - MedianEER Rating", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EERProportionsTable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("SGSHCS"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("SGSHCS"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("SGSHCS")
        
      )
    )
  )
}




###### Server ######
DomEPCs <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("DomEPCs.R")

  
  output$StockEPCSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12,  col_names = FALSE)[2:9]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data$Year <- as.numeric(Data$Year)
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$StockEPCPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12,  col_names = FALSE)[2:9]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data <- Data[complete.cases(Data),]
    
    Data$`C or Better` <- Data$A + Data$B + Data$C
    
    ChartColours <- c("#34d1a3", "#FF8500")
    
    BarColours <-
      c("#006837",
        "#1a9850",
        "#66bd63",
        "#fee08b",
        "#fdae61",
        "#f46d43",
        "#d73027")
    
    Data$Year <- paste0("<b>",Data$Year,"</b>")
    
    p <- plot_ly(data = Data, y = ~ `Year`) %>%
      
      add_trace(
        data = Data,
        x = ~ `A`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "A",
        text = paste0("A: ", percent(Data$`A`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `B`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "B",
        text = paste0("B: ", percent(Data$`B`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `C`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "C",
        text = paste0("C: ", percent(Data$`C`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `D`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "D",
        text = paste0("D: ", percent(Data$`D`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        x = ~ `E`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "E",
        text = paste0("E: ", percent(Data$`E`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = Data,
        x = ~ `F`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "F",
        text = paste0("F: ", percent(Data$`F`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = Data,
        x = ~ `G`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "G",
        text = paste0("G: ", percent(Data$`G`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[7]),
        legendgroup = 7
      ) %>%
      
      add_trace(
        data = Data,
        x = ~ 1.1 ,
        showlegend = TRUE,
        name = 'C or better',
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[2]),
        text =  paste0("<b>", ifelse(Data$`C or Better` > 0, percent(Data$`C or Better`, accuracy = 0.1), " "), "</b>"),
        legendgroup = 8
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$`Year`),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
    
  })
  
  output$StockEPCTable = renderDataTable({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12,  col_names = FALSE)[2:9]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data <- Data[complete.cases(Data),]
    
    Data$`C or Better` <- Data$A + Data$B + Data$C
    
    datatable(
      Data,
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
        title = "Distribution of Scottish housing Stock by EPC Band (SAP 2012 RdSAP v9.93)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Distribution of Scottish housing Stock by EPC Band (SAP 2012 RdSAP v9.93)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Distribution of Scottish housing Stock by EPC Band (SAP 2012 RdSAP v9.93)')
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
  
  output$EPCProportionsSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12,  col_names = FALSE)[12:13]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "SAP 2012")
    
    Data <- Data[which(Data$Year > 0),]
    
    Data <- Data[complete.cases(Data),]
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$EPCProportionsPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12,  col_names = FALSE)[12:13]
    
    
    
    names(Data) <- c("Year", "SAP 2012")
    
    Data <- Data[which(Data$Year > 0),]
    
    Data <- Data[complete.cases(Data),]
    
    Data[2] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    EPC <- Data
    
    ### variables
    ChartColours <- c("#34d1a3", "#8da0cb", "#fc8d62", "#34d1a3")
    
    EPC$Year <- paste0("01/01/", EPC$Year)
    
    EPC$Year <- dmy(EPC$Year)
    
    
    
    p <-  plot_ly(EPC,x = ~ Year ) %>% 
      add_trace(data = EPC,
                x = ~ Year,
                y = ~ `SAP 2012`,
                name = "SAP 2012 rdSAP v9.92",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "SAP 2012 rdSAP v9.92: ",
                  percent(EPC$`SAP 2012`, accuracy = 0.1),
                  "\nYear: ",
                  format(EPC$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EPC[which(EPC$`SAP 2012` > 0 | EPC$`SAP 2012` < 0),], 1),
        x = ~ Year,
        y = ~ `SAP 2012`,
        legendgroup = "1",
        name = "SAP 2012 rdSAP v9.92",
        text = paste0(
          "SAP 2012 rdSAP v9.92: ",
          percent(EPC[which(EPC$`SAP 2012` > 0 | EPC$`SAP 2012` < 0),][-1,]$`SAP 2012`, accuracy = 0.1),
          "\nYear: ",
          format(EPC[which(EPC$`SAP 2012` > 0 | EPC$`SAP 2012` < 0),][-1,]$Year, "%Y")
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
        bargap = 0.66,
        legend = list(font = list(color = "#34d1a3"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(EPC$Year)-100, max(EPC$Year)+100)),
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

  output$EPCProportionsTable = renderDataTable({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12,  col_names = FALSE)[c(12,15,13:14)]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year","SAP 2012 RdSAP v9.93",  "SAP 2012 RdSAP v9.92", "SAP 2009")
    
    Data <- Data[which(Data$Year > 0),]
    
    Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      Data,
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
        title = "Proportion of properties rated EPC band C or above, Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of properties rated EPC band C or above, Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of properties rated EPC band C or above, Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:4, 1)
  })
  
  output$EPCTenureSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12,  col_names = FALSE)[12:14]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "SAP 2012", "SAP 2009")
    
    Data <- Data[which(Data$Year > 0),]
    
    paste("Scotland,", max(Data$Year, na.rm = TRUE))
  })
  
  output$EPCTenurePlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12,  col_names = FALSE)[17:23]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[2:7] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[3:5,]
    
    Data[is.na(Data)] <- 0
    
    Data$`C or Better` <- Data$A + Data$B + Data$C
    
    Data$Total <- Data$`C or Better` + Data$D + Data$E + Data$`F & G`
    
    Data$Year <- paste0("<b>",Data$Year,"</b>")
    
    ChartColours <- c("#34d1a3", "#FF8500")
    
    BarColours <-
      c("#006837",
        "#1a9850",
        "#66bd63",
        "#fee08b",
        "#fdae61",
        "#f46d43",
        "#d73027")
    
    p <- plot_ly(data = Data, y = ~ `Year`) %>%
      
      add_trace(
        data = Data,
        x = ~ `A`/ Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "A",
        text = paste0("A: ", percent(Data$`A`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = Data,
        x = ~ `B` / Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "B",
        text = paste0("B: ", percent(Data$`B`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `C` / Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "C",
        text = paste0("C: ", percent(Data$`C`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        x = ~ `D` / Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "D",
        text = paste0("D: ", percent(Data$`D`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = Data,
        x = ~ `E` / Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "E",
        text = paste0("E: ", percent(Data$`E`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = Data,
        x = ~ `F & G` / Total,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "F & G",
        text = paste0("F & G: ", percent(Data$`F & G`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_trace(
        data = Data,
        x = ~ 1.1 ,
        showlegend = TRUE,
        name = 'C or better',
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[2]),
        text =  paste0("<b>", ifelse(Data$`C or Better` > 0, percent(Data$`C or Better`, accuracy = 0.1), " "), "</b>"),
        legendgroup = 8
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     ticktext = as.list(Data$`Year`),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
  })
  
  output$EPCTenureTable = renderDataTable({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12,  col_names = FALSE)[17:23]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Housing Tenure"
    
    Data[2:7] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[3:5,]
    
    Data[is.na(Data)] <- 0
    
    Data$`C or Better` <- Data$A + Data$B + Data$C
    
    Table <- Data
    
    datatable(
      Table,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Distribution of Scottish housing stock by EPC band and housing tenure (based on SAP 2012 RdSAP v9.93)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Distribution of Scottish housing stock by EPC band and housing tenure (based on SAP 2012 RdSAP v9.93)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Distribution of Scottish housing stock by EPC band and housing tenure (based on SAP 2012 RdSAP v9.93)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:8, 0)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Efficiency Measures/DomEPCs.txt")[2])
                     
                   )))
  })
 
  observeEvent(input$ToggleTable1, {
    toggle("StockEPCTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("EPCProportionsTable")
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("EPCTenureTable")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("EERProportionsTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$EPCProportions.png <- downloadHandler(
  filename = "EPCProportions.png",
  content = function(file) {
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 13, col_names = TRUE)[12:13]
    
    names(Data) <- c("Year", "SAP 2012")
    
    EPCProportion <- Data[which(Data$Year > 0),]
    
    EPCProportion <- EPCProportion[complete.cases(EPCProportion),]
    ### variables
    ChartColours <- c("#34d1a3", "#0868ac", "#4eb3d3", "#a8ddb5")
    sourcecaption = "Source: SG"
    plottitle = "Proportion of domestic properties rated\nEPC band C or above"
    
    #EPCProportion$`Low Carbon`Percentage <- PercentLabel(EPCProportion$`Low Carbon`)
    
    EPCProportion$`SAP 2012`
    
    EPCProportionChart <- EPCProportion %>%
      ggplot(aes(x = Year), family = "Century Gothic") +
      
      geom_line(
        aes(
          y = `SAP 2012`,
          colour = ChartColours[2],
          label = percent(`SAP 2012`)
        ),
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year,
          y = `SAP 2012`,
          label = ifelse(Year == min(Year[which(EPCProportion$`SAP 2012` > 0)]), percent(`SAP 2012`, accuracy = 0.1), ""),
          hjust = 0.5,
          vjust = -1,
          colour = ChartColours[2],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year,
          y = `SAP 2012`,
          label = ifelse(Year == max(Year), percent(`SAP 2012`, accuracy = 0.1), ""),
          hjust = .5,
          vjust = 2,
          colour = ChartColours[2],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(EPCProportion, 1),
        aes(
          x = Year,
          y = `SAP 2012`,
          colour = ChartColours[2],
          show_guide = FALSE
        ),
        size = 4,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = mean(Year[which(EPCProportion$`SAP 2012` > 0)]),
          y = mean(`SAP 2012`, na.rm = TRUE),
          label = "SAP 2012\nRdSAP v9.92",
          vjust = 3,
          colour = ChartColours[2],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = EPCProportion$Year,
        y = 0,
        label = ifelse(
          EPCProportion$Year == max(EPCProportion$Year) |
            EPCProportion$Year == min(EPCProportion$Year[which(EPCProportion$`SAP 2012` > 0)]) |
            EPCProportion$Year == min(EPCProportion$Year[which(EPCProportion$`SAP 2009` > 0)]),
          EPCProportion$Year,
          ""
        ),
        hjust = 0.5,
        vjust = 1.5,
        colour = ChartColours[1],
        fontface = 2,
        family = "Century Gothic"
      )
    
    
    EPCProportionChart <-
      LinePercentChart(EPCProportionChart,
                       EPCProportion,
                       plottitle,
                       sourcecaption,
                       ChartColours)
    
    EPCProportionChart
    
    ggsave(
      file,
      plot =  EPCProportionChart,
      width = 14,
      height = 14,
      units = "cm",
      dpi = 300
    )
    
    
  }
)
  
  output$StockEPC.png <- downloadHandler(
  filename = "StockEPC.png",
  content = function(file) {
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12, col_names = TRUE)[1:9]
    
    names(Data) <- c("Chart","Type", "A or better", "B", "C", "D", "E", "F", "G")
    
    Data <- Data[complete.cases(Data),]
    
    Data$Chart <- NULL
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Total <- Data$`A or better` + Data$B + Data$C
    HousingStockEPC <- as_tibble(Data)
    
    HousingStockEPC <-
      HousingStockEPC[c(1, ncol(HousingStockEPC):2)]
    
    HousingStockEPC <-
      arrange(HousingStockEPC,-row_number())
    
    HousingStockEPC$Type <-
      factor(HousingStockEPC$Type,
             levels = unique(HousingStockEPC$Type))
    
    HousingStockEPC <-
      melt(HousingStockEPC, id.vars = "Type")
    
    
    HousingStockEPC$variable <-
      factor(HousingStockEPC$variable,
             levels = unique(HousingStockEPC$variable))
    
    HousingStockEPC <- HousingStockEPC %>%
      group_by(Type) %>%
      mutate(pos = cumsum(value) - value / 2) %>%
      mutate(top = sum(value))
    
    plottitle <-
      "Distribution of housing stock by EPC band\n(based on SAP 2012 RdSAP v9.93)"
    sourcecaption <- "Source: SG"
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <-
      c("#006837",
        "#1a9850",
        "#66bd63",
        "#fee08b",
        "#fdae61",
        "#f46d43",
        "#d73027")
    
    
    HousingStockEPCChart <- HousingStockEPC %>%
      ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
      scale_fill_manual(
        "variable",
        values = c(
          "A or better" = BarColours[1],
          "B" = BarColours[2],
          "C" = BarColours[3],
          "D" = BarColours[4],
          "E" = BarColours[5],
          "F" = BarColours[6],
          "G" = BarColours[7],
          "Total" = "White"
        )
      ) +
      geom_bar(stat = "identity", width = .8) +
      annotate(
        "text",
        x = HousingStockEPC$Type,
        y = -.15,
        label = ifelse(
          HousingStockEPC$Type == "z",
          "",
          str_wrap(HousingStockEPC$Type, width = 8)
        ),
        family = "Century Gothic",
        fontface = 2,
        colour = ChartColours[1]
      ) +
      # geom_text(
      #   aes(x = 3.7,
      #       y = 0.5 * (1 / 7),
      #       label = "A"),
      #   fontface = 2,
      #   colour = BarColours[1],
      #   family = "Century Gothic",
      #   hjust = 0.5
      # ) +
      geom_text(
        aes(x = 2.8,
            y = .5 * (1 / 6),
            label = "B"),
        fontface = 2,
        colour = BarColours[2],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 2.8,
            y = 1.5 * (1 / 6),
            label = "C"),
        fontface = 2,
        colour = BarColours[3],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 2.8,
            y = 2.5 * (1 / 6),
            label = "D"),
        fontface = 2,
        colour = BarColours[4],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 2.8,
            y = 3.5 * (1 / 6),
            label = "E"),
        fontface = 2,
        colour = BarColours[5],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 2.8,
            y = 4.5 * (1 / 6),
            label = "F"),
        fontface = 2,
        colour = BarColours[6],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 2.8,
            y = 5.5 * (1 / 6),
            label = "G"),
        fontface = 2,
        colour = BarColours[7],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      annotate(
        "text",
        x = HousingStockEPC$Type,
        y = 1.1,
        label = ifelse(
          HousingStockEPC$Type == "z",
          "",
          percent(HousingStockEPC$value[which(HousingStockEPC$variable == "Total")], accuracy = 0.1)
        ),
        family = "Century Gothic",
        fontface = 2,
        colour = ChartColours[1]
      ) +
      geom_text(
        aes(x = 2.8,
            y = 1.1,
            label = "C\nor better"),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      )+
      geom_text(
        aes(x = 3.3,
            y = 1.05,
            label = " "),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      )
    
    HousingStockEPCChart
    
    
    HousingStockEPCChart <-
      StackedBars(
        HousingStockEPCChart,
        HousingStockEPC,
        plottitle,
        sourcecaption,
        ChartColours
      )
    
    HousingStockEPCChart <-
      HousingStockEPCChart +
      coord_flip() +
      labs(subtitle = paste0("Scotland, ", min(as.numeric(as.character(HousingStockEPC$Type))), " - ", max(as.numeric(as.character(HousingStockEPC$Type))))) +
      ylim(-.2, 1.13)
    
    HousingStockEPCChart
    
    ggsave(
      file,
      plot = HousingStockEPCChart,
      width = 14.5,
      height = 8,
      units = "cm",
      dpi = 300
    )
    
    
  }
)
  
  output$EPCTenure.png <- downloadHandler(
    filename = "EPCTenure.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Domestic EPCs", skip = 12)[17:23]
      Data <- Data[complete.cases(Data),]
      
      names(Data) <- c("Type", "A or better", "B", "C", "D", "E", "F & G")
      
      Data[2:7] %<>% lapply(function(x) as.numeric(as.character(x)))
      
      Data$Total <- Data$B + Data$C
      
      Data$`A or better` <- 0
      
      Data$BarTotal <- Data$`A or better`+Data$B + Data$C + Data$D + Data$E + Data$`F & G`
      
      Data$B <- Data$B / Data$BarTotal
      
      Data$C <- Data$C / Data$BarTotal
      
      Data$D <- Data$D / Data$BarTotal
      
      Data$E <- Data$E / Data$BarTotal
      
      Data$`F & G` <- Data$`F & G` / Data$BarTotal
      
      Data$BarTotal <- NULL
      
      DomesticEPC <- as_tibble(Data)
      
      DomesticEPC <-
        DomesticEPC[c(1, ncol(DomesticEPC):2)]
      
      DomesticEPC <-
        arrange(DomesticEPC,-row_number())
      
      DomesticEPC$Type <-
        factor(DomesticEPC$Type,
               levels = unique(rev(DomesticEPC$Type)))
      
      DomesticEPC <-
        melt(DomesticEPC, id.vars = "Type")
      
      
      DomesticEPC$variable <-
        factor(DomesticEPC$variable,
               levels = unique(DomesticEPC$variable))
      
      DomesticEPC <- DomesticEPC %>%
        group_by(Type) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Domestic EPC by housing tenure\n(based on SAP 2012 RdSAP v9.93)"
      sourcecaption <- "Source: SG"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <-
        c("#006837",
          "#1a9850",
          "#66bd63",
          "#fee08b",
          "#fdae61",
          "#f46d43",
          "#d73027")
      
      
      DomesticEPCChart <- DomesticEPC %>%
        ggplot(aes(x = Type, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "A or better" = BarColours[1],
            "B" = BarColours[2],
            "C" = BarColours[3],
            "D" = BarColours[4],
            "E" = BarColours[5],
            "F & G" = BarColours[6],
            "Total" = "White"
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = DomesticEPC$Type,
          y = -.15,
          label = ifelse(
            DomesticEPC$Type == "z",
            "",
            str_wrap(DomesticEPC$Type, width = 8)
          ),
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1]
        ) +
        # geom_text(
        #   aes(x = 3.7,
        #       y = 0.5 * (1 / 7),
        #       label = "A"),
        #   fontface = 2,
        #   colour = BarColours[1],
        #   family = "Century Gothic",
        #   hjust = 0.5
        # ) +
        geom_text(
          aes(x = 3.7,
              y = .5 * (1 / 5),
              label = "B"),
          fontface = 2,
          colour = BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 3.7,
              y = 1.5 * (1 / 5),
              label = "C"),
          fontface = 2,
          colour = BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 3.7,
              y = 2.5 * (1 / 5),
              label = "D"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 3.7,
              y = 3.5 * (1 / 5),
              label = "E"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 3.7,
              y = 4.5 * (1 / 5),
              label = "F & G"),
          fontface = 2,
          colour = BarColours[6],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        annotate(
          "text",
          x = DomesticEPC$Type,
          y = 1.1,
          label = ifelse(
            DomesticEPC$Type == "z",
            "",
            percent(DomesticEPC$value[which(DomesticEPC$variable == "Total")])
          ),
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1]
        ) +
        geom_text(
          aes(x = 3.7,
              y = 1.1,
              label = "C\nor better"),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5
        )+
        geom_text(
          aes(x = 4.1,
              y = 1.05,
              label = " "),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic",
          hjust = 0.5
        )
      
      DomesticEPCChart
      
      
      DomesticEPCChart <-
        StackedBars(
          DomesticEPCChart,
          DomesticEPC,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      DomesticEPCChart <-
        DomesticEPCChart +
        coord_flip() +
        labs(subtitle = "Scotland, 2019") +
        ylim(-.2, 1.13)
      
      DomesticEPCChart
                         
                         ggsave(
                           file,
                           plot = DomesticEPCChart,
                           width = 14.5,
                           height = 8,
                           units = "cm",
                           dpi = 300
                         )
      
      
    }
  )  
  
  output$EERProportionsSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Domestic EPCs", skip = 12,  col_names = FALSE)[12:14]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "SAP 2012", "SAP 2009")
    
    Data <- Data[which(Data$Year > 0),]
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$EERProportionsPlot <- renderPlotly  ({
    
    Data  <- read_excel("Structure/CurrentWorking.xlsx", 
                        sheet = "Domestic EPCs", skip = 34)
    
    
    
    names(Data) <- c("Year","SAP 2012 v2", "SAP 2012", "SAP 2009")
    
    Data <- Data[which(Data$Year > 0),]
    
    Data[2:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    EER <- Data
    
    ### variables
    ChartColours <- c("#34d1a3", "#0868ac", "#4eb3d3", "#a8ddb5")
    
    EER$Year <- paste0("01/01/", EER$Year)
    
    EER$Year <- dmy(EER$Year)
    
    
    
    p <-  plot_ly(EER,x = ~ Year ) %>% 
      add_trace(data = EER,
                x = ~ Year,
                y = ~ `SAP 2012`,
                name = "SAP 2012 rdSAP v9.92",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "SAP 2012 rdSAP v9.92: ",
                  EER$`SAP 2012`,
                  "\nYear: ",
                  format(EER$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EER[which(EER$`SAP 2012` > 0 | EER$`SAP 2012` < 0),], 1),
        x = ~ Year,
        y = ~ `SAP 2012`,
        legendgroup = "1",
        name = "SAP 2012 rdSAP v9.92",
        text = paste0(
          "SAP 2012 rdSAP v9.92: ",
          EER[which(EER$`SAP 2012` > 0 | EER$`SAP 2012` < 0),][-1,]$`SAP 2012`,
          "\nYear: ",
          format(EER[which(EER$`SAP 2012` > 0 | EER$`SAP 2012` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(data = EER,
                x = ~ Year,
                y = ~ `SAP 2012 v2`,
                name = "SAP 2012 rdSAP v9.93",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "SAP 2012 rdSAP v9.93: ",
                  EER$`SAP 2012 v2`,
                  "\nYear: ",
                  format(EER$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EER[which(EER$`SAP 2012 v2` > 0 | EER$`SAP 2012 v2` < 0),], 1),
        x = ~ Year,
        y = ~ `SAP 2012 v2`,
        legendgroup = "2",
        name = "SAP 2012 rdSAP v9.93",
        text = paste0(
          "SAP 2012 rdSAP v9.93: ",
          EER[which(EER$`SAP 2012 v2` > 0 | EER$`SAP 2012 v2` < 0),]$`SAP 2012 v2`,
          "\nYear: ",
          format(EER[which(EER$`SAP 2012 v2` > 0 | EER$`SAP 2012 v2` < 0),]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 12, 
                      color = ChartColours[3])
      ) %>% 
      add_trace(data = EER,
                x = ~ Year,
                y = ~ `SAP 2009`,
                name = "SAP 2009",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "SAP 2009: ",
                  EER$`SAP 2009`,
                  "\nYear: ",
                  format(EER$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "dash")
      ) %>% 
      add_trace(
        data = tail(EER[which(EER$`SAP 2009` > 0 | EER$`SAP 2009` < 0),], 1),
        x = ~ Year,
        y = ~ `SAP 2009`,
        legendgroup = "3",
        name = "SAP 2009",
        text = paste0(
          "SAP 2009: ",
          EER[which(EER$`SAP 2009` > 0 | EER$`SAP 2009` < 0),][-1,]$`SAP 2009`,
          "\nYear: ",
          format(EER[which(EER$`SAP 2009` > 0 | EER$`SAP 2009` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#34d1a3"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(EER$Year)-100, max(EER$Year)+100)),
        yaxis = list(
          title = "",
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
  
  output$EERProportionsTable = renderDataTable({
    
    
    Data  <- read_excel("Structure/CurrentWorking.xlsx", 
                        sheet = "Domestic EPCs", skip = 35)
    
    
    
    names(Data) <- c("Year","SAP 2012 RdSAP v9.93", "SAP 2012 RdSAP v9.92", "SAP 2009")
    
    Data <- Data[which(Data$Year > 0),]
    
    Data[2:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      Data,
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
        title = "MedianEER Rating",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'MedianEER Rating',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'MedianEER Rating')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:4, 0)
  })
  
  output$EERProportions.png <- downloadHandler(
    filename = "EERProportions.png",
    content = function(file) {
      
      
      Data  <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Domestic EPCs", skip = 35)
      
      
      
      names(Data) <- c("Year","SAP 2012 v2", "SAP 2012", "SAP 2009")
      
      EERProportion <- Data
      ### variables
      ChartColours <- c("#34d1a3", "#0868ac", "#4eb3d3", "#a8ddb5")
      sourcecaption = "Source: SG"
      plottitle = "MedianEER Rating"
      
      Length <- max(EERProportion$Year) - min(EERProportion$Year)
      Height <- max(EERProportion$`SAP 2009`)
      
      #EERProportion$`Low Carbon`Percentage <- PercentLabel(EERProportion$`Low Carbon`)
      
      EERProportion$`SAP 2012`
      
      EERProportionChart <- EERProportion %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        geom_line(
          aes(
            y = `SAP 2009`,
            colour = ChartColours[3],
            label = paste0(`SAP 2009` * 100, "%")
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `SAP 2009`,
            label = ifelse(Year == min(Year), `SAP 2009`, ""),
            hjust = 1,
            vjust = 0.5,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `SAP 2009`,
            label = ifelse(Year == max(Year), `SAP 2009`, ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EERProportion, 1),
          aes(
            x = Year,
            y = `SAP 2009`,
            colour = ChartColours[3],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        
        geom_text(
          aes(
            x = mean(Year[which(EERProportion$`SAP 2009` > 0)]),
            y = mean(`SAP 2009`, na.rm = TRUE),
            label = "SAP 2009",
            hjust = 0.5,
            vjust = -1.8,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `SAP 2012`,
            colour = ChartColours[2],
            label = percent(`SAP 2012`)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `SAP 2012`,
            label = ifelse(Year == min(Year[which(EERProportion$`SAP 2012` > 0)]),`SAP 2012`, ""),
            vjust = 2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `SAP 2012`,
            label = ifelse(Year == max(Year),`SAP 2012`, ""),
            hjust = -1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EERProportion, 1),
          aes(
            x = Year,
            y = `SAP 2012`,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        
        geom_text(
          aes(
            x = max(Year[which(EERProportion$`SAP 2012` > 0)])+Length*0.25,
            y = mean(`SAP 2012`, na.rm = TRUE),
            label = "SAP 2012\nRdSAP v9.92",
            
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `SAP 2012 v2`,
            colour = ChartColours[4],
            label = percent(`SAP 2012 v2`)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `SAP 2012 v2`,
            label = ifelse(Year == min(Year[which(EERProportion$`SAP 2012 v2` > 0)]),`SAP 2012 v2`, ""),
            vjust = 1.5,
            colour = ChartColours[4],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `SAP 2012 v2`,
            label = ifelse(Year == max(Year), `SAP 2012 v2`, ""),
            vjust = 2,
            colour = ChartColours[4],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EERProportion, 1),
          aes(
            x = Year,
            y = `SAP 2012 v2`,
            colour = ChartColours[4],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = max(Year[which(EERProportion$`SAP 2012 v2` > 0)]),
            y = mean(`SAP 2012 v2`, na.rm = TRUE),
            label = "SAP 2012\nRdSAP v9.93",
            hjust = 0.5,
            vjust = 2.5,
            colour = ChartColours[4],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = EERProportion$Year,
          y = 0,
          label = ifelse(
            EERProportion$Year == max(EERProportion$Year) |
              EERProportion$Year == min(EERProportion$Year[which(EERProportion$`SAP 2012` > 0)]) |
              EERProportion$Year == min(EERProportion$Year[which(EERProportion$`SAP 2009` > 0)]),
            EERProportion$Year,
            ""
          ),
          hjust = 0.5,
          vjust = 1.5,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(EERProportion$Year)+Length*0.35,
          y = Height*1.05,
          label = "",
          hjust = 0.5,
          vjust = 1.5,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = min(EERProportion$Year) - Length*0.02,
          y = Height*1.05,
          label = "",
          hjust = 0.5,
          vjust = 1.5,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        )
      
      
      EERProportionChart <-
        LinePercentChart(EERProportionChart,
                         EERProportion,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      EERProportionChart
      
      ggsave(
        file,
        plot =  EERProportionChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  
  
}
