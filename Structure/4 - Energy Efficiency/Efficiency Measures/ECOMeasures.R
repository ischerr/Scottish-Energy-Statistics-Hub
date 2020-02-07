require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ECOMeasuresOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("ECO measures delivered",
    fluidRow(column(8,
                    h3("Total number of ECO measures delivered", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('ECOMeasuresSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ECOMeasures.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("ECOMeasuresPlot")),
    plotlyOutput(ns("ECOMeasuresPlot"))%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Households in receipt of ECO measures",
             fluidRow(column(8,
                             h3("Households in receipt of ECO measures", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('ECOObligationSubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ECOObligation.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("ECOMeasuresPlot")),
             plotlyOutput(ns("ECOObligationPlot"))%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    tabsetPanel(
      tabPanel("ECO measures",
    fluidRow(
    column(10, h3("Data - ECO measures", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ECOMeasuresTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
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
ECOMeasures <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ECOMeasures.R")

  
  output$ECOMeasuresSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "ECO",
      col_names = FALSE,
      skip = 12,
      n_max = 7
    )
    
    Data <- as_tibble(t(Data))[1:3]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- head(Data, -1)
    
    Data <- Data[complete.cases(Data),]
    
    #Data$Year <- paste0("<b>",Data$Year,"</b>")
    
    Data$Year <- paste(substr(Data$Year, 1,3), substr(Data$Year,11,14))
    
    Data$Year <-  as.yearmon(Data$Year, format = "%b %Y")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$ECOMeasuresPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "ECO",
      col_names = FALSE,
      skip = 12,
      n_max = 7
    )
    
    Data <- as_tibble(t(Data))[1:3]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- head(Data, -1)
    
    Data <- Data[complete.cases(Data),]
    
    #Data$Year <- paste0("<b>",Data$Year,"</b>")
    
    Data$Year <- paste(substr(Data$Year, 1,3), substr(Data$Year,11,14))
    
    Data$Year <-  as.yearmon(Data$Year, format = "%b %Y")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    ChartColours <- c("#34d1a3", "#FF8500")
    
    p <-  plot_ly(Data,x = ~ Year ) %>% 
      add_trace(data = Data,
                x = ~ Year,
                y = ~ `Total number of ECO measures delivered`,
                name = "Total number of ECO measures delivered",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Total number of ECO measures delivered: ",
                  format(Data$`Total number of ECO measures delivered`, big.mark = ","),
                  "\nYear: ",
                  Data$Year
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(Data[which(Data$`Total number of ECO measures delivered` > 0 | Data$`Total number of ECO measures delivered` < 0),], 1),
        x = ~ Year,
        y = ~ `Total number of ECO measures delivered`,
        legendgroup = "1",
        name = "Total number of ECO measures delivered",
        text = paste0(
          "Total number of ECO measures delivered: ",
          format(Data[which(Data$`Total number of ECO measures delivered` > 0 | Data$`Total number of ECO measures delivered` < 0),][-1,]$`Total number of ECO measures delivered`, big.mark = ","),
          "\nYear: ",
          Data[which(Data$`Total number of ECO measures delivered` > 0 | Data$`Total number of ECO measures delivered` < 0),][-1,]$Year
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
                     showgrid = FALSE
        ),
        yaxis = list(
          title = "",
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
  
  output$ECOMeasuresTable = renderDataTable({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "ECO", skip = 12, n_max = 3,  col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Quarter"
    
    Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[complete.cases(Data),]
    
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
        title = "ECO Measures",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'ECO Measures',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'ECO Measures')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:3, 0)
  })
  
  output$ECOObligationSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "ECO",
      col_names = FALSE,
      skip = 12,
      n_max = 7
    )
    
    Data <- as_tibble(t(Data))[1:3]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- head(Data, -1)
    
    Data <- Data[complete.cases(Data),]
    
    #Data$Year <- paste0("<b>",Data$Year,"</b>")
    
    Data$Year <- paste(substr(Data$Year, 1,3), substr(Data$Year,11,14))
    
    Data$Year <-  as.yearmon(Data$Year, format = "%b %Y")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$ECOObligationPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "ECO",
      col_names = FALSE,
      skip = 12,
      n_max = 7
    )
    
    Data <- as_tibble(t(Data))[1:3]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- head(Data, -1)
    
    Data <- Data[complete.cases(Data),]
    
    #Data$Year <- paste0("<b>",Data$Year,"</b>")
    
    Data$Year <- paste(substr(Data$Year, 1,3), substr(Data$Year,11,14))
    
    Data$Year <-  as.yearmon(Data$Year, format = "%b %Y")
    
    Data$Year <- as.yearqtr(Data$Year)
    
    ChartColours <- c("#34d1a3", "#FF8500")
    
    p <-  plot_ly(Data,x = ~ Year ) %>% 
      add_trace(data = Data,
                x = ~ Year,
                y = ~ `Households in receipt of ECO measures`,
                name = "Households in receipt of ECO measures",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Households in receipt of ECO measures: ",
                  format(Data$`Households in receipt of ECO measures`, big.mark = ","), 
                  "\nYear: ",
                  Data$Year
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(Data[which(Data$`Households in receipt of ECO measures` > 0 | Data$`Households in receipt of ECO measures` < 0),], 1),
        x = ~ Year,
        y = ~ `Households in receipt of ECO measures`,
        legendgroup = "1",
        name = "Households in receipt of ECO measures",
        text = paste0(
          "Households in receipt of ECO measures: ",
          format(Data[which(Data$`Households in receipt of ECO measures` > 0 | Data$`Households in receipt of ECO measures` < 0),][-1,]$`Households in receipt of ECO measures`, big.mark = ","), 
          "\nYear: ",
          Data[which(Data$`Households in receipt of ECO measures` > 0 | Data$`Households in receipt of ECO measures` < 0),][-1,]$Year
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
                     showgrid = FALSE
        ),
        yaxis = list(
          title = "",
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

  output$ECOObligationTable = renderDataTable({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "ECO", skip = 12,  col_names = FALSE)[10:12]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "SAP 2012", "SAP 2009")
    
    Data <- Data[which(Data$Year > 0),]
    
    Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[-1,]
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
      formatPercentage(2:9, 1)
  })
  
  output$ECOGreenDealSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Energy consump sector",
      col_names = FALSE,
      skip = 12,
      n_max = 7
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:7] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$ECOGreenDealPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "ECO", skip = 12,  col_names = FALSE)[16:23]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[2:8] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[2:4,]
    
    Data[is.na(Data)] <- 0
    
    Data$`C or Better` <- Data$A + Data$B + Data$C
    
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
  
  output$ECOGreenDealTable = renderDataTable({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "ECO", skip = 12,  col_names = FALSE)[16:23]
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Housing Tenure"
    
    Data[2:8] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- Data[2:4,]
    
    Data[is.na(Data)] <- 0
    
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
        order = list(list(8, 'desc')),
        title = "Distribution of Scottish Housing Stock by EPC Band and Housing Tenure",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Distribution of Scottish Housing Stock by EPC Band and Housing Tenure',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Distribution of Scottish Housing Stock by EPC Band and Housing Tenure')
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
                     paste(readtext("Structure/4 - Energy Efficiency/Efficiency Measures/ECOMeasures.txt")[2])
                     
                   )))
  })
 
  observeEvent(input$ToggleTable, {
    toggle("ECOMeasuresTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ECOObligationTable")
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("ECOGreenDealTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$ECOObligation.png <- downloadHandler(
  filename = "ECOObligation.png",
  content = function(file) {
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "ECO", skip = 13, col_names = TRUE)[10:12]
    
    names(Data) <- c("Year", "SAP 2012", "SAP 2009")
    
    EPCProportion <- Data[which(Data$Year > 0),]
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
          label = "SAP 2012",
          hjust = .7,
          vjust = 3,
          colour = ChartColours[2],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
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
          label = ifelse(Year == min(Year), percent(`SAP 2009`, accuracy = 0.1), ""),
          hjust = 0.5,
          vjust = -1.6,
          colour = ChartColours[3],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year,
          y = `SAP 2009`,
          label = ifelse(Year == max(Year), percent(`SAP 2009`, accuracy = 0.1), ""),
          hjust = 0.5,
          vjust = 2,
          colour = ChartColours[3],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(EPCProportion, 1),
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
          x = mean(Year[which(EPCProportion$`SAP 2009` > 0)]),
          y = mean(`SAP 2009`, na.rm = TRUE),
          label = "SAP 2009",
          hjust = 0.5,
          vjust = -3.8,
          colour = ChartColours[3],
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
  
  output$ECOMeasures.png <- downloadHandler(
  filename = "ECOMeasures.png",
  content = function(file) {
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "ECO", skip = 12, col_names = TRUE)[1:8]
    
    names(Data) <- c("Type", "A or better", "B", "C", "D", "E", "F", "G")
    
    Data <- Data[complete.cases(Data),]
    
    Data$Total <- Data$`A or better` + Data$B + Data$C
    ECOMeasures <- Data
    
    ECOMeasures <-
      ECOMeasures[c(1, ncol(ECOMeasures):2)]
    
    ECOMeasures <-
      arrange(ECOMeasures,-row_number())
    
    ECOMeasures$Type <-
      factor(ECOMeasures$Type,
             levels = unique(ECOMeasures$Type))
    
    ECOMeasures <-
      melt(ECOMeasures, id.vars = "Type")
    
    
    ECOMeasures$variable <-
      factor(ECOMeasures$variable,
             levels = unique(ECOMeasures$variable))
    
    ECOMeasures <- ECOMeasures %>%
      group_by(Type) %>%
      mutate(pos = cumsum(value) - value / 2) %>%
      mutate(top = sum(value))
    
    plottitle <-
      "Distribution of housing stock by EPC band"
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
    
    
    ECOMeasuresChart <- ECOMeasures %>%
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
        x = ECOMeasures$Type,
        y = -.15,
        label = ifelse(
          ECOMeasures$Type == "z",
          "",
          str_wrap(ECOMeasures$Type, width = 8)
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
        aes(x = 4.7,
            y = .5 * (1 / 6),
            label = "B"),
        fontface = 2,
        colour = BarColours[2],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 4.7,
            y = 1.5 * (1 / 6),
            label = "C"),
        fontface = 2,
        colour = BarColours[3],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 4.7,
            y = 2.5 * (1 / 6),
            label = "D"),
        fontface = 2,
        colour = BarColours[4],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 4.7,
            y = 3.5 * (1 / 6),
            label = "E"),
        fontface = 2,
        colour = BarColours[5],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 4.7,
            y = 4.5 * (1 / 6),
            label = "F"),
        fontface = 2,
        colour = BarColours[6],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      geom_text(
        aes(x = 4.7,
            y = 5.5 * (1 / 6),
            label = "G"),
        fontface = 2,
        colour = BarColours[7],
        family = "Century Gothic",
        hjust = 0.5
      ) +
      annotate(
        "text",
        x = ECOMeasures$Type,
        y = 1.1,
        label = ifelse(
          ECOMeasures$Type == "z",
          "",
          percent(ECOMeasures$value[which(ECOMeasures$variable == "Total")], accuracy = 0.1)
        ),
        family = "Century Gothic",
        fontface = 2,
        colour = ChartColours[1]
      ) +
      geom_text(
        aes(x = 4.7,
            y = 1.1,
            label = "C\nor better"),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      )+
      geom_text(
        aes(x = 5.1,
            y = 1.05,
            label = " "),
        fontface = 2,
        colour = ChartColours[1],
        family = "Century Gothic",
        hjust = 0.5
      )
    
    ECOMeasuresChart
    
    
    ECOMeasuresChart <-
      StackedBars(
        ECOMeasuresChart,
        ECOMeasures,
        plottitle,
        sourcecaption,
        ChartColours
      )
    
    ECOMeasuresChart <-
      ECOMeasuresChart +
      coord_flip() +
      labs(subtitle = "Scotland, 2014 - 2017") +
      ylim(-.2, 1.13)
    
    ECOMeasuresChart
    
    ggsave(
      file,
      plot = ECOMeasuresChart,
      width = 14.5,
      height = 8,
      units = "cm",
      dpi = 300
    )
    
    
  }
)
  
  output$ECOGreenDeal.png <- downloadHandler(
    filename = "ECOGreenDeal.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "ECO", skip = 12)[16:23]
                         Data <- Data[complete.cases(Data),]
                         
                         names(Data) <- c("Type", "A or better", "B", "C", "D", "E", "F", "G")
                         
                         Data[2:8] %<>% lapply(function(x) as.numeric(as.character(x)))
                         
                         Data$Total <- Data$B + Data$C
                         
                         Data$`A or better` <- 0
                         
                         ECO <- as_tibble(Data)
                         
                         ECO <-
                           ECO[c(1, ncol(ECO):2)]
                         
                         ECO <-
                           arrange(ECO,-row_number())
                         
                         ECO$Type <-
                           factor(ECO$Type,
                                  levels = unique(ECO$Type))
                         
                         ECO <-
                           melt(ECO, id.vars = "Type")
                         
                         
                         ECO$variable <-
                           factor(ECO$variable,
                                  levels = unique(ECO$variable))
                         
                         ECO <- ECO %>%
                           group_by(Type) %>%
                           mutate(pos = cumsum(value) - value / 2) %>%
                           mutate(top = sum(value))
                         
                         plottitle <-
                           "Domestic EPC by housing tenure"
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
                         
                         
                         ECOChart <- ECO %>%
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
                             x = ECO$Type,
                             y = -.15,
                             label = ifelse(
                               ECO$Type == "z",
                               "",
                               str_wrap(ECO$Type, width = 8)
                             ),
                             family = "Century Gothic",
                             fontface = 2,
                             colour = ChartColours[1]
                           ) +
                           geom_text(
                             aes(x = 3.7,
                                 y = .5 * (1 / 6),
                                 label = "B"),
                             fontface = 2,
                             colour = BarColours[2],
                             family = "Century Gothic",
                             hjust = 0.5
                           ) +
                           geom_text(
                             aes(x = 3.7,
                                 y = 1.5 * (1 / 6),
                                 label = "C"),
                             fontface = 2,
                             colour = BarColours[3],
                             family = "Century Gothic",
                             hjust = 0.5
                           ) +
                           geom_text(
                             aes(x = 3.7,
                                 y = 2.5 * (1 / 6),
                                 label = "D"),
                             fontface = 2,
                             colour = BarColours[4],
                             family = "Century Gothic",
                             hjust = 0.5
                           ) +
                           geom_text(
                             aes(x = 3.7,
                                 y = 3.5 * (1 / 6),
                                 label = "E"),
                             fontface = 2,
                             colour = BarColours[5],
                             family = "Century Gothic",
                             hjust = 0.5
                           ) +
                           geom_text(
                             aes(x = 3.7,
                                 y = 4.5 * (1 / 6),
                                 label = "F"),
                             fontface = 2,
                             colour = BarColours[6],
                             family = "Century Gothic",
                             hjust = 0.5
                           ) +
                           geom_text(
                             aes(x = 3.7,
                                 y = 5.5 * (1 / 6),
                                 label = "G"),
                             fontface = 2,
                             colour = BarColours[7],
                             family = "Century Gothic",
                             hjust = 0.5
                           ) +
                           annotate(
                             "text",
                             x = ECO$Type,
                             y = 1.1,
                             label = ifelse(
                               ECO$Type == "z",
                               "",
                               percent(ECO$value[which(ECO$variable == "Total")])
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
                         
                         ECOChart
                         
                         
                         ECOChart <-
                           StackedBars(
                             ECOChart,
                             ECO,
                             plottitle,
                             sourcecaption,
                             ChartColours
                           )
                         
                         ECOChart <-
                           ECOChart +
                           coord_flip() +
                           labs(subtitle = "Scotland, 2017") +
                           ylim(-.2, 1.13)
                         
                         ECOChart
                         
                         ggsave(
                           file,
                           plot = ECOChart,
                           width = 14.5,
                           height = 8,
                           units = "cm",
                           dpi = 300
                         )
      
      
    }
  )  
  
}
