require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



GasConsumptionOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Trend",
    fluidRow(column(8,
                    h3("Total gas consumption by sector", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('GasConsumptionSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasConsumption.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("GasConsumptionPlot")),
    plotlyOutput(ns("GasConsumptionPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Households",
             fluidRow(column(8,
                             h3("Average domestic gas consumption", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('GasConsumptionHHoldSubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasConsumptionHHold.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("GasConsumptionHHoldPlot")),
             plotlyOutput(ns("GasConsumptionHHoldPlot"), height =  "900px")%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")
    ),
    tabPanel("LA",
             fluidRow(column(8,
                             h3("Average annual household consumption of gas by local authority", style = "color: #34d1a3;  font-weight:bold"),
                             h4(textOutput(ns('GasConsumptionLASubtitle')), style = "color: #34d1a3;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasConsumptionLA.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
             #dygraphOutput(ns("GasConsumptionLAPlot")),
             leafletOutput(ns("GasConsumptionMap"), height = "800px")%>% withSpinner(color="#34d1a3"),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")
             )),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    tabsetPanel(
      tabPanel("Trend",
    fluidRow(
    column(10, h3("Data - Total gas consumption by sector (GWh)", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("GasConsumptionTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Households",
             fluidRow(
               column(10, h3("Data - Average domestic gas consumption per consumer (kWh)", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("GasConsumptionHHoldTable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")        
    ),
    tabPanel("Local Authority",
             fluidRow(
               column(10, h3("Data - Average annual household consumption of gas by local authority, 2019", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("GasConsumptionLATable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")       
    )),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("SGGrowth"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("SGGrowth"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("BEISGasCustomers"),
        SourceLookup("BEISUKConsump"),
        SourceLookup("BEISSubNatGas")
        
      )
    )
  )
}




###### Server ######
GasConsumption <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("GasConsumption.R")

  
  output$GasConsumptionSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump",
      col_names = FALSE,
      skip = 16
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$GasConsumptionPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump",
      col_names = FALSE,
      skip = 16
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- as.character(Data$Year)
    
    Data[2,1] <- "Baseline\n2005/2007"
    
    Data[3,1] <- ""
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
    
    Data = subset(Data, !(Data$Year %in% c(2005, 2006, 2007)))
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    Data <- Data[-1,]
    
    Data$RowNumber <- as.numeric(rownames(Data))
    
    Data[is.na(Data)] <- 0
    
    DataTail <- tail(Data,1)
    
    DataLatest <- Data[nrow(Data)-1,]
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Domestic`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Domestic",
        text = paste0("Domestic: ", format(round(Data$`Domestic`, digits = 0), big.mark = ","), " GWh"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        x = ~ `Non-domestic`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Non-domestic",
        text = paste0("Non-domestic: ", format(round(Data$`Non-domestic`, digits = 0), big.mark = ","), " GWh"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = Data,
        y = ~ Year,
        x = ~ (Data$`Domestic` + Data$`Non-domestic`) + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Domestic` >0, paste("<b>",format(round((Data$`Domestic` + Data$`Non-domestic`), digits = 0), big.mark = ","),"GWh</b>")," "),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      )  %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Domestic`)/2,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[1]),
        text = paste0("<b>", percent(DataTail$`Domestic`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x =  mean(DataLatest$`Domestic`) + (mean(DataLatest$`Non-domestic`)/2),
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = BarColours[2]),
        text =  paste0("<b>", percent(DataTail$`Non-domestic`, accuracy = 0.1), "</b>")
      ) %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Total`)+ 3700,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = ChartColours[1]),
        text =  paste0("<b>", percent(DataTail$Total, accuracy = 0.1), "</b>")
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
                     ticktext = as.list(Data$Year),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          range = c(0,72000)
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  
  output$GasConsumptionTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump",
      col_names = FALSE,
      skip = 16
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data[1:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- as.character(Data$Year)
    
    Data[2,1] <- " Baseline\n2005/2007"
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
  
    Data <- Data[-1,]
    
    Data <- head(Data, -1)
    
    Data = subset(Data, !(Data$Year %in% c(2005, 2006, 2007)))
    
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
        title = "Total gas consumption by sector (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Total gas consumption by sector (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Total gas consumption by sector (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:6, 0)%>% 
      formatStyle(c(4:5), fontStyle = 'italic') %>% 
      formatStyle(c(6), fontWeight = 'bold')
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Demand Reduction/GasConsumption.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("GasConsumptionTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$GasConsumption.png <- downloadHandler(
    filename = "GasConsumption.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Gas consump", skip = 17, col_names = FALSE)[c(1,3,2,6)]
      
      Data[1,1] <- "2006"
      
      names(Data) <- c("Year", "Non-domestic", "Domestic", "Total")
      
      Data <- Data[complete.cases(Data),]
      
      Data[nrow(Data),1] <- as.character(max(as.numeric(Data$Year),na.rm = TRUE)+1)
      
      Data$Year <- as.numeric(Data$Year)
      
      Data <- Data[-c(2,3,4),]
      
      GasConsumptiontion <- Data
      
      GasConsumptiontion <- GasConsumptiontion[order(-GasConsumptiontion$Year),]
      
      GasConsumptiontion <- melt(GasConsumptiontion, id.vars = "Year")
      
      GasConsumptiontionMax <-
        subset(GasConsumptiontion, Year == max(GasConsumptiontion$Year))
      
      GasConsumptiontion <-
        subset(GasConsumptiontion,
               Year < max(GasConsumptiontion$Year) & variable != "Total")
      
      GasConsumptiontion$variable <-
        factor(GasConsumptiontion$variable, levels = unique(GasConsumptiontion$variable))
      
      GasConsumptiontion <- GasConsumptiontion %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Total gas consumption by sector"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
      
      
      GasConsumptiontionChart <- GasConsumptiontion %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Domestic" = BarColours[1],
                                     "Non-domestic" = BarColours[2])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = GasConsumptiontion$top,
          label = ifelse(
            GasConsumptiontion$variable == "Domestic",
            paste0(format(
              round(GasConsumptiontion$top, digits = 0), big.mark = ","
            ), " GWh"),
            ""
          ),
          hjust = -0.1,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = -6000,
          label =   ifelse(
            GasConsumptiontion$variable == "Domestic",
            ifelse(
              GasConsumptiontion$Year == 2006,
              "2005/2007\n(baseline)",
              GasConsumptiontion$Year
            ),
            ""
          ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        geom_text(
          y = GasConsumptiontion$top - GasConsumptiontion$pos,
          label =   ifelse(
            GasConsumptiontion$pos > 0,
            ifelse(
              GasConsumptiontion$Year == 2006 |
                GasConsumptiontion$Year ==  max(GasConsumptiontion$Year),
              paste0(format(
                round(GasConsumptiontion$value, digits = 0), big.mark = ","
              ), " GWh"),
              ""
            ),
            ""
          ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = "white"
        ) +
        annotate(
          "text",
          x = 2005,
          y = 17357,
          label = "Domestic",
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2005,
          y = 47247,
          label = "Non-domestic",
          fontface = 2,
          color = BarColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(GasConsumptiontion$Year) + 1.2,
          y = as.numeric(subset(
            GasConsumptiontion,
            Year == max(GasConsumptiontion$Year) &
              variable == "Domestic"
          )[1, 5]) - as.numeric(subset(
            GasConsumptiontion,
            Year == max(GasConsumptiontion$Year) & variable == "Domestic"
          )[1, 4]),
          label = percent((
            subset(GasConsumptiontionMax, variable == "Domestic")[1, 3]
          ), 0.1),
          fontface = 2,
          color = BarColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(GasConsumptiontion$Year) + 1.2,
          y = as.numeric(
            subset(
              GasConsumptiontion,
              Year == max(GasConsumptiontion$Year) &
                variable == "Non-domestic"
            )[1, 5]
          ) - as.numeric(
            subset(
              GasConsumptiontion,
              Year == max(GasConsumptiontion$Year) &
                variable == "Non-domestic"
            )[1, 4]
          ),
          label = percent((
            subset(GasConsumptiontionMax, variable == "Non-domestic")[1, 3]
          ), 0.1),
          fontface = 2,
          color = BarColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(GasConsumptiontion$Year) + 1.2,
          y = as.numeric(subset(
            GasConsumptiontion,
            Year == max(GasConsumptiontion$Year) & variable == "Domestic"
          )[1, 5]),
          label = percent((
            subset(GasConsumptiontionMax, variable == "Total")[1, 3]
          ), 0.1),
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic",
          hjust = -.75
        ) + annotate(
          "text",
          x = max(GasConsumptiontion$Year) + 1.2,
          y = -6000,
          label = "% Change\nfrom baseline",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      
      GasConsumptiontionChart
      
      
      GasConsumptiontionChart <-
        BaselineChart(GasConsumptiontionChart,
                      GasConsumptiontion,
                      plottitle,
                      sourcecaption,
                      ChartColours)
      
      GasConsumptiontionChart <-
        GasConsumptiontionChart +
        coord_flip() +
        labs(subtitle = paste("Scotland, 2005 -", max(GasConsumptiontion$Year))) +
        ylim(-10000, max(GasConsumptiontion$top) + 10500) +
        xlim(max(GasConsumptiontion$Year) + 1.2, 2005)
      
      GasConsumptiontionChart
      
      ggsave(
        file,
        plot = GasConsumptiontionChart,
        width = 20,
        height = 17.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$GasConsumptionHHoldSubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump by household",
      col_names = FALSE,
      skip = 12
    )

        names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(Data$Year, na.rm = TRUE),"-", max(Data$Year, na.rm = TRUE))
  })
  
  output$GasConsumptionHHoldPlot <- renderPlotly  ({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump by household",
      col_names = FALSE,
      skip = 13
    )
    
    names(Data) <- c("Year", "Consumption")
    
    Data[1:2] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- as.character(Data$Year)
    
    Data[1,1] <- "Baseline\n2005/2007"
    
    Data[2,1] <- " "
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
    
    Data = subset(Data, !(Data$Year %in% c(2005, 2006, 2007)))
    
    Data$Year <- paste("<b>", Data$Year, "</b>")
    
    Data$RowNumber <- as.numeric(rownames(Data))
    
    Data[is.na(Data)] <- 0
    
    DataTail <- tail(Data,1)
    
    DataLatest <- Data[nrow(Data)-1,]
    
    ChartColours <- c("#34d1a3", "#FF8500")
    BarColours <- c("#00441b", "#238b45", "#66c2a4", "#ef3b2c")
    
    p <- plot_ly(data = Data, y = ~ Year) %>%
      
      add_trace(
        data = Data,
        x = ~ `Consumption`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Consumption",
        text = paste0("Consumption: ", format(round(Data$`Consumption`, digits = 0), big.mark = ","), " kWh"),
        hoverinfo = 'text',
        marker = list(color = ChartColours[1]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = Data,
        y = ~ Year,
        x = ~ Data$`Consumption` + 0.1,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = ifelse(Data$`Consumption` >0, paste("<b>",format(round((Data$`Consumption`), digits = 0), big.mark = ","),"kWh</b>")," "),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      )  %>% 
      add_trace(
        data = tail(Data,1),
        y = ~Year,
        x = mean(DataLatest$`Consumption`)/2,
        showlegend = FALSE,
        mode = 'text',
        type = 'scatter',
        hoverinfo = 'skip',
        textfont = list(color = ChartColours[1]),
        text = paste0("<b>", percent(DataTail$`Consumption`, accuracy = 0.1), "</b>")
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
                     ticktext = as.list(Data$Year),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        ),
        xaxis = list(
          title = "",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          range = c(0,24000)
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
    
    
    
  })
  
  output$GasConsumptionHHoldTable = renderDataTable({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump by household",
      col_names = FALSE,
      skip = 13
    )
    
    names(Data) <- c("Year", "Consumption")
    
    Data[1:2] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- as.character(Data$Year)
    
    Data[1,1] <- "Baseline\n2005/2007"
    
    
    Data[nrow(Data),1] <- "% Change\nfrom baseline"
    
    Data<- Data[seq(dim(Data)[1],1),]
    
    Data <- Data[-1,]
    
    Data = subset(Data, !(Data$Year %in% c(2005, 2006, 2007)))
    
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
        title = "Average domestic gas consumption per consumer (kWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average domestic gas consumption per consumer (kWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average domestic gas consumption per consumer (kWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2, 0)%>% 
      formatStyle(c(2), fontWeight = 'bold')
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("GasConsumptionHHoldTable")
  })
  
  output$GasConsumptionHHold.png <- downloadHandler(
    filename = "GasConsumptionHHold.png",
    content = function(file) {
      
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Gas consump by household", skip = 13, col_names = FALSE)
      
      Data[1,1] <- "2006"
      
      names(Data) <- c("Year", "Consumption")
      
      Data <- Data[complete.cases(Data),]
      
      Data[nrow(Data),1] <- as.character(max(as.numeric(Data$Year),na.rm = TRUE)+1)
      
      Data$Total <- Data$Consumption
      
      Data$Year <- as.numeric(Data$Year)
      
      Data <- Data[-c(2,3,4),]
      
      GasConsumptionHHold <- Data
      
      
      GasConsumptionHHold <-
        GasConsumptionHHold[order(-GasConsumptionHHold$Year),]
      
      GasConsumptionHHold <-
        melt(GasConsumptionHHold, id.vars = "Year")
      
      FinalConsumptionMax <-
        subset(GasConsumptionHHold,
               Year == max(GasConsumptionHHold$Year))
      
      GasConsumptionHHold <-
        subset(
          GasConsumptionHHold,
          Year < max(GasConsumptionHHold$Year) & variable != "Total"
        )
      
      GasConsumptionHHold$variable <-
        factor(GasConsumptionHHold$variable,
               levels = unique(GasConsumptionHHold$variable))
      
      GasConsumptionHHold <- GasConsumptionHHold %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Average domestic gas consumption"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#34d1a3", "#FF8500")
      BarColours <- c("#00441b", "#238b45","#41ae76", "#66c2a4","#66c2a4", "#99d8c9", "ffffff")
      
      
      GasConsumptionHHoldChart <- GasConsumptionHHold %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Consumption" = ChartColours[1],
            "Gas" = BarColours[2],
            "Gastricity" = BarColours[3],
            "Bioenergy & wastes" = BarColours[4],
            "Coal" = BarColours[5],
            "Manufactured fuels" = BarColours[6]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = GasConsumptionHHold$top/2,
          label = ifelse(
            GasConsumptionHHold$value > 7000,
            paste0(format(
              round(GasConsumptionHHold$top, digits = 0), big.mark = ","
            ), " kWh"),
            ""
          ),
          family = "Century Gothic",
          fontface = 2,
          color = "white"
        ) +
        geom_text(
          y = -1750,
          label =   ifelse(
            GasConsumptionHHold$value > 7000,
            ifelse(
              GasConsumptionHHold$Year == 2006,
              "2005/2007\n(baseline)",
              GasConsumptionHHold$Year
            ),
            ""
          ),
          hjust = .5,
          family = "Century Gothic",
          fontface = 2,
          color = ChartColours[1]
        ) +
        annotate(
          "text",
          x = max(GasConsumptionHHold$Year) + 1.2,
          y = as.numeric(
            subset(
              GasConsumptionHHold,
              Year == max(GasConsumptionHHold$Year) &
                variable == "Consumption"
            )[1, 5]
          ) - as.numeric(
            subset(
              GasConsumptionHHold,
              Year == max(GasConsumptionHHold$Year) &
                variable == "Consumption"
            )[1, 4]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Consumption")[1, 3]
          ), accuracy = .1),
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = max(GasConsumptionHHold$Year) + 1.2,
          y = as.numeric(
            subset(
              GasConsumptionHHold,
              Year == max(GasConsumptionHHold$Year) &
                variable == "Gas"
            )[1, 5]
          ),
          label = percent((
            subset(FinalConsumptionMax, variable == "Total")[1, 3]
          ), accuracy  = .1),
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic",
          hjust = -.75
        ) + annotate(
          "text",
          x = max(GasConsumptionHHold$Year) + 1.2,
          y = -1750,
          label = "% Change\nfrom baseline",
          fontface = 2,
          color = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      
      GasConsumptionHHoldChart
      
      
      GasConsumptionHHoldChart <-
        BaselineChart(
          GasConsumptionHHoldChart,
          GasConsumptionHHold,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      GasConsumptionHHoldChart <-
        GasConsumptionHHoldChart +
        coord_flip() +
        labs(subtitle = paste("Scotland, 2005 -", max(GasConsumptionHHold$Year))) +
        ylim(-3100, max(GasConsumptionHHold$top)) +
        xlim(max(GasConsumptionHHold$Year) + 1.2, 2005)
      
      GasConsumptionHHoldChart
      
      ggsave(
        file,
        plot = GasConsumptionHHoldChart,
        width = 20,
        height = 17.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  
  output$GasConsumptionLASubtitle <- renderText({
    
    Data <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump by household",
      col_names = FALSE,
      skip = 12
    )
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year"
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", max(Data$Year, na.rm = TRUE))
  })
  
  output$GasConsumptionLAPlot <- renderImage({
    
    outfile <- tempfile(fileext='.png')
    
    writePNG(readPNG("Structure/4 - Energy Efficiency/Demand Reduction/GasConsumptionLAOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  output$GasConsumptionLATable = renderDataTable({
    

    GasConsumption <- read_csv("Processed Data/Output/Consumption/GasConsumption.csv")
    
    
    GasConsumption <- GasConsumption[which(GasConsumption$Year == max(GasConsumption$Year)),]
    
    GasConsumption <-  GasConsumption[c(3,2,11,10)]
    

    
    names(GasConsumption) <- c("Geography Code","Local Authority", "Average household consumption (kWh)", "Total Consumption (GWh)")
    
    GasConsumption <- GasConsumption[which(substr(GasConsumption$`Geography Code`,1,1) == "S"),]
    
    GasConsumption <- GasConsumption[complete.cases(GasConsumption),]
    
    
    LALookup <- read_excel("Structure/LALookup.xlsx", 
                           sheet = "Code to LA")
    
    names(LALookup) <- c("Geography Code","Local Authority")
    
    GasConsumption <- merge(GasConsumption, LALookup, all = TRUE)
    
    datatable(
      GasConsumption,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Average annual household consumption of gas by local authority, 2019",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average annual household consumption of gas by local authority, 2019',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average annual household consumption of gas by local authority, 2019')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(3:4, 0)
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("GasConsumptionLATable")
  })
  
  output$GasConsumptionLA.png <- downloadHandler(
    filename = "GasConsumptionLA.png",
    content = function(file) {
      writePNG(readPNG("Structure/4 - Energy Efficiency/Demand Reduction/GasConsumptionLAChart.png"), file) 
    }
  )
  
  
  output$GasConsumptionMap <- renderLeaflet({
    
    ### Load Packages
    library(readr)
    library("maptools")
    library(tmaptools)
    library(tmap)
    library("sf")
    library("leaflet")
    library("rgeos")
    library(readxl)
    library(ggplot2)
    library(spatialEco)
    
    print("MapsandGasGrid")
    
    # This is unlikely to change from 2012
    yearstart <- 2012
    
    ### Set the final year for the loop as the current year ###
    yearend <- format(Sys.Date(), "%Y")
    
    
    ### Add Simplified shape back to the Shapefile
    LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")
    
    LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
    ############ RENEWABLE GasTRICITY ################################################
    
    GasConsumptionLAMap <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Gas consump hhold LA",
      skip = 12,
      n_max = 34
    )[1:3]
    
    
    names(GasConsumptionLAMap) <- c("CODE", "LocalAuthority", "Total")
    
    GasConsumptionLAMapExtra <- data.frame(
      CODE = c ("S12000027", "S12000023"	), 
      LocalAuthority = c("Shetland Islands", "Orkney Islands"),
      Total = c(9000,9000)
    )
    
    
    GasConsumptionLAMap <- rbind(GasConsumptionLAMap, GasConsumptionLAMapExtra)
    GasConsumptionLAMap[order(substr(GasConsumptionLAMap$`CODE`,1,3), GasConsumptionLAMap$`LocalAuthority`),]
    
    
    GasConsumptionLAMap <- GasConsumptionLAMap[c(1,2,ncol(GasConsumptionLAMap))]
    
    GasConsumptionLAMap <- GasConsumptionLAMap[which(substr(GasConsumptionLAMap$CODE, 1,3)== "S12"),]
    
    GasConsumptionLAMap[is.na(GasConsumptionLAMap)] <- 10000
    
    GasConsumptionLAMap$Content <- paste0("<b>",GasConsumptionLAMap$LocalAuthority, "</b><br/>Average annual household gas consumption:<br/><em>", ifelse(GasConsumptionLAMap$Total > 10001,paste0(format(round(GasConsumptionLAMap$Total,0 ),  big.mark = ",")," kWh</em>"),"Not connected to gas grid" ))
    
    GasConsumptionLAMap$Hover <- paste0(GasConsumptionLAMap$LocalAuthority, " - ", ifelse(GasConsumptionLAMap$Total > 10001,paste0(format(round(GasConsumptionLAMap$Total,0 ),  big.mark = ",")," kWh"),"N/A" ))
    
    
    
    
    ### Change LA$CODE to string
    LA$CODE <- as.character(LA$CODE)
    
    ### Order LAs in Shapefile
    LA <- LA[order(LA$CODE),]
    
    ### Order LAs in Data
    GasConsumptionLAMap <- GasConsumptionLAMap[order(GasConsumptionLAMap$CODE),]
    
    ### Combine Data with Map data
    LAMap <-
      merge(LA, GasConsumptionLAMap)
    
    pal <- colorNumeric(
      palette = "Greens",
      domain = LAMap$Total)
    
    palWithoutNA <- colorNumeric(
      palette = "Greens",
      domain = LAMap$Total,
      na.color=rgb(0,0,0,0))
    
    leaflet(LAMap) %>% 
      addProviderTiles("Esri.WorldGrayCanvas" ) %>% 
      addPolygons(stroke = TRUE, 
                  weight = 0.1,
                  smoothFactor = 0.2,
                  popup = ~Content,
                  label = ~Hover,
                  fillOpacity = 1,
                  color = ~pal(Total),
                  highlightOptions = list(color = "white", weight = 2,
                                          bringToFront = TRUE)) %>%
      leaflet::addLegend("bottomright", pal = palWithoutNA, values = ~Total,
                         title = "Gas consumption",
                         labFormat = labelFormat(suffix = " kWh"),
                         opacity = 1
      ) %>% 
      htmlwidgets::prependContent(html_fix) 
    
  }) 
}
