require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



DualFuelBreakdownOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Dual Fuel breakdown",
    fluidRow(column(8,
                    h3("Breakdown of a dual fuel bill", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('DualFuelBreakdownSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('DualFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("DualFuelBreakdownPlot")),
    plotlyOutput(ns("DualFuelBreakdownPlot"))%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Dual Fuel Time Series - North Scotland",
             fluidRow(column(8,
                             h3("Breakdown of a dual fuel bill", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('NorthDualFuelBreakdownSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('NorthDualFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("DualFuelBreakdownPlot")),
             plotlyOutput(ns("NorthDualFuelBreakdownPlot"), height = "600px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Dual Fuel Time Series - South Scotland",
             fluidRow(column(8,
                             h3("Breakdown of a dual fuel bill", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('SouthDualFuelBreakdownSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('SouthDualFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("DualFuelBreakdownPlot")),
             plotlyOutput(ns("SouthDualFuelBreakdownPlot"), height = "600px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Electricity bill breakdown",
             fluidRow(column(8,
                             h3("Breakdown of an electricity bill", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('ElecFuelBreakdownSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("ElecFuelBreakdownPlot")),
             plotlyOutput(ns("ElecFuelBreakdownPlot"))%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Gas bill breakdown",
             fluidRow(column(8,
                             h3("Breakdown of a gas bill", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('GasFuelBreakdownSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("GasFuelBreakdownPlot")),
             plotlyOutput(ns("GasFuelBreakdownPlot"))%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    tabsetPanel(
      tabPanel("North Scotland",
    fluidRow(
    column(10, h3("Data - North Scotland", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("NorthDualFuelBreakdownTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("South Scotland",
             fluidRow(
               column(10, h3("Data - South Scotland", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("SouthDualFuelBreakdownTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("North Scotland",
             fluidRow(
               column(10, h3("Data - North Scotland", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("NorthElecFuelBreakdownTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("South Scotland",
             fluidRow(
               column(10, h3("Data - South Scotland", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("SouthElecFuelBreakdownTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("North Scotland",
             fluidRow(
               column(10, h3("Data - North Scotland", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("NorthGasFuelBreakdownTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("South Scotland",
             fluidRow(
               column(10, h3("Data - South Scotland", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("SouthGasFuelBreakdownTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("OFGEMTariff"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("OFGEMTariff")
        
      )
    )
  )
}




###### Server ######
DualFuelBreakdown <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("DualFuelBreakdown.R")

  
  output$DualFuelBreakdownSubtitle <- renderText({
    
    NorthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandDualFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    NorthScotlandDualFuelBreakdown <- head(NorthScotlandDualFuelBreakdown,1)
    
    paste("Scotland,", NorthScotlandDualFuelBreakdown$Dates)
  })
  
  output$DualFuelBreakdownPlot <- renderPlotly  ({
    
    NorthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandDualFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    NorthScotlandDualFuelBreakdown <- head(NorthScotlandDualFuelBreakdown,1)
    
    NorthScotlandDualFuelBreakdown$Region <- "North Scotland"
    
    
    SouthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandDualFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    SouthScotlandDualFuelBreakdown <- head(SouthScotlandDualFuelBreakdown,1)
    
    SouthScotlandDualFuelBreakdown$Region <- "South Scotland"
    
    AllScotlandDualFuelBreakdown <- rbind(NorthScotlandDualFuelBreakdown, SouthScotlandDualFuelBreakdown)
    
    
    AllScotlandDualFuelBreakdown$Region <- paste("<b>", AllScotlandDualFuelBreakdown$Region, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
    
    p <- plot_ly(data = AllScotlandDualFuelBreakdown, y = ~ Region) %>%
      
      add_trace(
        data = AllScotlandDualFuelBreakdown,
        x = ~ `Wholesale`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Wholesale costs",
        text = paste0("Wholesale costs: ", percent(AllScotlandDualFuelBreakdown$`Wholesale`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = AllScotlandDualFuelBreakdown,
        x = ~ `Network costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Network costs",
        text = paste0("Network costs: ", percent(AllScotlandDualFuelBreakdown$`Network costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = AllScotlandDualFuelBreakdown,
        x = ~ `Policy costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Policy costs",
        text = paste0("Policy costs: ", percent(AllScotlandDualFuelBreakdown$`Policy costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = AllScotlandDualFuelBreakdown,
        x = ~ `Operating costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Operating costs",
        text = paste0("Operating costs: ", percent(AllScotlandDualFuelBreakdown$`Operating costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = AllScotlandDualFuelBreakdown,
        x = ~ `EBIT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "EBIT",
        text = paste0("EBIT: ", percent(AllScotlandDualFuelBreakdown$`EBIT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = AllScotlandDualFuelBreakdown,
        x = ~ `Adjustment allowance`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Adjustment allowance",
        text = paste0("Adjustment allowance: ", percent(AllScotlandDualFuelBreakdown$`Adjustment allowance`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
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
                     ticktext = as.list(AllScotlandDualFuelBreakdown$`Region`),
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
  
  output$NorthDualFuelBreakdownSubtitle <- renderText({
    
    NorthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandDualFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    paste("Scotland,",
          str_sub(tail(NorthScotlandDualFuelBreakdown,1)$Dates,-4,-1),
          "-",
          str_sub(head(NorthScotlandDualFuelBreakdown,1)$Dates,-4,-1))
  })
  
  output$NorthDualFuelBreakdownPlot <- renderPlotly  ({
    
    
    NorthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandDualFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    NorthScotlandDualFuelBreakdown$Dates <- paste("<b>", NorthScotlandDualFuelBreakdown$Dates, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
    
    p <- plot_ly(data = NorthScotlandDualFuelBreakdown, y = ~ Dates) %>%
      
      add_trace(
        data = NorthScotlandDualFuelBreakdown,
        x = ~ `Wholesale`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Wholesale costs",
        text = paste0("Wholesale costs: ", percent(NorthScotlandDualFuelBreakdown$`Wholesale`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = NorthScotlandDualFuelBreakdown,
        x = ~ `Network costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Network costs",
        text = paste0("Network costs: ", percent(NorthScotlandDualFuelBreakdown$`Network costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = NorthScotlandDualFuelBreakdown,
        x = ~ `Policy costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Policy costs",
        text = paste0("Policy costs: ", percent(NorthScotlandDualFuelBreakdown$`Policy costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = NorthScotlandDualFuelBreakdown,
        x = ~ `Operating costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Operating costs",
        text = paste0("Operating costs: ", percent(NorthScotlandDualFuelBreakdown$`Operating costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = NorthScotlandDualFuelBreakdown,
        x = ~ `EBIT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "EBIT",
        text = paste0("EBIT: ", percent(NorthScotlandDualFuelBreakdown$`EBIT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = NorthScotlandDualFuelBreakdown,
        x = ~ `Adjustment allowance`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Adjustment allowance",
        text = paste0("Adjustment allowance: ", percent(NorthScotlandDualFuelBreakdown$`Adjustment allowance`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 6
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
                     ticktext = as.list(NorthScotlandDualFuelBreakdown$`Dates`),
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
  
  output$SouthDualFuelBreakdownSubtitle <- renderText({
    
    SouthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandDualFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    paste("Scotland,",
          str_sub(tail(SouthScotlandDualFuelBreakdown,1)$Dates,-4,-1),
          "-",
          str_sub(head(SouthScotlandDualFuelBreakdown,1)$Dates,-4,-1))
  })
  
  output$SouthDualFuelBreakdownPlot <- renderPlotly  ({
    
    
    SouthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandDualFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    SouthScotlandDualFuelBreakdown$Dates <- paste("<b>", SouthScotlandDualFuelBreakdown$Dates, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
    
    p <- plot_ly(data = SouthScotlandDualFuelBreakdown, y = ~ Dates) %>%
      
      add_trace(
        data = SouthScotlandDualFuelBreakdown,
        x = ~ `Wholesale`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Wholesale costs",
        text = paste0("Wholesale costs: ", percent(SouthScotlandDualFuelBreakdown$`Wholesale`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = SouthScotlandDualFuelBreakdown,
        x = ~ `Network costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Network costs",
        text = paste0("Network costs: ", percent(SouthScotlandDualFuelBreakdown$`Network costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = SouthScotlandDualFuelBreakdown,
        x = ~ `Policy costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Policy costs",
        text = paste0("Policy costs: ", percent(SouthScotlandDualFuelBreakdown$`Policy costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = SouthScotlandDualFuelBreakdown,
        x = ~ `Operating costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Operating costs",
        text = paste0("Operating costs: ", percent(SouthScotlandDualFuelBreakdown$`Operating costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = SouthScotlandDualFuelBreakdown,
        x = ~ `EBIT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "EBIT",
        text = paste0("EBIT: ", percent(SouthScotlandDualFuelBreakdown$`EBIT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = SouthScotlandDualFuelBreakdown,
        x = ~ `Adjustment allowance`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Adjustment allowance",
        text = paste0("Adjustment allowance: ", percent(SouthScotlandDualFuelBreakdown$`Adjustment allowance`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
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
                     ticktext = as.list(SouthScotlandDualFuelBreakdown$`Dates`),
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
  
  output$NorthDualFuelBreakdownTable = renderDataTable({
    
    
    NorthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandDualFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    NorthScotlandDualFuelBreakdown <- rbind(NorthScotlandDualFuelBreakdown,c("April 2015 - September 2015",	0.450785585238946,	0.261135156074665,	0.091990188767057,	0.177089062071332,	0.0190000078479999, 0, 0))
    
    datatable(
      NorthScotlandDualFuelBreakdown[1:7],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Breakdown of a dual fuel bill, North Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Breakdown of a dual fuel bill, North Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Breakdown of a dual fuel bill, North Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:7, 1)
  })
  
  output$SouthDualFuelBreakdownTable = renderDataTable({
    
    
    SouthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandDualFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    SouthScotlandDualFuelBreakdown <- rbind(SouthScotlandDualFuelBreakdown,c("April 2015 - September 2015",	0.462706502598415,	0.242183563345471,	0.0944366540135878,	0.181673272194526,	0.0190000078479999,0,0))
    
    datatable(
      as_tibble(SouthScotlandDualFuelBreakdown[1:7]),
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Breakdown of a dual fuel bill, South Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Breakdown of a dual fuel bill, South Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Breakdown of a dual fuel bill, South Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:7, 1)
  })
  
  output$ElecFuelBreakdownSubtitle <- renderText({
    
    NorthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandElecFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    NorthScotlandElecFuelBreakdown <- head(NorthScotlandElecFuelBreakdown,1)
    
    paste("Scotland,", NorthScotlandElecFuelBreakdown$Dates)
  })
  
  output$ElecFuelBreakdownPlot <- renderPlotly  ({
    
    NorthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandElecFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    NorthScotlandElecFuelBreakdown <- head(NorthScotlandElecFuelBreakdown,1)
    
    NorthScotlandElecFuelBreakdown$Region <- "North Scotland"
    
    
    SouthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandElecFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    SouthScotlandElecFuelBreakdown <- head(SouthScotlandElecFuelBreakdown,1)
    
    SouthScotlandElecFuelBreakdown$Region <- "South Scotland"
    
    AllScotlandElecFuelBreakdown <- rbind(NorthScotlandElecFuelBreakdown, SouthScotlandElecFuelBreakdown)
    
    
    AllScotlandElecFuelBreakdown$Region <- paste("<b>", AllScotlandElecFuelBreakdown$Region, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
    
    p <- plot_ly(data = AllScotlandElecFuelBreakdown, y = ~ Region) %>%
      
      add_trace(
        data = AllScotlandElecFuelBreakdown,
        x = ~ `Wholesale`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Wholesale costs",
        text = paste0("Wholesale costs: ", percent(AllScotlandElecFuelBreakdown$`Wholesale`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = AllScotlandElecFuelBreakdown,
        x = ~ `Network costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Network costs",
        text = paste0("Network costs: ", percent(AllScotlandElecFuelBreakdown$`Network costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = AllScotlandElecFuelBreakdown,
        x = ~ `Policy costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Policy costs",
        text = paste0("Policy costs: ", percent(AllScotlandElecFuelBreakdown$`Policy costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = AllScotlandElecFuelBreakdown,
        x = ~ `Operating costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Operating costs",
        text = paste0("Operating costs: ", percent(AllScotlandElecFuelBreakdown$`Operating costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = AllScotlandElecFuelBreakdown,
        x = ~ `EBIT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "EBIT",
        text = paste0("EBIT: ", percent(AllScotlandElecFuelBreakdown$`EBIT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = AllScotlandElecFuelBreakdown,
        x = ~ `Adjustment allowance`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Adjustment allowance",
        text = paste0("Adjustment allowance: ", percent(AllScotlandElecFuelBreakdown$`Adjustment allowance`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
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
                     ticktext = as.list(AllScotlandElecFuelBreakdown$`Region`),
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
  
  output$NorthElecFuelBreakdownTable = renderDataTable({
    
    
    NorthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandElecFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    NorthScotlandElecFuelBreakdown <- rbind(NorthScotlandElecFuelBreakdown,c("April 2015 - September 2015",	0.450785585238946,	0.261135156074665,	0.091990188767057,	0.177089062071332,	0.0190000078479999, 0, 0))
    
    datatable(
      NorthScotlandElecFuelBreakdown[1:7],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Breakdown of a electricity bill, North Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Breakdown of a electricity bill, North Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Breakdown of a electricity bill, North Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:7, 1)
  })
  
  output$SouthElecFuelBreakdownTable = renderDataTable({
    
    
    SouthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandElecFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    SouthScotlandElecFuelBreakdown <- rbind(SouthScotlandElecFuelBreakdown,c("April 2015 - September 2015",	0.462706502598415,	0.242183563345471,	0.0944366540135878,	0.181673272194526,	0.0190000078479999,0,0))
    
    datatable(
      as_tibble(SouthScotlandElecFuelBreakdown[1:7]),
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Breakdown of a electricity bill, South Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Breakdown of a electricity bill, South Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Breakdown of a electricity bill, South Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:7, 1)
  })
  
  output$GasFuelBreakdownSubtitle <- renderText({
    
    NorthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandGasFuelBreakdown.txt", 
                                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    NorthScotlandGasFuelBreakdown <- head(NorthScotlandGasFuelBreakdown,1)
    
    paste("Scotland,", NorthScotlandGasFuelBreakdown$Dates)
  })
  
  output$GasFuelBreakdownPlot <- renderPlotly  ({
    
    NorthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandGasFuelBreakdown.txt", 
                                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    NorthScotlandGasFuelBreakdown <- head(NorthScotlandGasFuelBreakdown,1)
    
    NorthScotlandGasFuelBreakdown$Region <- "North Scotland"
    
    
    SouthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandGasFuelBreakdown.txt", 
                                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    SouthScotlandGasFuelBreakdown <- head(SouthScotlandGasFuelBreakdown,1)
    
    SouthScotlandGasFuelBreakdown$Region <- "South Scotland"
    
    AllScotlandGasFuelBreakdown <- rbind(NorthScotlandGasFuelBreakdown, SouthScotlandGasFuelBreakdown)
    
    
    AllScotlandGasFuelBreakdown$Region <- paste("<b>", AllScotlandGasFuelBreakdown$Region, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
    
    p <- plot_ly(data = AllScotlandGasFuelBreakdown, y = ~ Region) %>%
      
      add_trace(
        data = AllScotlandGasFuelBreakdown,
        x = ~ `Wholesale`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Wholesale costs",
        text = paste0("Wholesale costs: ", percent(AllScotlandGasFuelBreakdown$`Wholesale`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = AllScotlandGasFuelBreakdown,
        x = ~ `Network costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Network costs",
        text = paste0("Network costs: ", percent(AllScotlandGasFuelBreakdown$`Network costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = AllScotlandGasFuelBreakdown,
        x = ~ `Policy costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Policy costs",
        text = paste0("Policy costs: ", percent(AllScotlandGasFuelBreakdown$`Policy costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = AllScotlandGasFuelBreakdown,
        x = ~ `Operating costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Operating costs",
        text = paste0("Operating costs: ", percent(AllScotlandGasFuelBreakdown$`Operating costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = AllScotlandGasFuelBreakdown,
        x = ~ `EBIT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "EBIT",
        text = paste0("EBIT: ", percent(AllScotlandGasFuelBreakdown$`EBIT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = AllScotlandGasFuelBreakdown,
        x = ~ `Adjustment allowance`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Adjustment allowance",
        text = paste0("Adjustment allowance: ", percent(AllScotlandGasFuelBreakdown$`Adjustment allowance`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
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
                     ticktext = as.list(AllScotlandGasFuelBreakdown$`Region`),
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
  
  output$NorthGasFuelBreakdownTable = renderDataTable({
    
    
    NorthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandGasFuelBreakdown.txt", 
                                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    NorthScotlandGasFuelBreakdown <- rbind(NorthScotlandGasFuelBreakdown,c("April 2015 - September 2015",	0.450785585238946,	0.261135156074665,	0.091990188767057,	0.177089062071332,	0.0190000078479999, 0, 0))
    
    datatable(
      NorthScotlandGasFuelBreakdown[1:7],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Breakdown of a gas bill, North Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Breakdown of a gas bill, North Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Breakdown of a gas bill, North Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:7, 1)
  })
  
  output$SouthGasFuelBreakdownTable = renderDataTable({
    
    
    SouthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandGasFuelBreakdown.txt", 
                                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    SouthScotlandGasFuelBreakdown <- rbind(SouthScotlandGasFuelBreakdown,c("April 2015 - September 2015",	0.462706502598415,	0.242183563345471,	0.0944366540135878,	0.181673272194526,	0.0190000078479999,0,0))
    
    datatable(
      as_tibble(SouthScotlandGasFuelBreakdown[1:7]),
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Breakdown of a gas bill, South Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Breakdown of a gas bill, South Scotland',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Breakdown of a gas bill, South Scotland')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:7, 1)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/DualFuelBreakdown.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("DualFuelBreakdownTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$DualFuelBreakdown.png <- downloadHandler(
    filename = "DualFuelBreakdown.png",
    content = function(file) {


      NorthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandDualFuelBreakdown.txt", 
                                                   "\t", escape_double = FALSE, trim_ws = TRUE)
      
      NorthScotlandDualFuelBreakdown <- head(NorthScotlandDualFuelBreakdown,1)
      
      NorthScotlandDualFuelBreakdown$Region <- "North Scotland"
      
      
      SouthScotlandDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandDualFuelBreakdown.txt", 
                                                   "\t", escape_double = FALSE, trim_ws = TRUE)
      
      SouthScotlandDualFuelBreakdown <- head(SouthScotlandDualFuelBreakdown,1)
      
      SouthScotlandDualFuelBreakdown$Region <- "South Scotland"
      
      DualFuelBreakdown <- rbind(NorthScotlandDualFuelBreakdown, SouthScotlandDualFuelBreakdown)[c(2:7,9)]
      
      DualFuelBreakdown <- melt(DualFuelBreakdown, id.vars = "Region")
      
      DualFuelBreakdown$Region <- factor(DualFuelBreakdown$Region, levels = rev(unique(DualFuelBreakdown$Region)))
      
      DualFuelBreakdown$variable <-
        factor(DualFuelBreakdown$variable,
               levels = rev(unique(DualFuelBreakdown$variable)))
      
      DualFuelBreakdown <- DualFuelBreakdown %>%
        group_by(Region) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Breakdown of a dual fuel bill"
      sourcecaption <- "Source: OFGEM"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
      
      
      DualFuelBreakdownChart <- DualFuelBreakdown %>%
        ggplot(aes(x = Region, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Wholesale" = BarColours[1],
            "Network costs" = BarColours[2],
            "Policy costs" = BarColours[3],
            "Operating costs" = BarColours[4],
            "EBIT" = BarColours[5],
            "Adjustment allowance" = BarColours[6]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            y = 0 - .12,
            label = ifelse(variable == "EBIT", as.character(Region), ""),
            color = ChartColours[2]
          ),
          fontface = 2,
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = 2.7,
              y = (0.5/6)*1,
              label = "Wholesale\nCosts"),
          fontface = 2,
          colour = BarColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2.7,
              y = (1.5/6)*1,
              label = "Network\nCosts"),
          fontface = 2,
          colour = BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2.7,
              y = (2.5/6)*1,
              label = "Policy\ncosts"),
          fontface = 2,
          colour = BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2.7,
              y = (3.5/6)*1,
              label = "Operating\ncosts"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2.7,
              y = (4.5/6)*1,
              label = "EBIT"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 2.7,
              y = (5.5/6)*1,
              label = "Adjustment\nallowance"),
          fontface = 2,
          colour = BarColours[6],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = 3,
              y = (6.5/7)*1,
              label = " "),
          fontface = 2,
          colour = BarColours[7],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          y = DualFuelBreakdown$pos,
          label = ifelse(DualFuelBreakdown$value > .03, percent(DualFuelBreakdown$value, 0.1), ""),
          family = "Century Gothic",
          fontface = 2,
          color = "white",
          size = 3
        )
      
      
      DualFuelBreakdownChart
      
      
      DualFuelBreakdownChart <-
        StackedBars(
          DualFuelBreakdownChart,
          DualFuelBreakdown,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      DualFuelBreakdownChart <-
        DualFuelBreakdownChart +
        ylim(-.18,1.01) +
        coord_flip() +
        labs(subtitle = paste("Scotland,", tail(NorthScotlandDualFuelBreakdown,1)[1]))
      
      DualFuelBreakdownChart
      
      ggsave(
        file,
        plot = DualFuelBreakdownChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$NorthDualFuelBreakdown.png <- downloadHandler(
    filename = "DualFuelBreakdown.png",
    content = function(file) {
      
      
      NorthDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandDualFuelBreakdown.txt", 
                                           "\t", escape_double = FALSE, trim_ws = TRUE)[1:7]
      
      height <- nrow(NorthDualFuelBreakdown)
      
      
      NorthDualFuelBreakdown <- melt(NorthDualFuelBreakdown, id.vars = "Dates")
      
      NorthDualFuelBreakdown$Dates <- factor(NorthDualFuelBreakdown$Dates, levels = rev(unique(NorthDualFuelBreakdown$Dates)))
      
      NorthDualFuelBreakdown$variable <-
        factor(NorthDualFuelBreakdown$variable,
               levels = rev(unique(NorthDualFuelBreakdown$variable)))
      
      NorthDualFuelBreakdown <- NorthDualFuelBreakdown %>%
        group_by(Dates) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Breakdown of a Dual fuel bill, North Scotland"
      sourcecaption <- "Source: OFGEM"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
      
      
      NorthDualFuelBreakdownChart <- NorthDualFuelBreakdown %>%
        ggplot(aes(x = Dates, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Wholesale" = BarColours[1],
            "Network costs" = BarColours[2],
            "Policy costs" = BarColours[3],
            "Operating costs" = BarColours[4],
            "EBIT" = BarColours[5],
            "Adjustment allowance" = BarColours[6]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            y = 0 - .025,
            label = ifelse(variable == "EBIT", as.character(Dates), ""),
            color = ChartColours[2]
          ),
          fontface = 2,
          hjust = 1,
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (0.5/6)*1,
              label = "Wholesale\nCosts"),
          fontface = 2,
          colour = BarColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (1.5/6)*1,
              label = "Network\nCosts"),
          fontface = 2,
          colour = BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (2.5/6)*1,
              label = "Policy\ncosts"),
          fontface = 2,
          colour = BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (3.5/6)*1,
              label = "Operating\ncosts"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (4.5/6)*1,
              label = "EBIT"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (5.5/6)*1,
              label = "Adjustment\nallowance"),
          fontface = 2,
          colour = BarColours[6],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.2,
              y = (6.5/7)*1,
              label = " "),
          fontface = 2,
          colour = BarColours[7],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          y = NorthDualFuelBreakdown$pos,
          label = ifelse(NorthDualFuelBreakdown$value > .03, percent(NorthDualFuelBreakdown$value, 0.1), ""),
          family = "Century Gothic",
          fontface = 2,
          color = "white",
          size = 3
        )
      
      
      NorthDualFuelBreakdownChart
      
      
      NorthDualFuelBreakdownChart <-
        StackedBars(
          NorthDualFuelBreakdownChart,
          NorthDualFuelBreakdown,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      NorthDualFuelBreakdownChart <-
        NorthDualFuelBreakdownChart +
        ylim(-.42,1.01) +
        coord_flip() +
        labs(subtitle =     paste("Scotland,",
                                  str_sub(tail(NorthDualFuelBreakdown,1)$Dates,-4,-1),
                                  "-",
                                  str_sub(head(NorthDualFuelBreakdown,1)$Dates,-4,-1)))
      
      NorthDualFuelBreakdownChart
      
      ggsave(
        file,
        plot = NorthDualFuelBreakdownChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$SouthDualFuelBreakdown.png <- downloadHandler(
    filename = "DualFuelBreakdown.png",
    content = function(file) {
      
      
      SouthDualFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandDualFuelBreakdown.txt", 
                                           "\t", escape_double = FALSE, trim_ws = TRUE)[1:7]
      
      height <- nrow(SouthDualFuelBreakdown)
      
      
      SouthDualFuelBreakdown <- melt(SouthDualFuelBreakdown, id.vars = "Dates")
      
      SouthDualFuelBreakdown$Dates <- factor(SouthDualFuelBreakdown$Dates, levels = rev(unique(SouthDualFuelBreakdown$Dates)))
      
      SouthDualFuelBreakdown$variable <-
        factor(SouthDualFuelBreakdown$variable,
               levels = rev(unique(SouthDualFuelBreakdown$variable)))
      
      SouthDualFuelBreakdown <- SouthDualFuelBreakdown %>%
        group_by(Dates) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Breakdown of a Dual fuel bill, South Scotland"
      sourcecaption <- "Source: OFGEM"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
      
      
      SouthDualFuelBreakdownChart <- SouthDualFuelBreakdown %>%
        ggplot(aes(x = Dates, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Wholesale" = BarColours[1],
            "Network costs" = BarColours[2],
            "Policy costs" = BarColours[3],
            "Operating costs" = BarColours[4],
            "EBIT" = BarColours[5],
            "Adjustment allowance" = BarColours[6]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          aes(
            y = 0 - .025,
            label = ifelse(variable == "EBIT", as.character(Dates), ""),
            color = ChartColours[2]
          ),
          fontface = 2,
          hjust = 1,
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (0.5/6)*1,
              label = "Wholesale\nCosts"),
          fontface = 2,
          colour = BarColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (1.5/6)*1,
              label = "Network\nCosts"),
          fontface = 2,
          colour = BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (2.5/6)*1,
              label = "Policy\ncosts"),
          fontface = 2,
          colour = BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (3.5/6)*1,
              label = "Operating\ncosts"),
          fontface = 2,
          colour = BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (4.5/6)*1,
              label = "EBIT"),
          fontface = 2,
          colour = BarColours[5],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.11,
              y = (5.5/6)*1,
              label = "Adjustment\nallowance"),
          fontface = 2,
          colour = BarColours[6],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(x = height *1.2,
              y = (6.5/7)*1,
              label = " "),
          fontface = 2,
          colour = BarColours[7],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          y = SouthDualFuelBreakdown$pos,
          label = ifelse(SouthDualFuelBreakdown$value > .03, percent(SouthDualFuelBreakdown$value, 0.1), ""),
          family = "Century Gothic",
          fontface = 2,
          color = "white",
          size = 3
        )
      
      
      SouthDualFuelBreakdownChart
      
      
      SouthDualFuelBreakdownChart <-
        StackedBars(
          SouthDualFuelBreakdownChart,
          SouthDualFuelBreakdown,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      SouthDualFuelBreakdownChart <-
        SouthDualFuelBreakdownChart +
        ylim(-.42,1.01) +
        coord_flip() +
        labs(subtitle =     paste("Scotland,",
                                  str_sub(tail(SouthDualFuelBreakdown,1)$Dates,-4,-1),
                                  "-",
                                  str_sub(head(SouthDualFuelBreakdown,1)$Dates,-4,-1)))
      
      SouthDualFuelBreakdownChart
      
      ggsave(
        file,
        plot = SouthDualFuelBreakdownChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  
}
