require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



GasFuelBreakdownOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("gas",
    fluidRow(column(8,
                    h3("Breakdown of a Gas bill", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('GasFuelBreakdownSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("GasFuelBreakdownPlot")),
    plotlyOutput(ns("GasFuelBreakdownPlot"))%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("North Scotland",
             fluidRow(column(8,
                             h3("Breakdown of a gas bill", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('NorthGasFuelBreakdownSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('NorthGasFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("GasFuelBreakdownPlot")),
             plotlyOutput(ns("NorthGasFuelBreakdownPlot"), height = "600px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("South Scotland",
             fluidRow(column(8,
                             h3("Breakdown of a gas bill", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('SouthGasFuelBreakdownSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('SouthGasFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("GasFuelBreakdownPlot")),
             plotlyOutput(ns("SouthGasFuelBreakdownPlot"), height = "600px")%>% withSpinner(color="#68c3ea"),
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
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("OFGEMTariff"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("OFGEMTariff"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("OFGEMTariff")
        
      )
    )
  )
}




###### Server ######
GasFuelBreakdown <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("GasFuelBreakdown.R")

  
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
  
  output$NorthGasFuelBreakdownSubtitle <- renderText({
    
    NorthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandGasFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    paste("Scotland,",
          str_sub(tail(NorthScotlandGasFuelBreakdown,1)$Dates,-4,-1),
          "-",
          str_sub(head(NorthScotlandGasFuelBreakdown,1)$Dates,-4,-1))
  })
  
  output$NorthGasFuelBreakdownPlot <- renderPlotly  ({
    
    
    NorthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandGasFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    NorthScotlandGasFuelBreakdown$Dates <- paste("<b>", NorthScotlandGasFuelBreakdown$Dates, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
    
    p <- plot_ly(data = NorthScotlandGasFuelBreakdown, y = ~ Dates) %>%
      
      add_trace(
        data = NorthScotlandGasFuelBreakdown,
        x = ~ `Wholesale`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Wholesale costs",
        text = paste0("Wholesale costs: ", percent(NorthScotlandGasFuelBreakdown$`Wholesale`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = NorthScotlandGasFuelBreakdown,
        x = ~ `Network costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Network costs",
        text = paste0("Network costs: ", percent(NorthScotlandGasFuelBreakdown$`Network costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = NorthScotlandGasFuelBreakdown,
        x = ~ `Policy costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Policy costs",
        text = paste0("Policy costs: ", percent(NorthScotlandGasFuelBreakdown$`Policy costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = NorthScotlandGasFuelBreakdown,
        x = ~ `Operating costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Operating costs",
        text = paste0("Operating costs: ", percent(NorthScotlandGasFuelBreakdown$`Operating costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = NorthScotlandGasFuelBreakdown,
        x = ~ `EBIT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "EBIT",
        text = paste0("EBIT: ", percent(NorthScotlandGasFuelBreakdown$`EBIT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = NorthScotlandGasFuelBreakdown,
        x = ~ `Adjustment allowance`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Adjustment allowance",
        text = paste0("Adjustment allowance: ", percent(NorthScotlandGasFuelBreakdown$`Adjustment allowance`, accuracy = 0.1)),
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
                     ticktext = as.list(NorthScotlandGasFuelBreakdown$`Dates`),
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
  
  output$SouthGasFuelBreakdownSubtitle <- renderText({
    
    SouthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandGasFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    paste("Scotland,",
          str_sub(tail(SouthScotlandGasFuelBreakdown,1)$Dates,-4,-1),
          "-",
          str_sub(head(SouthScotlandGasFuelBreakdown,1)$Dates,-4,-1))
  })
  
  output$SouthGasFuelBreakdownPlot <- renderPlotly  ({
    
    
    SouthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandGasFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    SouthScotlandGasFuelBreakdown$Dates <- paste("<b>", SouthScotlandGasFuelBreakdown$Dates, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
    
    p <- plot_ly(data = SouthScotlandGasFuelBreakdown, y = ~ Dates) %>%
      
      add_trace(
        data = SouthScotlandGasFuelBreakdown,
        x = ~ `Wholesale`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Wholesale costs",
        text = paste0("Wholesale costs: ", percent(SouthScotlandGasFuelBreakdown$`Wholesale`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = SouthScotlandGasFuelBreakdown,
        x = ~ `Network costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Network costs",
        text = paste0("Network costs: ", percent(SouthScotlandGasFuelBreakdown$`Network costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = SouthScotlandGasFuelBreakdown,
        x = ~ `Policy costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Policy costs",
        text = paste0("Policy costs: ", percent(SouthScotlandGasFuelBreakdown$`Policy costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = SouthScotlandGasFuelBreakdown,
        x = ~ `Operating costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Operating costs",
        text = paste0("Operating costs: ", percent(SouthScotlandGasFuelBreakdown$`Operating costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = SouthScotlandGasFuelBreakdown,
        x = ~ `EBIT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "EBIT",
        text = paste0("EBIT: ", percent(SouthScotlandGasFuelBreakdown$`EBIT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = SouthScotlandGasFuelBreakdown,
        x = ~ `Adjustment allowance`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Adjustment allowance",
        text = paste0("Adjustment allowance: ", percent(SouthScotlandGasFuelBreakdown$`Adjustment allowance`, accuracy = 0.1)),
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
                     ticktext = as.list(SouthScotlandGasFuelBreakdown$`Dates`),
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
                     paste(readtext("Structure/5 - Consumers/GasFuelBreakdown.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("GasFuelBreakdownTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$GasFuelBreakdown.png <- downloadHandler(
    filename = "GasFuelBreakdown.png",
    content = function(file) {


      NorthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandGasFuelBreakdown.txt", 
                                                   "\t", escape_double = FALSE, trim_ws = TRUE)
      
      NorthScotlandGasFuelBreakdown <- head(NorthScotlandGasFuelBreakdown,1)
      
      NorthScotlandGasFuelBreakdown$Region <- "North Scotland"
      
      
      SouthScotlandGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandGasFuelBreakdown.txt", 
                                                   "\t", escape_double = FALSE, trim_ws = TRUE)
      
      SouthScotlandGasFuelBreakdown <- head(SouthScotlandGasFuelBreakdown,1)
      
      SouthScotlandGasFuelBreakdown$Region <- "South Scotland"
      
      GasFuelBreakdown <- rbind(NorthScotlandGasFuelBreakdown, SouthScotlandGasFuelBreakdown)[c(2:7,9)]
      
      GasFuelBreakdown <- melt(GasFuelBreakdown, id.vars = "Region")
      
      GasFuelBreakdown$Region <- factor(GasFuelBreakdown$Region, levels = rev(unique(GasFuelBreakdown$Region)))
      
      GasFuelBreakdown$variable <-
        factor(GasFuelBreakdown$variable,
               levels = rev(unique(GasFuelBreakdown$variable)))
      
      GasFuelBreakdown <- GasFuelBreakdown %>%
        group_by(Region) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Breakdown of a gas bill"
      sourcecaption <- "Source: OFGEM"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
      
      
      GasFuelBreakdownChart <- GasFuelBreakdown %>%
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
          y = GasFuelBreakdown$pos,
          label = ifelse(GasFuelBreakdown$value > .03, percent(GasFuelBreakdown$value, 0.1), ""),
          family = "Century Gothic",
          fontface = 2,
          color = "white",
          size = 3
        )
      
      
      GasFuelBreakdownChart
      
      
      GasFuelBreakdownChart <-
        StackedBars(
          GasFuelBreakdownChart,
          GasFuelBreakdown,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      GasFuelBreakdownChart <-
        GasFuelBreakdownChart +
        ylim(-.18,1.01) +
        coord_flip() +
        labs(subtitle = paste("Scotland,", tail(NorthScotlandGasFuelBreakdown,1)[1]))
      
      GasFuelBreakdownChart
      
      ggsave(
        file,
        plot = GasFuelBreakdownChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$NorthGasFuelBreakdown.png <- downloadHandler(
    filename = "GasFuelBreakdown.png",
    content = function(file) {
      
      
      NorthGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandGasFuelBreakdown.txt", 
                                           "\t", escape_double = FALSE, trim_ws = TRUE)[1:7]
      
      height <- nrow(NorthGasFuelBreakdown)
      
      
      NorthGasFuelBreakdown <- melt(NorthGasFuelBreakdown, id.vars = "Dates")
      
      NorthGasFuelBreakdown$Dates <- factor(NorthGasFuelBreakdown$Dates, levels = rev(unique(NorthGasFuelBreakdown$Dates)))
      
      NorthGasFuelBreakdown$variable <-
        factor(NorthGasFuelBreakdown$variable,
               levels = rev(unique(NorthGasFuelBreakdown$variable)))
      
      NorthGasFuelBreakdown <- NorthGasFuelBreakdown %>%
        group_by(Dates) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Breakdown of a gas bill, North Scotland"
      sourcecaption <- "Source: OFGEM"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
      
      
      NorthGasFuelBreakdownChart <- NorthGasFuelBreakdown %>%
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
          y = NorthGasFuelBreakdown$pos,
          label = ifelse(NorthGasFuelBreakdown$value > .03, percent(NorthGasFuelBreakdown$value, 0.1), ""),
          family = "Century Gothic",
          fontface = 2,
          color = "white",
          size = 3
        )
      
      
      NorthGasFuelBreakdownChart
      
      
      NorthGasFuelBreakdownChart <-
        StackedBars(
          NorthGasFuelBreakdownChart,
          NorthGasFuelBreakdown,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      NorthGasFuelBreakdownChart <-
        NorthGasFuelBreakdownChart +
        ylim(-.42,1.01) +
        coord_flip() +
        labs(subtitle =     paste("Scotland,",
                                  str_sub(tail(NorthGasFuelBreakdown,1)$Dates,-4,-1),
                                  "-",
                                  str_sub(head(NorthGasFuelBreakdown,1)$Dates,-4,-1)))
      
      NorthGasFuelBreakdownChart
      
      ggsave(
        file,
        plot = NorthGasFuelBreakdownChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$SouthGasFuelBreakdown.png <- downloadHandler(
    filename = "GasFuelBreakdown.png",
    content = function(file) {
      
      
      SouthGasFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandGasFuelBreakdown.txt", 
                                           "\t", escape_double = FALSE, trim_ws = TRUE)[1:7]
      
      height <- nrow(SouthGasFuelBreakdown)
      
      
      SouthGasFuelBreakdown <- melt(SouthGasFuelBreakdown, id.vars = "Dates")
      
      SouthGasFuelBreakdown$Dates <- factor(SouthGasFuelBreakdown$Dates, levels = rev(unique(SouthGasFuelBreakdown$Dates)))
      
      SouthGasFuelBreakdown$variable <-
        factor(SouthGasFuelBreakdown$variable,
               levels = rev(unique(SouthGasFuelBreakdown$variable)))
      
      SouthGasFuelBreakdown <- SouthGasFuelBreakdown %>%
        group_by(Dates) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Breakdown of a gas bill, South Scotland"
      sourcecaption <- "Source: OFGEM"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
      
      
      SouthGasFuelBreakdownChart <- SouthGasFuelBreakdown %>%
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
          y = SouthGasFuelBreakdown$pos,
          label = ifelse(SouthGasFuelBreakdown$value > .03, percent(SouthGasFuelBreakdown$value, 0.1), ""),
          family = "Century Gothic",
          fontface = 2,
          color = "white",
          size = 3
        )
      
      
      SouthGasFuelBreakdownChart
      
      
      SouthGasFuelBreakdownChart <-
        StackedBars(
          SouthGasFuelBreakdownChart,
          SouthGasFuelBreakdown,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      SouthGasFuelBreakdownChart <-
        SouthGasFuelBreakdownChart +
        ylim(-.42,1.01) +
        coord_flip() +
        labs(subtitle =     paste("Scotland,",
                                  str_sub(tail(SouthGasFuelBreakdown,1)$Dates,-4,-1),
                                  "-",
                                  str_sub(head(SouthGasFuelBreakdown,1)$Dates,-4,-1)))
      
      SouthGasFuelBreakdownChart
      
      ggsave(
        file,
        plot = SouthGasFuelBreakdownChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  
}
