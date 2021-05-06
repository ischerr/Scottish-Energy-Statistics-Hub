require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



ElecFuelBreakdownOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Electricity",
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
    tabPanel("North Scotland",
             fluidRow(column(8,
                             h3("Breakdown of an electricity bill", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('NorthElecFuelBreakdownSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('NorthElecFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("ElecFuelBreakdownPlot")),
             plotlyOutput(ns("NorthElecFuelBreakdownPlot"), height = "600px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("South Scotland",
             fluidRow(column(8,
                             h3("Breakdown of an electricity bill", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('SouthElecFuelBreakdownSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('SouthElecFuelBreakdown.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("ElecFuelBreakdownPlot")),
             plotlyOutput(ns("SouthElecFuelBreakdownPlot"), height = "600px")%>% withSpinner(color="#68c3ea"),
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
      column(12, dataTableOutput(ns("NorthElecFuelBreakdownTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("South Scotland",
             fluidRow(
               column(10, h3("Data - South Scotland", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("SouthElecFuelBreakdownTable"))%>% withSpinner(color="#68c3ea"))),
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
ElecFuelBreakdown <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ElecFuelBreakdown.R")

  
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
  
  output$NorthElecFuelBreakdownSubtitle <- renderText({
    
    NorthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandElecFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    paste("Scotland,",
          str_sub(tail(NorthScotlandElecFuelBreakdown,1)$Dates,-4,-1),
          "-",
          str_sub(head(NorthScotlandElecFuelBreakdown,1)$Dates,-4,-1))
  })
  
  output$NorthElecFuelBreakdownPlot <- renderPlotly  ({
    
    
    NorthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandElecFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    NorthScotlandElecFuelBreakdown$Dates <- paste("<b>", NorthScotlandElecFuelBreakdown$Dates, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
    
    p <- plot_ly(data = NorthScotlandElecFuelBreakdown, y = ~ Dates) %>%
      
      add_trace(
        data = NorthScotlandElecFuelBreakdown,
        x = ~ `Wholesale`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Wholesale costs",
        text = paste0("Wholesale costs: ", percent(NorthScotlandElecFuelBreakdown$`Wholesale`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = NorthScotlandElecFuelBreakdown,
        x = ~ `Network costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Network costs",
        text = paste0("Network costs: ", percent(NorthScotlandElecFuelBreakdown$`Network costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = NorthScotlandElecFuelBreakdown,
        x = ~ `Policy costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Policy costs",
        text = paste0("Policy costs: ", percent(NorthScotlandElecFuelBreakdown$`Policy costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = NorthScotlandElecFuelBreakdown,
        x = ~ `Operating costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Operating costs",
        text = paste0("Operating costs: ", percent(NorthScotlandElecFuelBreakdown$`Operating costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = NorthScotlandElecFuelBreakdown,
        x = ~ `EBIT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "EBIT",
        text = paste0("EBIT: ", percent(NorthScotlandElecFuelBreakdown$`EBIT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = NorthScotlandElecFuelBreakdown,
        x = ~ `Adjustment allowance`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Adjustment allowance",
        text = paste0("Adjustment allowance: ", percent(NorthScotlandElecFuelBreakdown$`Adjustment allowance`, accuracy = 0.1)),
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
                     ticktext = as.list(NorthScotlandElecFuelBreakdown$`Dates`),
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
  
  output$SouthElecFuelBreakdownSubtitle <- renderText({
    
    SouthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandElecFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    paste("Scotland,",
          str_sub(tail(SouthScotlandElecFuelBreakdown,1)$Dates,-4,-1),
          "-",
          str_sub(head(SouthScotlandElecFuelBreakdown,1)$Dates,-4,-1))
  })
  
  output$SouthElecFuelBreakdownPlot <- renderPlotly  ({
    
    
    SouthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandElecFuelBreakdown.txt", 
                                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    SouthScotlandElecFuelBreakdown$Dates <- paste("<b>", SouthScotlandElecFuelBreakdown$Dates, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
    
    p <- plot_ly(data = SouthScotlandElecFuelBreakdown, y = ~ Dates) %>%
      
      add_trace(
        data = SouthScotlandElecFuelBreakdown,
        x = ~ `Wholesale`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Wholesale costs",
        text = paste0("Wholesale costs: ", percent(SouthScotlandElecFuelBreakdown$`Wholesale`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = SouthScotlandElecFuelBreakdown,
        x = ~ `Network costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Network costs",
        text = paste0("Network costs: ", percent(SouthScotlandElecFuelBreakdown$`Network costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = SouthScotlandElecFuelBreakdown,
        x = ~ `Policy costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Policy costs",
        text = paste0("Policy costs: ", percent(SouthScotlandElecFuelBreakdown$`Policy costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = SouthScotlandElecFuelBreakdown,
        x = ~ `Operating costs`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Operating costs",
        text = paste0("Operating costs: ", percent(SouthScotlandElecFuelBreakdown$`Operating costs`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = SouthScotlandElecFuelBreakdown,
        x = ~ `EBIT`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "EBIT",
        text = paste0("EBIT: ", percent(SouthScotlandElecFuelBreakdown$`EBIT`, accuracy = 0.1)),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = SouthScotlandElecFuelBreakdown,
        x = ~ `Adjustment allowance`,
        type = 'bar',
        width = 0.7,
        orientation = 'h',
        name = "Adjustment allowance",
        text = paste0("Adjustment allowance: ", percent(SouthScotlandElecFuelBreakdown$`Adjustment allowance`, accuracy = 0.1)),
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
                     ticktext = as.list(SouthScotlandElecFuelBreakdown$`Dates`),
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
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/ElecFuelBreakdown.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("ElecFuelBreakdownTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ElecFuelBreakdown.png <- downloadHandler(
    filename = "ElecFuelBreakdown.png",
    content = function(file) {


      NorthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandElecFuelBreakdown.txt", 
                                                   "\t", escape_double = FALSE, trim_ws = TRUE)
      
      NorthScotlandElecFuelBreakdown <- head(NorthScotlandElecFuelBreakdown,1)
      
      NorthScotlandElecFuelBreakdown$Region <- "North Scotland"
      
      
      SouthScotlandElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandElecFuelBreakdown.txt", 
                                                   "\t", escape_double = FALSE, trim_ws = TRUE)
      
      SouthScotlandElecFuelBreakdown <- head(SouthScotlandElecFuelBreakdown,1)
      
      SouthScotlandElecFuelBreakdown$Region <- "South Scotland"
      
      ElecFuelBreakdown <- rbind(NorthScotlandElecFuelBreakdown, SouthScotlandElecFuelBreakdown)[c(2:7,9)]
      
      ElecFuelBreakdown <- melt(ElecFuelBreakdown, id.vars = "Region")
      
      ElecFuelBreakdown$Region <- factor(ElecFuelBreakdown$Region, levels = rev(unique(ElecFuelBreakdown$Region)))
      
      ElecFuelBreakdown$variable <-
        factor(ElecFuelBreakdown$variable,
               levels = rev(unique(ElecFuelBreakdown$variable)))
      
      ElecFuelBreakdown <- ElecFuelBreakdown %>%
        group_by(Region) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Breakdown of a electricity bill"
      sourcecaption <- "Source: OFGEM"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
      
      
      ElecFuelBreakdownChart <- ElecFuelBreakdown %>%
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
          y = ElecFuelBreakdown$pos,
          label = ifelse(ElecFuelBreakdown$value > .03, percent(ElecFuelBreakdown$value, 0.1), ""),
          family = "Century Gothic",
          fontface = 2,
          color = "white",
          size = 3
        )
      
      
      ElecFuelBreakdownChart
      
      
      ElecFuelBreakdownChart <-
        StackedBars(
          ElecFuelBreakdownChart,
          ElecFuelBreakdown,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      ElecFuelBreakdownChart <-
        ElecFuelBreakdownChart +
        ylim(-.18,1.01) +
        coord_flip() +
        labs(subtitle = paste("Scotland,", tail(NorthScotlandElecFuelBreakdown,1)[1]))
      
      ElecFuelBreakdownChart
      
      ggsave(
        file,
        plot = ElecFuelBreakdownChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$NorthElecFuelBreakdown.png <- downloadHandler(
    filename = "ElecFuelBreakdown.png",
    content = function(file) {
      
      
      NorthElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/NorthScotlandElecFuelBreakdown.txt", 
                                           "\t", escape_double = FALSE, trim_ws = TRUE)[1:7]
      
      height <- nrow(NorthElecFuelBreakdown)
      
      
      NorthElecFuelBreakdown <- melt(NorthElecFuelBreakdown, id.vars = "Dates")
      
      NorthElecFuelBreakdown$Dates <- factor(NorthElecFuelBreakdown$Dates, levels = rev(unique(NorthElecFuelBreakdown$Dates)))
      
      NorthElecFuelBreakdown$variable <-
        factor(NorthElecFuelBreakdown$variable,
               levels = rev(unique(NorthElecFuelBreakdown$variable)))
      
      NorthElecFuelBreakdown <- NorthElecFuelBreakdown %>%
        group_by(Dates) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Breakdown of a electricity bill, North Scotland"
      sourcecaption <- "Source: OFGEM"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
      
      
      NorthElecFuelBreakdownChart <- NorthElecFuelBreakdown %>%
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
          y = NorthElecFuelBreakdown$pos,
          label = ifelse(NorthElecFuelBreakdown$value > .03, percent(NorthElecFuelBreakdown$value, 0.1), ""),
          family = "Century Gothic",
          fontface = 2,
          color = "white",
          size = 3
        )
      
      
      NorthElecFuelBreakdownChart
      
      
      NorthElecFuelBreakdownChart <-
        StackedBars(
          NorthElecFuelBreakdownChart,
          NorthElecFuelBreakdown,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      NorthElecFuelBreakdownChart <-
        NorthElecFuelBreakdownChart +
        ylim(-.42,1.01) +
        coord_flip() +
        labs(subtitle =     paste("Scotland,",
                                  str_sub(tail(NorthElecFuelBreakdown,1)$Dates,-4,-1),
                                  "-",
                                  str_sub(head(NorthElecFuelBreakdown,1)$Dates,-4,-1)))
      
      NorthElecFuelBreakdownChart
      
      ggsave(
        file,
        plot = NorthElecFuelBreakdownChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$SouthElecFuelBreakdown.png <- downloadHandler(
    filename = "ElecFuelBreakdown.png",
    content = function(file) {
      
      
      SouthElecFuelBreakdown <- read_delim("Processed Data/Output/Energy Bills/SouthScotlandElecFuelBreakdown.txt", 
                                           "\t", escape_double = FALSE, trim_ws = TRUE)[1:7]
      
      height <- nrow(SouthElecFuelBreakdown)
      
      
      SouthElecFuelBreakdown <- melt(SouthElecFuelBreakdown, id.vars = "Dates")
      
      SouthElecFuelBreakdown$Dates <- factor(SouthElecFuelBreakdown$Dates, levels = rev(unique(SouthElecFuelBreakdown$Dates)))
      
      SouthElecFuelBreakdown$variable <-
        factor(SouthElecFuelBreakdown$variable,
               levels = rev(unique(SouthElecFuelBreakdown$variable)))
      
      SouthElecFuelBreakdown <- SouthElecFuelBreakdown %>%
        group_by(Dates) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Breakdown of a electricity bill, South Scotland"
      sourcecaption <- "Source: OFGEM"
      
      ChartColours <- c("#68c3ea", "#FF8500")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0", "#41b6c4", "#7fcdbb", "#c7e9b4")
      
      
      SouthElecFuelBreakdownChart <- SouthElecFuelBreakdown %>%
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
          y = SouthElecFuelBreakdown$pos,
          label = ifelse(SouthElecFuelBreakdown$value > .03, percent(SouthElecFuelBreakdown$value, 0.1), ""),
          family = "Century Gothic",
          fontface = 2,
          color = "white",
          size = 3
        )
      
      
      SouthElecFuelBreakdownChart
      
      
      SouthElecFuelBreakdownChart <-
        StackedBars(
          SouthElecFuelBreakdownChart,
          SouthElecFuelBreakdown,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      SouthElecFuelBreakdownChart <-
        SouthElecFuelBreakdownChart +
        ylim(-.42,1.01) +
        coord_flip() +
        labs(subtitle =     paste("Scotland,",
                                  str_sub(tail(SouthElecFuelBreakdown,1)$Dates,-4,-1),
                                  "-",
                                  str_sub(head(SouthElecFuelBreakdown,1)$Dates,-4,-1)))
      
      SouthElecFuelBreakdownChart
      
      ggsave(
        file,
        plot = SouthElecFuelBreakdownChart,
        width = 20,
        height = 13,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  
}
