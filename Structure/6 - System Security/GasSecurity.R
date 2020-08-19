require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

GasSecurityOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Gas Distribution",
    fluidRow(column(8,
                    h3("Gas distribution from St. Fergus gas terminal", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('GasSecuritySubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasSecurity.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("GasSecurityPlot")),
    plotlyOutput(ns("GasSecurityPlot"))%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Rolling Average",
             fluidRow(column(8,
                             h3("Gas distribution from St. Fergus gas terminal - 12 month rolling average", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('GasSecurityRollingSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasSecurityRolling.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("GasSecurityPlot")),
             plotlyOutput(ns("GasSecurityRollingPlot"))%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("UK Proportion",
             fluidRow(column(8,
                             h3("Proportion of UK gas supply from St. Fergus", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('GasSecurityProportionSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasSecurityProportion.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("GasSecurityPlot")),
             plotlyOutput(ns("GasSecurityProportionPlot"))%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:;background-color:#5d8be1;"),
    fluidRow(
      column(10, h3("Daily Data", style = "color: #5d8be1;  font-weight:bold")),
      column(2, style = "padding:15px",  downloadButton(ns("FullData"), "Full Data", style = "float:right; "))
    ),
    tags$hr(style = "height:3px;border:none;color:;background-color:#5d8be1;"),
    tabsetPanel(
      tabPanel("Flows",
    fluidRow(
    column(10, h3("Data - Flows into St. Fergus, 12 month rolling average (GWh)", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("GasSecurityTable"))%>% withSpinner(color="#5d8be1")))),
    tabPanel("UK Proportion",
             fluidRow(
               column(10, h3("Data - Proportion of St. Fergus in UK gas, 12 month rolling average (GWh)", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("UKFergusTable"))%>% withSpinner(color="#5d8be1"))))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("NGData"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("NGData")
        
      )
    )
  )
}




###### Server ######
GasSecurity <- function(input, output, session) {

  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("GasSecurity.R")

  output$GasSecuritySubtitle <- renderText({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
    
    print("Energy daily demand")
    ###### Daily Demand  #####
    
    # GasSecurity <-
    #   read_csv(
    #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/GasSecurity.csv"
    #   )
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[c(1,2,4,3)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    GasSecurity <- Data
    
    paste("Scotland,", format(min(GasSecurity$Year),"%B %Y"),"-", format(max(GasSecurity$Year),"%B %Y"))
  })
  
  output$GasSecurityPlot <- renderPlotly  ({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
        ###### Daily Demand  #####
    
    # GasSecurity <-
    #   read_csv(
    #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/GasSecurity.csv"
    #   )
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[1:4]
    
    names(Data)[1] <-"Year"
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    GasSecurity <- Data
    
    ### variables
    ChartColours <- c("#5d8be1", "#238b45", "#a1d99b")
    BarColours <- c("#e34a33","#a8ddb5", "#43a2ca")
    sourcecaption = "Source: University of Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day"
    
    #GasSecurity$GasPercentage <- PercentLabel(GasSecurity$Gas)
    
    
    p <- plot_ly(GasSecurity, x = ~Year) %>% 
      add_trace(y = ~ScotDemand, 
                name = "Scottish Demand",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                text = paste0(
                  "Scottish Demand: ",
                  round(GasSecurity$ScotDemand, digits = 1),
                  " GWh\nDate: ",
                  format(GasSecurity$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                fillcolor = BarColours[1]) %>% 
      add_trace(y = ~NITransfer, 
                name = "Transfers to N.I.",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                text = paste0(
                  "Transfers to N.I.: ",
                  round(GasSecurity$NITransfer, digits = 1),
                  " GWh\nDate: ",
                  format(GasSecurity$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                fillcolor = BarColours[2]) %>% 
      add_trace(y = ~EnglandTransfer, 
                name = "Transfers to England",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                text = paste0(
                  "Transfers to England: ",
                  round(GasSecurity$EnglandTransfer, digits = 1),
                  " GWh\nDate: ",
                  format(GasSecurity$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                fillcolor = BarColours[3]) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     rangeselector = list(
                       buttons = list(
                         list(
                           count = 3,
                           label = "3 mo",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 6,
                           label = "6 mo",
                           step = "month",
                           stepmode = "backward"),
                         list(
                           count = 1,
                           label = "1 yr",
                           step = "year",
                           stepmode = "backward"),
                         list(
                           count = 1,
                           label = "YTD",
                           step = "year",
                           stepmode = "todate"),
                         list(step = "all"))),
                     
                     rangeslider = list(type = "date")),
        yaxis = list(
          title = "GWh",
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
  
  output$GasSecurityRollingSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[c(1,10, 11)]
    
    names(Data) <- c("Year", "Fergus", "GBDemand")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    Data <- Data[complete.cases(Data),]
    
    paste("Scotland,", format(min(Data$Year),"%B %Y"),"-", format(max(Data$Year),"%B %Y"))
  })
  
  output$GasSecurityRollingPlot <- renderPlotly  ({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
    
    print("Energy daily demand")
    ###### Daily Demand  #####
    
    # GasSecurityRolling <-
    #   read_csv(
    #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/GasSecurityRolling.csv"
    #   )
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[c(1,7,8,9)]
    
    names(Data)[1] <- "Year"
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    GasSecurityRolling <- Data[complete.cases(Data),]
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: University of Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day"
    
    #GasSecurityRolling$GasPercentage <- PercentLabel(GasSecurityRolling$Gas)
    
    
    p <- plot_ly(GasSecurityRolling, x = ~Year) %>% 
      add_trace(y = ~ RollingScotDemand, 
                name = "Scottish Demand",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Scottish Demand: ",
                  round(GasSecurityRolling$RollingScotDemand, digits = 1),
                  " GWh\n12 months ending: ",
                  format(GasSecurityRolling$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[2], dash = "none")
                )     %>% 
      add_trace(y = ~ RollingNITransfer, 
                name = "Transfers to N.I.",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Transfers to N.I.: ",
                  round(GasSecurityRolling$RollingNITransfer, digits = 1),
                  " GWh\n12 months ending: ",
                  format(GasSecurityRolling$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[3], dash = "none")
      )     %>% 
      add_trace(y = ~ RollingEnglandTransfer, 
                name = "Transfers to England",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Transfers to England: ",
                  round(GasSecurityRolling$RollingEnglandTransfer, digits = 1),
                  " GWh\n12 months ending: ",
                  format(GasSecurityRolling$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[1], dash = "none")
      )     %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(GasSecurityRolling$Year)-100, max(GasSecurityRolling$Year)+100)),
        yaxis = list(
          title = "GWh",
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
  
  output$GasSecurityProportionSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[c(1,10, 11)]
    
    names(Data) <- c("Year", "Fergus", "GBDemand")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    Data <- Data[complete.cases(Data),]
    
    paste("Scotland,", format(min(Data$Year),"%B %Y"),"-", format(max(Data$Year),"%B %Y"))
  })
  
  output$GasSecurityProportionPlot <- renderPlotly  ({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
    
    print("Energy daily demand")
    ###### Daily Demand  #####
    
    # GasSecurityProportion <-
    #   read_csv(
    #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/GasSecurityProportion.csv"
    #   )
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[c(1,10, 11)]
    
    names(Data) <- c("Year", "Fergus", "GBDemand")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    GasSecurityProportion <- Data[complete.cases(Data),]
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: University of Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day"
    
    #GasSecurityProportion$GasPercentage <- PercentLabel(GasSecurityProportion$Gas)
    
    
    p <- plot_ly(GasSecurityProportion, x = ~Year) %>% 
      add_trace(y = ~ GasSecurityProportion$Fergus / GasSecurityProportion$GBDemand, 
                name = "Scottish Demand",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Scottish Demand: ",
                  percent(GasSecurityProportion$Fergus / GasSecurityProportion$GBDemand, 0.1),
                  " GWh\nDate: ",
                  format(GasSecurityProportion$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[2], dash = "none")) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(GasSecurityProportion$Year)-100, max(GasSecurityProportion$Year)+100)),
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
  
  StFergusFlowRolling <- read_delim("Processed Data/Output/Gas Distribution/StFergusFlowRolling.txt", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)
  
  names(StFergusFlowRolling) <- c("Date", "Average Daily Scottish Demand (GWh)","Average % Scottish demand from St. Fergus", "Average Daily Transfers to N.I. (GWh)","Average % N.I. transfers from St. Fergus" , "Average Daily Transfers to England (GWh)","Average % Englannd transfers from St. Fergus" , "Average Daily ST. Fergus (GWh)", "Average Daily UK Demand (GWh)", "Average Proportion of U.K. gas supply from ST. Fergus" )
  
  StFergusFlowRolling$Date <- as.Date(StFergusFlowRolling$Date, format = "%d/%m/%Y")
  

  names(StFergusFlowRolling)[1] <- "12 month ending"
  
  StFergusFlowRolling <- StFergusFlowRolling[rev(order(StFergusFlowRolling$`12 month ending`)),]
  
  StFergusFlowRolling$`12 month ending` <- format(StFergusFlowRolling$`12 month ending`, format = "%B %Y")

  
  
  output$GasSecurityTable = renderDataTable({
    

   
    
    datatable(
      StFergusFlowRolling[1:7],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = 10,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Daily Demand - 12 month rolling average",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Daily Demand - 12 month rolling average',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Daily Demand - 12 month rolling average')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:6, 1) %>% 
      formatPercentage(c(3,5,7), 1)
  })
  
  output$UKFergusTable = renderDataTable({
    
    
    
    
    datatable(
      StFergusFlowRolling[c(1,8:10)],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = 10,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Daily Demand - 12 month rolling average",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Daily Demand - 12 month rolling average',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Daily Demand - 12 month rolling average')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:6, 1) %>% 
      formatPercentage(c(4), 1)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/6 - System Security/GasSecurity.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable1, {
    toggle("GasSecurityTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("UKFergusTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$GasSecurity.png <- downloadHandler(
    filename = "GasSecurity.png",
    content = function(file) {
      
      print("Energy daily demand")
      ###### Daily Demand  #####
      
      # GasSecurity <-
      #   read_csv(
      #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/GasSecurity.csv"
      #   )
      
      GasDistribution <- read_excel("Structure/CurrentWorking.xlsx", 
                                    sheet = "GasSecurityWorking")[c(1,2,3,5,6,4)] 
      
      names(GasDistribution)[1] <- c("Year")
      
      GasDistribution <- GasDistribution[complete.cases(GasDistribution),]
      
      GasDistribution$Year <-
        ymd(GasDistribution$Year)
      
      
      
      GasDistributionMin <- head(GasDistribution, 1)
      GasDistributionMax <- tail(GasDistribution, 1)
      
      GasDistribution <- melt(GasDistribution, id.vars = "Year")
      
      GasDistribution <- subset(GasDistribution, variable == "ScotDemand" | variable == "NITransfer" | variable == "EnglandTransfer")
      
      
      GasDistribution <- GasDistribution %>% mutate(variable = factor(variable),
                                                    variable = factor(variable, levels = rev(c("ScotDemand", "NITransfer", "EnglandTransfer"))))
      
      GasDistribution$value <- as.numeric(ifelse(GasDistribution$value <0, "0", GasDistribution$value))
      
      ### variables
      ChartColours <- c("#5d8be1", "#238b45", "#a1d99b")
      BarColours <- c("#e34a33","#a8ddb5", "#43a2ca")
      sourcecaption = "Source: National Grid"
      plottitle = "Gas distribution from St Fergus gas terminal"
      
      #GasDistribution$CavityPercentage <- PercentLabel(GasDistribution$Cavity)
      
      width <- max(GasDistribution$Year) - min(GasDistribution$Year)
      
      GasDistributionChart <- GasDistribution %>%
        ggplot(aes(
          x = Year,
          y = value,
          group = variable,
          fill = variable
        ))+
        scale_fill_manual(
          "variable",
          values = c(
            "ScotDemand" = BarColours[1],
            "NITransfer" = BarColours[2],
            "EnglandTransfer" = BarColours[3]
          )) +
        annotate(
          "segment",
          x = min(GasDistribution$Year),
          xend = max(GasDistribution$Year),
          y = 200,
          yend = 200,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        annotate(
          "segment",
          x = min(GasDistribution$Year),
          xend = max(GasDistribution$Year),
          y = 400,
          yend = 400,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        )+ 
        annotate(
          "segment",
          x = min(GasDistribution$Year),
          xend = max(GasDistribution$Year),
          y = 600,
          yend = 600,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        annotate(
          "segment",
          x = min(GasDistribution$Year),
          xend = max(GasDistribution$Year),
          y = 800,
          yend = 800,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        annotate(
          "segment",
          x = min(GasDistribution$Year),
          xend = max(GasDistribution$Year),
          y = 1000,
          yend = 1000,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        annotate(
          "segment",
          x = min(GasDistribution$Year),
          xend = max(GasDistribution$Year),
          y = 1200,
          yend = 1200,
          colour = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        geom_area(posistion = "fill") +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              format(Year, format = "%b %Y"),
              ""
            ),
            hjust = ifelse(Year == min(Year),0, 1),
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = max(Year)+145,
            y = 105,
            label = "Scottish\nDemand",
            fontface = 2
          ),
          colour = BarColours[1],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = max(Year)+145,
            y = 275,
            label = "Transfers to\nN.I.",
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = max(Year)+145,
            y = 520,
            label = "Transfers to\nEngland",
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic",
          size = 3
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 200,
            label = "200\nGWh",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 400,
            label = "400\nGWh",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 600,
            label = "600\nGWh",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 800,
            label = "800\nGWh",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 1000,
            label = "1000\nGWh",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 1200,
            label = "1200\nGWh",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        )
      
      
      GasDistributionChart
      
      
      GasDistributionChart <-
        DailyChart(GasDistributionChart,
                   GasDistribution,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      
      GasDistributionChart <- GasDistributionChart+
        coord_cartesian(xlim = c(min(GasDistribution$Year-(width*0.025)), max(GasDistribution$Year)+(width*0.07))) 
      
      
      ggsave(
        file,
        plot = GasDistributionChart,
        width = 20,
        height = 18,
        units = "cm",
        dpi = 300
      )
    }
)

output$GasSecurityRolling.png <- downloadHandler(
  filename = "GasSecurityRolling.png",
  content = function(file) {
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[c(1,7,8,9,12)]
    
    Data <- Data[complete.cases(Data),]
    
    names(Data) <- c("Year", "Scotland", "NI", "England", "FergusProp")
    
    Data <- subset(Data, Data$Year >= ymd("2015-01-31"))
    
    Data <- subset(Data, Data$Year <= floor_date(max(Data$Year), unit = "month") - days(1))
    
    GasSecurityRolling <- Data
    
    length <- max(GasSecurityRolling$Year) - min(GasSecurityRolling$Year)
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: National Grid"
    plottitle = "Gas distribution from St Fergus gas terminal\n12 month rolling average"
    
    #GasSecurityRolling$ScotlandPercentage <- PercentLabel(GasSecurityRolling$Scotland)
    
    
    GasSecurityRollingChart <- GasSecurityRolling %>%
      ggplot(aes(x = Year), family = "Century Gothic") +
      
      geom_line(
        aes(y = Scotland,
            label = Scotland),
        colour = ChartColours[2],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(GasSecurityRolling$Year),
        y = max(GasSecurityRolling$Scotland),
        label = "Scottish Demand",
        hjust = 0.5,
        vjust = .8,
        colour = ChartColours[2],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = NI,
            label = paste0(NI * 100, "%")),
        colour = ChartColours[3],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(GasSecurityRolling$Year),
        y = mean(GasSecurityRolling$NI),
        label = "Tranfers to\nNorthern Ireland",
        hjust = 0.5,
        vjust = 1.9,
        colour = ChartColours[3],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = England,
            label = paste0(England * 100, "%")),
        colour = ChartColours[4],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(GasSecurityRolling$Year),
        y = mean(GasSecurityRolling$England),
        label = "Transfers to\nEngland",
        hjust = 0.5,
        vjust = 0.7,
        colour = ChartColours[4],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year,
          y = 0,
          label = ifelse(
            Year == max(Year) |
              Year == min(Year),
            format(Year, format = "%b %Y"),
            ""
          ),
          hjust = 0.5,
          vjust = 1.5,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year),
          y = GasSecurityRolling$Scotland[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$Scotland[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          vjust = 1.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = GasSecurityRolling$Scotland[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$Scotland[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          vjust = -.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-52,
          y = GasSecurityRolling$NI[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$NI[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          vjust = -.2,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = GasSecurityRolling$NI[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$NI[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          vjust = 1.2,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-52,
          y = GasSecurityRolling$England[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$England[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          vjust = -1,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = GasSecurityRolling$England[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$England[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          vjust = -1,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )
    
    GasSecurityRollingChart
    
    GasSecurityRollingChart <-
      DailyChart(GasSecurityRollingChart,
                 GasSecurityRolling,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    GasSecurityRollingChart <- GasSecurityRollingChart +
      coord_cartesian(xlim = c(min(GasSecurityRolling$Year)-50, max(GasSecurityRolling$Year)+50)) +
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.7,
        linetype = 2
      )
    
    
    GasSecurityRollingChart
    
    ggsave(
      file,
      plot =  GasSecurityRollingChart,
      width = 20.5,
      height = 18,
      units = "cm",
      dpi = 300
    )
  }
)

output$GasSecurityProportion.png <- downloadHandler(
  filename = "GasSecurityProportion.png",
  content = function(file) {
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[c(1,7,8,9,12)]
    
    Data <- Data[complete.cases(Data),]
    
    names(Data) <- c("Year", "Scotland", "NI", "England", "FergusProp")
    
    Data <- subset(Data, Data$Year >= ymd("2015-01-31"))
    
    Data <- subset(Data, Data$Year <= floor_date(max(Data$Year), unit = "month") - days(1))
    
    GasSecurityRolling <- Data
    
    GasSecurityRolling$Year <- as.Date(GasSecurityRolling$Year, format = "%d/%m/%Y")
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: National Grid"
    plottitle = "Proportion of UK Gas from ST Fergus"
    
    #GasSecurityRolling$FergusPropPercentage <- PercentLabel(GasSecurityRolling$FergusProp)
    
    
    GasSecurityRollingChart <- GasSecurityRolling %>%
      ggplot(aes(x = Year), family = "Century Gothic") +
      
      geom_line(
        aes(y = FergusProp,
            label = FergusProp),
        colour = ChartColours[1],
        size = 1,
        family = "Century Gothic"
      ) +
      
      geom_text(
        aes(
          x = Year,
          y = 0,
          label = ifelse(
            Year == max(Year) |
              Year == min(Year),
            format(Year, format = "%b %Y"),
            ""
          ),
          hjust = 0.5,
          vjust = 1.5,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year),
          y = GasSecurityRolling$FergusProp[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))],
          label = percent(GasSecurityRolling$FergusProp[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))], accuracy = 0.1),
          hjust = 0.5,
          vjust = 1.3,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year),
          y = GasSecurityRolling$FergusProp[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))],
          label = percent(GasSecurityRolling$FergusProp[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))], accuracy = 0.1),
          hjust = 0.5,
          vjust = 1.3,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      )
    
    GasSecurityRollingChart
    
    GasSecurityRollingChart <-
      DailyChart(GasSecurityRollingChart,
                 GasSecurityRolling,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    GasSecurityRollingChart <- GasSecurityRollingChart +
      coord_cartesian(xlim = c(min(GasSecurityRolling$Year)-30, max(GasSecurityRolling$Year)+30)) +
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.7,
        linetype = 2
      )
    
    
    GasSecurityRollingChart
    
    ggsave(
      file,
      plot =  GasSecurityRollingChart,
      width = 20,
      height = 18,
      units = "cm",
      dpi = 300
    )
  }
)

output$FullData <- downloadHandler(
  filename = "GasSecurityFullData.csv",
  content = function(file){

    StFergusFlow <- read_delim("Processed Data/Output/Gas Distribution/StFergusFlowDaily.txt", 
                  "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(StFergusFlow) <- c("Date", "Scottish Demand (GWh)","% Scottish demand from St. Fergus", "Transfers to N.I. (GWh)","% N.I. transfers from St. Fergus" , "Transfers to England (GWh)","% Englannd transfers from St. Fergus" , "ST. Fergus (GWh)", "UK Demand (GWh)", "Proportion of U.K. gas supply from ST. Fergus" )
    
    write.csv(StFergusFlow, 
              file,
              row.names = FALSE)
  }
)

}
