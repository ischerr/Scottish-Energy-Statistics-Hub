require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")

###### UI Function ######

source("Structure/Global.R")

DailyDemandOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Daily Demand",
    fluidRow(column(8,
                    h3("Energy use in Scotland per day", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('DailyDemandSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('DailyDemand.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("DailyDemandPlot")),
    plotlyOutput(ns("DailyDemandPlot"))%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Rolling Daily Demand",
             fluidRow(column(8,
                             h3("Energy use in Scotland per day - 12 month rolling average", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('DailyDemandRollingSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('DailyDemandRolling.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("DailyDemandPlot")),
             plotlyOutput(ns("DailyDemandRollingPlot"))%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    )
    ),
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
    fluidRow(
    column(10, h3("Data - 12 month rolling average.", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("DailyDemandTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
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
        SourceLookup("NGElecDemand"),
        SourceLookup("NGData"),
        SourceLookup("BEISOil")
        
      )
    )
  )
}




###### Server ######
DailyDemand <- function(input, output, session) {

  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("DailyDemand.R")

  output$DailyDemandSubtitle <- renderText({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
    
    print("Energy daily demand")
    ###### Daily Demand  #####
    
    # DailyDemand <-
    #   read_csv(
    #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/DailyDemand.csv"
    #   )
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,2,4,3)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    DailyDemand <- Data
    
    paste("Scotland,", format(min(DailyDemand$Year),"%B %Y"),"-", format(max(DailyDemand$Year),"%B %Y"))
  })
  
  output$DailyDemandPlot <- renderPlotly  ({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
    
    print("Energy daily demand")
    ###### Daily Demand  #####
    
    # DailyDemand <-
    #   read_csv(
    #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/DailyDemand.csv"
    #   )
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,2,4,3)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    DailyDemand <- Data
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: National Grid, BEIS"
    plottitle = "Energy use in Scotland per day"
    
    #DailyDemand$GasPercentage <- PercentLabel(DailyDemand$Gas)
    
    
    p <- plot_ly(DailyDemand, x = ~Year) %>% 
      add_trace(y = ~Gas, 
                name = "Gas",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Gas: ",
                  round(DailyDemand$Gas, digits = 1),
                  " GWh\nDate: ",
                  format(DailyDemand$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[2], dash = "none")) %>% 
      add_trace(y = ~Transport, 
                name = "Transport",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Transport: ",
                  round(DailyDemand$Transport, digits = 1),
                  " GWh\nYear: ",
                  format(DailyDemand$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[4], dash = "none")) %>% 
      add_trace(y = ~Electricity, 
                name = "Electricity",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Electricity: ",
                  round(DailyDemand$Electricity, digits = 1),
                  " GWh\nYear: ",
                  format(DailyDemand$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[3], dash = "none")) %>% 
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
  
  output$DailyDemandRollingSubtitle <- renderText({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
    
    print("Energy daily demand")
    ###### Daily Demand  #####
    
    # DailyDemandRolling <-
    #   read_csv(
    #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/DailyDemandRolling.csv"
    #   )
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    DailyDemandRolling <- Data[complete.cases(Data),]
    
    paste("Scotland,", format(min(DailyDemandRolling$Year),"%B %Y"),"-", format(max(DailyDemandRolling$Year),"%B %Y"))
  })
  
  output$DailyDemandRollingPlot <- renderPlotly  ({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
    
    print("Energy daily demand")
    ###### Daily Demand  #####
    
    # DailyDemandRolling <-
    #   read_csv(
    #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/DailyDemandRolling.csv"
    #   )
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    DailyDemandRolling <- Data[complete.cases(Data),]
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day"
    
    #DailyDemandRolling$GasPercentage <- PercentLabel(DailyDemandRolling$Gas)
    
    
    p <- plot_ly(DailyDemandRolling, x = ~Year) %>% 
      add_trace(y = ~Gas, 
                name = "Gas",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Gas: ",
                  round(DailyDemandRolling$Gas, digits = 1),
                  " GWh\nDate: ",
                  format(DailyDemandRolling$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[2], dash = "none")) %>% 
      add_trace(y = ~Transport, 
                name = "Transport",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Transport: ",
                  round(DailyDemandRolling$Transport, digits = 1),
                  " GWh\nYear: ",
                  format(DailyDemandRolling$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[4], dash = "none")) %>% 
      add_trace(y = ~Electricity, 
                name = "Electricity",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Electricity: ",
                  round(DailyDemandRolling$Electricity, digits = 1),
                  " GWh\nYear: ",
                  format(DailyDemandRolling$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[3], dash = "none")) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE),
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
  
  output$DailyDemandTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas (Gwh)", "Transport (GWh)", "Electricity (GWh)")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    DailyDemandRolling <- Data[complete.cases(Data),]
    
    DailyDemandRolling <- DailyDemandRolling %>% 
      mutate(yr_mnth = format(Year, '%Y-%m')) %>% 
      group_by(yr_mnth) %>% 
      filter(Year == max(Year)) %>% 
      mutate(Year = format(Year, "%B %Y"))
    
    names(DailyDemandRolling)[1] <- "12 month ending"
    
    datatable(
      DailyDemandRolling,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = 10,
        searching = TRUE,
        fixedColumns = FALSE,
        columnDefs = list(list(visible=FALSE, targets=c(4))),
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(4, 'desc')),
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
      formatRound(2:5, 1) 
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/6 - System Security/DailyDemand.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("DailyDemandTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$DailyDemand.png <- downloadHandler(
    filename = "DailyDemand.png",
    content = function(file) {
      
      print("Energy daily demand")
      ###### Daily Demand  #####
      
      # DailyDemand <-
      #   read_csv(
      #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/DailyDemand.csv"
      #   )
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "DailyDemandWorking")[c(1,2,4,3)]
      
      names(Data) <- c("Year", "Gas", "Transport", "Electricity")
      
      Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
      
      DailyDemand <- Data
      
      ### variables
      ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: Sheffield, National Grid, BEIS"
      plottitle = "Energy use in Scotland per day"
      
      #DailyDemand$GasPercentage <- PercentLabel(DailyDemand$Gas)
      
      
      DailyDemandChart <- DailyDemand %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = Gas,
              label = Gas),
          colour = ChartColours[2],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(DailyDemand$Year),
          y = max(DailyDemand$Gas),
          label = "Gas",
          hjust = 0.5,
          vjust = 1,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Electricity,
              label = paste0(Electricity * 100, "%")),
          colour = ChartColours[3],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(DailyDemand$Year),
          y = mean(DailyDemand$Electricity),
          label = "Electricity",
          hjust = 0.5,
          vjust = 5.5,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Transport,
              label = paste0(Transport * 100, "%")),
          colour = ChartColours[4],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(DailyDemand$Year),
          y = mean(DailyDemand$Transport),
          label = "Transport",
          hjust = 0.5,
          vjust = 8,
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
        annotate(
          "segment",
          x = DailyDemand$Year[which(DailyDemand$Gas == max(DailyDemand$Gas))]-2,
          xend = DailyDemand$Year[which(DailyDemand$Gas == max(DailyDemand$Gas))] - 30,
          y = max(DailyDemand$Gas),
          yend = max(DailyDemand$Gas),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = DailyDemand$Year[which(DailyDemand$Gas == max(DailyDemand$Gas))] - 35,
          y = max(DailyDemand$Gas),
          label = paste(round(max(DailyDemand$Gas), digits = 0), "GWh"),
          hjust = 1,
          fontface = 2,
          size = 4,
          colour = ChartColours[2],
          family = "Century Gothic"
        )+
        annotate(
          "segment",
          x = DailyDemand$Year[which(DailyDemand$Electricity == max(DailyDemand$Electricity[which(DailyDemand$Year > dmy("01/08/18"))]))]+2,
          xend = DailyDemand$Year[which(DailyDemand$Electricity == max(DailyDemand$Electricity[which(DailyDemand$Year > dmy("01/08/18"))]))] + 30,
          y = max(DailyDemand$Electricity[which(DailyDemand$Year > dmy("01/08/18"))]),
          yend = max(DailyDemand$Electricity[which(DailyDemand$Year > dmy("01/08/18"))]),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = DailyDemand$Year[which(DailyDemand$Electricity == max(DailyDemand$Electricity[which(DailyDemand$Year > dmy("01/08/18"))]))] + 35,
          y = max(DailyDemand$Electricity[which(DailyDemand$Year > dmy("01/08/18"))]),
          label = paste(round(max(DailyDemand$Electricity[which(DailyDemand$Year > dmy("01/08/18"))]), digits = 0), "GWh"),
          hjust = 0,
          fontface = 2,
          size = 4,
          colour = ChartColours[3],
          family = "Century Gothic"
        )
      
      
      DailyDemandChart
      
      DailyDemandChart <-
        DailyChart(DailyDemandChart,
                   DailyDemand,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      DailyDemandChart <- DailyDemandChart +
        coord_cartesian(xlim = c(min(DailyDemand$Year), max(DailyDemand$Year)+30)) +
        ylim(-15, 352) +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      
      DailyDemandChart
      
      ggsave(
        file,
        plot =  DailyDemandChart,
        width = 30,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
)

output$DailyDemandRolling.png <- downloadHandler(
  filename = "DailyDemandRolling.png",
  content = function(file) {
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    DailyDemandRolling <- Data[complete.cases(Data),]
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day\n12 month rolling average"
    
    #DailyDemandRolling$GasPercentage <- PercentLabel(DailyDemandRolling$Gas)
    
    
    DailyDemandRollingChart <- DailyDemandRolling %>%
      ggplot(aes(x = Year), family = "Century Gothic") +
      
      geom_line(
        aes(y = Gas,
            label = Gas),
        colour = ChartColours[2],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(DailyDemandRolling$Year),
        y = max(DailyDemandRolling$Gas),
        label = "Gas Rolling Average",
        hjust = 0.5,
        vjust = 2,
        colour = ChartColours[2],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Electricity,
            label = paste0(Electricity * 100, "%")),
        colour = ChartColours[3],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(DailyDemandRolling$Year),
        y = mean(DailyDemandRolling$Electricity),
        label = "Electricity Rolling Average",
        hjust = 0.5,
        vjust = -1,
        colour = ChartColours[3],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = Transport,
            label = paste0(Transport * 100, "%")),
        colour = ChartColours[4],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(DailyDemandRolling$Year),
        y = mean(DailyDemandRolling$Transport),
        label = "Transport Rolling Average",
        hjust = 0.5,
        vjust = -1,
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
          x = min(Year)-50,
          y = DailyDemandRolling$Gas[which(DailyDemandRolling$Year == min(DailyDemandRolling$Year))],
          label = paste0(round(DailyDemandRolling$Gas[which(DailyDemandRolling$Year == min(DailyDemandRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = DailyDemandRolling$Gas[which(DailyDemandRolling$Year == max(DailyDemandRolling$Year))],
          label = paste0(round(DailyDemandRolling$Gas[which(DailyDemandRolling$Year == max(DailyDemandRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = DailyDemandRolling$Electricity[which(DailyDemandRolling$Year == min(DailyDemandRolling$Year))],
          label = paste0(round(DailyDemandRolling$Electricity[which(DailyDemandRolling$Year == min(DailyDemandRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = DailyDemandRolling$Electricity[which(DailyDemandRolling$Year == max(DailyDemandRolling$Year))],
          label = paste0(round(DailyDemandRolling$Electricity[which(DailyDemandRolling$Year == max(DailyDemandRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = DailyDemandRolling$Transport[which(DailyDemandRolling$Year == min(DailyDemandRolling$Year))],
          label = paste0(round(DailyDemandRolling$Transport[which(DailyDemandRolling$Year == min(DailyDemandRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = DailyDemandRolling$Transport[which(DailyDemandRolling$Year == max(DailyDemandRolling$Year))],
          label = paste0(round(DailyDemandRolling$Transport[which(DailyDemandRolling$Year == max(DailyDemandRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )
    
    DailyDemandRollingChart
    
    DailyDemandRollingChart <-
      DailyChart(DailyDemandRollingChart,
                 DailyDemandRolling,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    DailyDemandRollingChart <- DailyDemandRollingChart +
      coord_cartesian(xlim = c(min(DailyDemandRolling$Year)-30, max(DailyDemandRolling$Year)+30)) +
      ylim(-5,190)+
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.7,
        linetype = 2
      )
    
    
    DailyDemandRollingChart
    
    ggsave(
      file,
      plot =  DailyDemandRollingChart,
      width = 18,
      height = 12,
      units = "cm",
      dpi = 300
    )
  }
)

output$FullData <- downloadHandler(
  filename = "DailyDemandFullData.csv",
  content = function(file){
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,2,4,3)]
    
    names(Data) <- c("Year", "Gas (GWh)", "Transport (GWh)", "Electricity (GWh)")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    DailyDemand <- Data
    
    write.csv(DailyDemand, 
              file,
              row.names = FALSE)
  }
)

}
