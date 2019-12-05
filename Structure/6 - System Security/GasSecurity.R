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
      tabPanel("Daily Demand",
    fluidRow(column(8,
                    h3("Energy use in Scotland per day", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('GasSecuritySubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasSecurity.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("GasSecurityPlot")),
    plotlyOutput(ns("GasSecurityPlot"))%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Rolling Daily Demand",
             fluidRow(column(8,
                             h3("Energy use in Scotland per day - 12 month rolling average", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('GasSecurityRollingSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasSecurityRolling.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("GasSecurityPlot")),
             plotlyOutput(ns("GasSecurityRollingPlot"))%>% withSpinner(color="#5d8be1"),
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
      column(12, dataTableOutput(ns("GasSecurityTable"))%>% withSpinner(color="#5d8be1"))),
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
        SourceLookup("BEISFinalConsump"),
        SourceLookup("ETElecGen"),
        SourceLookup("ESTRenHeat")
        
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
    
    print("Energy daily demand")
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
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: University of Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day"
    
    #GasSecurity$GasPercentage <- PercentLabel(GasSecurity$Gas)
    
    
    p <- plot_ly(GasSecurity, x = ~Year) %>% 
      add_trace(y = ~ScotDemand, 
                name = "ScotDemand",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                text = paste0(
                  "ScotDemand: ",
                  round(GasSecurity$ScotDemand, digits = 1),
                  " GWh\nDate: ",
                  format(GasSecurity$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[2], dash = "none")) %>% 
      add_trace(y = ~NITransfer, 
                name = "NITransfer",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                text = paste0(
                  "NITransfer: ",
                  round(GasSecurity$NITransfer, digits = 1),
                  " GWh\nYear: ",
                  format(GasSecurity$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[4], dash = "none")) %>% 
      add_trace(y = ~EnglandTransfer, 
                name = "EnglandTransfer",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                text = paste0(
                  "EnglandTransfer: ",
                  round(GasSecurity$EnglandTransfer, digits = 1),
                  " GWh\nYear: ",
                  format(GasSecurity$Year, "%d/%m/%Y")
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
                     range = c(min(GasSecurity$Year)-100, max(GasSecurity$Year)+100)),
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
                       sheet = "GasSecurityWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    GasSecurityRolling <- Data[complete.cases(Data),]
    
    paste("Scotland,", format(min(GasSecurityRolling$Year),"%B %Y"),"-", format(max(GasSecurityRolling$Year),"%B %Y"))
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
                       sheet = "GasSecurityWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    GasSecurityRolling <- Data[complete.cases(Data),]
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: University of Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day"
    
    #GasSecurityRolling$GasPercentage <- PercentLabel(GasSecurityRolling$Gas)
    
    
    p <- plot_ly(GasSecurityRolling, x = ~Year) %>% 
      add_trace(y = ~Gas, 
                name = "Gas",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Gas: ",
                  round(GasSecurityRolling$Gas, digits = 1),
                  " GWh\nDate: ",
                  format(GasSecurityRolling$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[2], dash = "none")) %>% 
      add_trace(y = ~Transport, 
                name = "Transport",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Transport: ",
                  round(GasSecurityRolling$Transport, digits = 1),
                  " GWh\nYear: ",
                  format(GasSecurityRolling$Year, "%d/%m/%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 3, color = ChartColours[4], dash = "none")) %>% 
      add_trace(y = ~Electricity, 
                name = "Electricity",
                type = 'scatter',
                mode = 'lines',
                text = paste0(
                  "Electricity: ",
                  round(GasSecurityRolling$Electricity, digits = 1),
                  " GWh\nYear: ",
                  format(GasSecurityRolling$Year, "%d/%m/%Y")
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
  
  output$GasSecurityTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    GasSecurityRolling <- Data[complete.cases(Data),]
    
    GasSecurityRolling <- GasSecurityRolling %>% 
      mutate(yr_mnth = format(Year, '%Y-%m')) %>% 
      group_by(yr_mnth) %>% 
      filter(Year == max(Year)) %>% 
      mutate(Year = format(Year, "%B %Y"))
    
    names(GasSecurityRolling)[1] <- "12 month ending"
    
    datatable(
      GasSecurityRolling,
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
                  tags$p(
                    HTML(
                      "<p>For energy supplies to remain secure, the system must be able to meet&nbsp;<strong>demand&nbsp;</strong>at&nbsp;<strong>peak times</strong>.
                      &nbsp;&nbsp;Hence&nbsp;<strong>seasonality&nbsp;</strong>is key as demand is&nbsp;<strong>higher&nbsp;</strong>in the&nbsp;
                      <strong>winter&nbsp;</strong>than the&nbsp;<strong>summer</strong>.&nbsp;&nbsp;The chart below illustrates this, showing Scotland&rsquo;s 
                      energy use on a daily basis from 2013.&nbsp;&nbsp;It shows that&nbsp;<strong>seasonal variations&nbsp;</strong>in the demand for&nbsp;
                      <strong>electricity&nbsp;</strong>are much&nbsp;<strong>smaller&nbsp;</strong>than they are for&nbsp;<strong>gas</strong>.</p>
                      <p>During the winter of 2018/19&nbsp;<strong>the peak daily demand</strong>&nbsp;for gas was&nbsp;<strong>344 GWh&nbsp;</strong>
                      (on 31st January 2019). This was almost&nbsp;<strong>six times greater&nbsp;</strong>than the minimum gas demand in summer 2018 
                      (<strong>58 GWh&nbsp;</strong>on 28th July 2018).</p><p>In 2018/19,&nbsp;<strong>peak electricity daily demand&nbsp;</strong>
                      (<strong>100 GWh&nbsp;</strong>on the 1st February 2019) was&nbsp;<strong>double&nbsp;</strong>that of&nbsp;<strong>minimum&nbsp;</strong>
                      demand (<strong>51 GWh</strong>&nbsp;on the 20th May 2018).&nbsp;&nbsp;</p>
"
                    )
                  )))
 })
 
 
  observeEvent(input$ToggleTable, {
    toggle("GasSecurityTable")
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
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "GasSecurityWorking")[c(1,2,4,3)]
      
      names(Data) <- c("Year", "Gas", "Transport", "Electricity")
      
      Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
      
      GasSecurity <- Data
      
      ### variables
      ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: University of Sheffield, National Grid, BEIS"
      plottitle = "Energy use in Scotland per day"
      
      #GasSecurity$GasPercentage <- PercentLabel(GasSecurity$Gas)
      
      
      GasSecurityChart <- GasSecurity %>%
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
          x = mean(GasSecurity$Year),
          y = max(GasSecurity$Gas),
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
          x = mean(GasSecurity$Year),
          y = mean(GasSecurity$Electricity),
          label = "Electricity",
          hjust = 0.5,
          vjust = 4.5,
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
          x = mean(GasSecurity$Year),
          y = mean(GasSecurity$Transport),
          label = "Transport",
          hjust = 0.5,
          vjust = 6,
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
          x = GasSecurity$Year[which(GasSecurity$Gas == max(GasSecurity$Gas))]-2,
          xend = GasSecurity$Year[which(GasSecurity$Gas == max(GasSecurity$Gas))] - 30,
          y = max(GasSecurity$Gas),
          yend = max(GasSecurity$Gas),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = GasSecurity$Year[which(GasSecurity$Gas == max(GasSecurity$Gas))] - 35,
          y = max(GasSecurity$Gas),
          label = paste(round(max(GasSecurity$Gas), digits = 0), "GWh"),
          hjust = 1,
          fontface = 2,
          size = 4,
          colour = ChartColours[2],
          family = "Century Gothic"
        )+
        annotate(
          "segment",
          x = GasSecurity$Year[which(GasSecurity$Electricity == max(GasSecurity$Electricity[which(GasSecurity$Year > dmy("01/08/18"))]))]+2,
          xend = GasSecurity$Year[which(GasSecurity$Electricity == max(GasSecurity$Electricity[which(GasSecurity$Year > dmy("01/08/18"))]))] + 30,
          y = max(GasSecurity$Electricity[which(GasSecurity$Year > dmy("01/08/18"))]),
          yend = max(GasSecurity$Electricity[which(GasSecurity$Year > dmy("01/08/18"))]),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = GasSecurity$Year[which(GasSecurity$Electricity == max(GasSecurity$Electricity[which(GasSecurity$Year > dmy("01/08/18"))]))] + 35,
          y = max(GasSecurity$Electricity[which(GasSecurity$Year > dmy("01/08/18"))]),
          label = paste(round(max(GasSecurity$Electricity[which(GasSecurity$Year > dmy("01/08/18"))]), digits = 0), "GWh"),
          hjust = 0,
          fontface = 2,
          size = 4,
          colour = ChartColours[3],
          family = "Century Gothic"
        )
      
      
      GasSecurityChart
      
      GasSecurityChart <-
        DailyChart(GasSecurityChart,
                   GasSecurity,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      GasSecurityChart <- GasSecurityChart +
        coord_cartesian(xlim = c(min(GasSecurity$Year), max(GasSecurity$Year)+30)) +
        ylim(-15, 352) +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      
      GasSecurityChart
      
      ggsave(
        file,
        plot =  GasSecurityChart,
        width = 30,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
)

output$GasSecurityRolling.png <- downloadHandler(
  filename = "GasSecurityRolling.png",
  content = function(file) {
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    GasSecurityRolling <- Data[complete.cases(Data),]
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: University of Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day\n12 month rolling average"
    
    #GasSecurityRolling$GasPercentage <- PercentLabel(GasSecurityRolling$Gas)
    
    
    GasSecurityRollingChart <- GasSecurityRolling %>%
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
        x = mean(GasSecurityRolling$Year),
        y = max(GasSecurityRolling$Gas),
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
        x = mean(GasSecurityRolling$Year),
        y = mean(GasSecurityRolling$Electricity),
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
        x = mean(GasSecurityRolling$Year),
        y = mean(GasSecurityRolling$Transport),
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
          y = GasSecurityRolling$Gas[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$Gas[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = GasSecurityRolling$Gas[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$Gas[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = GasSecurityRolling$Electricity[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$Electricity[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = GasSecurityRolling$Electricity[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$Electricity[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = GasSecurityRolling$Transport[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$Transport[which(GasSecurityRolling$Year == min(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = GasSecurityRolling$Transport[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))],
          label = paste0(round(GasSecurityRolling$Transport[which(GasSecurityRolling$Year == max(GasSecurityRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
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
      coord_cartesian(xlim = c(min(GasSecurityRolling$Year)-30, max(GasSecurityRolling$Year)+30)) +
      ylim(-5,190)+
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
      width = 18,
      height = 12,
      units = "cm",
      dpi = 300
    )
  }
)

output$FullData <- downloadHandler(
  filename = "GasSecurityFullData.csv",
  content = function(file){
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "GasSecurityWorking")[c(1,2,4,3)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    GasSecurity <- Data
    
    write.csv(GasSecurity, 
              file,
              row.names = FALSE)
  }
)

}
