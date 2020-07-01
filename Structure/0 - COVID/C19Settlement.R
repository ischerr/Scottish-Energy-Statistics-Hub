require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")

###### UI Function ######

source("Structure/Global.R")

C19SettlementOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Demand by settlement period",
    fluidRow(column(8,
                    h3("Electricity demand by settlement period", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('C19SettlementSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;', downloadButton(ns('C19Settlement.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("C19SettlementPlot")),
    plotlyOutput(ns("C19SettlementPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Average demand",
             fluidRow(column(8,
                             h3("Average electricity demand by weekday/weekend, post lockdown", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19SettlementRollingSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;', downloadButton(ns('C19SettlementRolling.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19SettlementPlot")),
             plotlyOutput(ns("C19SettlementRollingPlot"))%>% withSpinner(color="#5d8be1"),
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
    tabsetPanel(
      tabPanel("Demand by settlement period",
    fluidRow(
    column(10, h3("Data - Electricity demand by settlement period, week commencing 22/06/20 and equivalent week in 2019.", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("C19SettlementTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Demand by settlement period",
             fluidRow(
               column(10, h3("Data - Average electricity demand by weekday/weekend, post lockdown", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("C19SettlementRollingTable"))%>% withSpinner(color="#5d8be1"))),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
    fluidRow(
      column(2, p(" ")),
      column(2,
             p(" ")),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("NGElecDemand")
        
      )
    )
  )
}




###### Server ######
C19Settlement <- function(input, output, session) {

  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("C19Settlement.R")

  output$C19SettlementSubtitle <- renderText({
    
    paste("Scotland, week commencing 22/06/20 and equivalent week in 2019")
  })
  
  output$C19SettlementPlot <- renderPlotly  ({
    
    library(readr)
    library(lubridate)
    
    ElecDemandHalfHourly <- read_csv("CovidAnalysis/ElecDemandHalfHourly.csv")
    
    names(ElecDemandHalfHourly) <- c("Date", "SettlementPeriod", "Total", "Quarter")
    
    ElecDemandHalfHourly$Date <- ymd(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Year <-year(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2013),]
    
    ElecDemandHalfHourly$Month <-month(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Week <- isoweek(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Weekday <- weekdays(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$DayofYear <- yday(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$PostLockdown <- ifelse(ElecDemandHalfHourly$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2019),]
    
    # MaxWeek <- {x <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year == max(ElecDemandHalfHourly$Year)),] 
    # 
    # x <- x[which(x$Weekday == "Sunday"),]
    # 
    # x <- x[which(x$Week == max(x$Week)),]
    # 
    # max(x$Week)}
    
    MaxWeek = 26
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Week == MaxWeek),] 
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[order(ElecDemandHalfHourly$Date, ElecDemandHalfHourly$SettlementPeriod),]
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly %>% group_by(Year) %>% mutate(id = row_number())
    
    
    ElecDemandHalfHourly  <- dcast(ElecDemandHalfHourly, id + SettlementPeriod + Weekday ~ Year, value.var = 'Total')
    
    ElecDemandHalfHourly$Date <- ymd("2020/05/25")
    
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Tuesday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Tuesday"),]$Date) + 1
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Wednesday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Wednesday"),]$Date) + 2
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Thursday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Thursday"),]$Date) + 3
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Friday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Friday"),]$Date) + 4
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Saturday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Saturday"),]$Date) + 5
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Sunday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Sunday"),]$Date) + 6
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    
    p1 <-  plot_ly(ElecDemandHalfHourly,x = ~ id ) %>% 
      add_trace(data = ElecDemandHalfHourly,
                x = ~ id,
                y = ~ `2020`,
                name = "2020",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "2020 Demand: ",
                  round(ElecDemandHalfHourly$`2020`, digits = 1),
                  " MW\nSettlement Period: ", ElecDemandHalfHourly$SettlementPeriod,
                  "\nDate: ",
                  format(ElecDemandHalfHourly$Date, format="%d/%m/%y")
                ),
                hoverinfo = 'text',
                line = list(width = 4)
      ) %>% 
      add_trace(data = ElecDemandHalfHourly,
                x = ~ id,
                y = ~ `2019`,
                name = "2019",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "2019 demand on equivalent date: ",
                  round(ElecDemandHalfHourly$`2019`, digits = 1),
                  " MW\nSettlement Period: ", ElecDemandHalfHourly$SettlementPeriod,
                  "\nDate: ",
                  format(ElecDemandHalfHourly$Date, format="%d/%m/%y")
                ),
                hoverinfo = 'text',
                line = list(width = 4)
      )  %>% 
      layout(
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'
        ),
        hovername = 'text',
        
        xaxis = list(title = "",
                     showgrid = TRUE,
                     ticktext = list("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 
                     tickvals = list(1, 49,97,145,193,241,289),
                     tickangle = 90,
                     tickmode = "array"),
        yaxis = list(
          title = "MW",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    p1

    
    
   
  })
  
  output$C19SettlementTable = renderDataTable({
    
    library(readr)
    library(lubridate)
    
    ElecDemandHalfHourly <- read_csv("CovidAnalysis/ElecDemandHalfHourly.csv")
    
    names(ElecDemandHalfHourly) <- c("Date", "SettlementPeriod", "Total", "Quarter")
    
    ElecDemandHalfHourly$Date <- ymd(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Year <-year(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2013),]
    
    ElecDemandHalfHourly$Month <-month(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Week <- isoweek(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Weekday <- weekdays(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$DayofYear <- yday(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$PostLockdown <- ifelse(ElecDemandHalfHourly$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2019),]
    
    # MaxWeek <- {x <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year == max(ElecDemandHalfHourly$Year)),] 
    # 
    # x <- x[which(x$Weekday == "Sunday"),]
    # 
    # x <- x[which(x$Week == max(x$Week)),]
    # 
    # max(x$Week)}
    
    MaxWeek = 26
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Week == MaxWeek),] 
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[order(ElecDemandHalfHourly$Date, ElecDemandHalfHourly$SettlementPeriod),]
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly %>% group_by(Year) %>% mutate(id = row_number())
    
    
    ElecDemandHalfHourly  <- dcast(ElecDemandHalfHourly, id + SettlementPeriod + Weekday ~ Year, value.var = 'Total')
    
    ElecDemandHalfHourly$Date <- ymd("2020/05/25")
    
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Tuesday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Tuesday"),]$Date) + 1
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Wednesday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Wednesday"),]$Date) + 2
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Thursday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Thursday"),]$Date) + 3
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Friday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Friday"),]$Date) + 4
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Saturday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Saturday"),]$Date) + 5
    ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Sunday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Sunday"),]$Date) + 6
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    
    ElecDemandHalfHourly$Date <- as.character((hms("00:30:00")*(ElecDemandHalfHourly$SettlementPeriod-1))+ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[6:4]
    
    names(ElecDemandHalfHourly) <- c("Date", "Half hourly demand in 2020 (MW)", "Demand in equivalent half hour in 2019 (MW)")
    
    DT::datatable(
      ElecDemandHalfHourly,
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Electricity demand by settlement period, week commencing 22/06/20 and equivalent week in 2019",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Electricity demand by settlement period, week commencing 22/06/20 and equivalent week in 2019',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Electricity demand by settlement period, week commencing 22/06/20 and equivalent week in 2019')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      ) 
    ) %>%
        formatRound(2:5, 0) %>% 
      formatDate(1,
                 method =  "toLocaleString")
  }) 
   
  output$C19SettlementRollingSubtitle <- renderText({
    
    paste("Scotland, 2020 vs 2019")
  })
  
  output$C19SettlementRollingPlot <- renderPlotly  ({
    library(readr)
    library(lubridate)
    
    ElecDemandHalfHourly <- read_csv("CovidAnalysis/ElecDemandHalfHourly.csv")
    
    names(ElecDemandHalfHourly) <- c("Date", "SettlementPeriod", "Total", "Quarter")
    
    ElecDemandHalfHourly$Date <- ymd(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Year <-year(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2013),]
    
    ElecDemandHalfHourly$Month <-month(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Week <- isoweek(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Weekday <- weekdays(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$DayofYear <- yday(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$PostLockdown <- ifelse(ElecDemandHalfHourly$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2019),]
    
    # MaxWeek <- {x <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year == max(ElecDemandHalfHourly$Year)),] 
    # 
    # x <- x[which(x$Weekday == "Sunday"),]
    # 
    # x <- x[which(x$Week == max(x$Week)),]
    # 
    # max(x$Week)}
    
    MaxWeek = 26
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Week <= MaxWeek),] 
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[order(ElecDemandHalfHourly$Date, ElecDemandHalfHourly$SettlementPeriod),]
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$PostLockdown == "PostLockdown"),]
    
    ElecDemandHalfHourly$Weekend <- ifelse(substr(ElecDemandHalfHourly$Weekday,1,1) == "S", "Weekend", "Weekday")
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly %>% group_by(Year, Weekend, SettlementPeriod) %>% 
      summarise(Total = mean(Total))
    
    ElecDemandHalfHourly$Variable <- paste0(ElecDemandHalfHourly$Year," - ",ElecDemandHalfHourly$Weekend)
    
    ElecDemandHalfHourly <- dcast(ElecDemandHalfHourly, SettlementPeriod ~ Variable, value.var = "Total")
    
    ChartColours <- c("#1f77b4","#fdae6b","#ff7f0e","#9ecae1")
    
    p2 <-  plot_ly(ElecDemandHalfHourly,x = ~ SettlementPeriod ) %>% 
      add_trace(data = ElecDemandHalfHourly,
                x = ~ SettlementPeriod,
                y = ~ `2019 - Weekday`,
                name = "2019 - Weekday",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "2019 - Weekday average demand: ",
                  round(ElecDemandHalfHourly$`2019 - Weekday`, digits = 1),
                  " MW\nSettlement Period: ", ElecDemandHalfHourly$SettlementPeriod
                ),
                hoverinfo = 'text',
                line = list(width = 4, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(data = ElecDemandHalfHourly,
                x = ~ SettlementPeriod,
                y = ~ `2019 - Weekend`,
                name = "2019 - Weekend",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "2019 - Weekend average demand: ",
                  round(ElecDemandHalfHourly$`2019 - Weekend`, digits = 1),
                  " MW\nSettlement Period: ", ElecDemandHalfHourly$SettlementPeriod
                ),
                hoverinfo = 'text',
                line = list(width = 4, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(data = ElecDemandHalfHourly,
                x = ~ SettlementPeriod,
                y = ~ `2020 - Weekday`,
                name = "2020 - Weekday",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "2020 - Weekday average demand: ",
                  round(ElecDemandHalfHourly$`2020 - Weekday`, digits = 1),
                  " MW\nSettlement Period: ", ElecDemandHalfHourly$SettlementPeriod
                ),
                hoverinfo = 'text',
                line = list(width = 4, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(data = ElecDemandHalfHourly,
                x = ~ SettlementPeriod,
                y = ~ `2020 - Weekend`,
                name = "2020 - Weekend",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "4",
                text = paste0(
                  "2020 - Weekend average demand: ",
                  round(ElecDemandHalfHourly$`2020 - Weekend`, digits = 1),
                  " MW\nSettlement Period: ", ElecDemandHalfHourly$SettlementPeriod
                ),
                hoverinfo = 'text',
                line = list(width = 4, color = ChartColours[4], dash = "none")
      ) %>% 
      layout(
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'
        ),
        hovername = 'text',
        
        xaxis = list(title = "\nSettlement period",
                     showgrid = TRUE),
        yaxis = list(
          title = "MW",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    p2
    
    
    
  })
  
  output$C19SettlementRollingTable = renderDataTable({
    
    library(readr)
    library(lubridate)
    
    ElecDemandHalfHourly <- read_csv("CovidAnalysis/ElecDemandHalfHourly.csv")
    
    names(ElecDemandHalfHourly) <- c("Date", "SettlementPeriod", "Total", "Quarter")
    
    ElecDemandHalfHourly$Date <- ymd(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Year <-year(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2013),]
    
    ElecDemandHalfHourly$Month <-month(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Week <- isoweek(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Weekday <- weekdays(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$DayofYear <- yday(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$PostLockdown <- ifelse(ElecDemandHalfHourly$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2019),]
    
    # MaxWeek <- {x <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year == max(ElecDemandHalfHourly$Year)),] 
    # 
    # x <- x[which(x$Weekday == "Sunday"),]
    # 
    # x <- x[which(x$Week == max(x$Week)),]
    # 
    # max(x$Week)}
    
    MaxWeek = 26
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Week <= MaxWeek),] 
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[order(ElecDemandHalfHourly$Date, ElecDemandHalfHourly$SettlementPeriod),]
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$PostLockdown == "PostLockdown"),]
    
    ElecDemandHalfHourly$Weekend <- ifelse(substr(ElecDemandHalfHourly$Weekday,1,1) == "S", "Weekend", "Weekday")
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly %>% group_by(Year, Weekend, SettlementPeriod) %>% 
      summarise(Total = mean(Total))
    
    ElecDemandHalfHourly$Variable <- paste0(ElecDemandHalfHourly$Year," - ",ElecDemandHalfHourly$Weekend)
    
    ElecDemandHalfHourly <- dcast(ElecDemandHalfHourly, SettlementPeriod ~ Variable, value.var = "Total")
    
    names(ElecDemandHalfHourly) <- c("Settlement Period", "2019 - Weekday (MW)",   "2019 - Weekend (MW)",   "2020 - Weekday (MW)",   "2020 - Weekend (MW)")  
    
    DT::datatable(
      ElecDemandHalfHourly,
      extensions = 'Buttons',
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Average electricity demand by weekday/weekend, post lockdown",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average electricity demand by weekday/weekend, post lockdown',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average electricity demand by weekday/weekend, post lockdown')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      ) 
    ) %>%
      formatRound(2:5, 0)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/0 - COVID/C19Settlement.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("C19SettlementTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$C19Settlement.png <- downloadHandler(
    filename = "C19HalfHourlyDemand.png",
    content = function(file) {
      
      library(readr)
      library(lubridate)
      
      ElecDemandHalfHourly <- read_csv("CovidAnalysis/ElecDemandHalfHourly.csv")
      
      names(ElecDemandHalfHourly) <- c("Date", "SettlementPeriod", "Total", "Quarter")
      
      ElecDemandHalfHourly$Date <- ymd(ElecDemandHalfHourly$Date)
      
      ElecDemandHalfHourly$Year <-year(ElecDemandHalfHourly$Date)
      
      ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2013),]
      
      ElecDemandHalfHourly$Month <-month(ElecDemandHalfHourly$Date)
      
      ElecDemandHalfHourly$Week <- isoweek(ElecDemandHalfHourly$Date)
      
      ElecDemandHalfHourly$Weekday <- weekdays(ElecDemandHalfHourly$Date)
      
      ElecDemandHalfHourly$DayofYear <- yday(ElecDemandHalfHourly$Date)
      
      ElecDemandHalfHourly$PostLockdown <- ifelse(ElecDemandHalfHourly$Week >= 13, "PostLockdown", "BeforeLockdown")
      
      ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2019),]
      
      # MaxWeek <- {x <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year == max(ElecDemandHalfHourly$Year)),] 
      # 
      # x <- x[which(x$Weekday == "Sunday"),]
      # 
      # x <- x[which(x$Week == max(x$Week)),]
      # 
      # max(x$Week)}
      
      MaxWeek = 26
      
      ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Week == MaxWeek),] 
      
      ElecDemandHalfHourly <- ElecDemandHalfHourly[order(ElecDemandHalfHourly$Date, ElecDemandHalfHourly$SettlementPeriod),]
      
      ElecDemandHalfHourly <- ElecDemandHalfHourly %>% group_by(Year) %>% mutate(id = row_number())
      
      
      ElecDemandHalfHourly  <- dcast(ElecDemandHalfHourly, id + SettlementPeriod + Weekday ~ Year, value.var = 'Total')
      
      ElecDemandHalfHourly$Date <- ymd("2020/05/25")
      
      ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Tuesday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Tuesday"),]$Date) + 1
      ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Wednesday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Wednesday"),]$Date) + 2
      ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Thursday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Thursday"),]$Date) + 3
      ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Friday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Friday"),]$Date) + 4
      ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Saturday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Saturday"),]$Date) + 5
      ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Sunday"),]$Date <- (ElecDemandHalfHourly[which(ElecDemandHalfHourly$Weekday == "Sunday"),]$Date) + 6
      
      ElecDemandHalfHourly$Year <- ElecDemandHalfHourly$id
      
      width <- 336
      
      ChartColours <- c("#126992", "#1f77b4", "#ff7f0e", "#8da0cb")
      BarColours <- c("#126992", "#1f77b4", "#ff7f0e", "#8da0cb")
      
      ElecDemandHalfHourlyChart <- ElecDemandHalfHourly %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        geom_line(
          aes(y = `2019`,
              label = paste0(`2019` * 100, "%")),
          colour = ChartColours[3],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecDemandHalfHourly$Year),
          y = mean(ElecDemandHalfHourly$`2019`),
          label = "2019",
          hjust = 0.5,
          vjust = -5,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `2020`,
              label = `2020`),
          colour = ChartColours[2],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecDemandHalfHourly$Year),
          y = mean(ElecDemandHalfHourly$`2020`),
          label = "2020",
          hjust = 0.5,
          vjust = 2,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year %in% c(1, 49,97,145,193,241,289),
              substr(ElecDemandHalfHourly$Weekday,1,3),
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 500,
            label = "500\nMW",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        annotate(
          "segment",
          x = min(ElecDemandHalfHourly$Year)-100,
          xend = max(ElecDemandHalfHourly$Year)+100,
          y = 500,
          yend = 500,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 1000,
            label = "1000\nMW",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        annotate(
          "segment",
          x = min(ElecDemandHalfHourly$Year)-100,
          xend = max(ElecDemandHalfHourly$Year)+100,
          y = 1000,
          yend = 1000,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 1500,
            label = "1500\nMW",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        annotate(
          "segment",
          x = min(ElecDemandHalfHourly$Year)-100,
          xend = max(ElecDemandHalfHourly$Year)+100,
          y = 1500,
          yend = 1500,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 2000,
            label = "2000\nMW",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        
        annotate(
          "segment",
          x = min(ElecDemandHalfHourly$Year)-100,
          xend = max(ElecDemandHalfHourly$Year)+100,
          y = 2000,
          yend = 2000,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        ) +
        annotate(
          "segment",
          x = min(ElecDemandHalfHourly$Year)-100,
          xend = max(ElecDemandHalfHourly$Year)+100,
          y = 2500,
          yend = 2500,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 2500,
            label = "2500\nMW",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 3000,
            label = "3000\nMW",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        annotate(
          "segment",
          x = min(ElecDemandHalfHourly$Year)-100,
          xend = max(ElecDemandHalfHourly$Year)+100,
          y = 3000,
          yend = 3000,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        ) +
        geom_text(
          aes(
            x = min(Year)-(width*0.03),
            y = 3500,
            label = "3500\nMW",
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3
        ) +
        annotate(
          "segment",
          x = min(ElecDemandHalfHourly$Year)-100,
          xend = max(ElecDemandHalfHourly$Year)+100,
          y = 3500,
          yend = 3500,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        )+
        annotate(
          "segment",
          x = c(1, 49,97,145,193,241,289),
          xend = c(1, 49,97,145,193,241,289),
          y = 0,
          yend = 10000,
          colour = "grey",
          alpha = 0.4,
          linetype = 2
        )
      
      
      ElecDemandHalfHourlyChart
      
      plottitle = "Electricity demand by settlement period"
      sourcecaption = "Source: National Grid"
      
      ElecDemandHalfHourlyChart <-
        DailyChart(ElecDemandHalfHourlyChart,
                   ElecDemandHalfHourly,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      ElecDemandHalfHourlyChart <- ElecDemandHalfHourlyChart +
        coord_cartesian(xlim = c(min(ElecDemandHalfHourly$Year-1), max(ElecDemandHalfHourly$Year)-13),
                        ylim = c(-40,max(ElecDemandHalfHourly$`2019`)*1.03)) +
        labs(
          title = plottitle,
          face = 2,
          subtitle = paste(
            "Scotland, week commencing 22/06/20 and equivalent week in 2019"
          )
        )+
        
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.9
        )
      
      ElecDemandHalfHourlyChart
      
      ggsave(
        file,
        plot =  ElecDemandHalfHourlyChart,
        width = 30,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
)

output$C19SettlementRolling.png <- downloadHandler(
  filename = "C19AverageElecSettlement.png",
  content = function(file) {
    ElecDemandHalfHourly <- read_csv("CovidAnalysis/ElecDemandHalfHourly.csv")
    
    names(ElecDemandHalfHourly) <- c("Date", "SettlementPeriod", "Total", "Quarter")
    
    ElecDemandHalfHourly$Date <- ymd(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Year <-year(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2013),]
    
    ElecDemandHalfHourly$Month <-month(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Week <- isoweek(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$Weekday <- weekdays(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$DayofYear <- yday(ElecDemandHalfHourly$Date)
    
    ElecDemandHalfHourly$PostLockdown <- ifelse(ElecDemandHalfHourly$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year >= 2019),]
    
    # MaxWeek <- {x <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Year == max(ElecDemandHalfHourly$Year)),] 
    # 
    # x <- x[which(x$Weekday == "Sunday"),]
    # 
    # x <- x[which(x$Week == max(x$Week)),]
    # 
    # max(x$Week)}
    
    MaxWeek = 26
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Week <= MaxWeek),] 
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[order(ElecDemandHalfHourly$Date, ElecDemandHalfHourly$SettlementPeriod),]
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$PostLockdown == "PostLockdown"),]
    
    ElecDemandHalfHourly$Weekend <- ifelse(substr(ElecDemandHalfHourly$Weekday,1,1) == "S", "Weekend", "Weekday")
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly %>% group_by(Year, Weekend, SettlementPeriod) %>% 
      summarise(Total = mean(Total))
    
    ElecDemandHalfHourly$Variable <- paste0(ElecDemandHalfHourly$Year," - ",ElecDemandHalfHourly$Weekend)
    
    ElecDemandHalfHourly <- dcast(ElecDemandHalfHourly, SettlementPeriod ~ Variable, value.var = "Total")
    
    ChartColours <- c("#1f77b4","#fdae6b","#ff7f0e","#9ecae1")
    
    ElecDemandHalfHourly$Year <- ElecDemandHalfHourly$SettlementPeriod
    width <- 48
    
    ChartColours <- c("#126992", "#1f77b4", "#ff7f0e", "#8da0cb", "#fdae6b")
    BarColours <- c("#126992", "#1f77b4", "#ff7f0e", "#8da0cb")
    
    ElecDemandHalfHourlyChart <- ElecDemandHalfHourly %>%
      ggplot(aes(x = Year), family = "Century Gothic") +
      geom_line(
        aes(y = `2019 - Weekday`,
            label = paste0(`2019 - Weekday` * 100, "%")),
        colour = ChartColours[3],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(ElecDemandHalfHourly$Year),
        y = mean(ElecDemandHalfHourly$`2019 - Weekday`),
        label = "2019 - Weekday",
        hjust = 0.5,
        vjust = -6,
        colour = ChartColours[3],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = `2020 - Weekday`,
            label = `2020 - Weekday`),
        colour = ChartColours[2],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(ElecDemandHalfHourly$Year),
        y = mean(ElecDemandHalfHourly$`2020 - Weekday`),
        label = "2020 - Weekday",
        hjust = 0.5,
        vjust = 2,
        colour = ChartColours[2],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = `2019 - Weekend`,
            label = paste0(`2019 - Weekend` * 100, "%")),
        colour = ChartColours[5],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(ElecDemandHalfHourly$Year),
        y = mean(ElecDemandHalfHourly$`2019 - Weekend`),
        label = "2019 - Weekend",
        hjust = 0.5,
        vjust = -6,
        colour = ChartColours[5],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = `2020 - Weekend`,
            label = `2020 - Weekend`),
        colour = ChartColours[4],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(ElecDemandHalfHourly$Year),
        y = mean(ElecDemandHalfHourly$`2020 - Weekend`),
        label = "2020 - Weekend",
        hjust = 0.5,
        vjust = 2.2,
        colour = ChartColours[4],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year,
          y = 0,
          label = ifelse(
            Year %in% c(1, 12, 24, 36, 48),
            ElecDemandHalfHourly$Year,
            ""
          ),
          hjust = 0.5,
          vjust = 1.5,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = min(Year)-(width*0.03),
          y = 500,
          label = "500\nMW",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      ) +
      annotate(
        "segment",
        x = min(ElecDemandHalfHourly$Year)-100,
        xend = max(ElecDemandHalfHourly$Year)+100,
        y = 500,
        yend = 500,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(Year)-(width*0.03),
          y = 1000,
          label = "1000\nMW",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      ) +
      annotate(
        "segment",
        x = min(ElecDemandHalfHourly$Year)-100,
        xend = max(ElecDemandHalfHourly$Year)+100,
        y = 1000,
        yend = 1000,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(Year)-(width*0.03),
          y = 1500,
          label = "1500\nMW",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      ) +
      annotate(
        "segment",
        x = min(ElecDemandHalfHourly$Year)-100,
        xend = max(ElecDemandHalfHourly$Year)+100,
        y = 1500,
        yend = 1500,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(Year)-(width*0.03),
          y = 2000,
          label = "2000\nMW",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      ) +
      
      annotate(
        "segment",
        x = min(ElecDemandHalfHourly$Year)-100,
        xend = max(ElecDemandHalfHourly$Year)+100,
        y = 2000,
        yend = 2000,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      annotate(
        "segment",
        x = min(ElecDemandHalfHourly$Year)-100,
        xend = max(ElecDemandHalfHourly$Year)+100,
        y = 2500,
        yend = 2500,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(Year)-(width*0.03),
          y = 2500,
          label = "2500\nMW",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      ) +
      geom_text(
        aes(
          x = min(Year)-(width*0.03),
          y = 3000,
          label = "3000\nMW",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      ) +
      annotate(
        "segment",
        x = min(ElecDemandHalfHourly$Year)-100,
        xend = max(ElecDemandHalfHourly$Year)+100,
        y = 3000,
        yend = 3000,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(Year)-(width*0.03),
          y = 3500,
          label = "3500\nMW",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      ) +
      annotate(
        "segment",
        x = min(ElecDemandHalfHourly$Year)-100,
        xend = max(ElecDemandHalfHourly$Year)+100,
        y = 3500,
        yend = 3500,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      )
    # )+
    # annotate(
    #   "segment",
    #   x = c(1, 49,97,145,193,241,289),
    #   xend = c(1, 49,97,145,193,241,289),
    #   y = 0,
    #   yend = 10000,
    #   colour = "grey",
    #   alpha = 0.4,
    #   linetype = 2
    # )
    
    
    ElecDemandHalfHourlyChart
    
    plottitle = "Average electricity demand by weekday/weekend, post lockdown"
    sourcecaption = "Source: National Grid"
    
    ElecDemandHalfHourlyChart <-
      DailyChart(ElecDemandHalfHourlyChart,
                 ElecDemandHalfHourly,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    ElecDemandHalfHourlyChart <- ElecDemandHalfHourlyChart +
      coord_cartesian(xlim = c(min(ElecDemandHalfHourly$Year-1), max(ElecDemandHalfHourly$Year)-1),
                      ylim = c(-45,max(ElecDemandHalfHourly$`2019 - Weekday`)*1.14)) +
      labs(
        title = plottitle,
        face = 2,
        subtitle = paste(
          "Scotland, 2020 vs 2019"
        )
      )+
      
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.9
      )
    
    ElecDemandHalfHourlyChart
    
    ggsave(
      file,
      plot =  ElecDemandHalfHourlyChart,
      width = 30,
      height = 12,
      units = "cm",
      dpi = 300
    )
  }
)

output$FullData <- downloadHandler(
  filename = "C19SettlementFullData.csv",
  content = function(file){
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,2,4,3)]
    
    names(Data) <- c("Year", "Gas (MW)", "Transport (MW)", "Electricity (MW)")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19Settlement <- Data
    
    write.csv(C19Settlement, 
              file,
              row.names = FALSE)
  }
)

}
