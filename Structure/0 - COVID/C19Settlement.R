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
               4, style = 'padding:15px;'#,
               #downloadButton(ns('C19Settlement.png'), 'Download Graph', style="float:right")
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
               4, style = 'padding:15px;'#,
               #downloadButton(ns('C19SettlementRolling.png'), 'Download Graph', style="float:right")
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
    # fluidRow(
    # column(10, h3("Data - 12 month rolling average.", style = "color: #5d8be1;  font-weight:bold")),
    # column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    # ),
    # fluidRow(
    #   column(12, dataTableOutput(ns("C19SettlementTable"))%>% withSpinner(color="#5d8be1"))),
    # tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
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
    
    paste("Scotland, week commencing 27/04/2020 and equivalent week in 2019")
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
    
    MaxWeek = 18
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[which(ElecDemandHalfHourly$Week == MaxWeek),] 
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly[order(ElecDemandHalfHourly$Date, ElecDemandHalfHourly$SettlementPeriod),]
    
    ElecDemandHalfHourly <- ElecDemandHalfHourly %>% group_by(Year) %>% mutate(id = row_number())
    
    
    ElecDemandHalfHourly  <- dcast(ElecDemandHalfHourly, id + SettlementPeriod + Weekday ~ Year, value.var = 'Total')
    
    ElecDemandHalfHourly$Date <- ymd("2020/04/27")
    
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
    
    MaxWeek = 18
    
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
        
        xaxis = list(title = "Settlement period",
                     showgrid = TRUE),
        yaxis = list(
          title = "MW",
          tickformat = "Settlement Period",
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
  
  output$C19SettlementTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas (Gwh)", "Transport (GWh)", "Electricity (GWh)")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19SettlementRolling <- Data[complete.cases(Data),]
    
    C19SettlementRolling <- C19SettlementRolling %>% 
      mutate(yr_mnth = format(Year, '%Y-%m')) %>% 
      group_by(yr_mnth) %>% 
      filter(Year == max(Year)) %>% 
      mutate(Year = format(Year, "%B %Y"))
    
    names(C19SettlementRolling)[1] <- "12 month ending"
    
    datatable(
      C19SettlementRolling,
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
    filename = "C19Settlement.png",
    content = function(file) {
      
      print("Energy daily demand")
      ###### Daily Demand  #####
      
      # C19Settlement <-
      #   read_csv(
      #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/C19Settlement.csv"
      #   )
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "DailyDemandWorking")[c(1,2,4,3)]
      
      Data <- Data[complete.cases(Data),]
      
      names(Data) <- c("Year", "Gas", "Transport", "Electricity")
      
      Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
      
      C19Settlement <- Data
      
      ### variables
      ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: Sheffield, National Grid, BEIS"
      plottitle = "Energy use in Scotland per day"
      
      #C19Settlement$GasPercentage <- PercentLabel(C19Settlement$Gas)
      
      
      C19SettlementChart <- C19Settlement %>%
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
          x = mean(C19Settlement$Year),
          y = max(C19Settlement$Gas),
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
          x = mean(C19Settlement$Year),
          y = mean(C19Settlement$Electricity),
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
          x = mean(C19Settlement$Year),
          y = mean(C19Settlement$Transport),
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
          x = C19Settlement$Year[which(C19Settlement$Gas == max(C19Settlement$Gas))]-2,
          xend = C19Settlement$Year[which(C19Settlement$Gas == max(C19Settlement$Gas))] - 30,
          y = max(C19Settlement$Gas),
          yend = max(C19Settlement$Gas),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = C19Settlement$Year[which(C19Settlement$Gas == max(C19Settlement$Gas))] - 35,
          y = max(C19Settlement$Gas),
          label = paste(round(max(C19Settlement$Gas), digits = 0), "GWh"),
          hjust = 1,
          fontface = 2,
          size = 4,
          colour = ChartColours[2],
          family = "Century Gothic"
        )+
        annotate(
          "segment",
          x = C19Settlement$Year[which(C19Settlement$Electricity == max(C19Settlement$Electricity[which(C19Settlement$Year > dmy("01/08/18"))]))]+2,
          xend = C19Settlement$Year[which(C19Settlement$Electricity == max(C19Settlement$Electricity[which(C19Settlement$Year > dmy("01/08/18"))]))] + 30,
          y = max(C19Settlement$Electricity[which(C19Settlement$Year > dmy("01/08/18"))]),
          yend = max(C19Settlement$Electricity[which(C19Settlement$Year > dmy("01/08/18"))]),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = C19Settlement$Year[which(C19Settlement$Electricity == max(C19Settlement$Electricity[which(C19Settlement$Year > dmy("01/08/18"))]))] + 35,
          y = max(C19Settlement$Electricity[which(C19Settlement$Year > dmy("01/08/18"))]),
          label = paste(round(max(C19Settlement$Electricity[which(C19Settlement$Year > dmy("01/08/18"))]), digits = 0), "GWh"),
          hjust = 0,
          fontface = 2,
          size = 4,
          colour = ChartColours[3],
          family = "Century Gothic"
        )
      
      
      C19SettlementChart
      
      C19SettlementChart <-
        DailyChart(C19SettlementChart,
                   C19Settlement,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      C19SettlementChart <- C19SettlementChart +
        coord_cartesian(xlim = c(min(C19Settlement$Year), max(C19Settlement$Year)+130)) +
        
        ylim(-15, 352) +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      C19SettlementChart
      
      ggsave(
        file,
        plot =  C19SettlementChart,
        width = 30,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
)

output$C19SettlementRolling.png <- downloadHandler(
  filename = "C19SettlementRolling.png",
  content = function(file) {
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19SettlementRolling <- Data[complete.cases(Data),]
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day\n12 month rolling average"
    
    #C19SettlementRolling$GasPercentage <- PercentLabel(C19SettlementRolling$Gas)
    
    
    C19SettlementRollingChart <- C19SettlementRolling %>%
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
        x = mean(C19SettlementRolling$Year),
        y = max(C19SettlementRolling$Gas),
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
        x = mean(C19SettlementRolling$Year),
        y = mean(C19SettlementRolling$Electricity),
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
        x = mean(C19SettlementRolling$Year),
        y = mean(C19SettlementRolling$Transport),
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
          y = C19SettlementRolling$Gas[which(C19SettlementRolling$Year == min(C19SettlementRolling$Year))],
          label = paste0(round(C19SettlementRolling$Gas[which(C19SettlementRolling$Year == min(C19SettlementRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = C19SettlementRolling$Gas[which(C19SettlementRolling$Year == max(C19SettlementRolling$Year))],
          label = paste0(round(C19SettlementRolling$Gas[which(C19SettlementRolling$Year == max(C19SettlementRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = C19SettlementRolling$Electricity[which(C19SettlementRolling$Year == min(C19SettlementRolling$Year))],
          label = paste0(round(C19SettlementRolling$Electricity[which(C19SettlementRolling$Year == min(C19SettlementRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = C19SettlementRolling$Electricity[which(C19SettlementRolling$Year == max(C19SettlementRolling$Year))],
          label = paste0(round(C19SettlementRolling$Electricity[which(C19SettlementRolling$Year == max(C19SettlementRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = C19SettlementRolling$Transport[which(C19SettlementRolling$Year == min(C19SettlementRolling$Year))],
          label = paste0(round(C19SettlementRolling$Transport[which(C19SettlementRolling$Year == min(C19SettlementRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = C19SettlementRolling$Transport[which(C19SettlementRolling$Year == max(C19SettlementRolling$Year))],
          label = paste0(round(C19SettlementRolling$Transport[which(C19SettlementRolling$Year == max(C19SettlementRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )
    
    C19SettlementRollingChart
    
    C19SettlementRollingChart <-
      DailyChart(C19SettlementRollingChart,
                 C19SettlementRolling,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    C19SettlementRollingChart <- C19SettlementRollingChart +
      coord_cartesian(xlim = c(min(C19SettlementRolling$Year)-30, max(C19SettlementRolling$Year)+30)) +
      ylim(-5,190)+
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.7,
        linetype = 2
      )
    
    
    C19SettlementRollingChart
    
    ggsave(
      file,
      plot =  C19SettlementRollingChart,
      width = 18,
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
    
    names(Data) <- c("Year", "Gas (GWh)", "Transport (GWh)", "Electricity (GWh)")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19Settlement <- Data
    
    write.csv(C19Settlement, 
              file,
              row.names = FALSE)
  }
)

}
