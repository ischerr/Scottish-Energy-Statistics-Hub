require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")

###### UI Function ######



C19ElecOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Daily demand",
               fluidRow(column(8,
                               h3("Daily electricity demand, annual comparison", style = "color: #5d8be1;  font-weight:bold"),
                               h4(textOutput(ns('C19ElecRollingSubtitle')), style = "color: #5d8be1;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('C19ElecRolling.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
               #dygraphOutput(ns("C19ElecPlot")),
               plotlyOutput(ns("C19ElecRollingPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
               HTML("<blockquote><p>Dates taken from the Scottish Parliament Information Centre <a href='https://spice-spotlight.scot/2021/01/08/timeline-of-coronavirus-covid-19-in-scotland/' target='_blank' rel='noopener'>Timeline of Coronavirus</a></p></blockquote>"),
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
      ),
    tabPanel("Average Annual Demand",
             fluidRow(column(8,
                             h3("Average daily electricity demand", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19Elec2Subtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;', downloadButton(ns('C19Elec2.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19ElecPlot")),
             plotlyOutput(ns("C19Elec2Plot"), height = "600px")%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    # ),
    # tabPanel("Weekday Demand - 2021 Comparison",
    #          fluidRow(column(8,
    #                          h3("Average weekday demand compared to same period in previous years", style = "color: #5d8be1;  font-weight:bold"),
    #                          h4(textOutput(ns('C19ElecSubtitle')), style = "color: #5d8be1;")
    #          ),
    #          column(
    #            4, style = 'padding:15px;', downloadButton(ns('C19Elec.png'), 'Download Graph', style="float:right")
    #          )),
    #          
    #          tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #          #dygraphOutput(ns("C19ElecPlot")),
    #          plotlyOutput(ns("C19ElecPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
    #          tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
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
    tabPanel("Daily demand",
             fluidRow(
               column(10, h3("Data - Daily electricity demand", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("C19ElecRollingTable"))%>% withSpinner(color="#5d8be1"))),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Average daily demand",
             fluidRow(
               column(10, h3("Data - Average daily electricity demand (GWh)", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("C19ElecTable"))%>% withSpinner(color="#5d8be1"))),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
    fluidRow(
      column(2, p(" ")),
      column(2,
             p(" ")),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("NGElecDemand"),
        SourceLookup("SPICeCovid")
        
      )
    )
  )
}




###### Server ######
C19Elec <- function(input, output, session) {

  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("C19Elec.R")

  output$C19ElecSubtitle <- renderText({
    
    paste("Scotland, 2013 - 2021")
  })
  
  output$C19Elec2Subtitle <- renderText({
    
    paste("Scotland, 2013 - 2020")
  })
  
  output$C19ElecPlot <- renderPlotly  ({
    
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 1, "PostLockdown", "BeforeLockdown")
    
    #DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 23, "Phase 1, 2 & 3", DailyDemand$PostLockdown)
    
    WeekdayElecDemand <- DailyDemand
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
    
    #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
    
    maxweek <- max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week[-1])
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Week <= maxweek),]
    
    WeekdayElecDemand <- WeekdayElecDemand %>% group_by(Year) %>% 
      summarise(Electricity = mean(Electricity))
  
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#045a8d", "#a6bddb", "#3690c0",  "#8da0cb")
    
    WeekdayElecDemand$YearFormat <- paste0("<b>", WeekdayElecDemand$Year, "</b>")
    
    p1 <-  plot_ly(WeekdayElecDemand, x = ~ Year ) %>%  
      add_trace(y = ~ `Electricity`,
                name = "Weekday Demand",
                type = 'bar',
                legendgroup = "2",
                marker = list(color = BarColours[2]),
                text = paste0(
                  "Average Weekday Demand: ", format(round(WeekdayElecDemand$`Electricity`, 2), big.mark = ",")," GWh\n",
                  "Year: ", WeekdayElecDemand$Year, "\n"),
                hoverinfo = 'text',
                line = list(width = 4)
      ) %>% 
      layout(
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "GWh",
                     zeroline = FALSE,
                     tickformat = "",
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        )
      ) %>% 
      config(displayModeBar = F)
    
    p1

    
    
   
  })
  
  output$C19Elec2Plot <- renderPlotly  ({
    
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013 & DailyDemand$Year < 2021),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 1, "PostLockdown", "BeforeLockdown")
    
    WeekdayElecDemand <- DailyDemand
    
    #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
    
    #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
    
    maxweek <- 56 #max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week)
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Week <= maxweek),]
    
    WeekdayElecDemand <- WeekdayElecDemand %>% group_by(Year, PostLockdown) %>% 
      summarise(Electricity = mean(Electricity))
    
    WeekdayElecDemand <- dcast(WeekdayElecDemand, Year ~ PostLockdown)
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    
    WeekdayElecDemand$YearFormat <- paste0("<b>", WeekdayElecDemand$Year, "</b>")
    
    p1 <-  plot_ly(WeekdayElecDemand, x = ~ Year ) %>%  

      add_trace(y = ~ `PostLockdown`,
                name = "Electricity Demand After Lockdown",
                type = 'bar',
                legendgroup = "2",
                text = ifelse(WeekdayElecDemand$Year == 2021, paste0(
                  "Average Weekday Electricity Demand: ", format(round(WeekdayElecDemand$`PostLockdown`, 2), big.mark = ",")," GWh\n",
                  "Year: ", WeekdayElecDemand$Year, " so far\n"),paste0(
                    "Average Weekday Electricity Demand: ", format(round(WeekdayElecDemand$`PostLockdown`, 2), big.mark = ",")," GWh\n",
                    "Year: ", WeekdayElecDemand$Year, "\n")),
                hoverinfo = 'text',
                line = list(width = 4)
      ) %>% 
      layout(
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "GWh",
                     zeroline = FALSE,
                     tickformat = "",
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        xaxis = list(
          title = "",
          showgrid = FALSE,
          zeroline = FALSE
        )
      ) %>% 
      config(displayModeBar = F)
    
    p1
    
    
    
    
  })
  
  output$C19ElecRollingSubtitle <- renderText({
    
    paste("Scotland, 2019 - 2021")
  })
  
  output$C19ElecRollingPlot <- renderPlotly  ({
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    
    ChartColours <- c("#fc4e2a", "#1d91c0", "#bdbdbd", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$ChartYear <- substr(ISOweek(DailyDemand$Date),1,4)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[-which(substr(DailyDemand$Date,6,10) == "02-29"),]
    
    #DailyDemand <- DailyDemand[c(5,6,7,9,1,8,4)]
    
    DailyDemand <- DailyDemand %>% group_by(Year) %>% mutate(id = row_number()-1)
    
    DailyDemand$NewDate <- ymd("2021/01/01") + DailyDemand$id
    
    DailyDemand  <- dcast(DailyDemand, NewDate ~ Year, value.var = 'Electricity')
    
    vline1 <- function(x = 0, color = "#74a9cf") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color)
      )}
    
    vline2 <- function(x = 0, color = "#74a9cf") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color, dash = "dash")
      )}
      
      vline3 <- function(x = 0, color = "#ef6548") {
        list(
          type = "line", 
          y0 = 0, 
          y1 = 1, 
          yref = "paper",
          x0 = x, 
          x1 = x, 
          line = list(color = color)
        )}
      
    
    
    p2 <-  plot_ly(DailyDemand,x = ~ Date ) %>% 

      add_trace(data = DailyDemand,
                x = ~ NewDate,
                y = ~ `2019`,
                name = "2019",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Demand: ",
                  round(DailyDemand$`2019`, digits = 1),
                  " GWh\n", weekdays(ymd(paste0("2019-",substr(DailyDemand$NewDate,6,10)))), " ",
                  format(ymd(paste0("2019-",substr(DailyDemand$NewDate,6,10))), format="%d %B %Y")
                ),
                hoverinfo = 'text',
                colour = ChartColours[3],
                line = list(width = 3,
                            color = ChartColours[3])
      )  %>% 
      
      add_trace(data = DailyDemand,
                x = ~ NewDate,
                y = ~ `2020`,
                name = "2020",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Demand: ",
                  round(DailyDemand$`2020`, digits = 1),
                  " GWh\n", weekdays(ymd(paste0("2020-",substr(DailyDemand$NewDate,6,10)))), " ",
                  format(ymd(paste0("2020-",substr(DailyDemand$NewDate,6,10))), format="%d %B %Y")
                ),
                hoverinfo = 'text',
                colour = ChartColours[2],
                line = list(width = 3,
                            color = ChartColours[2])
      )  %>% 
      add_trace(data = DailyDemand,
                x = ~ NewDate,
                y = ~ `2021`,
                name = "2021",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Demand: ",
                  round(DailyDemand$`2021`, digits = 1),
                  " GWh\n", weekdays(ymd(paste0("2021-",substr(DailyDemand$NewDate,6,10)))), " ",
                  format(ymd(paste0("2021-",substr(DailyDemand$NewDate,6,10))), format="%d %B %Y")
                ),
                hoverinfo = 'text',
                colour = ChartColours[1],
                line = list(width = 3,
                            color = ChartColours[1])
      ) %>% 

      
      add_annotations(
        x = dmy("25/03/2021"),
        y = 30,
        text = "<b>24/03/2020</b>\nNational\nLockdown",
        align = 'left',
        xanchor = 'left',
        font = list(color = "#74a9cf",
                    family = "Century Gothic"),
        showarrow = FALSE
      ) %>% 
      add_annotations(
        x = dmy("29/05/2021"),
        y = 30,
        text = "<b>28/05/2020</b>\nPhased\nRoutemap",
        align = 'left',
        xanchor = 'left',
        font = list(color = "#74a9cf",
                    family = "Century Gothic"),
        showarrow = FALSE
      ) %>% 
      add_annotations(
        x = dmy("24/10/2021"),
        y = 30,
        text = "<b>24/10/2020</b>\nFive-level\nStrategic\nFramework",
        align = 'left',
        xanchor = 'left',
        font = list(color = "#74a9cf",
                    family = "Century Gothic"),
        showarrow = FALSE
      ) %>% 
      add_annotations(
        x = dmy("05/01/2021"),
        y = 30,
        text = "<b>04/01/2021</b>\nMainland\nLockdown",
        align = 'left',
        xanchor = 'left',
        font = list(color = "#ef6548",
                    family = "Century Gothic"),
        showarrow = FALSE
      ) %>% 
      layout(
        barmode = 'stack',
        shapes = list( vline1(dmy("24/03/2021")),vline2(dmy("28/05/2021")),vline1(dmy("23/10/2021")),vline3(dmy("04/01/2021"))),
        bargap = 0.66,
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "Date in 2021",
                     showgrid = FALSE,
                     tickfont = list(size = 15)),
        yaxis = list(
          title = "GWh",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = "#5d8be1",
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    p2
    
    
  })
  
  output$C19ElecTable = renderDataTable({
    
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013 & DailyDemand$Year < 2021),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 1, "PostLockdown", "BeforeLockdown")
    
    WeekdayElecDemand <- DailyDemand
    
    #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
    
    #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
    
    maxweek <- 56 #max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week)
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Week <= maxweek),]
    
    WeekdayElecDemand <- WeekdayElecDemand %>% group_by(Year, PostLockdown) %>% 
      summarise(Electricity = mean(Electricity))
    
    WeekdayElecDemand <- dcast(WeekdayElecDemand, Year ~ PostLockdown)
    
    names(WeekdayElecDemand)[1:2] <- c("Year", "Demand (GWh)")
    datatable(
      WeekdayElecDemand,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = 10,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        title = "Average daily electricity demand (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average daily electricity demand (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average daily electricity demand (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:3, 1) 
  })
  
  output$C19ElecRollingTable = renderDataTable({
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2019),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    names(DailyDemand)[4] <- c("Daily electricity demand(GWh)")
    
    datatable(
      DailyDemand[c(1,4)],
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
        title = "Daily electricity demand (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Daily electricity demand (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Daily electricity demand (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c( -1, 10, 20) # declare values
                           , c("All", 10, 20 ) # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2, 1) 
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/6 - System Security/C19Elec.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("C19ElecTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("C19ElecRollingTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$C19Elec.png <- downloadHandler(
    filename = "C19ElecAverage.png",
    content = function(file) {
      
      
      library(readr)
      library(ISOweek)
      library(lubridate)
      library(zoo)
      library(plotly)
      
      plottitle <-
        "Average daily electricity demand."
      sourcecaption <- "Source: National Grid"
      
      DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      DailyDemand$Date <- ymd(DailyDemand$Date)
      
      DailyDemand$Year <-year(DailyDemand$Date)
      
      DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
      
      DailyDemand <- DailyDemand[which(DailyDemand$Year < 2021),]
      
      DailyDemand$Month <-month(DailyDemand$Date)
      
      DailyDemand$Week <- isoweek(DailyDemand$Date)
      
      DailyDemand$Weekday <- weekdays(DailyDemand$Date)
      
      DailyDemand$DayofYear <- yday(DailyDemand$Date)
      
      DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 1, "Post-Lockdown", "Pre-Lockdown")
      
      WeekdayElecDemand <- DailyDemand
      
      #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
      
     maxweek <- max(WeekdayElecDemand[which(WeekdayElecDemand$Year == max(WeekdayElecDemand$Year)),]$Week[-1])
      
      WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Week <= maxweek),]
      
      WeekdayElecDemand <- WeekdayElecDemand %>% group_by(Year, PostLockdown) %>% 
        summarise(Electricity = mean(Electricity))
      
      #WeekdayElecDemand <- as.data.frame(dcast(WeekdayElecDemand, Year ~ PostLockdown))
      
      ChartColours <- c("#126992", "#2078b4", "#ff7f0e", "#8da0cb")
      BarColours <- c("#045a8d", "#a6bddb", "#3690c0",  "#8da0cb")
      
      WeekdayElecDemandChart <- WeekdayElecDemand  %>%
        ggplot(aes(x = Year, y = Electricity, fill = PostLockdown), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Post-Lockdown" = BarColours[1],
            "Pre-Lockdown" = BarColours[1],
            "Phase 1, 2 & 3" = BarColours[3]
          )
        ) +
        geom_bar(position = "dodge",
                 stat = "identity",
                 width = -.8) +
        geom_text(position = position_dodge(width = -.8),
                  aes(
                    y = Electricity + 4,
                    fill = PostLockdown,
                    label = paste(format(round(Electricity, digits = 1), big.mark = ","), "\nGWh")
                  ),
                  fontface = 2,
                  colour =  ChartColours[1],
                  family = "Century Gothic",
                  size = 3) +
        annotate(
          "text",
          x = WeekdayElecDemand$Year,
          y = -3,
          label = WeekdayElecDemand$Year,
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        )
      
      WeekdayElecDemandChart <-
        StackedArea(WeekdayElecDemandChart,
                    WeekdayElecDemand,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      WeekdayElecDemandChart
      
      ggsave(
        file,
        plot = WeekdayElecDemandChart,
        width = 26,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  
  output$C19Elec2.png <- downloadHandler(
    filename = "C19ElecAverage.png",
    content = function(file) {
      
      
      library(readr)
      library(ISOweek)
      library(lubridate)
      library(zoo)
      library(plotly)
      
      plottitle <-
        "Average daily electricity demand."
      
      sourcecaption <- "Source: National Grid"
      
      DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      DailyDemand$Date <- ymd(DailyDemand$Date)
      
      DailyDemand$Year <-year(DailyDemand$Date)
      
      DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013 & DailyDemand$Year < 2021),]
      
      DailyDemand$Month <-month(DailyDemand$Date)
      
      DailyDemand$Week <- isoweek(DailyDemand$Date)
      
      DailyDemand$Weekday <- weekdays(DailyDemand$Date)
      
      DailyDemand$DayofYear <- yday(DailyDemand$Date)
      
      DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 1, "PostLockdown", "BeforeLockdown")
      
      WeekdayElecDemand <- DailyDemand
      
      #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
      
      #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
      
      maxweek <- 56 #max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week)
      
      WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Week <= maxweek),]
      
      WeekdayElecDemand <- WeekdayElecDemand %>% group_by(Year, PostLockdown) %>% 
        summarise(Electricity = mean(Electricity))
      
      #WeekdayElecDemand <- as.data.frame(dcast(WeekdayElecDemand, Year ~ PostLockdown))
      
      ChartColours <- c("#126992", "#2078b4", "#ff7f0e", "#8da0cb")
      BarColours <- c("#126992", "#2078b4", "#ff7f0e", "#8da0cb")
      
      WeekdayElecDemandChart <- WeekdayElecDemand  %>%
        ggplot(aes(x = Year, y = Electricity, fill = PostLockdown), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "PostLockdown" = BarColours[2]
          )
        ) +
        geom_bar(position = "dodge",
                 stat = "identity",
                 width = -.8) +
        geom_text(position = position_dodge(width = -.8),
                  aes(
                    y = Electricity + 4,
                    fill = PostLockdown,
                    label = paste(format(round(Electricity, digits = 1), big.mark = ","), "\nGWh")
                  ),
                  fontface = 2,
                  colour =  ChartColours[1],
                  family = "Century Gothic",
                  size = 3) +
        annotate(
          "text",
          x = WeekdayElecDemand$Year,
          y = -3,
          label = WeekdayElecDemand$Year,
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        )
      
      WeekdayElecDemandChart <-
        StackedArea(WeekdayElecDemandChart,
                    WeekdayElecDemand,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      WeekdayElecDemandChart
      
      ggsave(
        file,
        plot = WeekdayElecDemandChart,
        width = 20,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
)

output$C19ElecRolling.png <- downloadHandler(
  filename = "C19DailyDemand.png",
  content = function(file) {
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    
    ChartColours <- c("#126992", "#1d91c0", "#bdbdbd", "#8da0cb")
    BarColours <- c("#fc4e2a", "#1d91c0", "#bdbdbd", "#8da0cb")
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$ChartYear <- substr(ISOweek(DailyDemand$Date),1,4)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[-which(substr(DailyDemand$Date,6,10) == "02-29"),]
    
    #DailyDemand <- DailyDemand[c(5,6,7,9,1,8,4)]
    
    DailyDemand <- DailyDemand %>% group_by(Year) %>% mutate(id = row_number()-1)
    
    DailyDemand$NewDate <- ymd("2021/01/01") + DailyDemand$id
    
    DailyDemand  <- dcast(DailyDemand, NewDate ~ Year, value.var = 'Electricity')
    
    DailyDemand$NewDate <- ymd(DailyDemand$NewDate)
    
    DailyDemand$Year <- ymd(DailyDemand$NewDate)
    
    width <- max(DailyDemand$NewDate) - min(DailyDemand$NewDate)
    
    DailyDemandChart <- DailyDemand %>%
      ggplot(aes(x = NewDate), family = "Century Gothic") +
      geom_line(
        aes(y = `2019`,
            label = paste0(`2019` * 100, "%")),
        colour = BarColours[3],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(DailyDemand$NewDate),
        y = 80,
        label = "2019",
        hjust = 0.5,
        colour = BarColours[3],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = `2020`,
            label = `2020`),
        colour = BarColours[2],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(DailyDemand$NewDate),
        y = 45,
        label = "2021",
        hjust = 0.5,
        colour = BarColours[1],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = `2021`,
            label = `2021`),
        colour = BarColours[1],
        size = 1,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(DailyDemand$NewDate),
        y = 50,
        label = "2020",
        hjust = 0.5,
        colour = BarColours[2],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = NewDate,
          y = 0,
          label = ifelse(
            NewDate == max(NewDate) |
              NewDate == min(NewDate),
            format(NewDate, format = "%d %b"),
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
        x = min(DailyDemand$NewDate)-100,
        xend = max(DailyDemand$NewDate)+100,
        y = 25,
        yend = 25,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(NewDate)-(width*0.03),
          y = 25,
          label = "25\nGWh",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      )+
      geom_text(
        aes(
          x = min(NewDate)-(width*0.03),
          y = 50,
          label = "50\nGWh",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      )+
      annotate(
        "segment",
        x = min(DailyDemand$NewDate)-100,
        xend = max(DailyDemand$NewDate)+100,
        y = 50,
        yend = 50,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(NewDate)-(width*0.03),
          y = 75,
          label = "75\nGWh",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      )+
      annotate(
        "segment",
        x = min(DailyDemand$NewDate)-100,
        xend = max(DailyDemand$NewDate)+100,
        y = 75,
        yend = 75,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(DailyDemand$NewDate)-(width*0.03),
          y = 100,
          label = "100\nGWh",
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic",
        size = 3
      ) +
      annotate(
        "segment",
        x = min(DailyDemand$NewDate)-100,
        xend = max(DailyDemand$NewDate)+100,
        y = 100,
        yend = 100,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = dmy("24/03/2021"),
          y = 40,
          label = "24/03/2020",
          fontface = 2
        ),
        hjust = 0,
        vjust = -.2,
        colour = "#74a9cf",
        family = "Century Gothic",
        size = 3
      )+
      geom_text(
        aes(
          x = dmy("24/03/2021"),
          y = 40,
          label = "National\nLockdown "
        ),
        vjust = 1.1,
        hjust = 0,
        colour = "#74a9cf",
        family = "Century Gothic",
        size = 3
      )+
      annotate(
        "segment",
        x = dmy("23/03/2021"),
        xend = dmy("23/03/2021"),
        y = -0,
        yend = 1000,
        colour = "#74a9cf",
        alpha = 0.9
      ) +
      
      
      
      
      
      geom_text(
        aes(
          x = dmy("29/05/2021"),
          y = 40,
          label = "28/05/2020",
          fontface = 2
        ),
        hjust = 0,
        vjust = -.2,
        colour = "#74a9cf",
        family = "Century Gothic",
        size = 3
      )+
      geom_text(
        aes(
          x = dmy("29/05/2021"),
          y = 40,
          label = "Phased\nRoutemap "
        ),
        vjust = 1.1,
        hjust = 0,
        colour = "#74a9cf",
        family = "Century Gothic",
        size = 3
      )+
      annotate(
        "segment",
        x = dmy("28/05/2021"),
        xend = dmy("28/05/2021"),
        y = 0,
        yend = 1000,
        colour = "#74a9cf",
        alpha = 0.9,
        linetype = 2
      ) +
      
      
      
      
      
      geom_text(
        aes(
          x = dmy("25/10/2021"),
          y = 40,
          label = "24/10/2020",
          fontface = 2
        ),
        hjust = 0,
        vjust = -.2,
        colour = "#74a9cf",
        family = "Century Gothic",
        size = 3
      )+
      geom_text(
        aes(
          x = dmy("25/10/2021"),
          y = 40,
          label = "Five-Level\nStrategic Framework "
        ),
        vjust = 1.1,
        hjust = 0,
        colour = "#74a9cf",
        family = "Century Gothic",
        size = 3
      )+
      annotate(
        "segment",
        x = dmy("24/10/2021"),
        xend = dmy("24/10/2021"),
        y = 0,
        yend = 1000,
        colour = "#74a9cf",
        alpha = 0.9
      ) +
      
      
      
      
      
      geom_text(
        aes(
          x = dmy("05/01/2021"),
          y = 40,
          label = "04/01/2021",
          fontface = 2
        ),
        hjust = 0,
        vjust = -.2,
        colour = "#ef6548",
        family = "Century Gothic",
        size = 3
      )+
      geom_text(
        aes(
          x = dmy("05/01/2021"),
          y = 40,
          label = "Mainland\nLockdown"
        ),
        vjust = 1.1,
        hjust = 0,
        colour = "#ef6548",
        family = "Century Gothic",
        size = 3
      )+
      annotate(
        "segment",
        x = dmy("04/01/2021"),
        xend = dmy("04/01/2021"),
        y = 0,
        yend = 1000,
        colour = "#ef6548",
        alpha = 0.9
      ) 
    
    
    DailyDemandChart
    
    plottitle = "Daily electricity demand, annual comparison"
    sourcecaption = "Source: National Grid, SPICe"
    
    DailyDemandChart <-
      DailyChart(DailyDemandChart,
                 DailyDemand,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    DailyDemandChart <- DailyDemandChart +
      coord_cartesian(xlim = c(min(DailyDemand$NewDate-1), max(DailyDemand$NewDate)-8),
                      ylim = c(-3,101)) +
      labs(
        title = plottitle,
        face = 2,
        subtitle = paste(
          "Scotland"
        )
      )+
      
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.9
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

output$FullData <- downloadHandler(
  filename = "C19ElecFullData.csv",
  content = function(file){
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,2,4,3)]
    
    names(Data) <- c("Year", "Gas (GWh)", "Transport (GWh)", "Electricity (GWh)")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19Elec <- Data
    
    write.csv(C19Elec, 
              file,
              row.names = FALSE)
  }
)

}