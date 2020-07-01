require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")

###### UI Function ######

source("Structure/Global.R")

C19ElecOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
    tabPanel("Average Demand since lockdown",
             fluidRow(column(8,
                             h3("Average daily electricity demand post-lockdown and in equivalent periods in previous years", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19Elec2Subtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;', downloadButton(ns('C19Elec2.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19ElecPlot")),
             plotlyOutput(ns("C19Elec2Plot"), height = "600px")%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Average Demand by phase",
             fluidRow(column(8,
                             h3("Average daily electricity demand before lockdown, after lockdown and in the phases as lockdown is lifted compared with equivalent time period in previous years.", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19ElecSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;', downloadButton(ns('C19Elec.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19ElecPlot")),
             plotlyOutput(ns("C19ElecPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Daily demand",
             fluidRow(column(8,
                             h3("Daily electricity demand - 2020 vs 2019", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19ElecRollingSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('C19ElecRolling.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("C19ElecPlot")),
             plotlyOutput(ns("C19ElecRollingPlot"))%>% withSpinner(color="#5d8be1"),
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
      tabPanel("Weekday demand",
    fluidRow(
    column(10, h3("Data - Average daily electricity demand post-lockdown and in equivalent periods in previous years (GWh)", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("C19ElecTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Daily demand",
             fluidRow(
               column(10, h3("Data - Daily electricity demand - 2020 vs 2019 (GWh)", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("C19ElecRollingTable"))%>% withSpinner(color="#5d8be1"))),
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
C19Elec <- function(input, output, session) {

  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("C19Elec.R")

  output$C19ElecSubtitle <- renderText({
    
    paste("Scotland, 2013 - 2020")
  })
  
  output$C19Ele2cSubtitle <- renderText({
    
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
    
    DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 23, "Phase 1 & 2", DailyDemand$PostLockdown)
    
    WeekdayElecDemand <- DailyDemand
    
    #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
    
    maxweek <- max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week)
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Week <= maxweek),]
    
    WeekdayElecDemand <- WeekdayElecDemand %>% group_by(Year, PostLockdown) %>% 
      summarise(Electricity = mean(Electricity))
    
    WeekdayElecDemand <- dcast(WeekdayElecDemand, Year ~ PostLockdown)
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#045a8d", "#a6bddb", "#3690c0",  "#8da0cb")
    
    WeekdayElecDemand$YearFormat <- paste0("<b>", WeekdayElecDemand$Year, "</b>")
    
    p1 <-  plot_ly(WeekdayElecDemand, x = ~ Year ) %>%  
      add_trace(y = ~ `BeforeLockdown`,
                name = "Before Lockdown",
                type = 'bar',
                legendgroup = "1",
                marker = list(color = BarColours[1]), 
                text = paste0(
                  "Before Lockdown: ", format(round(WeekdayElecDemand$`BeforeLockdown`, 2), big.mark = ",")," GWh\n",
                  "Year: ", WeekdayElecDemand$Year, "\n"),
                hoverinfo = 'text',
                line = list(width = 4)
      ) %>%
      add_trace(y = ~ `PostLockdown`,
                name = "After Lockdown",
                type = 'bar',
                legendgroup = "2",
                marker = list(color = BarColours[2]),
                text = paste0(
                  "After Lockdown: ", format(round(WeekdayElecDemand$`PostLockdown`, 2), big.mark = ",")," GWh\n",
                  "Year: ", WeekdayElecDemand$Year, "\n"),
                hoverinfo = 'text',
                line = list(width = 4)
      ) %>% 
      
      add_trace(y = ~ `Phase 1 & 2`,
                name = "Phase 1 & 2",
                type = 'bar',
                legendgroup = "3",
                marker = list(color = BarColours[3]),
                text = paste0(
                  "Phase 1 & 2: ", format(round(WeekdayElecDemand$`Phase 1 & 2`, 2), big.mark = ",")," GWh\n",
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
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    WeekdayElecDemand <- DailyDemand
    
    #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
    
    maxweek <- max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week)
    
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
                text = paste0(
                  "Electricity Demand After Lockdown: ", format(round(WeekdayElecDemand$`PostLockdown`, 2), big.mark = ",")," GWh\n",
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
  
  output$C19ElecRollingSubtitle <- renderText({
    
    paste("Scotland")
  })
  
  output$C19ElecRollingPlot <- renderPlotly  ({
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    DailyDemandFromMarch <- DailyDemand[which(DailyDemand$Week >= 2 & DailyDemand$Week <= 51),]
    
    DailyDemandFromMarch <- DailyDemandFromMarch[c(5,6,7,9,1,8,4)]
    
    DailyDemandFromMarch <- DailyDemandFromMarch %>% group_by(Year) %>% mutate(id = row_number())
    
    
    DailyDemandFromMarch  <- dcast(DailyDemandFromMarch, id ~ Year, value.var = 'Electricity')
    
    DailyDemandFromMarch$Date <- ymd("2020/01/05") + DailyDemandFromMarch$id
    
    DailyDemandFromMarch <- DailyDemandFromMarch[complete.cases(DailyDemandFromMarch),]
    
    vline1 <- function(x = 0, color = "#02818a") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color)
      )}
    
    vline2 <- function(x = 0, color = "#67a9cf") {
      list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color, dash = "dash")
      )
      

    }
    
    
    p2 <-  plot_ly(DailyDemandFromMarch,x = ~ Date ) %>% 
      add_trace(data = DailyDemandFromMarch,
                x = ~ Date,
                y = ~ `2020`,
                name = "2020",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "2020 Demand: ",
                  round(DailyDemandFromMarch$`2020`, digits = 1),
                  " GWh\nDate: ",
                  format(DailyDemandFromMarch$Date, format="%d/%m/%y")
                ),
                hoverinfo = 'text',
                line = list(width = 4)
      ) %>% 
      add_trace(data = DailyDemandFromMarch,
                x = ~ Date,
                y = ~ `2019`,
                name = "2019",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "2019 demand on equivalent date: ",
                  round(DailyDemandFromMarch$`2019`, digits = 1),
                  " GWh\nDate: ",
                  format(DailyDemandFromMarch$Date, format="%d/%m/%y")
                ),
                hoverinfo = 'text',
                line = list(width = 4)
      )  %>% 
      add_annotations(
        x = dmy("31/03/2020"),
        y = .4,
        yref = "paper",
        text = "<b>23/03</b>\nLockdown",
        font = list(color = "#02818a",
                    family = "Century Gothic"),
        textposistion = "bottom right",
        showarrow = FALSE
      ) %>% 
      add_annotations(
        x = dmy("4/06/2020"),
        y = .4,
        yref = "paper",
        text = "<b>28/05</b>\nPhase 1 & 2",
        font = list(color = "#02818a",
                    family = "Century Gothic"),
        textposistion = "bottom right",
        showarrow = FALSE
      ) %>% 
      add_annotations(
        x = dmy("25/06/2020"),
        y = .4,
        yref = "paper",
        text = "<b>19/06</b>\nPhase 2",
        font = list(color = "#02818a",
                    family = "Century Gothic"),
        textposistion = "bottom right",
        showarrow = FALSE
      ) %>% 
      layout(
        barmode = 'stack',
        shapes = list( vline1(dmy("23/03/2020")),vline2(dmy("28/05/2020")),vline2(dmy("19/06/2020"))),
        bargap = 0.66,
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "Date in 2020",
                     showgrid = FALSE,
                     tickfont = list(size = 15)),
        yaxis = list(
          title = "GWh",
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
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    WeekdayElecDemand <- DailyDemand
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
    
    maxweek <- max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week)
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Week <= maxweek),]
    
    WeekdayElecDemand <- WeekdayElecDemand %>% group_by(Year, PostLockdown) %>% 
      summarise(Electricity = mean(Electricity))
    
    WeekdayElecDemand <- dcast(WeekdayElecDemand, Year ~ PostLockdown)
    
    names(WeekdayElecDemand) <- c("Year", "First three weeks of March (GWh)", "fourth week of March to fourth week of June (GWh)")
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
        title = "Average daily electricity demand post-lockdown and in equivalent periods in previous years (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average daily electricity demand post-lockdown and in equivalent periods in previous years (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average daily electricity demand post-lockdown and in equivalent periods in previous years (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:5, 0) 
  })
  
  output$C19ElecRollingTable = renderDataTable({
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    DailyDemandFromMarch <- DailyDemand[which(DailyDemand$Week >= 2 & DailyDemand$Week <= 51),]
    
    DailyDemandFromMarch <- DailyDemandFromMarch[c(5,6,7,9,1,8,4)]
    
    DailyDemandFromMarch <- DailyDemandFromMarch %>% group_by(Year) %>% mutate(id = row_number())
    
    
    DailyDemandFromMarch  <- dcast(DailyDemandFromMarch, id ~ Year, value.var = 'Electricity')
    
    DailyDemandFromMarch$Year <- ymd("2020/01/05") + DailyDemandFromMarch$id
    
    DailyDemandFromMarch <- DailyDemandFromMarch[complete.cases(DailyDemandFromMarch),]
    
    DailyDemandFromMarch <- DailyDemandFromMarch[10:8]
    
    names(DailyDemandFromMarch) <- c("Date", "Daily electricity demand in 2020 (GWh)", "Electricity demand on equivalent day in 2019 (GWh)")
    
    datatable(
      DailyDemandFromMarch,
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
        title = "Daily electricity demand - 2020 vs 2019 (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Daily electricity demand - 2020 vs 2019 (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Daily electricity demand - 2020 vs 2019 (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c( -1, 10, 20) # declare values
                           , c("All", 10, 20 ) # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:5, 1) 
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/0 - COVID/C19Elec.txt")[2])
                     
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
        "Average daily electricity demand before lockdown, after lockdown and in\nthe phases as lockdown is lifted compared with equivalent time period\nin previous years."
      sourcecaption <- "Source: National Grid"
      
      DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      DailyDemand$Date <- ymd(DailyDemand$Date)
      
      DailyDemand$Year <-year(DailyDemand$Date)
      
      DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
      
      DailyDemand$Month <-month(DailyDemand$Date)
      
      DailyDemand$Week <- isoweek(DailyDemand$Date)
      
      DailyDemand$Weekday <- weekdays(DailyDemand$Date)
      
      DailyDemand$DayofYear <- yday(DailyDemand$Date)
      
      DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 13, "Post-Lockdown", "Pre-Lockdown")
      
      DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 23, "Phase 1 & 2", DailyDemand$PostLockdown)
      
      WeekdayElecDemand <- DailyDemand
      
      #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
      
      WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
      
      maxweek <- max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week)
      
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
            "Post-Lockdown" = BarColours[2],
            "Pre-Lockdown" = BarColours[1],
            "Phase 1 & 2" = BarColours[3]
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
        geom_text(position = position_dodge(width = -.8),
                  aes(
                    y = 1,
                    fill = PostLockdown,
                    angle = 90,
                    label = PostLockdown,
                    hjust = 0
                  ),
                  fontface = 2,
                  colour =  "White",
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
        "Average daily electricity demand post-lockdown\nand in equivalent periods in previous years"
      sourcecaption <- "Source: National Grid"
      
      DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      DailyDemand$Date <- ymd(DailyDemand$Date)
      
      DailyDemand$Year <-year(DailyDemand$Date)
      
      DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
      
      DailyDemand$Month <-month(DailyDemand$Date)
      
      DailyDemand$Week <- isoweek(DailyDemand$Date)
      
      DailyDemand$Weekday <- weekdays(DailyDemand$Date)
      
      DailyDemand$DayofYear <- yday(DailyDemand$Date)
      
      DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 13, "Post", "Pre")
      
      WeekdayElecDemand <- DailyDemand
      
      #WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
      
      WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
      
      WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$PostLockdown == "Post"),]
      
      maxweek <- max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week)
      
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
            "Post" = BarColours[2]
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
    
    ChartColours <- c("#126992", "#1f77b4", "#ff7f0e", "#8da0cb")
    BarColours <- c("#126992", "#1f77b4", "#ff7f0e", "#8da0cb")
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 13, "PostLockdown", "BeforeLockdown")
    
    DailyDemandFromMarch <- DailyDemand[which(DailyDemand$Week >= 2 & DailyDemand$Week <= 51),]
    
    DailyDemandFromMarch <- DailyDemandFromMarch[c(5,6,7,9,1,8,4)]
    
    DailyDemandFromMarch <- DailyDemandFromMarch %>% group_by(Year) %>% mutate(id = row_number())
    
    
    DailyDemandFromMarch  <- dcast(DailyDemandFromMarch, id ~ Year, value.var = 'Electricity')
    
    DailyDemandFromMarch$Year <- ymd("2020/01/05") + DailyDemandFromMarch$id
    
    DailyDemandFromMarch <- DailyDemandFromMarch[complete.cases(DailyDemandFromMarch),]
    
    width <- max(DailyDemandFromMarch$Year) - min(DailyDemandFromMarch$Year)
    
    DailyDemandFromMarchChart <- DailyDemandFromMarch %>%
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
        x = mean(DailyDemandFromMarch$Year),
        y = 90,
        label = "2019",
        hjust = 0.5,
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
        x = mean(DailyDemandFromMarch$Year),
        y = 40,
        label = "2020",
        hjust = 0.5,
        colour = ChartColours[2],
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
            format(Year, format = "%d/%m/%y"),
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
        x = min(DailyDemandFromMarch$Year)-100,
        xend = max(DailyDemandFromMarch$Year)+100,
        y = 25,
        yend = 25,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(Year)-(width*0.03),
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
          x = min(Year)-(width*0.03),
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
        x = min(DailyDemandFromMarch$Year)-100,
        xend = max(DailyDemandFromMarch$Year)+100,
        y = 50,
        yend = 50,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(Year)-(width*0.03),
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
        x = min(DailyDemandFromMarch$Year)-100,
        xend = max(DailyDemandFromMarch$Year)+100,
        y = 75,
        yend = 75,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = min(Year)-(width*0.03),
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
        x = min(DailyDemandFromMarch$Year)-100,
        xend = max(DailyDemandFromMarch$Year)+100,
        y = 100,
        yend = 100,
        colour = "grey",
        alpha = 0.4,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = dmy("24/03/2020"),
          y = 50,
          label = "23/03",
          fontface = 2
        ),
        hjust = 0,
        vjust = -.2,
        colour = "#07818a",
        family = "Century Gothic",
        size = 3
      )+
      geom_text(
        aes(
          x = dmy("24/03/2020"),
          y = 50,
          label = "Lockdown\n "
        ),
        vjust = 1.1,
        hjust = 0,
        colour = "#07818a",
        family = "Century Gothic",
        size = 3
      )+
      annotate(
        "segment",
        x = dmy("23/03/2020"),
        xend = dmy("23/03/2020"),
        y = -1000,
        yend = 1000,
        colour = "#07818a",
        alpha = 0.9
      ) +
      geom_text(
        aes(
          x = dmy("29/05/2020"),
          y = 40,
          label = "28/05",
          fontface = 2
        ),
        vjust = -.2,
        hjust = 0,
        colour = "#07818a",
        family = "Century Gothic",
        size = 3
      )+
      geom_text(
        aes(
          x = dmy("29/05/2020"),
          y = 40,
          label = "Phase 1 & 2 "
        ),
        vjust = 1.1,
        hjust = 0,
        colour = "#07818a",
        family = "Century Gothic",
        size = 3
      )+
      annotate(
        "segment",
        x = dmy("28/05/2020"),
        xend = dmy("28/05/2020"),
        y = -1000,
        yend = 1000,
        colour = "#07818a",
        alpha = 0.9,
        linetype = 2
      ) +
      geom_text(
        aes(
          x = dmy("20/06/2020"),
          y = 40,
          label = "19/06",
          fontface = 2
        ),
        vjust = -.2,
        hjust = 0,
        colour = "#07818a",
        family = "Century Gothic",
        size = 3
      )+
      geom_text(
        aes(
          x = dmy("20/06/2020"),
          y = 40,
          label = "Phase 2 "
        ),
        vjust = 1.1,
        hjust = 0,
        colour = "#07818a",
        family = "Century Gothic",
        size = 3
      )+
      annotate(
        "segment",
        x = dmy("19/06/2020"),
        xend = dmy("19/06/2020"),
        y = -1000,
        yend = 1000,
        colour = "#07818a",
        alpha = 0.9,
        linetype = 2
      ) 
    
    
    DailyDemandFromMarchChart
    
    plottitle = "Daily electricity demand - 2020 vs 2019"
    sourcecaption = "Source: National Grid"
    
    DailyDemandFromMarchChart <-
      DailyChart(DailyDemandFromMarchChart,
                 DailyDemandFromMarch,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    DailyDemandFromMarchChart <- DailyDemandFromMarchChart +
      coord_cartesian(xlim = c(min(DailyDemandFromMarch$Year-1), max(DailyDemandFromMarch$Year)),
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
    
    DailyDemandFromMarchChart
    
    ggsave(
      file,
      plot =  DailyDemandFromMarchChart,
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
