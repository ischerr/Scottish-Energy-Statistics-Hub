require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")

###### UI Function ######



C19GasOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Average daily gas demand by quarter", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('C19GasSubtitle')), style = "color: #5d8be1;")
    ),
    column(
      4, style = 'padding:15px;', downloadButton(ns('C19Gas.png'), 'Download Graph', style="float:right")
    )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("C19GasPlot")),
    plotlyOutput(ns("C19GasPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
      column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:;background-color:#5d8be1;"),
    tabsetPanel(
      tabPanel("Weekday Demand",
               fluidRow(
                 column(10, h3("Data - Average weekday daily gas demand (GWh)", style = "color: #5d8be1;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("C19GasTable"))%>% withSpinner(color="#5d8be1"))),
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
      tabPanel("Daily demand",
               fluidRow(
                 column(10, h3("Data - Daily gas demand", style = "color: #5d8be1;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("C19GasRollingTable"))%>% withSpinner(color="#5d8be1"))),
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
    fluidRow(
      column(2, p(" ")),
      column(2,
             p(" ")),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
             SourceLookup("NGData")
             
      )
    )
  )
}




###### Server ######
C19Gas <- function(input, output, session) {
  
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("C19Gas.R")
  
  output$C19GasSubtitle <- renderText({
    
    paste("Scotland, 2013 - 2020")
  })
  
  output$C19GasPlot <- renderPlotly  ({
    
    library(readr)
    library(ISOweek)
    library(lubridate)
    library(zoo)
    library(plotly)
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013 & DailyDemand$Year <= 2020),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand$Quarter <- quarters(DailyDemand$Date)
    
    DailyDemand <- DailyDemand %>% group_by(Year, Quarter) %>% 
      summarise(Gas = mean(Gas))
    
    DailyDemand <- dcast(DailyDemand, Year ~ Quarter)
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0")
    
    DailyDemand$YearFormat <- paste0("<b>", DailyDemand$Year, "</b>")
    
    p1 <-  plot_ly(DailyDemand, x = ~ YearFormat ) %>%  
      add_trace(y = ~ `Q1`,
                name = "Q1",
                type = 'bar',
                legendgroup = "1",
                text = paste0(
                  "Q1: ", format(round(DailyDemand$`Q1`, 0.1), big.mark = ",")," GWh\n",
                  "Year: ", DailyDemand$Year, "\n"),
                hoverinfo = 'text',
                marker = list(color = BarColours[1]),
                line = list(width = 4
                )
      ) %>% 
      add_trace(y = ~ `Q2`, 
                name = "Q2",
                type = 'bar',
                legendgroup = "2",
                text = paste0(
                  "Q2: ", format(round(DailyDemand$`Q2`, 0.1), big.mark = ",")," GWh\n",
                  "Year: ", DailyDemand$Year, "\n"),
                hoverinfo = 'text',
                marker = list(color = BarColours[2]),
                line = list(width = 4)
      ) %>% 
      add_trace(y = ~ `Q3`, 
                name = "Q3",
                type = 'bar',
                legendgroup = "3",
                text = paste0(
                  "Q3: ", format(round(DailyDemand$`Q3`, 0.1), big.mark = ",")," GWh\n",
                  "Year: ", DailyDemand$Year, "\n"),
                hoverinfo = 'text',
                marker = list(color = BarColours[3]),
                line = list(width = 4)
      ) %>% 
      add_trace(y = ~ `Q4`, 
                name = "Q4",
                type = 'bar',
                legendgroup = "4",
                text = paste0(
                  "Q4: ", format(round(DailyDemand$`Q4`, 0.1), big.mark = ",")," GWh\n",
                  "Year: ", DailyDemand$Year, "\n"),
                hoverinfo = 'text',
                marker = list(color = BarColours[4]),
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
  
  output$C19GasTable = renderDataTable({
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013 & DailyDemand$Year <= 2020),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    DailyDemand$Quarter <- quarters(DailyDemand$Date)
    
    DailyDemand <- DailyDemand %>% group_by(Year, Quarter) %>% 
      summarise(Gas = mean(Gas))
    
    DailyDemand <- dcast(DailyDemand, Year ~ Quarter)
    datatable(
      DailyDemand,
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
        title = "Average daily gas demand per quarter (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average daily gas demand per quarter (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average daily gas demand per quarter (GWh)')
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
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/6 - System Security/C19Gas.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("C19GasTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("C19GasRollingTable")
  })
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$C19Gas.png <- downloadHandler(
    filename = "C19GasAverage.png",
    content = function(file) {
      
      
      library(readr)
      library(ISOweek)
      library(lubridate)
      library(zoo)
      library(plotly)
      
      DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      DailyDemand$Date <- ymd(DailyDemand$Date)
      
      DailyDemand$Year <-year(DailyDemand$Date)
      
      DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2013 & DailyDemand$Year <= 2020),]
      
      DailyDemand$Month <-month(DailyDemand$Date)
      
      DailyDemand$Week <- isoweek(DailyDemand$Date)
      
      DailyDemand$Weekday <- weekdays(DailyDemand$Date)
      
      DailyDemand$DayofYear <- yday(DailyDemand$Date)
      
      DailyDemand$Quarter <- quarters(DailyDemand$Date)
      
      DailyDemand <- DailyDemand %>% group_by(Year, Quarter) %>% 
        summarise(Gas = mean(Gas))
      
      plottitle <- "Average daily gas demand by quarter"
      sourcecaption <- "Source: National Grid"
      #WeekdayElecDemand <- as.data.frame(dcast(WeekdayElecDemand, Year ~ PostLockdown))
      
      ChartColours <- c("#126992", "#2078b4", "#ff7f0e", "#8da0cb")
      BarColours <- c("#081d58", "#253494", "#225ea8", "#1d91c0")
      
      DailyDemandChart <-  DailyDemand  %>%
        ggplot(aes(x = Year, y = Gas, fill = Quarter), family = "Century Gothic") +
        scale_fill_manual(
          "Quarter",
          values = c(
            "Q1" = BarColours[1],
            "Q2" = BarColours[2],
            "Q3" = BarColours[3],
            "Q4" = BarColours[4]
          )
        ) +
        geom_bar(position = "dodge",
                 stat = "identity",
                 width = .8) +
        geom_text(position = position_dodge(width = .8),
                  aes(
                    y = Gas + 12,
                    fill = Quarter,
                    label = paste(format(round(Gas, digits = 0), big.mark = ","), "\nGWh")
                  ),
                  fontface = 2,
                  colour =  ChartColours[1],
                  family = "Century Gothic",
                  size = 3) +
        geom_text(position = position_dodge(width = .8),
                  aes(
                    y = 5,
                    fill = Quarter,
                    angle = 90,
                    label = ifelse(Year == min(Year), as.character(Quarter), ""),
                    hjust = 0
                  ),
                  fontface = 2,
                  colour =  "white",
                  family = "Century Gothic",
                  size = 4) +
        annotate(
          "text",
          x = DailyDemand$Year,
          y = -9,
          label = DailyDemand$Year,
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1]
        )
      
      DailyDemandChart <-
        StackedArea(DailyDemandChart,
                    DailyDemand,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      DailyDemandChart
      
      ggsave(
        file,
        plot = DailyDemandChart,
        width = 20,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  
  output$FullData <- downloadHandler(
    filename = "C19GasFullData.csv",
    content = function(file){
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "DailyDemandWorking")[c(1,2,4,3)]
      
      names(Data) <- c("Year", "Gas (GWh)", "Transport (GWh)", "Electricity (GWh)")
      
      Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
      
      C19Gas <- Data
      
      write.csv(C19Gas, 
                file,
                row.names = FALSE)
    }
  )
  
  output$C19GasRollingTable = renderDataTable({
    
    DailyDemand <- read_delim("CovidAnalysis/DailyDemand.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    DailyDemand$Date <- ymd(DailyDemand$Date)
    
    DailyDemand$Year <-year(DailyDemand$Date)
    
    DailyDemand <- DailyDemand[which(DailyDemand$Year >= 2019),]
    
    DailyDemand$Month <-month(DailyDemand$Date)
    
    DailyDemand$Week <- isoweek(DailyDemand$Date)
    
    DailyDemand$Weekday <- weekdays(DailyDemand$Date)
    
    DailyDemand$DayofYear <- yday(DailyDemand$Date)
    
    names(DailyDemand)[2] <- c("Daily gas demand(GWh)")
    
    datatable(
      DailyDemand[c(1,2)],
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
        title = "Daily gas demand (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Daily gas demand (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Daily gas demand (GWh)')
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
  
}
