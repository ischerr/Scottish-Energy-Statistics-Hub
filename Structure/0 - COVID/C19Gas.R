require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")

###### UI Function ######

source("Structure/Global.R")

C19GasOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Average weekday daily gas demand", style = "color: #5d8be1;  font-weight:bold"),
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
    fluidRow(
    column(10, h3("Data - Average weekday daily gas demand (GWh)", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("C19GasTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
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
      summarise(Gas = mean(Gas))
    
    WeekdayElecDemand <- dcast(WeekdayElecDemand, Year ~ PostLockdown)
    
    ChartColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    BarColours <- c("#126992", "#1d91c0", "#7fcdbb", "#8da0cb")
    
    WeekdayElecDemand$YearFormat <- paste0("<b>", WeekdayElecDemand$Year, "</b>")
    
    p1 <-  plot_ly(WeekdayElecDemand, x = ~ YearFormat ) %>%  
      add_trace(y = ~ `BeforeLockdown`,
                name = "First three weeks of March",
                type = 'bar',
                legendgroup = "1",
                text = paste0(
                  "Average weekday gas consumption in first three weeks of March: ", format(round(WeekdayElecDemand$`BeforeLockdown`, 0.1), big.mark = ",")," GWh\n",
                  "Year: ", WeekdayElecDemand$Year, "\n"),
                hoverinfo = 'text',
                line = list(width = 4)
      ) %>% 
      add_trace(y = ~ `PostLockdown`, 
                name = "Last week of March to third week in May",
                type = 'bar',
                legendgroup = "2",
                text = paste0(
                  "Average weekday gas consumption in from the last week in March to third week of May: ", format(round(WeekdayElecDemand$`PostLockdown`, 0.1), big.mark = ",")," GWh\n",
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
  
  output$C19GasTable = renderDataTable({
    
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
      summarise(Gas = mean(Gas))
    
    WeekdayElecDemand <- dcast(WeekdayElecDemand, Year ~ PostLockdown)
    
    names(WeekdayElecDemand) <- c("Year", "First three weeks of March (GWh)", "Last week of March to third week in May (GWh)")
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
        title = "Average weekday daily electricity demand (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Average weekday daily electricity demand (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Average weekday daily electricity demand (GWh)')
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
                     paste(readtext("Structure/0 - COVID/C19Gas.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("C19GasTable")
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
      
      plottitle <-
        "Average weekday daily gas demand"
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
      
      DailyDemand$PostLockdown <- ifelse(DailyDemand$Week >= 13, "First three weeks of March", "Last week of March to third week in May")
      
      WeekdayElecDemand <- DailyDemand
      
      WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
      
      WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
      
      maxweek <- max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week)
      
      WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Week <= maxweek),]
      
      WeekdayElecDemand <- WeekdayElecDemand %>% group_by(Year, PostLockdown) %>% 
        summarise(Gas = mean(Gas))
      
      #WeekdayElecDemand <- as.data.frame(dcast(WeekdayElecDemand, Year ~ PostLockdown))
      
      ChartColours <- c("#126992", "#2078b4", "#ff7f0e", "#8da0cb")
      BarColours <- c("#126992", "#2078b4", "#ff7f0e", "#8da0cb")
      
      WeekdayElecDemandChart <- WeekdayElecDemand  %>%
        ggplot(aes(x = Year, y = Gas, fill = PostLockdown), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "First three weeks of March" = BarColours[3],
            "Last week of March to third week in May" = BarColours[2]
          )
        ) +
        geom_bar(position = "dodge",
                 stat = "identity",
                 width = -.8) +
        geom_text(position = position_dodge(width = -.8),
                  aes(
                    y = Gas + 12,
                    fill = PostLockdown,
                    label = paste(format(round(Gas, digits = 0), big.mark = ","), "\nGWh")
                  ),
                  fontface = 2,
                  colour =  ChartColours[1],
                  family = "Century Gothic",
                  size = 3) +
        geom_text(position = position_dodge(width = .8),
                  aes(
                    y = 5,
                    fill = PostLockdown,
                    angle = 90,
                    label = ifelse(Year == min(Year), as.character(PostLockdown), ""),
                    hjust = 0
                  ),
                  fontface = 2,
                  colour =  "white",
                  family = "Century Gothic",
                  size = 4) +
        annotate(
          "text",
          x = WeekdayElecDemand$Year,
          y = -9,
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

}
