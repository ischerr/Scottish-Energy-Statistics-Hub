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
               4, style = 'padding:15px;'#,
               #downloadButton(ns('C19Gas.png'), 'Download Graph', style="float:right")
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
    # fluidRow(
    # column(10, h3("Data - 12 month rolling average.", style = "color: #5d8be1;  font-weight:bold")),
    # column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    # ),
    # fluidRow(
    #   column(12, dataTableOutput(ns("C19GasTable"))%>% withSpinner(color="#5d8be1"))),
    # tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
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
    
    p1 <-  plot_ly(WeekdayElecDemand, y = ~ YearFormat ) %>%  
      add_trace(x = ~ `BeforeLockdown`, 
                orientation = 'h',
                name = "First three weeks of March",
                type = 'bar',
                legendgroup = "1",
                text = paste0(
                  "Average weekday gas consumption in first three weeks of March: ", format(round(WeekdayElecDemand$`BeforeLockdown`, 0.1), big.mark = ",")," GWh\n",
                  "Year: ", WeekdayElecDemand$Year, "\n"),
                hoverinfo = 'text',
                line = list(width = 4)
      ) %>% 
      add_trace(x = ~ `PostLockdown`, 
                orientation = 'h',
                name = "Last week of March to first week in May",
                type = 'bar',
                legendgroup = "2",
                text = paste0(
                  "Average weekday gas consumption in from the last week in March to first week of May: ", format(round(WeekdayElecDemand$`PostLockdown`, 0.1), big.mark = ",")," GWh\n",
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
        xaxis = list(title = "GWh",
                     zeroline = FALSE,
                     tickformat = "",
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(
          title = "",
          tickformat = ",d",
          autorange = "reversed",
          ticktext = as.list(WeekdayElecDemand$`Year`),
          tickmode = "array",
          tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
          showgrid = FALSE,
          zeroline = FALSE,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    
    p1

    
    
   
  })
  
  output$C19GasTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas (Gwh)", "Transport (GWh)", "Electricity (GWh)")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19GasRolling <- Data[complete.cases(Data),]
    
    C19GasRolling <- C19GasRolling %>% 
      mutate(yr_mnth = format(Year, '%Y-%m')) %>% 
      group_by(yr_mnth) %>% 
      filter(Year == max(Year)) %>% 
      mutate(Year = format(Year, "%B %Y"))
    
    names(C19GasRolling)[1] <- "12 month ending"
    
    datatable(
      C19GasRolling,
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
    filename = "C19Gas.png",
    content = function(file) {
      
      print("Energy daily demand")
      ###### Daily Demand  #####
      
      # C19Gas <-
      #   read_csv(
      #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/C19Gas.csv"
      #   )
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "DailyDemandWorking")[c(1,2,4,3)]
      
      Data <- Data[complete.cases(Data),]
      
      names(Data) <- c("Year", "Gas", "Transport", "Electricity")
      
      Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
      
      C19Gas <- Data
      
      ### variables
      ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: Sheffield, National Grid, BEIS"
      plottitle = "Energy use in Scotland per day"
      
      #C19Gas$GasPercentage <- PercentLabel(C19Gas$Gas)
      
      
      C19GasChart <- C19Gas %>%
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
          x = mean(C19Gas$Year),
          y = max(C19Gas$Gas),
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
          x = mean(C19Gas$Year),
          y = mean(C19Gas$Electricity),
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
          x = mean(C19Gas$Year),
          y = mean(C19Gas$Transport),
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
          x = C19Gas$Year[which(C19Gas$Gas == max(C19Gas$Gas))]-2,
          xend = C19Gas$Year[which(C19Gas$Gas == max(C19Gas$Gas))] - 30,
          y = max(C19Gas$Gas),
          yend = max(C19Gas$Gas),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = C19Gas$Year[which(C19Gas$Gas == max(C19Gas$Gas))] - 35,
          y = max(C19Gas$Gas),
          label = paste(round(max(C19Gas$Gas), digits = 0), "GWh"),
          hjust = 1,
          fontface = 2,
          size = 4,
          colour = ChartColours[2],
          family = "Century Gothic"
        )+
        annotate(
          "segment",
          x = C19Gas$Year[which(C19Gas$Electricity == max(C19Gas$Electricity[which(C19Gas$Year > dmy("01/08/18"))]))]+2,
          xend = C19Gas$Year[which(C19Gas$Electricity == max(C19Gas$Electricity[which(C19Gas$Year > dmy("01/08/18"))]))] + 30,
          y = max(C19Gas$Electricity[which(C19Gas$Year > dmy("01/08/18"))]),
          yend = max(C19Gas$Electricity[which(C19Gas$Year > dmy("01/08/18"))]),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = C19Gas$Year[which(C19Gas$Electricity == max(C19Gas$Electricity[which(C19Gas$Year > dmy("01/08/18"))]))] + 35,
          y = max(C19Gas$Electricity[which(C19Gas$Year > dmy("01/08/18"))]),
          label = paste(round(max(C19Gas$Electricity[which(C19Gas$Year > dmy("01/08/18"))]), digits = 0), "GWh"),
          hjust = 0,
          fontface = 2,
          size = 4,
          colour = ChartColours[3],
          family = "Century Gothic"
        )
      
      
      C19GasChart
      
      C19GasChart <-
        DailyChart(C19GasChart,
                   C19Gas,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      C19GasChart <- C19GasChart +
        coord_cartesian(xlim = c(min(C19Gas$Year), max(C19Gas$Year)+130)) +
        
        ylim(-15, 352) +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      C19GasChart
      
      ggsave(
        file,
        plot =  C19GasChart,
        width = 30,
        height = 12,
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
