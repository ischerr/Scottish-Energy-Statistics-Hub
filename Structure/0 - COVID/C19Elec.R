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
      tabPanel("Weekday demand",
    fluidRow(column(8,
                    h3("Average weekday daily electricity demand", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('C19ElecSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;'#,
               #downloadButton(ns('C19Elec.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("C19ElecPlot")),
    plotlyOutput(ns("C19ElecPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")
    ),
    tabPanel("Daily demand",
             fluidRow(column(8,
                             h3("Daily electricity demand from start of March - 2020 vs 2019", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('C19ElecRollingSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;'#,
               #downloadButton(ns('C19ElecRolling.png'), 'Download Graph', style="float:right")
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
    # fluidRow(
    # column(10, h3("Data - 12 month rolling average.", style = "color: #5d8be1;  font-weight:bold")),
    # column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    # ),
    # fluidRow(
    #   column(12, dataTableOutput(ns("C19ElecTable"))%>% withSpinner(color="#5d8be1"))),
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
C19Elec <- function(input, output, session) {

  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("C19Elec.R")

  output$C19ElecSubtitle <- renderText({
    
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
    
    WeekdayElecDemand <- DailyDemand
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Weekday %in%c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")),]
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Month >= 3),]
    
    maxweek <- max(WeekdayElecDemand[which(WeekdayElecDemand$Year ==max(WeekdayElecDemand$Year)),]$Week)
    
    WeekdayElecDemand <- WeekdayElecDemand[which(WeekdayElecDemand$Week <= maxweek),]
    
    WeekdayElecDemand <- WeekdayElecDemand %>% group_by(Year, PostLockdown) %>% 
      summarise(Electricity = mean(Electricity))
    
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
                  "Average weekday electricity consumption in first three weeks of March: ", format(round(WeekdayElecDemand$`BeforeLockdown`, 0.1), big.mark = ",")," GWh\n",
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
                  "Average weekday electricity consumption in from the last week in March to first week of May: ", format(round(WeekdayElecDemand$`PostLockdown`, 0.1), big.mark = ",")," GWh\n",
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
    
    DailyDemandFromMarch <- DailyDemand[which(DailyDemand$Week >= 10 & DailyDemand$Week <= 51),]
    
    DailyDemandFromMarch <- DailyDemandFromMarch[c(5,6,7,9,1,8,4)]
    
    DailyDemandFromMarch <- DailyDemandFromMarch %>% group_by(Year) %>% mutate(id = row_number())
    
    
    DailyDemandFromMarch  <- dcast(DailyDemandFromMarch, id ~ Year, value.var = 'Electricity')
    
    DailyDemandFromMarch$Date <- ymd("2020/03/01") + DailyDemandFromMarch$id
    
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
        x = dmy("13/03/2020"),
        y = .4,
        yref = "paper",
        text = "<b>20/03</b>\nClosure of pubs, gyms\netc.",
        font = list(color = "#67a9cf",
                    family = "Century Gothic"),
        textposistion = "bottom right",
        showarrow = FALSE
      ) %>% 
      add_annotations(
        x = dmy("29/03/2020"),
        y = .4,
        yref = "paper",
        text = "<b>23/03</b>\nLockdown",
        font = list(color = "#02818a",
                    family = "Century Gothic"),
        textposistion = "bottom right",
        showarrow = FALSE
      ) %>% 
      layout(
        barmode = 'stack',
        shapes = list(vline1(dmy("23/03/2020")), vline2(dmy("20/03/2020"))),
        bargap = 0.66,
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "Date in 2020",
                     showgrid = FALSE),
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
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas (Gwh)", "Transport (GWh)", "Electricity (GWh)")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19ElecRolling <- Data[complete.cases(Data),]
    
    C19ElecRolling <- C19ElecRolling %>% 
      mutate(yr_mnth = format(Year, '%Y-%m')) %>% 
      group_by(yr_mnth) %>% 
      filter(Year == max(Year)) %>% 
      mutate(Year = format(Year, "%B %Y"))
    
    names(C19ElecRolling)[1] <- "12 month ending"
    
    datatable(
      C19ElecRolling,
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
                     paste(readtext("Structure/0 - COVID/C19Elec.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("C19ElecTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$C19Elec.png <- downloadHandler(
    filename = "C19Elec.png",
    content = function(file) {
      
      print("Energy daily demand")
      ###### Daily Demand  #####
      
      # C19Elec <-
      #   read_csv(
      #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/C19Elec.csv"
      #   )
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "DailyDemandWorking")[c(1,2,4,3)]
      
      Data <- Data[complete.cases(Data),]
      
      names(Data) <- c("Year", "Gas", "Transport", "Electricity")
      
      Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
      
      C19Elec <- Data
      
      ### variables
      ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: Sheffield, National Grid, BEIS"
      plottitle = "Energy use in Scotland per day"
      
      #C19Elec$GasPercentage <- PercentLabel(C19Elec$Gas)
      
      
      C19ElecChart <- C19Elec %>%
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
          x = mean(C19Elec$Year),
          y = max(C19Elec$Gas),
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
          x = mean(C19Elec$Year),
          y = mean(C19Elec$Electricity),
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
          x = mean(C19Elec$Year),
          y = mean(C19Elec$Transport),
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
          x = C19Elec$Year[which(C19Elec$Gas == max(C19Elec$Gas))]-2,
          xend = C19Elec$Year[which(C19Elec$Gas == max(C19Elec$Gas))] - 30,
          y = max(C19Elec$Gas),
          yend = max(C19Elec$Gas),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = C19Elec$Year[which(C19Elec$Gas == max(C19Elec$Gas))] - 35,
          y = max(C19Elec$Gas),
          label = paste(round(max(C19Elec$Gas), digits = 0), "GWh"),
          hjust = 1,
          fontface = 2,
          size = 4,
          colour = ChartColours[2],
          family = "Century Gothic"
        )+
        annotate(
          "segment",
          x = C19Elec$Year[which(C19Elec$Electricity == max(C19Elec$Electricity[which(C19Elec$Year > dmy("01/08/18"))]))]+2,
          xend = C19Elec$Year[which(C19Elec$Electricity == max(C19Elec$Electricity[which(C19Elec$Year > dmy("01/08/18"))]))] + 30,
          y = max(C19Elec$Electricity[which(C19Elec$Year > dmy("01/08/18"))]),
          yend = max(C19Elec$Electricity[which(C19Elec$Year > dmy("01/08/18"))]),
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = C19Elec$Year[which(C19Elec$Electricity == max(C19Elec$Electricity[which(C19Elec$Year > dmy("01/08/18"))]))] + 35,
          y = max(C19Elec$Electricity[which(C19Elec$Year > dmy("01/08/18"))]),
          label = paste(round(max(C19Elec$Electricity[which(C19Elec$Year > dmy("01/08/18"))]), digits = 0), "GWh"),
          hjust = 0,
          fontface = 2,
          size = 4,
          colour = ChartColours[3],
          family = "Century Gothic"
        )
      
      
      C19ElecChart
      
      C19ElecChart <-
        DailyChart(C19ElecChart,
                   C19Elec,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      C19ElecChart <- C19ElecChart +
        coord_cartesian(xlim = c(min(C19Elec$Year), max(C19Elec$Year)+130)) +
        
        ylim(-15, 352) +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      C19ElecChart
      
      ggsave(
        file,
        plot =  C19ElecChart,
        width = 30,
        height = 12,
        units = "cm",
        dpi = 300
      )
    }
)

output$C19ElecRolling.png <- downloadHandler(
  filename = "C19ElecRolling.png",
  content = function(file) {
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "DailyDemandWorking")[c(1,5,7,6)]
    
    names(Data) <- c("Year", "Gas", "Transport", "Electricity")
    
    Data$Year <- as.Date(Data$Year, format = "%d/%m/%Y")
    
    C19ElecRolling <- Data[complete.cases(Data),]
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day\n12 month rolling average"
    
    #C19ElecRolling$GasPercentage <- PercentLabel(C19ElecRolling$Gas)
    
    
    C19ElecRollingChart <- C19ElecRolling %>%
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
        x = mean(C19ElecRolling$Year),
        y = max(C19ElecRolling$Gas),
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
        x = mean(C19ElecRolling$Year),
        y = mean(C19ElecRolling$Electricity),
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
        x = mean(C19ElecRolling$Year),
        y = mean(C19ElecRolling$Transport),
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
          y = C19ElecRolling$Gas[which(C19ElecRolling$Year == min(C19ElecRolling$Year))],
          label = paste0(round(C19ElecRolling$Gas[which(C19ElecRolling$Year == min(C19ElecRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = C19ElecRolling$Gas[which(C19ElecRolling$Year == max(C19ElecRolling$Year))],
          label = paste0(round(C19ElecRolling$Gas[which(C19ElecRolling$Year == max(C19ElecRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = C19ElecRolling$Electricity[which(C19ElecRolling$Year == min(C19ElecRolling$Year))],
          label = paste0(round(C19ElecRolling$Electricity[which(C19ElecRolling$Year == min(C19ElecRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = C19ElecRolling$Electricity[which(C19ElecRolling$Year == max(C19ElecRolling$Year))],
          label = paste0(round(C19ElecRolling$Electricity[which(C19ElecRolling$Year == max(C19ElecRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = min(Year)-50,
          y = C19ElecRolling$Transport[which(C19ElecRolling$Year == min(C19ElecRolling$Year))],
          label = paste0(round(C19ElecRolling$Transport[which(C19ElecRolling$Year == min(C19ElecRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = max(Year)+50,
          y = C19ElecRolling$Transport[which(C19ElecRolling$Year == max(C19ElecRolling$Year))],
          label = paste0(round(C19ElecRolling$Transport[which(C19ElecRolling$Year == max(C19ElecRolling$Year))], digits = 0), "\nGWh"),
          hjust = 0.5,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      )
    
    C19ElecRollingChart
    
    C19ElecRollingChart <-
      DailyChart(C19ElecRollingChart,
                 C19ElecRolling,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    C19ElecRollingChart <- C19ElecRollingChart +
      coord_cartesian(xlim = c(min(C19ElecRolling$Year)-30, max(C19ElecRolling$Year)+30)) +
      ylim(-5,190)+
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.7,
        linetype = 2
      )
    
    
    C19ElecRollingChart
    
    ggsave(
      file,
      plot =  C19ElecRollingChart,
      width = 18,
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
