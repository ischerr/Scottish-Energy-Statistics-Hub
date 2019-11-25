require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

PeakDayOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Electricity",
    fluidRow(column(8,
                    h3("Electricity demand on day of peak electricity demand", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('ElecPeakDaySubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecPeakDay.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("ElecPeakDayPlot")),
    plotlyOutput(ns("ElecPeakDayPlot"))%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Gas",
             fluidRow(column(8,
                             h3("Gas demand on day of peak gas demand", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('GasPeakDaySubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasPeakDay.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("ElecPeakDayPlot")),
             plotlyOutput(ns("GasPeakDayPlot"))%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    tabsetPanel(
      tabPanel("Electricity",
    fluidRow(
    column(10, h3("Data", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ElecPeakDayTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Gas",
             fluidRow(
               column(10, h3("Data", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("GasPeakDayTable"))%>% withSpinner(color="#5d8be1"))),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
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
PeakDay <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("PeakDay.R")

  
  output$ElecPeakDaySubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Peak elec and gas day", skip = 15, col_names = FALSE)
    
    Date <- as.Date(as.numeric(as.character(Data[1,1])), origin = "1899-12-30")
    
    
    paste("Scotland:", Date)
  })
  
  output$ElecPeakDayPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Peak elec and gas day", skip = 15, col_names = FALSE)
    
    Date <- as.Date(as.numeric(as.character(Data[1,1])), origin = "1899-12-30")
    
    Data <- Data[1:2]
    
    Data <- tail(Data, -2)
    
    names(Data) <- c("Year", "Demand")
    
    Year <- ymd_hms(paste(Date, "00:00:00"))
    
    HalfHour <- hms("00:30:00")
    
    setDT(Data, keep.rownames = TRUE)[]
    
    Data$Year <- Year + (HalfHour * (as.numeric(Data$rn)-1))
    
    Data$Demand <- as.numeric(Data$Demand)
    
    Data$Year <- ymd_hms(Data$Year)
    
    ElecPeakDay <- Data[,2:3]
    
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    
    
    p <-  plot_ly(ElecPeakDay,x = ~ Year ) %>% 
      add_trace(data = ElecPeakDay,
                x = ~ Year,
                y = ~ Demand,
                name = "Demand",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Demand: ",
                  round(ElecPeakDay$Demand, digits = 0),
                  " MW\nTime: ",
                  format(ElecPeakDay$Year, "%H:%M:%S")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      
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
                     range = c(min(ElecPeakDay$Year)-100, max(ElecPeakDay$Year)+100)),
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
    p
    
    
    
  })
  
  output$ElecPeakDayTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Peak elec and gas day", skip = 15, col_names = FALSE)
    
    Date <- as.Date(as.numeric(as.character(Data[1,1])), origin = "1899-12-30")
    
    Data <- Data[1:2]
    
    Data <- tail(Data, -2)
    
    names(Data) <- c("Year", "Demand")
    
    Year <- ymd_hms(paste(Date, "00:00:00"))
    
    HalfHour <- hms("00:30:00")
    
    setDT(Data, keep.rownames = TRUE)[]
    
    Data$Year <- Year + (HalfHour * (as.numeric(Data$rn)-1))
    
    Data$Demand <- as.numeric(Data$Demand)
    
    names(Data) <- c("rn", "Time", "Demand (MW)")
    
    ElecPeakDay <- Data[,2:3]
    
    datatable(
      ElecPeakDay,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'asc')),
        title = "Hourly gas demand on peak day, 2018/19: 29/01/2019",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Half-hourly electricity demand on peak day, 2018/19: 29/01/2019',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Half-hourly electricity demand on peak day, 2018/19: 29/01/2019')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2, 0) %>% 
      formatDate(1, "toLocaleTimeString")
  })
  
 output$Text <- renderUI({
   tagList(column(12,
                                   tags$p(
                                     HTML(
                                       "<p>Electricity generated and consumed</p>"
                                     )
                                   )))
 })
  observeEvent(input$ToggleTable, {
    toggle("ElecPeakDayTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("GasPeakDayTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ElecPeakDay.png <- downloadHandler(
    filename = "ElecPeakDay.png",
    content = function(file) {

      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Peak elec and gas day", skip = 15, col_names = FALSE)
      
      Date <- as.Date(as.numeric(as.character(Data[1,1])), origin = "1899-12-30")
      
      Data <- Data[1:2]
      
      Data <- tail(Data, -2)
      
      names(Data) <- c("Year", "Demand")
      
      Year <- ymd_hms(paste(Date, "00:00:00"))
      
      HalfHour <- hms("00:30:00")
      
      setDT(Data, keep.rownames = TRUE)[]
      
      Data$Year <- Year + (HalfHour * (as.numeric(Data$rn)-1))
      
      Data$Demand <- as.numeric(Data$Demand)
      
      Data$Year <- ymd_hms(Data$Year)
      
      ElecPeakDay <- Data[,2:3]
      
      
      ### variables
      ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = paste("Electricity demand :", format(min(
        ElecPeakDay$Year), format = "%d/%m/%Y"))
      
      #ElecPeakDay$TotalPercentage <- PercentLabel(ElecPeakDay$Total)
      
      
      ElecPeakDayChart <- ElecPeakDay %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = Demand,
              label = Demand),
          colour = ChartColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year %in% quantile(Year), format(Year, format = "%H:%M"), ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ElecPeakDay$Year[11],
          y = ElecPeakDay$Demand[11] - 200,
          label = paste(round(ElecPeakDay$Demand[11], digits = 0), "kWh"),
          family = "Century Gothic",
          colour = "#034e7b",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = ElecPeakDay$Year[11],
          xend = ElecPeakDay$Year[11],
          y = ElecPeakDay$Demand[11] - 150,
          yend = ElecPeakDay$Demand[11] - 50,
          colour = "#034e7b"
        ) +
        annotate(
          "text",
          x = ElecPeakDay$Year[16],
          y = ElecPeakDay$Demand[16]+ 400,
          label = paste(round(ElecPeakDay$Demand[16], digits = 0), "kWh"),
          family = "Century Gothic",
          colour = "#3690c0",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = ElecPeakDay$Year[16],
          xend = ElecPeakDay$Year[16],
          y = ElecPeakDay$Demand[16]+50,
          yend = ElecPeakDay$Demand[16] + 350,
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = ElecPeakDay$Year[36],
          y = ElecPeakDay$Demand[36] + 200,
          label = paste(round(ElecPeakDay$Demand[36], digits = 0), "kWh"),
          family = "Century Gothic",
          colour = "#3690c0",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = ElecPeakDay$Year[36],
          xend = ElecPeakDay$Year[36],
          y = ElecPeakDay$Demand[36]+50,
          yend = ElecPeakDay$Demand[36] + 150,
          colour = "#3690c0"
        )
      
      ElecPeakDayChart
      
      ElecPeakDayChart <-
        DailyChart(
          ElecPeakDayChart,
          ElecPeakDay,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      
      ElecPeakDayChart <- ElecPeakDayChart +
        coord_cartesian(xlim = c(
          min(ElecPeakDay$Year) - 3000,
          max(ElecPeakDay$Year) + 3000
        )) +
        ylim(-150, 5600) +
        labs(subtitle = "Scotland") +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      ElecPeakDayChart
      
      ggsave(
        file,
        plot =  ElecPeakDayChart,
        width = 11,
        height = 11,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
  output$GasPeakDaySubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Peak elec and gas day", skip = 15, col_names = FALSE)
    
    Date <- Data[1,10]
    
    
    paste("Scotland:", Date)
  })
  
  output$GasPeakDayPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Peak elec and gas day", skip = 15, col_names = FALSE)
    
    Date <- substr(Data[1,10], 1, 10)
    
    Data <- Data[10:11]
    
    Data <- tail(Data, -2)
    
    names(Data) <- c("Year", "Demand")
    
    Year <- dmy_hms(paste(Date, "06:00:00"))
    
    Hour <- hms("01:00:00")
    
    setDT(Data, keep.rownames = TRUE)[]
    
    Data$Year <- Year + (Hour * (as.numeric(Data$rn)-1))
    
    Data$Demand <- as.numeric(Data$Demand)
    
    Data$Year <- ymd_hms(Data$Year)
    
    GasPeakDay <- Data[,2:3][complete.cases(Data)]
    
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    
    
    p <-  plot_ly(GasPeakDay,x = ~ Year ) %>% 
      add_trace(data = GasPeakDay,
                x = ~ Year,
                y = ~ Demand,
                name = "Demand",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Demand: ",
                  round(GasPeakDay$Demand, digits = 0),
                  " MW\nTime: ",
                  format(GasPeakDay$Year, "%H:%M:%S")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      
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
                     range = c(min(GasPeakDay$Year)-100, max(GasPeakDay$Year)+100)),
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
    p
    
    
    
  })
  
  output$GasPeakDayTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Peak elec and gas day", skip = 15, col_names = FALSE)
    
    Date <- substr(Data[1,10], 1, 10)
    
    Data <- Data[10:11]
    
    Data <- tail(Data, -2)
    
    names(Data) <- c("Year", "Demand")
    
    Year <- dmy_hms(paste(Date, "06:00:00"))
    
    Hour <- hms("01:00:00")
    
    setDT(Data, keep.rownames = TRUE)[]
    
    Data$Year <- Year + (Hour * (as.numeric(Data$rn)-1))
    
    Data$Demand <- as.numeric(Data$Demand)
    
    Data$Year <- ymd_hms(Data$Year)
    
    GasPeakDay <- Data[,2:3][complete.cases(Data)]
    
    names(GasPeakDay) <- c("Time", "Demand")
    
    datatable(
      GasPeakDay,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'asc')),
        title = "Hourly gas demand on peak day, 2018/19: 31/01/2019",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Hourly gas demand on peak day, 2018/19: 31/01/2019',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Hourly gas demand on peak day, 2018/19: 31/01/2019')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2, 0) %>% 
      formatDate(1, "toLocaleTimeString")
  })
  
  output$GasPeakDay.png <- downloadHandler(
    filename = "GasPeakDay.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Peak elec and gas day", skip = 15, col_names = FALSE)
      
      Date <- substr(Data[1,10], 1, 10)
      
      Data <- Data[10:11]
      
      Data <- tail(Data, -2)
      
      names(Data) <- c("Year", "Demand")
      
      Year <- dmy_hms(paste(Date, "06:00:00"))
      
      Hour <- hms("01:00:00")
      
      setDT(Data, keep.rownames = TRUE)[]
      
      Data$Year <- Year + (Hour * (as.numeric(Data$rn)-1))
      
      Data$Demand <- as.numeric(Data$Demand)
      
      Data$Year <- ymd_hms(Data$Year)
      
      GasPeakDay <- Data[,2:3][complete.cases(Data)]
      
      
      ### variables
      ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = paste("Gas demand :", format(min(
        GasPeakDay$Year), format = "%d/%m/%Y"))
      
      #GasPeakDay$TotalPercentage <- PercentLabel(GasPeakDay$Total)
      
      
      GasPeakDayChart <- GasPeakDay %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = Demand,
              label = Demand),
          colour = ChartColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year %in% quantile(Year), format(Year, format = "%H:%M"), ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = GasPeakDay$Year[1],
          y = GasPeakDay$Demand[1] - 400,
          label = paste(format(round(GasPeakDay$Demand[1], digits = 0), big.mark = ","), "kWh"),
          family = "Century Gothic",
          colour = "#034e7b",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = GasPeakDay$Year[1],
          xend = GasPeakDay$Year[1],
          y = GasPeakDay$Demand[1] - 350,
          yend = GasPeakDay$Demand[1] - 150,
          colour = "#034e7b"
        ) +
        annotate(
          "text",
          x = GasPeakDay$Year[3],
          y = GasPeakDay$Demand[3]+ 600,
          label = paste(format(round(GasPeakDay$Demand[3], digits = 0), big.mark = ","), "kWh"),
          family = "Century Gothic",
          colour = "#3690c0",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = GasPeakDay$Year[3],
          xend = GasPeakDay$Year[3],
          y = GasPeakDay$Demand[3]+50,
          yend = GasPeakDay$Demand[3] + 350,
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = GasPeakDay$Year[13],
          y = GasPeakDay$Demand[13] + 400,
          label = paste(format(round(GasPeakDay$Demand[13], digits = 0), big.mark = ","), "kWh"),
          family = "Century Gothic",
          colour = "#3690c0",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = GasPeakDay$Year[13],
          xend = GasPeakDay$Year[13],
          y = GasPeakDay$Demand[13]+150,
          yend = GasPeakDay$Demand[13] + 350,
          colour = "#3690c0"
        )
      
      GasPeakDayChart
      
      GasPeakDayChart <-
        DailyChart(
          GasPeakDayChart,
          GasPeakDay,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      
      GasPeakDayChart <- GasPeakDayChart +
        coord_cartesian(xlim = c(
          min(GasPeakDay$Year) - 6000,
          max(GasPeakDay$Year) + 3000
        )) +
        ylim(-200, 16000) +
        labs(subtitle = "Scotland") +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      GasPeakDayChart
      
      ggsave(
        file,
        plot =  GasPeakDayChart,
        width = 11,
        height = 11,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
