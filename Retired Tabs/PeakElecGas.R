require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

PeakElecGasOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Electricity",
    fluidRow(column(8,
                    h3("Electricity demand on day of peak electricity demand", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('ElecPeakElecGasSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecPeakElecGas.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("ElecPeakElecGasPlot")),
    plotlyOutput(ns("ElecPeakElecGasPlot"))%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Gas",
             fluidRow(column(8,
                             h3("Gas demand on day of peak gas demand", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('GasPeakElecGasSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('GasPeakElecGas.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("ElecPeakElecGasPlot")),
             plotlyOutput(ns("GasPeakElecGasPlot"))%>% withSpinner(color="#5d8be1"),
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
      column(12, dataTableOutput(ns("ElecPeakElecGasTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Gas",
             fluidRow(
               column(10, h3("Data", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("GasPeakElecGasTable"))%>% withSpinner(color="#5d8be1"))),
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
PeakElecGas <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("PeakElecGas.R")

  
  output$ElecPeakElecGasSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Peak elec and gas day", skip = 15, col_names = FALSE)
    
    Date <- as.Date(as.numeric(as.character(Data[1,1])), origin = "1899-12-30")
    
    
    paste("Scotland:", Date)
  })
  
  output$ElecPeakElecGasPlot <- renderPlotly  ({
    
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
    
    ElecPeakElecGas <- Data[,2:3]
    
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    
    
    p <-  plot_ly(ElecPeakElecGas,x = ~ Year ) %>% 
      add_trace(data = ElecPeakElecGas,
                x = ~ Year,
                y = ~ Demand,
                name = "Demand",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Demand: ",
                  round(ElecPeakElecGas$Demand, digits = 0),
                  " MW\nTime: ",
                  format(ElecPeakElecGas$Year, "%H:%M:%S")
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
                     range = c(min(ElecPeakElecGas$Year)-100, max(ElecPeakElecGas$Year)+100)),
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
  
  output$ElecPeakElecGasTable = renderDataTable({
    
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
    
    ElecPeakElecGas <- Data[,2:3]
    
    datatable(
      ElecPeakElecGas,
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
                   
                   HTML(
                     paste(readtext("Structure/6 - System Security/PeakElecGas.txt")[2])
                     
                   )))
  })
 
  observeEvent(input$ToggleTable, {
    toggle("ElecPeakElecGasTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("GasPeakElecGasTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ElecPeakElecGas.png <- downloadHandler(
    filename = "ElecPeakElecGas.png",
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
      
      ElecPeakElecGas <- Data[,2:3]
      
      
      ### variables
      ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = paste("Electricity demand :", format(min(
        ElecPeakElecGas$Year), format = "%d/%m/%Y"))
      
      #ElecPeakElecGas$TotalPercentage <- PercentLabel(ElecPeakElecGas$Total)
      
      
      ElecPeakElecGasChart <- ElecPeakElecGas %>%
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
          x = ElecPeakElecGas$Year[11],
          y = ElecPeakElecGas$Demand[11] - 200,
          label = paste(round(ElecPeakElecGas$Demand[11], digits = 0), "kWh"),
          family = "Century Gothic",
          colour = "#034e7b",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = ElecPeakElecGas$Year[11],
          xend = ElecPeakElecGas$Year[11],
          y = ElecPeakElecGas$Demand[11] - 150,
          yend = ElecPeakElecGas$Demand[11] - 50,
          colour = "#034e7b"
        ) +
        annotate(
          "text",
          x = ElecPeakElecGas$Year[16],
          y = ElecPeakElecGas$Demand[16]+ 400,
          label = paste(round(ElecPeakElecGas$Demand[16], digits = 0), "kWh"),
          family = "Century Gothic",
          colour = "#3690c0",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = ElecPeakElecGas$Year[16],
          xend = ElecPeakElecGas$Year[16],
          y = ElecPeakElecGas$Demand[16]+50,
          yend = ElecPeakElecGas$Demand[16] + 350,
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = ElecPeakElecGas$Year[36],
          y = ElecPeakElecGas$Demand[36] + 200,
          label = paste(round(ElecPeakElecGas$Demand[36], digits = 0), "kWh"),
          family = "Century Gothic",
          colour = "#3690c0",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = ElecPeakElecGas$Year[36],
          xend = ElecPeakElecGas$Year[36],
          y = ElecPeakElecGas$Demand[36]+50,
          yend = ElecPeakElecGas$Demand[36] + 150,
          colour = "#3690c0"
        )
      
      ElecPeakElecGasChart
      
      ElecPeakElecGasChart <-
        DailyChart(
          ElecPeakElecGasChart,
          ElecPeakElecGas,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      
      ElecPeakElecGasChart <- ElecPeakElecGasChart +
        coord_cartesian(xlim = c(
          min(ElecPeakElecGas$Year) - 3000,
          max(ElecPeakElecGas$Year) + 3000
        )) +
        ylim(-150, 5600) +
        labs(subtitle = "Scotland") +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      ElecPeakElecGasChart
      
      ggsave(
        file,
        plot =  ElecPeakElecGasChart,
        width = 11,
        height = 11,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
  output$GasPeakElecGasSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Peak elec and gas day", skip = 15, col_names = FALSE)
    
    Date <- Data[1,10]
    
    
    paste("Scotland:", Date)
  })
  
  output$GasPeakElecGasPlot <- renderPlotly  ({
    
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
    
    GasPeakElecGas <- Data[,2:3][complete.cases(Data)]
    
    
    ### variables
    ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
    
    
    p <-  plot_ly(GasPeakElecGas,x = ~ Year ) %>% 
      add_trace(data = GasPeakElecGas,
                x = ~ Year,
                y = ~ Demand,
                name = "Demand",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Demand: ",
                  round(GasPeakElecGas$Demand, digits = 0),
                  " MW\nTime: ",
                  format(GasPeakElecGas$Year, "%H:%M:%S")
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
                     range = c(min(GasPeakElecGas$Year)-100, max(GasPeakElecGas$Year)+100)),
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
  
  output$GasPeakElecGasTable = renderDataTable({
    
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
    
    GasPeakElecGas <- Data[,2:3][complete.cases(Data)]
    
    names(GasPeakElecGas) <- c("Time", "Demand (MW)")
    
    datatable(
      GasPeakElecGas,
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
  
  output$GasPeakElecGas.png <- downloadHandler(
    filename = "GasPeakElecGas.png",
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
      
      GasPeakElecGas <- Data[,2:3][complete.cases(Data)]
      
      
      ### variables
      ChartColours <- c("#5d8be1", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = paste("Gas demand :", format(min(
        GasPeakElecGas$Year), format = "%d/%m/%Y"))
      
      #GasPeakElecGas$TotalPercentage <- PercentLabel(GasPeakElecGas$Total)
      
      
      GasPeakElecGasChart <- GasPeakElecGas %>%
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
          x = GasPeakElecGas$Year[1],
          y = GasPeakElecGas$Demand[1] - 400,
          label = paste(format(round(GasPeakElecGas$Demand[1], digits = 0), big.mark = ","), "kWh"),
          family = "Century Gothic",
          colour = "#034e7b",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = GasPeakElecGas$Year[1],
          xend = GasPeakElecGas$Year[1],
          y = GasPeakElecGas$Demand[1] - 350,
          yend = GasPeakElecGas$Demand[1] - 150,
          colour = "#034e7b"
        ) +
        annotate(
          "text",
          x = GasPeakElecGas$Year[3],
          y = GasPeakElecGas$Demand[3]+ 600,
          label = paste(format(round(GasPeakElecGas$Demand[3], digits = 0), big.mark = ","), "kWh"),
          family = "Century Gothic",
          colour = "#3690c0",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = GasPeakElecGas$Year[3],
          xend = GasPeakElecGas$Year[3],
          y = GasPeakElecGas$Demand[3]+50,
          yend = GasPeakElecGas$Demand[3] + 350,
          colour = "#3690c0"
        ) +
        annotate(
          "text",
          x = GasPeakElecGas$Year[13],
          y = GasPeakElecGas$Demand[13] + 400,
          label = paste(format(round(GasPeakElecGas$Demand[13], digits = 0), big.mark = ","), "kWh"),
          family = "Century Gothic",
          colour = "#3690c0",
          fontface = 2
        ) +
        annotate(
          "segment",
          x = GasPeakElecGas$Year[13],
          xend = GasPeakElecGas$Year[13],
          y = GasPeakElecGas$Demand[13]+150,
          yend = GasPeakElecGas$Demand[13] + 350,
          colour = "#3690c0"
        )
      
      GasPeakElecGasChart
      
      GasPeakElecGasChart <-
        DailyChart(
          GasPeakElecGasChart,
          GasPeakElecGas,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      
      GasPeakElecGasChart <- GasPeakElecGasChart +
        coord_cartesian(xlim = c(
          min(GasPeakElecGas$Year) - 6000,
          max(GasPeakElecGas$Year) + 3000
        )) +
        ylim(-200, 16000) +
        labs(subtitle = "Scotland") +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      GasPeakElecGasChart
      
      ggsave(
        file,
        plot =  GasPeakElecGasChart,
        width = 11,
        height = 11,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
