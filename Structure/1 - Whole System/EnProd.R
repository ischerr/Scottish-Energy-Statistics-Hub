require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnProdOutput <- function(id) {
  ns <- NS(id)
  tagList(
      tabsetPanel(
      tabPanel("Energy productivity target progress", 
             fluidRow(
                     column(8,
                      h3("Energy productivity target progress", style = "color: #1A5D38;  font-weight:bold"),
                      h4(textOutput(ns('EnProdSubtitle')), style = "color: #1A5D38;")
                     ),
                      column(
                           4, style = 'padding:15px;',
                           downloadButton(ns('EnProd.png'), 'Download Graph', style="float:right")
                         )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("RenEnTgtPlot")),
             plotlyOutput(ns("EnProdPlot"))%>% withSpinner(color="#1A5D38"),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
      tabPanel("Change in energy productivity", 
             fluidRow(column(8,
                                h3("Estimated change in energy productivity", style = "color: #1A5D38;  font-weight:bold"),
                             h4(textOutput(ns('EnProdHistSubtitle')), style = "color: #1A5D38;")
             ),
                         column(
                           4, style = 'padding:15px;',
                           downloadButton(ns('EnProdHist.png'), 'Download Graph', style="float:right")
                         )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("RenEnTgtPlot")),
             plotlyOutput(ns("EnProdHistPlot"))%>% withSpinner(color="#1A5D38"),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"))
    ),
    fluidRow(
      column(10,h3("Commentary", style = "color: #1A5D38;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    fluidRow(
      column(10, h3("Data", style = "color: #1A5D38;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnProdTable"))%>% withSpinner(color="#1A5D38"))),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
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
EnProd <- function(input, output, session) {
  # output$RenEnTgtPlot <- renderDygraph({
  #   RenEn <-
  #     read.csv(
  #       "Structure/1 - Whole System/RenEnTgt.csv",
  #       header = TRUE,
  #       sep = ",",
  #       na.strings = "-"
  #     )
  #
  #   YearLow <- as.numeric(min(RenEn$Year))
  #   YearHigh <- as.numeric(max(RenEn$Year +1))
  #
  #   dygraph(RenEn, main = "Renewable Energy Target") %>%
  #     dyAxis("y", label = "% Progress", valueRange = c(0,30)) %>%
  #     dyAxis("x", label = "Year", drawGrid = TRUE) %>%
  #     dyOptions(colors =  c("Green","Orange", "Blue")) %>%
  #     dyLegend(width = 170 ,
  #              labelsSeparateLines = TRUE ,
  #              show = "always") %>%
  #     dyOptions(
  #       stackedGraph = TRUE,
  #       axisLineColor = "white",
  #       gridLineColor = "white",
  #       includeZero = TRUE,
  #       fillAlpha = .65
  #     ) %>%
  #     #    dyRangeSelector() %>%
  #     dyCSS("Structure/1 - Whole System/legend.css")
  #
  # })
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("EnProd.R")


#####################################################################
  output$EnProdPlot <- renderPlotly  ({
    
    EnProd <- read_excel(
      "Structure/CurrentWorking.xlsx", 
      sheet = "Energy productivity", col_names = FALSE, 
      skip = 16, n_max = 4)
    
    EnProd <- as.data.frame(t(EnProd))
    
    EnProd <- EnProd[c(1,3)]
    
    EnProd <- EnProd[complete.cases(EnProd),]
    
    names(EnProd) <- c("Year", "Renewables")
    
    EnProd %<>% lapply(function(x) as.numeric(as.character(x)))
    
    EnProd <- merge(EnProd, data.frame(Year = 2030, Renewables = NA, Tgt = 0.3), all = T)
    
    ### Variables
    ChartColours <- c("#1a5d38", "#FF8500")
    sourcecaption = "Source: BEIS, SG"
    plottitle = "Energy productivity target progress"
    
    #EnProd$OilPercentage <- PercentLabel(EnProd$Oil)
    
    EnProd$Year <-
      paste0("01/01/", EnProd$Year)
    
    EnProd$Year <- dmy(EnProd$Year)
    
    p <-  plot_ly(EnProd, x = ~ Year) %>% 
      add_trace(
      y = ~ Renewables,
      name = "Renewables",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "Progress: ",
        percent(EnProd$Renewables, accuracy = 0.1),
        "\nYear: ",
        format(EnProd$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = tail(EnProd[which(EnProd$Renewables > 0 | EnProd$Renewables < 0),],1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewables",
        text = paste0(
          "Progress: ",
          percent(tail(EnProd[which(EnProd$Renewables > 0 | EnProd$Renewables < 0),],1)$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(tail(EnProd[which(EnProd$Renewables > 0 | EnProd$Renewables < 0),],1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>%
      add_trace(
        data = EnProd[nrow(EnProd), ],
        x = ~ Year,
        y = ~ Tgt,
        name = "Target",
        text = paste0(
          "Target: ",
          percent(EnProd$Tgt, accuracy = 0.1),
          "\nYear: ",
          format(EnProd$Year, "%Y")
        ),
        hoverinfo = 'text',
        mode = 'markers',
        marker = list(size = 25,
                      symbol = "diamond",
                      color = ChartColours[2])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(EnProd$Year)-100, max(EnProd$Year)+100)),
        yaxis = list(
          title = "",
          tickformat = "%",
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
  
  output$EnProdSubtitle <- renderText({
    
    EnProd <- read_excel(
      "Structure/CurrentWorking.xlsx", 
      sheet = "Energy productivity", col_names = FALSE, 
      skip = 16, n_max = 4)
    
    EnProd <- as.data.frame(t(EnProd))
    
    EnProd <- EnProd[c(1,3)]
    
    EnProd <- EnProd[complete.cases(EnProd),]
    
    names(EnProd) <- c("Year", "Renewables")
    
    EnProd %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(EnProd$Year),"-", max(EnProd$Year))
    
    
    
  })

  output$EnProdHistPlot <- renderPlotly  ({
    
    EnProdHist <- read_excel(
      "Structure/CurrentWorking.xlsx", 
      sheet = "Energy productivity", col_names = FALSE, 
      skip = 16, n_max = 4)
    
    
    EnProdHist <- as.data.frame(t(EnProdHist))
    
    EnProdHist <- EnProdHist[c(1,4)]
    
    EnProdHist <- EnProdHist[complete.cases(EnProdHist),]
    
    names(EnProdHist) <- c("Year", "Renewables")
    
    EnProdHist %<>% lapply(function(x) as.numeric(as.character(x)))
    
    EnProdHist <- as.data.frame(EnProdHist)
    
    plottitle <- "Estimated change in energy productivity"
    sourcecaption <- "Source: BEIS, SG"
    ChartColours <- c("#1a5d38", "#FF8500")
    
    #EnProdHist$OilPercentage <- PercentLabel(EnProdHist$Oil)
    
    EnProdHist$Year <-
      paste0("01/01/", EnProdHist$Year)
    
    EnProdHist$Year <- dmy(EnProdHist$Year)
    
    p <-  plot_ly(
      EnProdHist,
      x = ~ Year,
      y = ~ Renewables,
      name = "Renewables",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "Progress: ",
        percent(EnProdHist$Renewables, accuracy = 0.1),
        "\nYear: ",
        format(EnProdHist$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = EnProdHist[nrow(EnProdHist), ],
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewables",
        text = paste0(
          "Progress: ",
          percent(EnProdHist$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(EnProdHist$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[1])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(EnProdHist$Year)-100, max(EnProdHist$Year)+100)),
        yaxis = list(
          title = "",
          tickformat = "%",
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
  
  output$EnProdHistSubtitle <- renderText({
    
    EnProdHist <- read_excel(
      "Structure/CurrentWorking.xlsx", 
      sheet = "Energy productivity", col_names = FALSE, 
      skip = 16, n_max = 4)
    
    
    EnProdHist <- as.data.frame(t(EnProdHist))
    
    EnProdHist <- EnProdHist[c(1,4)]
    
    EnProdHist <- EnProdHist[complete.cases(EnProdHist),]
    
    names(EnProdHist) <- c("Year", "Renewables")
    
    EnProdHist %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(EnProdHist$Year),"-", max(EnProdHist$Year))
    
    })
  
  output$EnProdTable = renderDataTable({
    
    EnProdData <- read_excel(
      "Structure/CurrentWorking.xlsx", 
      sheet = "Energy productivity", col_names = FALSE, 
      skip = 16, n_max = 10)
    
    
    EnProdData <- as.data.frame(t(EnProdData))
    
    EnProdData <- EnProdData[c(1:4,9:10)]
    
    names(EnProdData) <- as.character(unlist(EnProdData[1,]))
    
    EnProdData <- EnProdData[-1,]
    
    names(EnProdData)[1] <- "Year"
    
    EnProdData %<>% lapply(function(x) as.numeric(as.character(x)))
    
    EnProdData <- as_tibble(EnProdData)
    
    
    datatable(
      EnProdData,
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
        title = "Energy Productivity",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Energy Productivity',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Energy Productivity')
        ),
        
        # customize the length menu
      lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(3:4, 1) %>% 
      formatRound(5:7, 0) %>% 
      formatRound(2,3)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/1 - Whole System/EnProd.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("EnProdTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnProd.png <- downloadHandler(
    filename = "EnProd.png",
    content = function(file) {
      ### Load Packages and Functions
      ###
      
      
      # RenEn2 <-
      #   read.csv(
      #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Datae/RenEnTgt.csv",
      #     header = TRUE,
      #     sep = ",",
      #     na.strings = "-"
      #   )
      
      ### variables
      
      EnProd <- read_excel(
        "Structure/CurrentWorking.xlsx", 
        sheet = "Energy productivity", col_names = FALSE, 
        skip = 16, n_max = 4)
      
      EnProd <- as.data.frame(t(EnProd))
      
      EnProd <- EnProd[c(1,3)]
      
      EnProd <- EnProd[complete.cases(EnProd),]
      
      names(EnProd) <- c("Year", "Renewables")
      
      EnProd %<>% lapply(function(x) as.numeric(as.character(x)))
      
      EnProd <- merge(EnProd, data.frame(Year = 2030, Renewables = NA, Tgt = 0.3), all = T)
      
      ### Variables
      ChartColours <- c("#1a5d38", "#FF8500")
      sourcecaption = "Source: BEIS, SG"
      plottitle = "Energy productivity target progress"
      
      EnProdChart <-
        TargetChart(EnProd, plottitle, sourcecaption, ChartColours)
      
      EnProdChart <- EnProdChart +
        xlim(min(EnProd$Year -1), max(EnProd$Year +1))
      
      ggsave(
        file,
        plot = EnProdChart,
        width = 14,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EnProdHist.png <- downloadHandler(
    filename = "EnProdHist.png",
    content = function(file) {
      ### Load Packages and Functions
      ###
      
      
      # RenEn2 <-
      #   read.csv(
      #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Datae/RenEnTgt.csv",
      #     header = TRUE,
      #     sep = ",",
      #     na.strings = "-"
      #   )
      
      ### variables
      
      EnProdHist <- read_excel(
        "Structure/CurrentWorking.xlsx", 
        sheet = "Energy productivity", col_names = FALSE, 
        skip = 16, n_max = 4)
      
      
      EnProdHist <- as.data.frame(t(EnProdHist))
      
      EnProdHist <- EnProdHist[c(1,4)]
      
      EnProdHist <- EnProdHist[complete.cases(EnProdHist),]
      
      names(EnProdHist) <- c("Year", "Renewables")
      
      EnProdHist %<>% lapply(function(x) as.numeric(as.character(x)))
      
      EnProdHist <- as.data.frame(EnProdHist)
      
      plottitle <- "Estimated change in energy productivity"
      sourcecaption <- "Source: BEIS, SG"
      ChartColours <- c("#1a5d38", "#FF8500")
      
      
      EnProdHistChart <-
        EnProdHist %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        
        ### Line of Values
        geom_line(
          aes(y = Renewables,
              colour = ChartColours[1]),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == min(Year), percent(Renewables, accuracy = 0.1), ""),
            hjust = 0.9,
            vjust = -.8,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == max(Year), percent(Renewables, accuracy = 0.1), ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnProdHist, 1),
          aes(
            x = Year,
            y = Renewables,
            colour = ChartColours[1],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year) |
                             Year == min(Year), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == 2015, Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          aes(
            x = 2015,
            y = Renewables[which(Year == 2015)],
            colour = ChartColours[1],
            show_guide = FALSE
          ),
          size = 3,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == 2015, percent(Renewables, accuracy = 0.1), ""), ### NEEDS AUTOMATED
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      EnProdHistChart <-
        LinePercentChart(EnProdHistChart,
                         EnProdHist,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      EnProdHistChart <- EnProdHistChart +
        xlim(min(EnProdHist$Year)-0.5,max(EnProdHist$Year)+0.5)
      
      EnProdHistChart
      
      ggsave(
        file,
        plot = EnProdHistChart,
        width = 14,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  )
}
