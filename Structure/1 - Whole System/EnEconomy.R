require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



EnEconomyOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Energy Sector Employment",
               fluidRow(
                 column(
                   8,
                   h3("Employment in the energy sector", style = "color: #1A5D38;  font-weight:bold"),
                   h4(textOutput(ns('EnergySectorEmploymentSubtitle')), style = "color: #1A5D38;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('EnergySectorEmployment.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
               plotlyOutput(ns("EnergySectorEmploymentPlot"))%>% withSpinner(color="#1A5D38"),
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
      tabPanel("Turnover associated with the energy sector",
               fluidRow(
                 column(
                   8,
                   h3("Energy sector turnover", style = "color: #1A5D38;  font-weight:bold"),
                   h4(textOutput(ns('EnergySectorTurnoverSubtitle')), style = "color: #1A5D38;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('EnergySectorTurnover.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
               plotlyOutput(ns("EnergySectorTurnoverPlot"))%>% withSpinner(color="#1A5D38"),
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
      tabPanel("GVA associated with the energy sector",
               fluidRow(
                 column(
                   8,
                   h3("Energy sector GVA", style = "color: #1A5D38;  font-weight:bold"),
                   h4(textOutput(ns('EnergySectorGVASubtitle')), style = "color: #1A5D38;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('EnergySectorGVA.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
               plotlyOutput(ns("EnergySectorGVAPlot"))%>% withSpinner(color="#1A5D38"),
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
      tabPanel("Exports associated with the energy sector",
               fluidRow(
                 column(
                   8,
                   h3("Energy sector exports", style = "color: #1A5D38;  font-weight:bold"),
                   h4(textOutput(ns('EnergySectorExportsSubtitle')))
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('EnergySectorExports.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
               plotlyOutput(ns("EnergySectorExports"))%>% withSpinner(color="#1A5D38"),
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
      tabPanel("Total exports associated with the energy sector",
               fluidRow(
                 column(
                   8,
                   h3("Energy sector exports", style = "color: #1A5D38;  font-weight:bold"),
                   h4(textOutput(ns('EnergySectorTotalExportsSubtitle')))
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('EnergySectorTotalExports.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
               plotlyOutput(ns("EnergySectorTotalExports"))%>% withSpinner(color="#1A5D38"),
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
      column(
        2,
        style = "padding:15px",
        actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; ")
      )
    ),
    fluidRow(column(12, dataTableOutput(
      ns("EnEconomyTable")
    )%>% withSpinner(color="#1A5D38"))),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("SGGrowth"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("SGGrowth"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
             SourceLookup("SGGrowth")
             
      )
    )
  )
  
}




###### Server ######
EnEconomy <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("EnEconomy.R")
  ###### Energy Sector Emplyment ######
  
  output$EnergySectorEmploymentSubtitle <- renderText({
    
    EnergySectorEmployment <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Energy economy",
        col_names = FALSE,
        skip = 12,
        n_max = 4
      )
    
    EnergySectorEmployment <- as.data.frame(t(EnergySectorEmployment))
    
    EnergySectorEmployment <-
      tail(EnergySectorEmployment[c(1, 3:4)], -1)
    EnergySectorEmployment %<>% lapply(function(x)
      as.numeric(as.character(x)))
    EnergySectorEmployment <- as.data.frame(EnergySectorEmployment)
    EnergySectorEmployment[EnergySectorEmployment == 0] <- NA
    names(EnergySectorEmployment) <-
      c("Year", "Excluding PAYE", "Including PAYE")
    
    EnergySectorEmployment <-
      EnergySectorEmployment[which(EnergySectorEmployment$Year >= 2009), ]
    
    paste("Scotland,", min(EnergySectorEmployment$Year),"-", max(EnergySectorEmployment$Year))
  })
  
  
  output$EnergySectorEmploymentPlot <- renderPlotly  ({
    
    EnergySectorEmployment <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Energy economy",
        col_names = FALSE,
        skip = 12,
        n_max = 4
      )
    
    EnergySectorEmployment <- as.data.frame(t(EnergySectorEmployment))
    
    EnergySectorEmployment <-
      tail(EnergySectorEmployment[c(1, 3:4)], -1)
    EnergySectorEmployment %<>% lapply(function(x)
      as.numeric(as.character(x)))
    EnergySectorEmployment <- as.data.frame(EnergySectorEmployment)
    EnergySectorEmployment[EnergySectorEmployment == 0] <- NA
    names(EnergySectorEmployment) <-
      c("Year", "Excluding PAYE", "Including PAYE")
    
    EnergySectorEmployment <-
      EnergySectorEmployment[which(EnergySectorEmployment$Year >= 2009), ]
    ### variables
    ChartColours <- c("#1a5d38", "#41ab5d", "#1a5d38")
    sourcecaption = "Source: SG"
    plottitle = "Employment in the energy sector"  
    
    
    EnergySectorEmployment$Year <-
      paste0("01/01/", EnergySectorEmployment$Year)
    
    EnergySectorEmployment$Year <- dmy(EnergySectorEmployment$Year)
    
    p <-
      plot_ly(
        EnergySectorEmployment,
        x = ~ Year,
        y = ~ `Excluding PAYE`,
        name = "Excluding PAYe",
        type = 'scatter',
        mode = 'lines',
        legendgroup = "1",
        text = paste0(
          "Excluding PAYE: ",
          format(EnergySectorEmployment$`Excluding PAYE`, big.mark = ","),
          "\nYear: ",
          format(EnergySectorEmployment$Year, "%Y")
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[2], dash = "dash")
      ) %>%
      add_trace(
        y = ~ `Including PAYE`,
        name = "Including PAYe",
        mode = 'lines',
        legendgroup = "2",
        text = paste0(
          "Including PAYE: ",
          format(EnergySectorEmployment$`Including PAYE`, big.mark = ","),
          "\nYear: ",
          format(EnergySectorEmployment$Year, "%Y")
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>%
      add_trace(
        data = EnergySectorEmployment[nrow(EnergySectorEmployment), ],
        x = ~ Year,
        y = ~ `Including PAYE`,
        name = "Including PAYe",
        legendgroup = "2",
        text = paste0(
          "Including PAYE: ",
          format(EnergySectorEmployment$`Including PAYE`, big.mark = ","),
          "\nYear: ",
          format(EnergySectorEmployment$Year, "%Y")
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
                     range = c(min(EnergySectorEmployment$Year)-100, max(EnergySectorEmployment$Year)+100)),
        yaxis = list(
          title = "",
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
  
  
  
  output$EnergySectorTurnoverSubtitle <- renderText({
    
    EnergySectorTurnover <- read_excel("Structure/CurrentWorking.xlsx", 
                                       sheet = "Energy economy", col_names = FALSE, 
                                       skip = 12, n_max = 9)
    
    EnergySectorTurnover <- as.data.frame(t(EnergySectorTurnover))
    
    EnergySectorTurnover <- tail(EnergySectorTurnover[c(1,5)],-1)
    EnergySectorTurnover %<>% lapply(function(x) as.numeric(as.character(x)))
    EnergySectorTurnover[2] %<>% lapply(function(x) x/1000)
    EnergySectorTurnover <- as_tibble(EnergySectorTurnover)
    names(EnergySectorTurnover) <- c("Year", "Turnover")
    
    EnergySectorTurnover <- EnergySectorTurnover[which(EnergySectorTurnover$Year >= 2008),]
    
    EnergySectorTurnover$Year <- as.numeric(substr(EnergySectorTurnover$Year, 1,4))
    
    EnergySectorTurnover[EnergySectorTurnover == 0] <- NA
    
    EnergySectorTurnover <- EnergySectorTurnover[complete.cases(EnergySectorTurnover),]
    
    paste("Scotland,", min(EnergySectorTurnover$Year),"-", max(EnergySectorTurnover$Year))
    
    
    
  })
  
  output$EnergySectorTurnoverPlot <- renderPlotly  ({
    
    EnergySectorTurnover <- read_excel("Structure/CurrentWorking.xlsx", 
                                       sheet = "Energy economy", col_names = FALSE, 
                                       skip = 12, n_max = 9)
    
    EnergySectorTurnover <- as.data.frame(t(EnergySectorTurnover))
    
    EnergySectorTurnover <- tail(EnergySectorTurnover[c(1,5)],-1)
    EnergySectorTurnover %<>% lapply(function(x) as.numeric(as.character(x)))
    EnergySectorTurnover[2] %<>% lapply(function(x) x/1000)
    EnergySectorTurnover <- as_tibble(EnergySectorTurnover)
    names(EnergySectorTurnover) <- c("Year", "Turnover")
    
    EnergySectorTurnover <- EnergySectorTurnover[which(EnergySectorTurnover$Year >= 2008),]
    
    EnergySectorTurnover$Year <- as.numeric(substr(EnergySectorTurnover$Year, 1,4))
    
    ### variables
    ChartColours <- c("#1a5d38", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: SG"
    plottitle = "Turnover associated with the energy sector"
    
    #EnergySectorTurnover$OilPercentage <- PercentLabel(EnergySectorTurnover$Oil)
    
    EnergySectorTurnover$Year <-
      paste0("01/01/", EnergySectorTurnover$Year)
    
    EnergySectorTurnover$Year <- dmy(EnergySectorTurnover$Year)
    
    EnergySectorTurnover[EnergySectorTurnover == 0] <- NA
    
    EnergySectorTurnover <- EnergySectorTurnover[complete.cases(EnergySectorTurnover),]
    
    p <-  plot_ly(
      EnergySectorTurnover,
      x = ~ Year,
      y = ~ Turnover,
      name = "Turnover",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "Turnover: \u00A3",
        round(EnergySectorTurnover$Turnover, digits = 3),
        " billion\nYear: ",
        format(EnergySectorTurnover$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = EnergySectorTurnover[nrow(EnergySectorTurnover), ],
        x = ~ Year,
        y = ~ `Turnover`,
        name = "Turnover",
        text = paste0(
          "Turnover: \u00A3",
          round(EnergySectorTurnover$Turnover, digits = 3),
          " billion\nYear: ",
          format(EnergySectorTurnover$Year, "%Y")
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
                     range = c(min(EnergySectorTurnover$Year)-100, max(EnergySectorTurnover$Year)+100)),
        yaxis = list(
          title = "\u00A3 billion",
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
  
  output$EnergySectorGVASubtitle <- renderText({
    EnergySectorGVA <- read_excel("Structure/CurrentWorking.xlsx", 
                                  sheet = "Energy economy", col_names = FALSE, 
                                  skip = 12, n_max = 9)
    
    EnergySectorGVA <- as.data.frame(t(EnergySectorGVA))
    
    EnergySectorGVA <- tail(EnergySectorGVA[c(1,6)],-1)
    EnergySectorGVA %<>% lapply(function(x) as.numeric(as.character(x)))
    EnergySectorGVA <- as.data.frame(EnergySectorGVA)
    names(EnergySectorGVA) <- c("Year", "GVA")
    
    EnergySectorGVA <- EnergySectorGVA[which(EnergySectorGVA$Year >= 2008),]
    
    EnergySectorGVA$Year <- as.numeric(substr(EnergySectorGVA$Year, 1,4))
    
    EnergySectorGVA$GVA <- EnergySectorGVA$GVA /1000
    
    EnergySectorGVA[EnergySectorGVA == 0] <- NA
    
    EnergySectorGVA <- EnergySectorGVA[complete.cases(EnergySectorGVA),]
    
    paste("Scotland,", min(EnergySectorGVA$Year),"-", max(EnergySectorGVA$Year))
  })
  
  output$EnergySectorGVAPlot <- renderPlotly  ({
    
    EnergySectorGVA <- read_excel("Structure/CurrentWorking.xlsx", 
                                  sheet = "Energy economy", col_names = FALSE, 
                                  skip = 12, n_max = 9)
    
    EnergySectorGVA <- as.data.frame(t(EnergySectorGVA))
    
    EnergySectorGVA <- tail(EnergySectorGVA[c(1,6)],-1)
    EnergySectorGVA %<>% lapply(function(x) as.numeric(as.character(x)))
    EnergySectorGVA <- as.data.frame(EnergySectorGVA)
    names(EnergySectorGVA) <- c("Year", "GVA")
    
    EnergySectorGVA <- EnergySectorGVA[which(EnergySectorGVA$Year >= 2008),]
    
    EnergySectorGVA$Year <- as.numeric(substr(EnergySectorGVA$Year, 1,4))
    
    EnergySectorGVA$GVA <- EnergySectorGVA$GVA /1000
    
    ### variables
    ChartColours <- c("#1a5d38", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: SG"
    plottitle = "GVA associated with the energy sector"
    
    #EnergySectorGVA$OilPercentage <- PercentLabel(EnergySectorGVA$Oil)
    
    EnergySectorGVA$Year <-
      paste0("01/01/", EnergySectorGVA$Year)
    
    EnergySectorGVA$Year <- dmy(EnergySectorGVA$Year)
    
    EnergySectorGVA[EnergySectorGVA == 0] <- NA
    
    EnergySectorGVA <- EnergySectorGVA[complete.cases(EnergySectorGVA),]
    
    p <-  plot_ly(
      EnergySectorGVA,
      x = ~ Year,
      y = ~ GVA,
      name = "GVA",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "GVA: \u00A3",
        round(EnergySectorGVA$GVA, digits = 3),
        " billion\nYear: ",
        format(EnergySectorGVA$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = EnergySectorGVA[nrow(EnergySectorGVA), ],
        x = ~ Year,
        y = ~ `GVA`,
        name = "GVA",
        text = paste0(
          "GVA: \u00A3",
          round(EnergySectorGVA$GVA, digits = 3),
          " billion\nYear: ",
          format(EnergySectorGVA$Year, "%Y")
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
                     range = c(min(EnergySectorGVA$Year)-100, max(EnergySectorGVA$Year)+100)),
        yaxis = list(
          title = "\u00A3 billion",
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
  
  output$EnergySectorGVA.png <- downloadHandler(
    filename = "EnergySectorGVA.png",
    content = function(file) {
      
      EnergySectorGVA <- read_excel("Structure/CurrentWorking.xlsx", 
                                    sheet = "Energy economy", col_names = FALSE, 
                                    skip = 12, n_max = 9)
      
      EnergySectorGVA <- as.data.frame(t(EnergySectorGVA))
      
      EnergySectorGVA <- tail(EnergySectorGVA[c(1,6)],-1)
      EnergySectorGVA %<>% lapply(function(x) as.numeric(as.character(x)))
      EnergySectorGVA <- as.data.frame(EnergySectorGVA)
      names(EnergySectorGVA) <- c("Year", "GVA")
      
      EnergySectorGVA <- EnergySectorGVA[which(EnergySectorGVA$Year >= 2008),]
      
      EnergySectorGVA$Year <- as.numeric(substr(EnergySectorGVA$Year, 1,4))
      
      EnergySectorGVA$GVA <- EnergySectorGVA$GVA /1000
      ### variables
      ChartColours <- c("#1a5d38", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: SG"
      plottitle = "GVA associated with the energy sector"
      
      #EnergySectorGVA$OilPercentage <- PercentLabel(EnergySectorGVA$Oil)
      
      EnergySectorGVA[EnergySectorGVA == 0] <- NA
      
      EnergySectorGVA <- EnergySectorGVA[complete.cases(EnergySectorGVA),]
      
      
      EnergySectorGVAChart <- EnergySectorGVA %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        geom_line(
          aes(
            y = GVA,
            colour = ChartColours[2],
            label = percent(GVA)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.8,
            y = GVA,
            label = ifelse(Year == min(Year), paste0("\u00A3", format(round(GVA, digits = 3),nsmall = 0, big.mark = ",", trim = TRUE), "\nbillion"), ""),
            hjust = 0.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+1,
            y = GVA,
            label = ifelse(Year == max(Year), paste0("\u00A3", format(round(GVA, digits = 3),nsmall = 0, big.mark = ",", trim = TRUE), "\nbillion"), ""),
            hjust = 0.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnergySectorGVA, 1),
          aes(
            x = Year,
            y = GVA,
            colour = ChartColours[2],
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
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      EnergySectorGVAChart <-
        LinePercentChart(EnergySectorGVAChart,
                         EnergySectorGVA,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      EnergySectorGVAChart <- EnergySectorGVAChart +
        xlim(min(EnergySectorGVA$Year) -1.4 , max(EnergySectorGVA$Year) +1.4)+
        ylim(-.1, max(EnergySectorGVA$GVA+.5))
      
      
      ggsave(
        file,
        plot =  EnergySectorGVAChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  
  output$EnergySectorExportsSubtitle <- renderText({
    GrowthExports <- read_delim("Processed Data/Output/Growth Exports/GrowthExports.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
    paste("Scotland,", min(GrowthExports$Year),"-", max(GrowthExports$Year))
  })
  
  output$EnergySectorExports <- renderPlotly  ({
    GrowthExports <- read_delim("Processed Data/Output/Growth Exports/GrowthExports.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ChartColours <- c("#1a5d38", "#fc8d62", "#8da0cb")
    
    GrowthExports$Year <-
      paste0("01/01/", GrowthExports$Year)
    
    GrowthExports$Year <- dmy(GrowthExports$Year)
    
    p <- plot_ly(
      GrowthExports,
      x = ~ Year,
      y = ~ `UKExports`,
      name = "UK exports",
      legendgroup = "1",
      type = 'scatter',
      mode = 'none',
      stackgroup = 'one',
      text = paste0(
        "UK exports: \u00A3",
        round(GrowthExports$`UKExports`, digits = 3),
        " billion\nYear: ",
        format(GrowthExports$Year, "%Y")
      ),
      hoverinfo = 'text',
      fillcolor = ChartColours[1]
    )    %>%
      add_trace(
        y = ~ `EUExports`,
        name = "EU exports",
        legendgroup = "2",
        text = paste0(
          "EU exports: \u00A3",
          round(GrowthExports$`EUExports`, digits = 3),
          " billion\nYear: ",
          format(GrowthExports$Year, "%Y")
        ),
        hoverinfo = 'text',
        fillcolor = ChartColours[2]
      )   %>%
      add_trace(
        y = ~ `NonEUExports`,
        name = "NonEU exports",
        legendgroup = "3",
        text = paste0(
          "Non EU exports: \u00A3",
          round(GrowthExports$`NonEUExports`, digits = 3),
          " billion\nYear: ",
          format(GrowthExports$Year, "%Y")
        ),
        hoverinfo = 'text',
        fillcolor = ChartColours[3]
      )   %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(GrowthExports$Year)-100, max(GrowthExports$Year)+100)),
        yaxis = list(
          title = "\u00A3 billion",
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
  
  output$EnergySectorTotalExportsSubtitle <- renderText({
    GrowthExports <- read_delim("Processed Data/Output/Growth Exports/GrowthExports.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
    paste("Scotland,", min(GrowthExports$Year),"-", max(GrowthExports$Year))
  })
  
  output$EnergySectorTotalExports <- renderPlotly  ({
    TotalExports <- read_delim("Processed Data/Output/Growth Exports/GrowthExports.txt", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ChartColours <- c("#1a5d38", "#fc8d62", "#8da0cb")
    
    TotalExports$Year <-
      paste0("01/01/", TotalExports$Year)
    
    TotalExports$Year <- dmy(TotalExports$Year)
    
    TotalExports$Total <- TotalExports$UKExports+TotalExports$EUExports+TotalExports$NonEUExports
    
    p <-  plot_ly(
      TotalExports,
      x = ~ Year,
      y = ~ Total,
      name = "Total",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "Total: \u00A3",
        round(TotalExports$Total, digits = 1),
        " billion\nYear: ",
        format(TotalExports$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = TotalExports[nrow(TotalExports), ],
        x = ~ Year,
        y = ~ `Total`,
        name = "Total",
        text = paste0(
          "Total: \u00A3",
          round(TotalExports$Total, digits = 1),
          " billion\nYear: ",
          format(TotalExports$Year, "%Y")
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
                     range = c(min(TotalExports$Year)-100, max(TotalExports$Year)+100)),
        yaxis = list(
          title = "\u00A3 billion",
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
  
  output$EnEconomyTable = renderDataTable({
    EnergySectorExports <- read_excel("Structure/CurrentWorking.xlsx", 
                                      sheet = "Energy economy", col_names = FALSE, 
                                      skip = 12, n_max = 9)
    
    EnergySectorExports[1,1] <- "Year"
    
    EnergySectorExports <- as.data.frame(t(EnergySectorExports))
    
    EnergySectorExports <- EnergySectorExports[c(1,3:9)]
    
    names(EnergySectorExports) <- as.character(unlist(EnergySectorExports[1,]))
    
    EnergySectorExports <- EnergySectorExports[-1,]
    
    EnergySectorExports %<>% lapply(function(x) as.numeric(as.character(x)))
    
    EnergySectorExports[4:8] %<>% lapply(function(x) x/1000)
    
    EnergySectorExports <- as_tibble(EnergySectorExports)
    
    EnergySectorExports[EnergySectorExports == 0] <- NA
    
    GrowthExports <- read_delim("Processed Data/Output/Growth Exports/GrowthExports.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    EnergySectorExports <- merge(EnergySectorExports[1:5], GrowthExports)
    
    EnergySectorExports$TotalExports <- EnergySectorExports$UKExports + EnergySectorExports$EUExports + EnergySectorExports$NonEUExports
    
    names(EnergySectorExports) <- c("Year", "Employment - Excluding additional units registered for PAYE only", "Employment - Including additional PAYE only units", "Turnover (\u00A3b)", "GVA (\u00A3b)",  "Exports to rest of the U.K. (\u00A3b)", "Exports to EU Countries (\u00A3b)", "Exports to Non-EU Countries (\u00A3b)", "Total Exports (\u00A3b)")
    
    datatable(
      EnergySectorExports,
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
        title = "Energy Economy",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Energy Economy',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Energy Economy')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      
      formatRound(2:3, 0) %>% 
      formatRound(4:9, 3) %>% 
      formatStyle(9, fontWeight = "bold")
    
    
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/1 - Whole System/EnEconomy.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("EnEconomyTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnergySectorEmployment.png <- downloadHandler(
    filename = "EnergySectorEmployment.png",
    content = function(file) {
      
      EnergySectorEmployment <- read_excel("Structure/CurrentWorking.xlsx", 
                                           sheet = "Energy economy", col_names = FALSE, 
                                           skip = 12, n_max = 9)
      
      EnergySectorEmployment <- as.data.frame(t(EnergySectorEmployment))
      
      EnergySectorEmployment <- tail(EnergySectorEmployment[c(1,3,4)],-1)
      EnergySectorEmployment %<>% lapply(function(x) as.numeric(as.character(x)))
      EnergySectorEmployment <- as.data.frame(EnergySectorEmployment)
      names(EnergySectorEmployment) <- c("Year", "Excluding PAYE", "Including PAYE")
      
      EnergySectorEmployment <- EnergySectorEmployment[which(EnergySectorEmployment$Year >= 2009),]
      
      EnergySectorEmployment$Year <- as.numeric(substr(EnergySectorEmployment$Year, 1,4))
      
      EnergySectorEmployment[EnergySectorEmployment == 0] <- NA
      
      ChartColours <- c("#1a5d38", "#41ab5d", "#1a5d38")
      sourcecaption = "Source: Scottish Government"
      plottitle = "Employment in the energy sector"  
      
      length <- max(EnergySectorEmployment$Year) - min(EnergySectorEmployment$Year)
      
      EnergySectorEmploymentChart <- EnergySectorEmployment %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = `Excluding PAYE`,
            colour = ChartColours[2],
            label = percent(`Excluding PAYE`)
          ),
          size = 1.5,
          family = "Century Gothic",
          linetype = "dashed"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Excluding PAYE`,
            label = ifelse(
              Year == min(Year),
              format(`Excluding PAYE`, big.mark = ","),
              ""
            ),
            hjust = 0.5,
            vjust = 2.2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Excluding PAYE`,
            label = ifelse(
              Year == max(Year),
              format(`Excluding PAYE`, big.mark = ","),
              ""
            ),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnergySectorEmployment, 1),
          aes(
            x = Year,
            y = `Excluding PAYE`,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(2009:2015),
            y = mean(`Excluding PAYE`, na.rm = TRUE),
            label = "Excluding additional units\nregistered for PAYE only",
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `Including PAYE`,
            colour = ChartColours[3],
            label = paste0(`Including PAYE` * 100, "%")
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Including PAYE`,
            label = ifelse(
              Year == min(Year),
              format(`Including PAYE`, big.mark = ","),
              ""
            ),
            hjust = 0.5,
            vjust = -1.9,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Including PAYE`,
            label = ifelse(
              Year == max(Year),
              format(`Including PAYE`, big.mark = ","),
              ""
            ),
            hjust = 0.5,
            vjust = -1.5,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnergySectorEmployment, 1),
          aes(
            x = Year,
            y = `Including PAYE`,
            colour = ChartColours[3],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(2015:max(Year)),
            y = mean(`Including PAYE`, na.rm = TRUE),
            label = "Including additional\nPAYE only units",
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year) |
                             Year == min(Year) |
                             Year == 2015, Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      EnergySectorEmploymentChart <-
        LinePercentChart(
          EnergySectorEmploymentChart,
          EnergySectorEmployment,
          plottitle,
          sourcecaption,
          ChartColours
        )
      
      EnergySectorEmploymentChart <- EnergySectorEmploymentChart +
        xlim(min(EnergySectorEmployment$Year)-(length*0.03), max(EnergySectorEmployment$Year)+(length*0.03))
      
      ggsave(
        file,
        plot = EnergySectorEmploymentChart,
        width = 14,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EnergySectorTurnover.png <- downloadHandler(
    filename = "EnergySectorTurnover.png",
    content = function(file) {
      EnergySectorTurnover <- read_excel("Structure/CurrentWorking.xlsx", 
                                         sheet = "Energy economy", col_names = FALSE, 
                                         skip = 12, n_max = 9)
      
      EnergySectorTurnover <- as.data.frame(t(EnergySectorTurnover))
      
      EnergySectorTurnover <- tail(EnergySectorTurnover[c(1,5)],-1)
      EnergySectorTurnover %<>% lapply(function(x) as.numeric(as.character(x)))
      EnergySectorTurnover <- as.data.frame(EnergySectorTurnover)
      names(EnergySectorTurnover) <- c("Year", "Turnover")
      
      EnergySectorTurnover <- EnergySectorTurnover[which(EnergySectorTurnover$Year >= 2008),]
      
      EnergySectorTurnover$Year <- as.numeric(substr(EnergySectorTurnover$Year, 1,4))
      ### variables
      ChartColours <- c("#1a5d38", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: SG"
      plottitle = "Turnover associated with the energy sector"
      EnergySectorTurnover$Turnover <- EnergySectorTurnover$Turnover / 1000
      
      #EnergySectorTurnover$OilPercentage <- PercentLabel(EnergySectorTurnover$Oil)
      
      EnergySectorTurnover[EnergySectorTurnover == 0] <- NA
      
      EnergySectorTurnover <- EnergySectorTurnover[complete.cases(EnergySectorTurnover),]
      
      EnergySectorTurnoverChart <- EnergySectorTurnover %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        geom_line(
          aes(
            y = Turnover,
            colour = ChartColours[2],
            label = percent(Turnover)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.8,
            y = Turnover,
            label = ifelse(Year == min(Year), paste0("\u00A3", format(round(Turnover, digits = 3),nsmall = 0, big.mark = ",", trim = TRUE), "\nbillion"), ""),
            hjust = 0.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+1,
            y = Turnover,
            label = ifelse(Year == max(Year), paste0("\u00A3", format(round(Turnover, digits = 3),nsmall = 0, big.mark = ",", trim = TRUE), "\nbillion"), ""),
            hjust = 0.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnergySectorTurnover, 1),
          aes(
            x = Year,
            y = Turnover,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(Turnover),
            label = "Turnover",
            hjust = 0.5,
            vjust = 1,
            colour = ChartColours[2],
            fontface = 2
          ),
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
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      EnergySectorTurnoverChart <-
        LinePercentChart(EnergySectorTurnoverChart,
                         EnergySectorTurnover,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      
      EnergySectorTurnoverChart
      
      
      EnergySectorTurnoverChart <- EnergySectorTurnoverChart +
        xlim(min(EnergySectorTurnover$Year) -1.4 , max(EnergySectorTurnover$Year) +1.4)+
        ylim(-.4, max(EnergySectorTurnover$Turnover+2))
      
      
      ggsave(
        file,
        plot =  EnergySectorTurnoverChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EnergySectorExports.png <- downloadHandler(
    filename = "EnergySectorExports.png",
    content = function(file) {
      
      
      GrowthExports <- read_delim("Processed Data/Output/Growth Exports/GrowthExports.txt", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)
      
      GrowthExportsMin <- head(GrowthExports, 1)
      
      GrowthExportsMax <- tail(GrowthExports, 1)
      
      GrowthExports <- melt(GrowthExports, id.vars = "Year")
      
      GrowthExports <- GrowthExports %>% mutate(variable = factor(variable),
                                                variable = factor(variable, levels = rev(levels(variable))))
      
      
      
      ### variables
      ChartColours <- c("#1a5d38", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: SG"
      plottitle = "Exports associated with the energy sector"
      
      #GrowthExports$CavityPercentage <- PercentLabel(GrowthExports$Cavity)
      
      
      GrowthExportsChart <- GrowthExports %>%
        ggplot(aes(
          x = Year,
          y = value,
          group = variable,
          fill = variable
        )) +
        scale_fill_manual(
          "variable",
          values = c(
            "UKExports" = ChartColours[1],
            "EUExports" = ChartColours[2],
            "NonEUExports" = ChartColours[3]
          )
        ) +
        geom_area(posistion = "fill") +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              format(Year),
              ""
            ),
            hjust = ifelse(Year == min(Year), 0, 1),
            vjust = 1.5,
            colour = "white",
            fontface = 2,
            family = "Century Gothic"
          )
        ) +
        annotate(
          "text",
          x = GrowthExportsMin$Year,
          y = GrowthExportsMin$`UKExports` * 0.5,
          label = paste0("\u00A3", format(round(GrowthExportsMin$`UKExports`, digits = 1),big.mark = ","), " bn"),
          hjust = -.1,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = GrowthExportsMin$Year,
          y = (GrowthExportsMin$`EUExports` * 0.5) + GrowthExportsMin$`UKExports`,
          label = paste0("\u00A3", format(round(GrowthExportsMin$`EUExports`, digits = 1),big.mark = ","), " bn"),
          hjust = -.1,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = GrowthExportsMin$Year,
          y = (GrowthExportsMin$`NonEUExports` * 0.5) + GrowthExportsMin$EUExports + GrowthExportsMin$`UKExports`,
          label = paste0("\u00A3", format(round(GrowthExportsMin$`NonEUExports`, digits = 1),big.mark = ","), " bn"),
          hjust = -.1,
          vjust = -3,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = GrowthExportsMax$Year,
          y = GrowthExportsMax$`UKExports` * 0.5,
          label = paste0("\u00A3", format(round(GrowthExportsMax$`UKExports`, digits = 1),big.mark = ","), " bn"),
          hjust = 1.1,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = GrowthExportsMax$Year,
          y = (GrowthExportsMax$`EUExports` * 0.5) + GrowthExportsMax$`UKExports`,
          label = paste0("\u00A3", format(round(GrowthExportsMax$`EUExports`, digits = 1),big.mark = ","), " bn"),
          hjust = 1.1,
          vjust = 2.5,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = GrowthExportsMax$Year,
          y = (GrowthExportsMax$`NonEUExports` * 0.5) + GrowthExportsMax$`EUExports` + GrowthExportsMax$`UKExports`,
          label = paste0("\u00A3", format(round(GrowthExportsMax$`NonEUExports`, digits = 1),big.mark = ","), " bn"),
          hjust = 1.1,
          vjust = 2.5,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2004.5,
          y = 8,
          label = "Exports to the \nrest of the UK",
          hjust = .5,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2004.5,
          y = 10,
          label = "Exports to\nEU Countries",
          hjust = .5,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 2004.5,
          y = 12,
          label = "Exports to\nNon-EU Countries",
          hjust = .5,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) 
      
      
      GrowthExportsChart
      
      
      GrowthExportsChart <-
        StackedArea(GrowthExportsChart,
                    GrowthExports,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      
      GrowthExportsChart <- GrowthExportsChart 
      
      
      
      
      ggsave(
        file,
        plot =  GrowthExportsChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  
  output$EnergySectorTotalExports.png <- downloadHandler(
    filename = "EnergySectorTotalExports.png",
    content = function(file) {
      
      EnergySectorTotal <- read_delim("Processed Data/Output/Growth Exports/GrowthExports.txt", 
                                      "\t", escape_double = FALSE, trim_ws = TRUE)
      
      ChartColours <- c("#1a5d38", "#fc8d62", "#8da0cb")
      
      EnergySectorTotal$Total <- EnergySectorTotal$UKExports+EnergySectorTotal$EUExports+EnergySectorTotal$NonEUExports
      
      sourcecaption <- "Source: SG"
      plottitle <- "Total energy sector exports"
      
      
      EnergySectorTotalChart <- EnergySectorTotal %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        geom_line(
          aes(
            y = Total,
            colour = ChartColours[2],
            label = percent(Total)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-.99,
            y = Total,
            label = ifelse(Year == min(Year), paste0("\u00A3", format(round(Total, digits = 3),nsmall = 0, big.mark = ",", trim = TRUE), "\nbillion"), ""),
            hjust = 0.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+1.08,
            y = Total,
            label = ifelse(Year == max(Year), paste0("\u00A3", format(round(Total, digits = 3),nsmall = 0, big.mark = ",", trim = TRUE), "\nbillion"), ""),
            hjust = 0.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnergySectorTotal, 1),
          aes(
            x = Year,
            y = Total,
            colour = ChartColours[2],
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
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      EnergySectorTotalChart <-
        LinePercentChart(EnergySectorTotalChart,
                         EnergySectorTotal,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      EnergySectorTotalChart <- EnergySectorTotalChart +
        xlim(min(EnergySectorTotal$Year) -1.4 , max(EnergySectorTotal$Year) +1.4)+
        ylim(-.15, max(EnergySectorTotal$Total+1))
      
      
      ggsave(
        file,
        plot =  EnergySectorTotalChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  
  
}