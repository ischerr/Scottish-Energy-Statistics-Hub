require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenEnTgtOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Share of renewable energy in gross final consumption", style = "color: #1A5D38;  font-weight:bold"),
                    h4(textOutput(ns('RenEnTgtSubtitle')), style = "color: #1A5D38;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenEnTgt.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    #dygraphOutput(ns("RenEnTgtPlot")),
    plotlyOutput(ns("RenEnTgtPlot"))%>% withSpinner(color="#1A5D38"),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
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
      column(12, dataTableOutput(ns("RenEnTgtTable"))%>% withSpinner(color="#1A5D38"))),
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
RenEnTgt <- function(input, output, session) {
 
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenEnTgt.R")

  output$RenEnTgtSubtitle <- renderText({
    
    RenEn <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Renewable energy target",
      col_names = FALSE,
      skip = 21,
      n_max = 23
    )
    RenEn <- as.data.frame(t(RenEn))
    RenEn <- RenEn[, c(1, 6, 12, 18, 23)]
    RenEn <- tail(RenEn,-5)
    names(RenEn) <-
      c("Year", "Electricity", "Heat", "Transport", "Renewables")
    RenEn[, c(1, 2, 3, 4, 5)] %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenEn[which(RenEn$Year != max(RenEn$Year)),][2:4] <- 0
    
    paste("Scotland,", min(RenEn$Year),"-", max(RenEn$Year))
  })
  
  output$RenEnTgtPlot <- renderPlotly  ({
    
    RenEn <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Renewable energy target",
      col_names = FALSE,
      skip = 21,
      n_max = 23
    )
    RenEn <- as.data.frame(t(RenEn))
    RenEn <- RenEn[, c(1, 6, 12, 18, 23)]
    RenEn <- tail(RenEn,-5)
    names(RenEn) <-
      c("Year", "Electricity", "Heat", "Transport", "Renewables")
    RenEn[, c(1, 2, 3, 4, 5)] %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenEn[which(RenEn$Year != max(RenEn$Year)),][2:4] <- 0
    RenEnTgts <-
      read_csv("Structure/1 - Whole System/Renewable energy target.csv")
    RenEn <- bind_rows(RenEn, RenEnTgts)
    
    RenEn <-
      RenEn[, c(1, 5, 6, 2, 3, 4)] %>% distinct(Year, .keep_all =  TRUE) %>% arrange(Year)
    
    # RenEnTgt <- RenEn[c(1, 2, 4:6)]
    # 
    # RenEnTgt <- RenEnTgt[complete.cases(RenEnTgt), ]
    
    ChartColours <- c("#1a5d38", "#FF8500")
    sourcecaption = "Source: BEIS, Energy Saving Trust, HMRC"
    plottitle = "Share of renewable energy in\ngross final energy consumption"
    
    BarColours <- c("#2b8cbe", "#fc9272", "#34d1a3")
    
    RenEn$Year <- paste0("01/01/", RenEn$Year)
    
    RenEn$Year <- dmy(RenEn$Year)
    
    
    p <-  plot_ly(RenEn,x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "Renewables",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Progress: ",
                  percent(RenEn$Renewables, accuracy = 0.1),
                  "\nYear: ",
                  format(RenEn$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(RenEn[which(RenEn$Renewables > 0 | RenEn$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewables",
        legendgroup = "1",
        text = paste0(
          "Progress: ",
          percent(RenEn[which(RenEn$Renewables > 0 | RenEn$Renewables < 0),][-1,]$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(RenEn[which(RenEn$Renewables > 0 | RenEn$Renewables < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(
        data = RenEn,
        x = ~ Year,
        y = ~ Tgt,
        name = "Target",
        legendgroup = "2",
        text = paste0(
          "Target: ",
          percent(RenEn$Tgt, accuracy = 0.1),
          "\nYear: ",
          format(RenEn$Year, "%Y")
        ),
        hoverinfo = 'text',
        mode = 'markers',
        marker = list(size = 25,
                      symbol = "diamond",
                      color = ChartColours[2])
      ) %>% 
      add_trace(x = ~Year, 
                type = 'bar', 
                y = ~Electricity, 
                name = "Electricity", 
                legendgroup = "3",
                text = paste0(
                  "Electricity: ",
                  percent(RenEn$Electricity, accuracy = 0.1),
                  "\nYear: ",
                  format(RenEn$Year, "%Y")
                ),
                marker = list(color = BarColours[3]),
                hoverinfo = "text")   %>% 
      add_trace(x = ~Year, 
                type = 'bar', 
                y = ~Heat, 
                name = "Heat", 
                legendgroup = "3",
                text = paste0(
                  "Heat: ",
                  percent(RenEn$Heat, accuracy = 0.1),
                  "\nYear: ",
                  format(RenEn$Year, "%Y")
                ),
                marker = list(color = BarColours[2]),
                hoverinfo = "text") %>% 
      add_trace(x = ~Year, 
                type = 'bar', 
                y = ~ Transport, 
                name = "Transport",
                legendgroup = "3",
                text = paste0(
                  "Transport: ",
                  percent(RenEn$Transport, accuracy = 0.1),
                  "\nYear: ",
                  format(RenEn$Year, "%Y")
                ),
                marker = list(color = BarColours[1]),
                hoverinfo = "text") %>%
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(RenEn$Year)-100, max(RenEn$Year)+100)),
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
  
  
  output$RenEnTgtTable = renderDataTable({
    
    RenEn <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Renewable energy target",
      col_names = FALSE,
      skip = 21,
      n_max = 23
    )
    RenEn <- as.data.frame(t(RenEn))
    RenEn <- RenEn[, c(1, 6, 12, 18, 23)]
    RenEn <- tail(RenEn,-5)
    names(RenEn) <-
      c("Year", "Electricity", "Heat", "Transport", "Renewables")
    RenEn[, c(1, 2, 3, 4, 5)] %<>% lapply(function(x)
      as.numeric(as.character(x)))
    RenEnTgts <-
      read_csv("Structure/1 - Whole System/Renewable energy target.csv")
    RenEn <- bind_rows(RenEn, RenEnTgts)
    
    RenEn <-
      RenEn[, c(1, 5, 6, 2, 3, 4)] %>% distinct(Year, .keep_all =  TRUE) %>% arrange(Year)
    
    RenEnTgt <- RenEn[c(1, 2, 4:6)]
    
    RenEnTgt <- RenEnTgt[complete.cases(RenEnTgt), ]
    
    datatable(
      RenEnTgt,
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
        title = "Renewable Energy",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Energy',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Energy')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:5, 1)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/1 - Whole System/RenEnTgt.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  observeEvent(input$ToggleTable, {
    toggle("RenEnTgtTable")
  })
  
  
  output$RenEnTgt.png <- downloadHandler(
    filename = "RenEnTgt.png",
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
      
      
      RenEn <- read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Renewable energy target",
        col_names = FALSE,
        skip = 21,
        n_max = 23
      )
      RenEn <- as.data.frame(t(RenEn))
      RenEn <- RenEn[, c(1, 6, 12, 18, 23)]
      RenEn <- tail(RenEn,-5)
      names(RenEn) <-
        c("Year", "Electricity", "Heat", "Transport", "Renewables")
      RenEn[, c(1, 2, 3, 4, 5)] %<>% lapply(function(x)
        as.numeric(as.character(x)))
      RenEnTgts <-
        read_csv("Structure/1 - Whole System/Renewable energy target.csv")
      RenEn <- bind_rows(RenEn, RenEnTgts)
      
      RenEn <-
        RenEn[, c(1, 5, 6, 2, 3, 4)] %>% distinct(Year, .keep_all =  TRUE) %>% arrange(Year)
      
      RenEnTgt <- RenEn[c(1, 2, 4:6)]
      
      RenEnTgt <- RenEnTgt[complete.cases(RenEnTgt), ]
      
      ChartColours <- c("#1a5d38", "#FF8500")
      sourcecaption = "Source: BEIS, Energy Saving Trust, HMRC"
      plottitle = "Share of renewable energy in\ngross final energy consumption"
      
      RenEnChart <-
        TargetChart(RenEn, plottitle, sourcecaption, ChartColours)

      RenEnChart
      data <- RenEn
      dataMax <- data
      dataMax[3] <- NULL # Remove Target Values
      dataMax <-
        dataMax[complete.cases(dataMax),] #Keep Only Years with Actual Data
      dataMax <- tail(dataMax, 1) # Keep Most Recent Year
      
      data[2] <- round(data[2], digits = 3) # Round Numberss
      data$Percentage <-
        paste0(data$Renewables * 100, "%") # Create Percentage Output for Labels
      data$Target <-
        paste0(data$Tgt * 100, "%") # Create Percentage Output for Labels
      
      dataBar <- dataMax[c(1, 3:5)]
      
      dataBar <- melt(dataBar, id = "Year")
      
      dataBar <- dataBar[order(-dataBar$value),]
      
      BarColours <- c("#2b8cbe", "#fc9272", "#34d1a3")
      
      RenEnChart <- RenEnChart +
        geom_bar(
          data = dataBar,
          aes(
            x = dataBar$Year,
            y = dataBar$value,
            fill = forcats::fct_rev(dataBar$variable)
          ),
          stat = "identity",
          width = 0.3
        ) +
        scale_fill_manual(values = BarColours) +
        geom_text(
          label = paste0(
            "Renewable Transport: ",
            round(as.numeric(dataMax$Transport[1]) * 100, digits = 1),
            "%"
          ),
          aes(
            x = mean(dataBar$Year) + 1,
            y = 0.135,
            hjust = 0
          ),
          colour = BarColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          label = paste0("Renewable Heat: ", round(
            as.numeric(dataMax$Heat[1]) *
              100, digits = 1
          ), "%"),
          aes(
            x = mean(dataBar$Year) + 1,
            y = 0.1,
            hjust = 0
          ),
          colour = BarColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          label = paste0(
            "Renewable Electricity: ",
            round(as.numeric(dataMax$Electricity[1]) * 100, digits = 1),
            "%"
          ),
          aes(
            x = mean(dataBar$Year) + 1,
            y = 0.065,
            hjust = 0
          ),
          colour = BarColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = dataMax,
          aes(
            x = dataMax$Year,
            y = Renewables,
            colour = "Renewables",
            show_guide = FALSE,
            size = 4
          )
        )
      
      
      RenEnChart
      
      ggsave(
        file,
        plot = RenEnChart,
        width = 14,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  )
}
