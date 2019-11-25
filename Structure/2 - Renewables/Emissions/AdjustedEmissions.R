require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

AdjustedEmissionsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Percentage Reduction Targets",
    fluidRow(column(8,
                    h3("Percentage reduction targets - based on adjusted emissions", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('AdjustedEmissionsPercentageReductionTargetsSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('AdjustedEmissionsPercentageReductionTargets.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("AdjustedEmissionsPlot")),
    plotlyOutput(ns("AdjustedEmissionsPercentageReductionTargetsPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Comparison",
             fluidRow(column(8,
                             h3("Comparison of adjusted emissions and the fixed annual targets, based on the 1990 - 2008 inventory", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('AdjustedEmissionsSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('AdjustedEmissions.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("AdjustedEmissionsPlot")),
             plotlyOutput(ns("AdjustedEmissionsPlot"))%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("AdjustedEmissionsTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
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
AdjustedEmissions <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }

  print("AdjustedEmissions.R")
  
    output$AdjustedEmissionsSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Adjusted emissions", skip = 12)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    Data <- setDT(Data, keep.rownames = TRUE)[]
    Data[1,1] <- 1990
    Data[is.na(Data)] <- 0
    
    Data <- Data %>% unite(Targets,5:6, sep = "", remove = TRUE)
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data[Data == 0] <- NA
    
    Data <- Data[,c(1,3,5,2,4)]
    
    names(Data) <- c("Year", "Renewables", "Tgt", "2008Inventory", "FixedTargets")
    
    AdjustedEmissions <- Data
    ### variables
    
    paste("Scotland, 1990","-", max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))
  })
    
    output$AdjustedEmissionsPercentageReductionTargetsSubtitle <- renderText({
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Adjusted emissions", skip = 12)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      
      colnames(Data) <- as.character(unlist(Data[1,]))
      Data = Data[-1, ]
      Data <- setDT(Data, keep.rownames = TRUE)[]
      Data[1,1] <- 1990
      Data[is.na(Data)] <- 0
      
      Data <- Data %>% unite(Targets,5:6, sep = "", remove = TRUE)
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      Data[Data == 0] <- NA
      
      Data <- Data[,c(1,3,5,2,4)]
      
      names(Data) <- c("Year", "Renewables", "Tgt", "2008Inventory", "FixedTargets")
      
      AdjustedEmissions <- Data
      ### variables
      
      paste("Scotland, 1990","-", max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))
    })
  
  output$AdjustedEmissionsPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Adjusted emissions", skip = 12)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    Data <- setDT(Data, keep.rownames = TRUE)[]
    Data[1,1] <- 1990
    Data[is.na(Data)] <- 0
    
    Data <- Data %>% unite(Targets,5:6, sep = "", remove = TRUE)
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data[Data == 0] <- NA
    
    Data <- Data[,c(1,3,5,2,4)]
    
    names(Data) <- c("Year", "Renewables", "Tgt", "2008Inventory", "FixedTargets")
    
    AdjustedEmissions <- Data
    
    plottitle <- "Percentage reduction targets - based on adjusted emissions (MtCO2e)"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#39ab2c", "#FF8500")
    LineColours <- c( "#39ab2c","#2b8cbe", "#FF8500", "#addd8e")
    
    AdjustedEmissions$Year <- paste0("01/01/", AdjustedEmissions$Year)
    
    AdjustedEmissions$Year <- dmy(AdjustedEmissions$Year)
    
    
    p <-  plot_ly(data = AdjustedEmissions,
                  x = ~ Year ) %>% 
      add_trace(data = AdjustedEmissions,
                x = ~ Year,
                y = ~ `Renewables`,
                name = "1998 - 2016 Inventory",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "1998 - 2016 Inventory: ",
                  round(AdjustedEmissions$`Renewables`, digits = 1),
                  " MtCO2e\nYear: ",
                  format(AdjustedEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))),],
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Total greenhouse gas emissions",
        text = paste0(
          "1998 - 2016 Inventory: ",
          round(AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))),]$`Renewables`, digits = 1),
          " MtCO2e\nYear: ",
          format(AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))),]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>% 
      
      add_trace(data = AdjustedEmissions,
                x = ~ Year,
                y = ~ `2008Inventory`,
                name = "1998 - 2008 Inventory",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "1998 - 2008 Inventory: ",
                  round(AdjustedEmissions$`2008Inventory`, digits = 1),
                  " MtCO2e\nYear: ",
                  format(AdjustedEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"), ymd("2008-01-01"))),],
        x = ~ Year,
        y = ~ `2008Inventory`,
        name = "1998 - 2008 Inventory",
        text = paste0(
          "1998 - 2008 Inventory: ",
          round(AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"), ymd("2008-01-01"))),]$`2008Inventory`, digits = 1),
          " MtCO2e\nYear: ",
          format(AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"), ymd("2008-01-01"))),]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "2",
        marker = list(size = 18, 
                      color = LineColours[2])
      ) %>% 
      
      add_trace(data = AdjustedEmissions,
                x = ~ Year,
                y = ~ `FixedTargets`,
                name = "Fixed Annual Targets",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Fixed Annual Target: ",
                  round(AdjustedEmissions$`FixedTargets`, digits = 1),
                  " MtCO2e\nYear: ",
                  format(AdjustedEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[3], dash = "dash")
      ) %>% 
      add_trace(
        data = AdjustedEmissions[which(AdjustedEmissions$Year == max(AdjustedEmissions[which(AdjustedEmissions$FixedTargets != 0),]$Year)),],
        x = ~ Year,
        y = ~ `FixedTargets`,
        name = "Fixed Annual Targets",
        text = paste0(
          "Fixed Annual Target: ",
          round(AdjustedEmissions[which(AdjustedEmissions$Year == max(AdjustedEmissions[which(AdjustedEmissions$FixedTargets != 0),]$Year)),]$`FixedTargets`, digits = 1),
          " MtCO2e\nYear: ",
          format(AdjustedEmissions[which(AdjustedEmissions$Year == max(AdjustedEmissions[which(AdjustedEmissions$FixedTargets != 0),]$Year)),]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter", 
        legendgroup = "3",
        mode = 'markers',
        marker = list(size = 25,
                      symbol = "diamond", 
                      color = LineColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(AdjustedEmissions$Year)-500, max(AdjustedEmissions[which(AdjustedEmissions$FixedTargets != 0),]$Year)+500)),
        yaxis = list(
          title = "MtCO2e",
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
  
  output$AdjustedEmissionsPercentageReductionTargetsPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Adjusted emissions", skip = 12)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    Data <- setDT(Data, keep.rownames = TRUE)[]
    Data[1,1] <- 1990
    Data[is.na(Data)] <- 0
    
    Data <- Data %>% unite(Targets,5:6, sep = "", remove = TRUE)
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data[Data == 0] <- NA
    
    Data <- Data[,c(1,3,5,2,4)]
    
    names(Data) <- c("Year", "Renewables", "Tgt", "2008Inventory", "Tgt")
    
    AdjustedEmissions <- Data
    
    plottitle <- "Percentage reduction targets - based on adjusted emissions (MtCO2e)"
    sourcecaption <- "Source: BEIS"
    ChartColours <- c("#39ab2c", "#FF8500")
    LineColours <- c( "#39ab2c","#2b8cbe", "#FF8500", "#addd8e")
    
    AdjustedEmissions$Year <- paste0("01/01/", AdjustedEmissions$Year)
    
    AdjustedEmissions$Year <- dmy(AdjustedEmissions$Year)
    
    
    p <-  plot_ly(data = AdjustedEmissions,
                  x = ~ Year ) %>% 
      add_trace(data = AdjustedEmissions,
                x = ~ Year,
                y = ~ `Renewables`,
                name = "1998 - 2016 Inventory",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "1998 - 2016 Inventory: ",
                  round(AdjustedEmissions$`Renewables`, digits = 1),
                  " MtCO2e\nYear: ",
                  format(AdjustedEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))),],
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Total greenhouse gas emissions",
        text = paste0(
          "1998 - 2016 Inventory: ",
          round(AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))),]$`Renewables`, digits = 1),
          " MtCO2e\nYear: ",
          format(AdjustedEmissions[which(AdjustedEmissions$Year %in% c(ymd("1990-01-01"),ymd("1995-01-01"),ymd("1998-01-01"),max(AdjustedEmissions[which(AdjustedEmissions$Renewables != 0),]$Year))),]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>% 
      add_trace(
        data = AdjustedEmissions[which(AdjustedEmissions$Tgt != 0),],
        x = ~ Year,
        y = ~ `Tgt`,
        name = "Targets",
        text = paste0(
          "Fixed Annual Target: ",
          round(AdjustedEmissions[which(AdjustedEmissions$Tgt != 0),]$`Tgt`, digits = 1),
          " MtCO2e\n(Reduction of ", percent(1 -( AdjustedEmissions[which(AdjustedEmissions$Tgt != 0),]$`Tgt` / AdjustedEmissions[which(AdjustedEmissions$Year == ymd("1990-01-01")),]$Renewables), accuracy = 1),
          
          
          ")\nYear: ",
          format(AdjustedEmissions[which(AdjustedEmissions$Tgt != 0),]$Year, "%Y")
        ),
        hoverinfo = 'text',
        type = "scatter", 
        legendgroup = "2",
        mode = 'markers',
        marker = list(size = 25,
                      symbol = "diamond", 
                      color = LineColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(AdjustedEmissions$Year)-500, max(AdjustedEmissions[which(AdjustedEmissions$Tgt != 0),]$Year)+500)),
        yaxis = list(
          title = "MtCO2e",
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
  
  
  output$AdjustedEmissionsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Adjusted emissions", skip = 12)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    Data <- setDT(Data, keep.rownames = TRUE)[]
    Data[1,1] <- 1990
    Data[is.na(Data)] <- 0
    
    Data <- Data %>% unite(Targets,5:6, sep = "", remove = TRUE)
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data <- Data[,c(1,3,5,2,4)]
    
    names(Data) <- c("Year", "1990 - 2016 Inventory", "Reduction Target", "1990 - 2008 Inventory", "Fixed Annual Targets")
    
    Data$CheckRow <- Data$`1990 - 2016 Inventory` + Data$`Reduction Target` +Data$`1990 - 2008 Inventory` + Data$`Fixed Annual Targets`
    
    Data <- Data[which(Data$CheckRow > 0),]
    
    Data[Data == 0] <- NA
    
    AdjustedEmissions <- Data[c(1,4,2,5,3)]
    datatable(
      AdjustedEmissions,
      extensions = 'Buttons',
     # container = sketch,
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'asc')),
        title = "Million tonnes of CO2 emissions displaced by renewables",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Million tonnes of CO2 emissions displaced by renewables',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Million tonnes of CO2 emissions displaced by renewables')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatRound(2:5, 1)
  })
  
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Emissions/AdjustedEmissions.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("AdjustedEmissionsTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$AdjustedEmissions.png <- downloadHandler(
    filename = "AdjustedEmissions.png",
    content = function(file) {

      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Adjusted emissions", skip = 12)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      
      colnames(Data) <- as.character(unlist(Data[1,]))
      Data = Data[-1, ]
      Data <- setDT(Data, keep.rownames = TRUE)[]
      Data[1,1] <- 1990
      Data[is.na(Data)] <- 0
      
      Data <- Data %>% unite(Targets,5:6, sep = "", remove = TRUE)
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      Data[Data == 0] <- NA
      
      Data <- Data[,c(1,3,5,2,4)]
      
      names(Data) <- c("Year", "Renewables", "Tgt", "2008Inventory", "FixedTargets")
      
      AdjustedEmissionsComparison <- Data
      
      plottitle <- "Comparison of adjusted emissions and the fixed annual targets, based on the\n1990-2008 inventory (MtCO2e)"
      sourcecaption <- "Source: BEIS"
      ChartColours <- c("#39ab2c", "#FF8500")
      LineColours <- c( "#39ab2c","#2b8cbe", "#FF8500", "#addd8e")
      
      AdjustedEmissionsComparisonChart <-
        AdjustedEmissionsComparison %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Renewables,
              
              label = Renewables),
          size = 1.5,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$Year == max(AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$Renewables > 0),]$Year)),],
          aes(
            x = Year,
            y = Renewables,
            label = round(`2008Inventory`, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          data = AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$Year == max(AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$Renewables > 0),]$Year)),],
          aes(
            x = Year,
            y = Renewables,
            label = round(Renewables, digits = 1),
            show_guide = FALSE
          ),
          fontface = 2,
          vjust = -1.2,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year %in% c(1990,1995, 1998), round(Renewables, digits= 1), ""),
            hjust = 0.5,
            vjust = -1,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+0.75,
            y = Renewables,
            label = ifelse(Year == max(AdjustedEmissionsComparison$Year[which(AdjustedEmissionsComparison$FixedTargets > 0)]), round(Renewables, digits= 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(c(1998,max(AdjustedEmissionsComparison$Year[which(AdjustedEmissionsComparison$Renewables > 0)]))),
            y = mean(Renewables, na.rm = TRUE),
            label = "1998 - 2016 Inventory",
            hjust = 0.5,
            vjust = -3,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(AdjustedEmissionsComparison, 1),
          aes(
            x = Year,
            y = Renewables,
            label = round(Renewables, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$Year %in% c(1990,1995,1998)),],
          aes(
            x = Year,
            y = Renewables,
            label = round(Renewables, digits = 1),
            show_guide = FALSE
          ),
          size = 3,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `2008Inventory`,
              
              label = `2008Inventory`),
          size = 1.5,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$Year == max(AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$`2008Inventory` > 0),]$Year)),],
          aes(
            x = Year,
            y = `2008Inventory`,
            label = round(`2008Inventory`, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          data = AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$Year == max(AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$`2008Inventory` > 0),]$Year)),],
          aes(
            x = Year,
            y = `2008Inventory`,
            label = round(`2008Inventory`, digits = 1),
            show_guide = FALSE
          ),
          vjust = 2,
          fontface = 2,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `2008Inventory`,
            label = ifelse(Year %in% c(1990,1995, 1998), round(`2008Inventory`, digits= 1), ""),
            hjust = 0.5,
            vjust = -.8,
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+0.75,
            y = `2008Inventory`,
            label = ifelse(Year == max(AdjustedEmissionsComparison$Year[which(AdjustedEmissionsComparison$FixedTargets > 0)]), round(`2008Inventory`, digits= 1), ""),
            hjust = 0.5,
            vjust = 0.3,
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(c(1998,max(AdjustedEmissionsComparison$Year[which(AdjustedEmissionsComparison$`2008Inventory` > 0)]))),
            y = mean(`2008Inventory`, na.rm = TRUE),
            label = "1998 - 2008 Inventory",
            hjust = 0.5,
            vjust = 4,
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(AdjustedEmissionsComparison, 1),
          aes(
            x = Year,
            y = `2008Inventory`,
            label = round(`2008Inventory`, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$Year %in% c(1990,1995,1998)),],
          aes(
            x = Year,
            y = `2008Inventory`,
            label = round(`2008Inventory`, digits = 1),
            show_guide = FALSE
          ),
          size = 3,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = FixedTargets,
              
              label = FixedTargets),
          size = 1.5,
          colour = LineColours[3],
          linetype = "dashed",
          family = "Century Gothic"
        ) +
        geom_point(
          data = AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$Year == max(AdjustedEmissionsComparison[which(AdjustedEmissionsComparison$FixedTargets > 0),]$Year)),],
          aes(
            x = Year,
            y = FixedTargets,
            label = round(`2008Inventory`, digits = 1),
            show_guide = FALSE
          ),
          size = 6,
          shape = 18,
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = FixedTargets,
            label = ifelse(Year %in% c(1990,1995, 1998), round(FixedTargets, digits= 1), ""),
            hjust = 0.5,
            vjust = 2,
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+1.5,
            y = FixedTargets,
            label = ifelse(Year == max(AdjustedEmissionsComparison$Year[which(AdjustedEmissionsComparison$FixedTargets > 0)]), round(FixedTargets, digits= 1), ""),
            hjust = 0.5,
            vjust = 0.7,
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(c(min(AdjustedEmissionsComparison$Year[which(AdjustedEmissionsComparison$FixedTargets > 0)]),max(AdjustedEmissionsComparison$Year[which(AdjustedEmissionsComparison$FixedTargets > 0)])), na.rm = TRUE),
            y = mean(FixedTargets, na.rm = TRUE),
            label = "Fixed Annual Targets",
            hjust = 0.5,
            vjust = 3,
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year %in% c(1995, 1998), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(AdjustedEmissionsComparison$Year[which(AdjustedEmissionsComparison$FixedTargets > 0)]), Year, ""),
            hjust = 0.5,
            vjust = 1.5
          ),
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = 1990,
            y = 0,
            label = "Baseline",
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      AdjustedEmissionsComparisonChart <-
        LinePercentChart(AdjustedEmissionsComparisonChart,
                         AdjustedEmissionsComparison,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      AdjustedEmissionsComparisonChart
      
      AdjustedEmissionsComparisonChart <- AdjustedEmissionsComparisonChart +
        labs(subtitle = paste("Scotland, 1990 -", max(AdjustedEmissionsComparison$Year[which(AdjustedEmissionsComparison$Renewables > 0)]))) +
        xlim(1990, max(AdjustedEmissionsComparison$Year[which(AdjustedEmissionsComparison$FixedTargets > 0)])+1.5)+
        ylim(0,79)
      
      
      ggsave(
       file,
       plot = AdjustedEmissionsComparisonChart,
        width = 26,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$AdjustedEmissionsPercentageReductionTargets.png <- downloadHandler(
    filename = "AdjustedEmissionsPercentageReductionTargets.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Adjusted emissions", skip = 12)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      
      colnames(Data) <- as.character(unlist(Data[1,]))
      Data = Data[-1, ]
      Data <- setDT(Data, keep.rownames = TRUE)[]
      Data[1,1] <- 1990
      Data[is.na(Data)] <- 0
      
      Data <- Data %>% unite(Targets,5:6, sep = "", remove = TRUE)
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      Data[Data == 0] <- NA
      
      Data <- Data[,c(1,3,5,2,4)]
      
      names(Data) <- c("Year", "Renewables", "Tgt", "2008Inventory", "FixedTargets")
      
      AdjustedEmissions <- Data
      
      plottitle <- "Percentage reduction targets - based on adjusted emissions (MtCO2e)"
      sourcecaption <- "Source: BEIS"
      ChartColours <- c("#39ab2c", "#FF8500")
      LineColours <- c( "#39ab2c","#2b8cbe", "#FF8500", "#addd8e")
      
      AdjustedEmissionsChart <-
        AdjustedEmissions %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Renewables,
              
              label = Renewables),
          size = 1.5,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = AdjustedEmissions[which(AdjustedEmissions$Year == max(AdjustedEmissions[which(AdjustedEmissions$Renewables > 0),]$Year)),],
          aes(
            x = Year,
            y = Renewables,
            label = round(`2008Inventory`, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          data = AdjustedEmissions[which(AdjustedEmissions$Year == max(AdjustedEmissions[which(AdjustedEmissions$Renewables > 0),]$Year)),],
          aes(
            x = Year,
            y = Renewables,
            label = round(Renewables, digits = 1),
            show_guide = FALSE
          ),
          fontface = 2,
          vjust = -1,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year %in% c(1990,1995, 1998), round(Renewables, digits= 1), ""),
            hjust = 0.5,
            vjust = -1,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+0.75,
            y = Renewables,
            label = ifelse(Year == max(AdjustedEmissions$Year[which(AdjustedEmissions$FixedTargets > 0)]), round(Renewables, digits= 1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        
        geom_point(
          data = tail(AdjustedEmissions, 1),
          aes(
            x = Year,
            y = Renewables,
            label = round(Renewables, digits = 1),
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = AdjustedEmissions[which(AdjustedEmissions$Year %in% c(1990,1995,1998)),],
          aes(
            x = Year,
            y = Renewables,
            label = round(Renewables, digits = 1),
            show_guide = FALSE
          ),
          size = 3,
          colour = LineColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year %in% c(1995, 1998, max(AdjustedEmissions[which(AdjustedEmissions$Renewables > 0),]$Year)), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year %in% AdjustedEmissions$Year[which(AdjustedEmissions$Tgt > 0)], Year, ""),
            hjust = 0.5,
            vjust = 1.5
          ),
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = 1990,
            y = 0,
            label = "Baseline",
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )+ geom_point(
          aes(
            x = Year,
            y = Tgt,
            label = round(`2008Inventory`, digits = 1),
            show_guide = FALSE
          ),
          size = 6,
          shape = 18,
          colour = LineColours[3],
          family = "Century Gothic" ) +
        geom_text(
          aes(
            x = Year,
            y = Tgt,
            label = paste0("Target: ", round(Tgt, digits = 1), "\n(Reduction of ", percent(1 - ( Tgt / AdjustedEmissions$Renewables[which(AdjustedEmissions$Year == 1990)]), accuracy = 1), ")"),
            show_guide = FALSE
          ),
          colour = LineColours[3],
          family = "Century Gothic",
          vjust = 1.5
        )
      
      AdjustedEmissionsChart <-
        LinePercentChart(AdjustedEmissionsChart,
                         AdjustedEmissions,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      AdjustedEmissionsChart
      
      AdjustedEmissionsChart <- AdjustedEmissionsChart +
        labs(subtitle = paste("Scotland, 1990 -", max(AdjustedEmissions$Year[which(AdjustedEmissions$Renewables > 0)]))) +
        xlim(min(AdjustedEmissions$Year), max(AdjustedEmissions$Year) + 2)+
        ylim(0,79)
      
      
      ggsave(
        file,
        plot = AdjustedEmissionsChart,
        width = 26,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  )
}
