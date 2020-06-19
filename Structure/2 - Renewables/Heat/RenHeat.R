require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenHeatOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Share of renewable heat of non-electrical heat demand", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenHeatSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenHeat.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenHeatPlot")),
    plotlyOutput(ns("RenHeatPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
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
      column(12, dataTableOutput(ns("RenHeatTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISSubNatEnergy", "ESTRenHeat", "BEISUKConsump"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("ESTRenHeat"),
        SourceLookup("BEISUKConsump")
      
      )
    )
  )
}




###### Server ######
RenHeat <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenHeat.R")
  
  output$RenHeatSubtitle <- renderText({
    
    RenHeat <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Renewable heat", col_names = FALSE, 
                          skip = 20)
    RenHeat <- RenHeat[c(1,4)]
    
    names(RenHeat) <- c("Year", "Renewables")
    RenHeat$Year <- substr(RenHeat$Year,1,4)
    RenHeat <- merge(RenHeat, data.frame(Year = 2020, Renewables = NA, Tgt = .11), all = T)
    RenHeat %<>% lapply(function(x) as.numeric(as.character(x)))
    RenHeat <- as.data.frame(RenHeat)
    
    paste("Scotland,", min(RenHeat$Year),"-", max(RenHeat$Year[which(RenHeat$Renewables != 0)]))
  })
  
  output$RenHeatPlot <- renderPlotly  ({
    
    RenHeat <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Renewable heat", col_names = FALSE, 
                          skip = 19)
    RenHeat <- RenHeat[c(1,4)]
    
    names(RenHeat) <- c("Year", "Renewables")
    RenHeat$Year <- substr(RenHeat$Year,1,4)
    RenHeat <- merge(RenHeat, data.frame(Year = 2020, Renewables = NA, Tgt = .11), all = T)
    RenHeat %<>% lapply(function(x) as.numeric(as.character(x)))
    RenHeat <- as.data.frame(RenHeat)
    RenHeat <- RenHeat[-2,] 
    ### variables
    ChartColours <- c("#39ab2c", "#FF8500")
    sourcecaption = "Source: BEIS, EST"
    plottitle = "Share of renewable heat in\nnon-electrical heat demand"
    
    RenHeat$Year <- paste0("01/01/", RenHeat$Year)
    
    RenHeat$Year <- dmy(RenHeat$Year)
    
    
    p <-  plot_ly(RenHeat,x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "Renewable Heat",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Progress: ",
                  percent(RenHeat$Renewables, accuracy = 0.1),
                  "\nYear: ",
                  format(RenHeat$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(RenHeat[which(RenHeat$Renewables > 0 | RenHeat$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewable Heat",
        legendgroup = "1",
        text = paste0(
          "Progress: ",
          percent(RenHeat[which(RenHeat$Renewables > 0 | RenHeat$Renewables < 0),][-1,]$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(RenHeat[which(RenHeat$Renewables > 0 | RenHeat$Renewables < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(
        data = RenHeat,
        x = ~ Year,
        y = ~ Tgt,
        name = "Target",
        legendgroup = "2",
        text = paste0(
          "Target: ",
          percent(RenHeat$Tgt, accuracy = 0.1),
          "\nYear: ",
          format(RenHeat$Year, "%Y")
        ),
        hoverinfo = 'text',
        mode = 'markers',
        marker = list(size = 25,
                      symbol = "diamond",
                      color = ChartColours[2])
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
                     range = c(min(RenHeat$Year)-100, max(RenHeat$Year)+100)),
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
  
  
  output$RenHeatTable = renderDataTable({
    
    RenHeat <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Renewable heat", col_names = TRUE, 
                          skip = 19)

    RenHeat %<>% lapply(function(x) as.numeric(as.character(x)))
   
    RenHeat <- as_tibble(RenHeat)
     RenHeat$Year <- as.character(RenHeat$Year)
    RenHeat[1,1] <- "2008"
    names(RenHeat)[5] <- "Renewable Heat Capacity (GW)"
    
    datatable(
      RenHeat,
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
        title = "Renewable Heat",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Heat',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Heat')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(4, 1) %>% 
      formatRound(2:3, 0) %>% 
      formatRound(5,3)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Heat/RenHeat.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("RenHeatTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenHeat.png <- downloadHandler(
    filename = "RenHeat.png",
    content = function(file) {

      RenHeat <- read_excel("Structure/CurrentWorking.xlsx", 
                            sheet = "Renewable heat", col_names = FALSE, 
                            skip = 20)
      
  RenHeat <- RenHeat[c(1,4)]
  
  names(RenHeat) <- c("Year", "Renewables")
  RenHeat$Year <- substr(RenHeat$Year,1,4)
  RenHeat <- merge(RenHeat, data.frame(Year = 2020, Renewables = NA, Tgt = .11), all = T)
  RenHeat %<>% lapply(function(x) as.numeric(as.character(x)))
  RenHeat <- as.data.frame(RenHeat)
  
  RenHeat <- RenHeat[-2,]
  
  ### variables
  ChartColours <- c("#39ab2c", "#FF8500")
  sourcecaption = "Source: BEIS, EST"
  plottitle = "Share of renewable heat of non-electrical\nheat demand"
  
  RenHeatChart <-
    TargetChart(RenHeat, plottitle, sourcecaption, ChartColours)
  
  RenHeatChart

  ggsave(
    file,
    plot = RenHeatChart,
    width = 14,
    height = 16,
    units = "cm",
    dpi = 300
  )
    }
  )
}
