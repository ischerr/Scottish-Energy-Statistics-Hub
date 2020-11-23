require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



LowCarbonEconomyOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Turnover",
        fluidRow(
          column(
            8,
            h3(
              "LCRE sector",
              style = "color: #39ab2c;  font-weight:bold"
            ),
            h4(textOutput(ns(
              'LowCarbonEconomyTurnoverSubtitle'
            )), style = "color: #39ab2c;")
          ,
          selectInput(ns("MeasureSelect1"), "Measure:", c("Turnover","Number of businesses", "Employment (full time equivalent)", "Exports", "Imports"), selected = "Turnover", multiple = FALSE,
                      selectize = TRUE, width = NULL, size = NULL)
        ),
          column(
            4,
            style = 'padding:15px;',
            downloadButton(ns('LowCarbonEconomyTurnover.png'), 'Download Graph', style =
                             "float:right"))),
        
        tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
        #dygraphOutput(ns("LowCarbonEconomyTurnoverPlot")),
        plotlyOutput(ns("LowCarbonEconomyTurnoverPlot")) %>% withSpinner(color =
                                                                   "#39ab2c"),
        tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Businesses",
               fluidRow(
                 column(
                   8,
                   h3(
                     "LCRE sector",
                     style = "color: #39ab2c;  font-weight:bold"
                   ),
                   h4(textOutput(ns(
                     'LowCarbonEconomyBusinessesSubtitle'
                   )), style = "color: #39ab2c;")
                   ,
                   selectInput(ns("MeasureSelect2"), "Measure:", c("Businesses","Number of businesses", "Employment (full time equivalent)", "Exports", "Imports"), selected = "Businesses", multiple = FALSE,
                               selectize = TRUE, width = NULL, size = NULL)
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('LowCarbonEconomyBusinesses.png'), 'Download Graph', style =
                                    "float:right"))),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("LowCarbonEconomyBusinessesPlot")),
               plotlyOutput(ns("LowCarbonEconomyBusinessesPlot")) %>% withSpinner(color =
                                                                                  "#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Employment",
               fluidRow(
                 column(
                   8,
                   h3(
                     "LCRE sector",
                     style = "color: #39ab2c;  font-weight:bold"
                   ),
                   h4(textOutput(ns(
                     'LowCarbonEconomyEmploymentSubtitle'
                   )), style = "color: #39ab2c;")
                   ,
                   selectInput(ns("MeasureSelect3"), "Measure:", c("Employment","Number of businesses", "Employment (full time equivalent)", "Exports", "Imports"), selected = "Employment", multiple = FALSE,
                               selectize = TRUE, width = NULL, size = NULL)
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('LowCarbonEconomyEmployment.png'), 'Download Graph', style =
                                    "float:right"))),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("LowCarbonEconomyEmploymentPlot")),
               plotlyOutput(ns("LowCarbonEconomyEmploymentPlot")) %>% withSpinner(color =
                                                                                  "#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Exports",
               fluidRow(
                 column(
                   8,
                   h3(
                     "LCRE sector",
                     style = "color: #39ab2c;  font-weight:bold"
                   ),
                   h4(textOutput(ns(
                     'LowCarbonEconomyExportsSubtitle'
                   )), style = "color: #39ab2c;")
                   ,
                   selectInput(ns("MeasureSelect4"), "Measure:", c("Exports","Number of businesses", "Employment (full time equivalent)", "Exports", "Imports"), selected = "Exports", multiple = FALSE,
                               selectize = TRUE, width = NULL, size = NULL)
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('LowCarbonEconomyExports.png'), 'Download Graph', style =
                                    "float:right"))),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("LowCarbonEconomyExportsPlot")),
               plotlyOutput(ns("LowCarbonEconomyExportsPlot")) %>% withSpinner(color =
                                                                                  "#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
      column(
        10,
        h3("Commentary", style = "color: #39ab2c;  font-weight:bold")
      ),
      column(
        2,
        style = "padding:15px",
        actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; ")
      )
    ),
    
    fluidRow(uiOutput(ns("Text"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel(
        "Employees",
        fluidRow(
          column(
            10,
            h3("Data - Employees directly supported by the LCRE sector", style = "color: #39ab2c;  font-weight:bold")
          ),
          column(
            2,
            style = "padding:15px",
            actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; ")
          )
        ),
        fluidRow(column(
          12, dataTableOutput(ns("LowCarbonEconomyEmployeesTable")) %>% withSpinner(color =
                                                                                      "#39ab2c")
        )),
        p("*Cells marked 'c' have been suppressed for disclosure control"),
        tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")
      ),
      tabPanel(
        "Turnover",
        fluidRow(
          column(
            10,
            h3("Data - Turnover directly supported by the LCRE sector (\u00A3bn)", style = "color: #39ab2c;  font-weight:bold")
          ),
          column(
            2,
            style = "padding:15px",
            actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; ")
          )
        ),
        fluidRow(column(
          12, dataTableOutput(ns("LowCarbonEconomyTurnoverTable")) %>% withSpinner(color =
                                                                                     "#39ab2c")
        )),
        p("*Cells marked 'c' have been suppressed for disclosure control"),
        tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")
      ),
      tabPanel(
        "Exports",
        fluidRow(
          column(
            10,
            h3("Data - Exports directly supported by the LCRE sector (\u00A3m)", style = "color: #39ab2c;  font-weight:bold")
          ),
          column(
            2,
            style = "padding:15px",
            actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; ")
          )
        ),
        fluidRow(column(
          12, dataTableOutput(ns("LowCarbonEconomyExportsTable")) %>% withSpinner(color =
                                                                                     "#39ab2c")
        )),
        p("*Cells marked 'c' have been suppressed for disclosure control"),
        tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")
      ),
      tabPanel(
        "Businesses",
        fluidRow(
          column(
            10,
            h3("Data - Businesses directly supported by the LCRE sector", style = "color: #39ab2c;  font-weight:bold")
          ),
          column(
            2,
            style = "padding:15px",
            actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; ")
          )
        ),
        fluidRow(column(
          12, dataTableOutput(ns("LowCarbonEconomyBusinessTable")) %>% withSpinner(color =
                                                                                     "#39ab2c")
        )),
        p("*Cells marked 'c' have been suppressed for disclosure control"),
        tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")
      )
    ),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("ONSLowCarbon"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("ONSLowCarbon")
        
      )
    )
  )
}




###### Server ######
LowCarbonEconomy <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }

  print("LowCarbonEconomy.R")
  
  Data <- read_excel("Structure/CurrentWorking.xlsx", 
                     sheet = "Low carbon economy", skip = 15, n_max = 12,col_names = FALSE)
  
  Data <- as_tibble(t(Data))
  
  Data <- fill(Data, 1)
  
  Data <- Data[c(1,2,5,10,11)]
  
  names(Data) <- c("Measure", "Year", "Direct", "Renewable", "Low Carbon")
  
  Data[2:5] %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  Data <- as_tibble(Data)
  
  Year <- max(Data$Year, na.rm = TRUE)
  
  Data <- Data[which(Data$Year == Year),]
  
  
    output$LowCarbonEconomyEmployeesTable = renderDataTable({
    
      Data <- read_excel("Structure/2 - Renewables/Economy/EconomyTables.xlsx", 
                         sheet = "Employees", col_names = TRUE)
      
      
      for (i in 2:6){
        Data[2,i] <- percent(as.numeric(Data[2,i]),0.1)
      }
    
      names(Data)[1] <- " "
      
      Data
      
      datatable(
        Data,
        extensions = 'Buttons',
        # container = sketch,
        rownames = FALSE,
        options = list(
          columnDefs = list(list(className = 'dt-right', targets = 1:5)),
          paging = TRUE,
          pageLength = -1,
          searching = TRUE,
          fixedColumns = FALSE,
          autoWidth = TRUE,
          ordering = TRUE,
          title = "Employees directly supported by the LCRE sector",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Employees directly supported by the LCRE sector',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Employees directly supported by the LCRE sector')
          ),
          
          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = -1
        )
      ) %>% 
        formatStyle(1,
                    target = 'row',
                    backgroundColor = styleEqual(c('Direct Activity',  'Renewable sector', 'Low carbon', 'Low carbon electricity', 'Low carbon heat', 'Energy from waste and biomass', 'Energy efficient products', 'Low carbon services', 'Low emission vehicles, infrastructure, fuels cells and energy storage'), c('#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd'))
        )
  })
    
    output$LowCarbonEconomyTurnoverTable = renderDataTable({
      
      Data <- read_csv("Structure/2 - Renewables/Economy/EconomyTablesTurnover.csv")
      
      
      for (i in 2:6){
        Data[2,i] <- percent(as.numeric(Data[2,i]),0.1)
      }
      
      names(Data)[1] <- " "
      datatable(
        Data,
        extensions = 'Buttons',
        # container = sketch,
        rownames = FALSE,
        options = list(
          columnDefs = list(list(className = 'dt-right', targets = 1:5)),
          paging = TRUE,
          pageLength = -1,
          searching = TRUE,
          fixedColumns = FALSE,
          autoWidth = TRUE,
          ordering = TRUE,
          title = "Turnover directly supported by the LCRE sector (\u00A3m)",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Turnover directly supported by the LCRE sector (\u00A3m)',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Turnover directly supported by the LCRE sector (\u00A3m)')
          ),
          
          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = -1
        )
      ) %>% 
        formatStyle(1,
                    target = 'row',
                    backgroundColor = styleEqual(c('Direct Activity',  'Renewable sector', 'Low carbon', 'Low carbon electricity', 'Low carbon heat', 'Energy from waste and biomass', 'Energy efficient products', 'Low carbon services', 'Low emission vehicles, infrastructure, fuels cells and energy storage'), c('#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd'))
        )
    })
    
    
    output$LowCarbonEconomyExportsTable = renderDataTable({
      
      Data <- read_excel("Structure/2 - Renewables/Economy/EconomyTables.xlsx", 
                         sheet = "Exports", col_names = TRUE)
      
      
      for (i in 2:6){
        Data[2,i] <- percent(as.numeric(Data[2,i]),0.1)
      }
      
      names(Data)[1] <- " "
      datatable(
        Data,
        extensions = 'Buttons',
        # container = sketch,
        rownames = FALSE,
        options = list(
          columnDefs = list(list(className = 'dt-right', targets = 1:5)),
          paging = TRUE,
          pageLength = -1,
          searching = TRUE,
          fixedColumns = FALSE,
          autoWidth = TRUE,
          ordering = TRUE,
          title = "Exports directly supported by the LCRE sector (\u00A3m)",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Exports directly supported by the LCRE sector (\u00A3m)',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Exports directly supported by the LCRE sector (\u00A3m)')
          ),
          
          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = -1
        )
      ) %>% 
        formatStyle(1,
                    target = 'row',
                    backgroundColor = styleEqual(c('Direct Activity',  'Renewable sector', 'Low carbon', 'Low carbon electricity', 'Low carbon heat', 'Energy from waste and biomass', 'Energy efficient products', 'Low carbon services', 'Low emission vehicles, infrastructure, fuels cells and energy storage'), c('#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd'))
        )
    })
    
    output$LowCarbonEconomyBusinessTable = renderDataTable({
      
      Data <- read_excel("Structure/2 - Renewables/Economy/EconomyTables.xlsx", 
                         sheet = "Number of Businesses", col_names = TRUE)
      
      
      for (i in 2:6){
        Data[2,i] <- percent(as.numeric(Data[2,i]),0.1)
      }
      
      names(Data)[1] <- " "
      datatable(
        Data,
        extensions = 'Buttons',
        # container = sketch,
        rownames = FALSE,
        options = list(
          columnDefs = list(list(className = 'dt-right', targets = 1:5)),
          paging = TRUE,
          pageLength = -1,
          searching = TRUE,
          fixedColumns = FALSE,
          autoWidth = TRUE,
          ordering = TRUE,
          title = "Businesses directly supported the LCRE sector",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Businesses directly supported the LCRE sector',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Businesses directly supported the LCRE sector')
          ),
          
          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = -1
        )
      ) %>% 
        formatStyle(1,
                    target = 'row',
                    backgroundColor = styleEqual(c('Direct Activity',  'Renewable sector', 'Low carbon', 'Low carbon electricity', 'Low carbon heat', 'Energy from waste and biomass', 'Energy efficient products', 'Low carbon services', 'Low emission vehicles, infrastructure, fuels cells and energy storage'), c('#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd'))
        )
    })
    
    observeEvent(input$ToggleTable1, {
      toggle("LowCarbonEconomyEmployeesTable")
    })
    observeEvent(input$ToggleTable2, {
      toggle("LowCarbonEconomyTurnoverTable")
    })
    observeEvent(input$ToggleTable3, {
      toggle("LowCarbonEconomyExportsTable")
    })
    observeEvent(input$ToggleTable4, {
      toggle("LowCarbonEconomyBusinessTable")
    })
  
   
    output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Economy/LowCarbonEconomy.txt")[2])
                     
                   )))
  })
  

  

  
    observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
    output$LowCarbonEconomyTurnoverSubtitle <- renderText({
      
      chartdata <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      paste("Scotland,",min(chartdata$Year), "-", max(chartdata$Year))
      
    })
    
    output$LowCarbonEconomyTurnoverPlot <- renderPlotly  ({
      
      ChartColours <- c("#39ab2c", "#FF8500")
      
      chartdata <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      chartdata <- chartdata[which(chartdata$Country == "Scotland"),]
      
      chartdata<- chartdata[which(chartdata$Category == "Turnover"),]
      
      chartdata$Estimate <- as.numeric(chartdata$Estimate)
      chartdata$`Lower CI` <- as.numeric(chartdata$`Lower CI`)
      chartdata$`Upper CI` <- as.numeric(chartdata$`Upper CI`)

        chartdata$unit <- "\u00A3"
      
      
      chartdata$HoverText <- paste0("<b>", "Turnover", ": ", chartdata$unit, format(round(chartdata$Estimate, 0), big.mark = ","), "</b>\nYear: ", chartdata$Year, "\n<i>Upper CI: ", chartdata$unit, format(round(chartdata$`Upper CI`, 0), big.mark = ","), "\nLower CI: ", chartdata$unit, format(round(chartdata$`Lower CI`, 0), big.mark = ","), "</i>")
      
      
      
      p <- plot_ly(chartdata,
                   x = ~Year,
                   y = ~Estimate, 
                   type = 'scatter', 
                   mode = 'lines',
                   text = chartdata$HoverText,
                   hoverinfo = 'text',
                   legendgroup = "1",
                   name = "Turnover",
                   error_y =  ~list(
                     array = c((`Upper CI` - Estimate)),
                     arrayminus = c(`Estimate` - `Lower CI`),
                     color = '#000000',
                     symmetric = FALSE
                   ),
                   line = list(width = 6, color = ChartColours[1], dash = "none")) %>% 
        add_trace(
          data = tail(chartdata, 1),
          x = ~Year,
          y = ~Estimate, 
          legendgroup = "1",
          showlegend = FALSE ,
          type = "scatter",
          mode = 'markers',
          hoverinfo = 'text',
          text = tail(chartdata, 1)$HoverText,
          marker = list(size = 18, 
                        color = ChartColours[1])
        )  %>% 
        layout(
          barmode = 'stack',
          bargap = 0.66,
          legend = list(font = list(color = "#39ab2c"),
                        orientation = 'h'),
          hoverlabel = list(font = list(color = "white"),
                            hovername = 'text'),
          hovername = 'text',
          xaxis = list(title = "",
                       showgrid = FALSE),
          yaxis = list(
            title = "",
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
    
    
    output$LowCarbonEconomyTurnover.png <- downloadHandler(
    filename = "LowCarbonEconomyTurnover.png",
    content = function(file) {ChartColours <- c("#39ab2c", "#FF8500")
    
    LCRE <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
    
    LCRE <- LCRE[which(LCRE$Country == "Scotland"),]
    
    LCRE<- LCRE[which(LCRE$Category == "Turnover"),]
    
    LCRE$Estimate <- as.numeric(LCRE$Estimate)
    LCRE$`Lower CI` <- as.numeric(LCRE$`Lower CI`)
    LCRE$`Upper CI` <- as.numeric(LCRE$`Upper CI`)
    
    sourcecaption <- "BEIS"

      LCRE$unit <- "\u00A3"
      plottitle <- "Turnover"
    
    LCRE$HoverText <- paste0("<b>", "Turnover", ": ", LCRE$unit, format(round(LCRE$Estimate, 0), big.mark = ","), "</b>\nYear: ", LCRE$Year, "\n<i>Upper CI: ", LCRE$unit, format(round(LCRE$`Upper CI`, 0), big.mark = ","), "\nLower CI: ", LCRE$unit, format(round(LCRE$`Lower CI`, 0), big.mark = ","), "</i>")
    
    length <- max(LCRE$Year) - min(LCRE$Year)
    
    LCREChart <-
      LCRE %>%  ggplot(aes(x = Year), family = "Century Gothic") +
      
      ### Line of Values
      geom_line(
        aes(y = Estimate,
            colour = ChartColours[1],
            label = Estimate),
        size = 1.5,
        family = "Century Gothic"
      ) + geom_errorbar(
        aes(
          y = Estimate,
          ymin = `Lower CI`,
          ymax = `Upper CI`
        ),
        width = .1
      )+
      geom_text(
        aes(
          x = Year-.1,
          y = Estimate,
          label = ifelse(Year == min(Year), paste0(LCRE$unit, format(round(LCRE$Estimate,1), big.mark = ",")), ""),
          hjust = 1,
          colour = ChartColours[1],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Year+.1,
          y = Estimate,
          label = ifelse(Year == max(Year), paste0(LCRE$unit, format(round(LCRE$Estimate,1), big.mark = ",")), ""),
          hjust = 0,
          colour = ChartColours[1],
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(LCRE, 1),
        aes(
          x = Year,
          y = Estimate,
          colour = ChartColours[1],
          label = Estimate,
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
      )
    
    
    LCREChart <-
      LinePercentChart(LCREChart,
                       LCRE,
                       plottitle,
                       sourcecaption,
                       ChartColours)
    
    
    LCREChart
    
    LCREChart <- LCREChart +
      ylim(-.5, max(LCRE$`Upper CI`)*1.05)+
      xlim(min(LCRE$Year)-(length*0.2), max(LCRE$Year)+(length*0.2))
    
    ggsave(
      file,
      plot = LCREChart,
      width = 15,
      height = 15,
      units = "cm",
      dpi = 300
    )
    }
    )
    
    
    output$LowCarbonEconomyBusinessesSubtitle <- renderText({
      
      chartdata <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      paste("Scotland,",min(chartdata$Year), "-", max(chartdata$Year))
      
    })
    
    output$LowCarbonEconomyBusinessesPlot <- renderPlotly  ({
      
      ChartColours <- c("#39ab2c", "#FF8500")
      
      chartdata <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      chartdata <- chartdata[which(chartdata$Country == "Scotland"),]
      
      chartdata<- chartdata[which(chartdata$Category == "Number of businesses"),]
      
      chartdata$Estimate <- as.numeric(chartdata$Estimate)
      chartdata$`Lower CI` <- as.numeric(chartdata$`Lower CI`)
      chartdata$`Upper CI` <- as.numeric(chartdata$`Upper CI`)
      

        chartdata$unit <- ""

      
      chartdata$HoverText <- paste0("<b>", "Number of businesses", ": ", chartdata$unit, format(round(chartdata$Estimate, 0), big.mark = ","), "</b>\nYear: ", chartdata$Year, "\n<i>Upper CI: ", chartdata$unit, format(round(chartdata$`Upper CI`, 0), big.mark = ","), "\nLower CI: ", chartdata$unit, format(round(chartdata$`Lower CI`, 0), big.mark = ","), "</i>")
      
      
      
      p <- plot_ly(chartdata,
                   x = ~Year,
                   y = ~Estimate, 
                   type = 'scatter', 
                   mode = 'lines',
                   text = chartdata$HoverText,
                   hoverinfo = 'text',
                   legendgroup = "1",
                   name = "Number of businesses",
                   error_y =  ~list(
                     array = c((`Upper CI` - Estimate)),
                     arrayminus = c(`Estimate` - `Lower CI`),
                     color = '#000000',
                     symmetric = FALSE
                   ),
                   line = list(width = 6, color = ChartColours[1], dash = "none")) %>% 
        add_trace(
          data = tail(chartdata, 1),
          x = ~Year,
          y = ~Estimate, 
          legendgroup = "1",
          showlegend = FALSE ,
          type = "scatter",
          mode = 'markers',
          hoverinfo = 'text',
          text = tail(chartdata, 1)$HoverText,
          marker = list(size = 18, 
                        color = ChartColours[1])
        )  %>% 
        layout(
          barmode = 'stack',
          bargap = 0.66,
          legend = list(font = list(color = "#39ab2c"),
                        orientation = 'h'),
          hoverlabel = list(font = list(color = "white"),
                            hovername = 'text'),
          hovername = 'text',
          xaxis = list(title = "",
                       showgrid = FALSE),
          yaxis = list(
            title = "",
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
    
    
    output$LowCarbonEconomyBusinesses.png <- downloadHandler(
      filename = "LowCarbonEconomyBusinesses.png",
      content = function(file) {ChartColours <- c("#39ab2c", "#FF8500")
      
      LCRE <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      LCRE <- LCRE[which(LCRE$Country == "Scotland"),]
      
      LCRE<- LCRE[which(LCRE$Category == "Number of businesses"),]
      
      LCRE$Estimate <- as.numeric(LCRE$Estimate)
      LCRE$`Lower CI` <- as.numeric(LCRE$`Lower CI`)
      LCRE$`Upper CI` <- as.numeric(LCRE$`Upper CI`)
      
      sourcecaption <- "BEIS"

        LCRE$unit <- ""
        plottitle <- "Number of businesses"
     
      
      LCRE$HoverText <- paste0("<b>", "Number of businesses", ": ", LCRE$unit, format(round(LCRE$Estimate, 0), big.mark = ","), "</b>\nYear: ", LCRE$Year, "\n<i>Upper CI: ", LCRE$unit, format(round(LCRE$`Upper CI`, 0), big.mark = ","), "\nLower CI: ", LCRE$unit, format(round(LCRE$`Lower CI`, 0), big.mark = ","), "</i>")
      
      length <- max(LCRE$Year) - min(LCRE$Year)
      
      LCREChart <-
        LCRE %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Estimate,
              colour = ChartColours[1],
              label = Estimate),
          size = 1.5,
          family = "Century Gothic"
        ) + geom_errorbar(
          aes(
            y = Estimate,
            ymin = `Lower CI`,
            ymax = `Upper CI`
          ),
          width = .1
        )+
        geom_text(
          aes(
            x = Year-.1,
            y = Estimate,
            label = ifelse(Year == min(Year), paste0(LCRE$unit, format(round(LCRE$Estimate,1), big.mark = ",")), ""),
            hjust = 1,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.1,
            y = Estimate,
            label = ifelse(Year == max(Year), paste0(LCRE$unit, format(round(LCRE$Estimate,1), big.mark = ",")), ""),
            hjust = 0,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(LCRE, 1),
          aes(
            x = Year,
            y = Estimate,
            colour = ChartColours[1],
            label = Estimate,
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
        )
      
      
      LCREChart <-
        LinePercentChart(LCREChart,
                         LCRE,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      LCREChart
      
      LCREChart <- LCREChart +
        ylim(-.5, max(LCRE$`Upper CI`)*1.05)+
        xlim(min(LCRE$Year)-(length*0.2), max(LCRE$Year)+(length*0.2))
      
      ggsave(
        file,
        plot = LCREChart,
        width = 15,
        height = 15,
        units = "cm",
        dpi = 300
      )
      }
    )
    
    
    
    output$LowCarbonEconomyEmploymentSubtitle <- renderText({
      
      chartdata <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      paste("Scotland,",min(chartdata$Year), "-", max(chartdata$Year))
      
    })
    
    output$LowCarbonEconomyEmploymentPlot <- renderPlotly  ({
      
      ChartColours <- c("#39ab2c", "#FF8500")
      
      chartdata <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      chartdata <- chartdata[which(chartdata$Country == "Scotland"),]
      
      chartdata<- chartdata[which(chartdata$Category == "Employment (full time equivalent)"),]
      
      chartdata$Estimate <- as.numeric(chartdata$Estimate)
      chartdata$`Lower CI` <- as.numeric(chartdata$`Lower CI`)
      chartdata$`Upper CI` <- as.numeric(chartdata$`Upper CI`)
      

        chartdata$unit <- ""
     
      
      chartdata$HoverText <- paste0("<b>", "Employment (full time equivalent)", ": ", chartdata$unit, format(round(chartdata$Estimate, 0), big.mark = ","), "</b>\nYear: ", chartdata$Year, "\n<i>Upper CI: ", chartdata$unit, format(round(chartdata$`Upper CI`, 0), big.mark = ","), "\nLower CI: ", chartdata$unit, format(round(chartdata$`Lower CI`, 0), big.mark = ","), "</i>")
      
      
      
      p <- plot_ly(chartdata,
                   x = ~Year,
                   y = ~Estimate, 
                   type = 'scatter', 
                   mode = 'lines',
                   text = chartdata$HoverText,
                   hoverinfo = 'text',
                   legendgroup = "1",
                   name = "Employment (full time equivalent)",
                   error_y =  ~list(
                     array = c((`Upper CI` - Estimate)),
                     arrayminus = c(`Estimate` - `Lower CI`),
                     color = '#000000',
                     symmetric = FALSE
                   ),
                   line = list(width = 6, color = ChartColours[1], dash = "none")) %>% 
        add_trace(
          data = tail(chartdata, 1),
          x = ~Year,
          y = ~Estimate, 
          legendgroup = "1",
          showlegend = FALSE ,
          type = "scatter",
          mode = 'markers',
          hoverinfo = 'text',
          text = tail(chartdata, 1)$HoverText,
          marker = list(size = 18, 
                        color = ChartColours[1])
        )  %>% 
        layout(
          barmode = 'stack',
          bargap = 0.66,
          legend = list(font = list(color = "#39ab2c"),
                        orientation = 'h'),
          hoverlabel = list(font = list(color = "white"),
                            hovername = 'text'),
          hovername = 'text',
          xaxis = list(title = "",
                       showgrid = FALSE),
          yaxis = list(
            title = "",
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
    
    
    output$LowCarbonEconomyEmployment.png <- downloadHandler(
      filename = "LowCarbonEconomyEmployment.png",
      content = function(file) {ChartColours <- c("#39ab2c", "#FF8500")
      
      LCRE <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      LCRE <- LCRE[which(LCRE$Country == "Scotland"),]
      
      LCRE<- LCRE[which(LCRE$Category == "Employment (full time equivalent)"),]
      
      LCRE$Estimate <- as.numeric(LCRE$Estimate)
      LCRE$`Lower CI` <- as.numeric(LCRE$`Lower CI`)
      LCRE$`Upper CI` <- as.numeric(LCRE$`Upper CI`)
      
      sourcecaption <- "BEIS"
      

        LCRE$unit <- ""
        plottitle <- "Employment"

      
      LCRE$HoverText <- paste0("<b>", "Employment (full time equivalent)", ": ", LCRE$unit, format(round(LCRE$Estimate, 0), big.mark = ","), "</b>\nYear: ", LCRE$Year, "\n<i>Upper CI: ", LCRE$unit, format(round(LCRE$`Upper CI`, 0), big.mark = ","), "\nLower CI: ", LCRE$unit, format(round(LCRE$`Lower CI`, 0), big.mark = ","), "</i>")
      
      length <- max(LCRE$Year) - min(LCRE$Year)
      
      LCREChart <-
        LCRE %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Estimate,
              colour = ChartColours[1],
              label = Estimate),
          size = 1.5,
          family = "Century Gothic"
        ) + geom_errorbar(
          aes(
            y = Estimate,
            ymin = `Lower CI`,
            ymax = `Upper CI`
          ),
          width = .1
        )+
        geom_text(
          aes(
            x = Year-.1,
            y = Estimate,
            label = ifelse(Year == min(Year), paste0(LCRE$unit, format(round(LCRE$Estimate,1), big.mark = ",")), ""),
            hjust = 1,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.1,
            y = Estimate,
            label = ifelse(Year == max(Year), paste0(LCRE$unit, format(round(LCRE$Estimate,1), big.mark = ",")), ""),
            hjust = 0,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(LCRE, 1),
          aes(
            x = Year,
            y = Estimate,
            colour = ChartColours[1],
            label = Estimate,
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
        )
      
      
      LCREChart <-
        LinePercentChart(LCREChart,
                         LCRE,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      LCREChart
      
      LCREChart <- LCREChart +
        ylim(-.5, max(LCRE$`Upper CI`)*1.05)+
        xlim(min(LCRE$Year)-(length*0.2), max(LCRE$Year)+(length*0.2))
      
      ggsave(
        file,
        plot = LCREChart,
        width = 15,
        height = 15,
        units = "cm",
        dpi = 300
      )
      }
    )
    
    
    
    output$LowCarbonEconomyExportsSubtitle <- renderText({
      
      chartdata <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      paste("Scotland,",min(chartdata$Year), "-", max(chartdata$Year))
      
    })
    
    output$LowCarbonEconomyExportsPlot <- renderPlotly  ({
      
      ChartColours <- c("#39ab2c", "#FF8500")
      
      chartdata <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      chartdata <- chartdata[which(chartdata$Country == "Scotland"),]
      
      chartdata<- chartdata[which(chartdata$Category == "Exports"),]
      
      chartdata$Estimate <- as.numeric(chartdata$Estimate)
      chartdata$`Lower CI` <- as.numeric(chartdata$`Lower CI`)
      chartdata$`Upper CI` <- as.numeric(chartdata$`Upper CI`)
      
      

        chartdata$unit <- "\u00A3"

      
      chartdata$HoverText <- paste0("<b>", "Exports", ": ", chartdata$unit, format(round(chartdata$Estimate, 0), big.mark = ","), "</b>\nYear: ", chartdata$Year, "\n<i>Upper CI: ", chartdata$unit, format(round(chartdata$`Upper CI`, 0), big.mark = ","), "\nLower CI: ", chartdata$unit, format(round(chartdata$`Lower CI`, 0), big.mark = ","), "</i>")
      
      
      
      p <- plot_ly(chartdata,
                   x = ~Year,
                   y = ~Estimate, 
                   type = 'scatter', 
                   mode = 'lines',
                   text = chartdata$HoverText,
                   hoverinfo = 'text',
                   legendgroup = "1",
                   name = "Exports",
                   error_y =  ~list(
                     array = c((`Upper CI` - Estimate)),
                     arrayminus = c(`Estimate` - `Lower CI`),
                     color = '#000000',
                     symmetric = FALSE
                   ),
                   line = list(width = 6, color = ChartColours[1], dash = "none")) %>% 
        add_trace(
          data = tail(chartdata, 1),
          x = ~Year,
          y = ~Estimate, 
          legendgroup = "1",
          showlegend = FALSE ,
          type = "scatter",
          mode = 'markers',
          hoverinfo = 'text',
          text = tail(chartdata, 1)$HoverText,
          marker = list(size = 18, 
                        color = ChartColours[1])
        )  %>% 
        layout(
          barmode = 'stack',
          bargap = 0.66,
          legend = list(font = list(color = "#39ab2c"),
                        orientation = 'h'),
          hoverlabel = list(font = list(color = "white"),
                            hovername = 'text'),
          hovername = 'text',
          xaxis = list(title = "",
                       showgrid = FALSE),
          yaxis = list(
            title = "",
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
    
    
    output$LowCarbonEconomyExports.png <- downloadHandler(
      filename = "LowCarbonEconomyExports.png",
      content = function(file) {ChartColours <- c("#39ab2c", "#FF8500")
      
      LCRE <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 
      
      LCRE <- LCRE[which(LCRE$Country == "Scotland"),]
      
      LCRE<- LCRE[which(LCRE$Category == "Exports"),]
      
      LCRE$Estimate <- as.numeric(LCRE$Estimate)
      LCRE$`Lower CI` <- as.numeric(LCRE$`Lower CI`)
      LCRE$`Upper CI` <- as.numeric(LCRE$`Upper CI`)
      
      sourcecaption <- "BEIS"
      

        LCRE$unit <- "\u00A3"
        plottitle <- "Exports"
       
      
      LCRE$HoverText <- paste0("<b>", "Exports", ": ", LCRE$unit, format(round(LCRE$Estimate, 0), big.mark = ","), "</b>\nYear: ", LCRE$Year, "\n<i>Upper CI: ", LCRE$unit, format(round(LCRE$`Upper CI`, 0), big.mark = ","), "\nLower CI: ", LCRE$unit, format(round(LCRE$`Lower CI`, 0), big.mark = ","), "</i>")
      
      length <- max(LCRE$Year) - min(LCRE$Year)
      
      LCREChart <-
        LCRE %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Estimate,
              colour = ChartColours[1],
              label = Estimate),
          size = 1.5,
          family = "Century Gothic"
        ) + geom_errorbar(
          aes(
            y = Estimate,
            ymin = `Lower CI`,
            ymax = `Upper CI`
          ),
          width = .1
        )+
        geom_text(
          aes(
            x = Year-.1,
            y = Estimate,
            label = ifelse(Year == min(Year), paste0(LCRE$unit, format(round(LCRE$Estimate,1), big.mark = ",")), ""),
            hjust = 1,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year+.1,
            y = Estimate,
            label = ifelse(Year == max(Year), paste0(LCRE$unit, format(round(LCRE$Estimate,1), big.mark = ",")), ""),
            hjust = 0,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(LCRE, 1),
          aes(
            x = Year,
            y = Estimate,
            colour = ChartColours[1],
            label = Estimate,
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
        )
      
      
      LCREChart <-
        LinePercentChart(LCREChart,
                         LCRE,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      LCREChart
      
      LCREChart <- LCREChart +
        ylim(-.5, max(LCRE$`Upper CI`)*1.05)+
        xlim(min(LCRE$Year)-(length*0.2), max(LCRE$Year)+(length*0.2))
      
      ggsave(
        file,
        plot = LCREChart,
        width = 15,
        height = 15,
        units = "cm",
        dpi = 300
      )
      }
    )
    

}
