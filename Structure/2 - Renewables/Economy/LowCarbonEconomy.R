require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

LowCarbonEconomyOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Employment",
        fluidRow(
          column(
            8,
            h3(
              "Full time equivalent jobs directly supported by the LCRE sector",
              style = "color: #39ab2c;  font-weight:bold"
            ),
            h4(textOutput(ns(
              'LowCarbonEconomySubtitle'
            )), style = "color: #39ab2c;")
          ),
          column(
            4,
            style = 'padding:15px;',
            downloadButton(ns('LowCarbonEconomy.png'), 'Download Graph', style =
                             "float:right")
          )
        ),
        
        tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
        #dygraphOutput(ns("LowCarbonEconomyPlot")),
        plotlyOutput(ns("LowCarbonEconomyPlot")) %>% withSpinner(color =
                                                                   "#39ab2c"),
        tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")
      ),
      tabPanel(
        "Turnover",
        fluidRow(
          column(
            8,
            h3("Turnover directly supported by the LCRE sector", style = "color: #39ab2c;  font-weight:bold"),
            h4(textOutput(
              ns('LowCarbonEconomyTurnoverSubtitle')
            ), style = "color: #39ab2c;")
          ),
          column(
            4,
            style = 'padding:15px;',
            downloadButton(
              ns('LowCarbonEconomyTurnover.png'),
              'Download Graph',
              style = "float:right"
            )
          )
        ),
        
        tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
        #dygraphOutput(ns("LowCarbonEconomyPlot")),
        plotlyOutput(ns("LowCarbonEconomyTurnoverPlot")) %>% withSpinner(color =
                                                                           "#39ab2c"),
        tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")
      )
      
    ),
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
            h3("Data - Turnover directly supported by the LCRE sector (\u00A3)", style = "color: #39ab2c;  font-weight:bold")
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
            h3("Data - Exports directly supported by the LCRE sector (\u00A3)", style = "color: #39ab2c;  font-weight:bold")
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
      column(1,
             p("Next update:")),
      column(2,
             DateLookup(c("ONSLowCarbon"))),
      column(1, align = "right",
             p("Sources:")),
      column(
        8,
        align = "right",
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
  
  
    output$LowCarbonEconomySubtitle <- renderText({
    

  
    
    paste("Scotland,", max(Year))
  })
    
    output$LowCarbonEconomyTurnoverSubtitle <- renderText({
      
    paste("Scotland,", max(Year))
    })
  
    output$LowCarbonEconomyPlot <- renderPlotly  ({
    
    LowCarbonEconomy <- Data[which(Data$Measure == "Employees  (FTE)"),]
    
    LowCarbonEmployment <- LowCarbonEconomy$Direct
    
    LowCarbonEconomy <- melt(LowCarbonEconomy[c(2,4,5)], id = c("Year"))
    
    LowCarbonEconomy$value <- as.numeric(LowCarbonEconomy$value)
    
    p <- plot_ly() %>% 
      add_pie(data = LowCarbonEconomy,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#2b8cbe", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(LowCarbonEconomy$variable,": ", format(round(LowCarbonEconomy$value, digits = 0), big.mark = ","), " employees\n", percent((LowCarbonEconomy$value)/ sum(LowCarbonEconomy$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Total Employees</b>:",format(round(LowCarbonEmployment, digits = 0), big.mark = ",")),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    
    #orca(p, "StaticCharts/LowCarbonEconomyPie.svg")
    
    p
  })
  
    output$LowCarbonEconomyTurnoverPlot <- renderPlotly  ({
      
    LowCarbonTurnover <- Data[which(Data$Measure == "Turnover (\u00A3000s)"),]
    
    LowCarbonDirectTurnover <- LowCarbonTurnover$Direct /1000000
    
    LowCarbonTurnover <- melt(LowCarbonTurnover[c(2,4,5)], id = c("Year"))
    
    LowCarbonTurnover$value <- as.numeric(LowCarbonTurnover$value) /1000000 
    
    p <- plot_ly() %>% 
      add_pie(data = LowCarbonTurnover,
              labels = ~variable,
              values = ~value,
              hole = .5,
              sort = FALSE,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#2b8cbe", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(LowCarbonTurnover$variable,": \u00A3", format(round(LowCarbonTurnover$value, digits = 2), big.mark = ","), " bn\n", percent((LowCarbonTurnover$value)/ sum(LowCarbonTurnover$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste0("<b>Total Turnover</b>: \u00A3",format(round(LowCarbonDirectTurnover, digits = 2), big.mark = ","), " bn"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    
    #orca(p, "StaticCharts/LowCarbonTurnoverPie.svg")
    
    p
    
    
    
  })
  
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
      
      Data <- read_excel("Structure/2 - Renewables/Economy/EconomyTables.xlsx", 
                         sheet = "Turnover", col_names = TRUE)
      
      
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
          title = "Turnover directly supported by the LCRE sector (\u00A3)",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Turnover directly supported by the LCRE sector (\u00A3)',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Turnover directly supported by the LCRE sector (\u00A3)')
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
          title = "Exports directly supported by the LCRE sector (\u00A3)",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Exports directly supported by the LCRE sector (\u00A3)',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Exports directly supported by the LCRE sector (\u00A3)')
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
  
    output$LowCarbonEconomy.png <- downloadHandler(
    filename = "LowCarbonEconomy.png",
    content = function(file) {
      writePNG(readPNG("Structure/2 - Renewables/Economy/LCREJobs.png"), file) 
    }
    )
    
    output$LowCarbonEconomyTurnover.png <- downloadHandler(
    filename = "LowCarbonEconomyTurnover.png",
    content = function(file) {
      writePNG(readPNG("Structure/2 - Renewables/Economy/LCRETurnover.png"), file) 
    }
    )
}
