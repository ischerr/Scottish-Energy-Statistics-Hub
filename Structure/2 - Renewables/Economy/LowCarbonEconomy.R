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
      tabPanel("Employment",
               fluidRow(column(8,
                               h3("Full time equivalent jobs directly supported by the LCRE sector", style = "color: #39ab2c;  font-weight:bold"),
                               h4(textOutput(ns('LowCarbonEconomySubtitle')), style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('LowCarbonEconomy.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("LowCarbonEconomyPlot")),
               plotlyOutput(ns("LowCarbonEconomyPlot"))%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Turnover",
    fluidRow(column(8,
                    h3("Turnover directly supported by the LCRE sector", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('LowCarbonEconomyTurnoverSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('LowCarbonEconomyTurnover.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("LowCarbonEconomyPlot")),
    plotlyOutput(ns("LowCarbonEconomyTurnoverPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))

    ),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Employment - Overview",
    fluidRow(
    column(10, h3("Data - Employment", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("LowCarbonEconomyTable"))%>% withSpinner(color="#39ab2c"))),
    p("*Blank cells have been suppressed for disclosure control"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Employment - Direct Activity",
             fluidRow(
               column(10, h3("Data - Full time equivalent jobs directly supported by the LCRE sector", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("LowCarbonEconomyDirectEmploymentTable"))%>% withSpinner(color="#39ab2c"))),
             p("*Blank cells have been suppressed for disclosure control"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Turnover - Overview",
             fluidRow(
               column(10, h3("Data - Turnover (\u00A3000s)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("LowCarbonEconomyTurnoverTable"))%>% withSpinner(color="#39ab2c"))),
             p("*Blank cells have been suppressed for disclosure control"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Turnover - Direct Activity",
             fluidRow(
               column(10, h3("Data - Turnover directly supported by the LCRE sector (\u00A3000s)", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("LowCarbonEconomyDirectTurnoverTable"))%>% withSpinner(color="#39ab2c"))),
             p("*Blank cells have been suppressed for disclosure control"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
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
LowCarbonEconomy <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }

  print("LowCarbonEconomy.R")
  
    output$LowCarbonEconomySubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Low carbon economy", skip = 15, col_names = FALSE)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    Data <- fill(Data, 1)
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1:2] <- c("Year", "Type")
    
    Data = Data[-1, ]
    
    Data <- as_tibble(Data[c(1,2,5,6,7)])
    
    LowCarbonEconomy <- Data
    
    ### variables
    
    paste("Scotland,", max(LowCarbonEconomy$Year))
  })
    
    output$LowCarbonEconomyTurnoverSubtitle <- renderText({
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Low carbon economy", skip = 15, col_names = FALSE)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      
      Data <- fill(Data, 1)
      
      names(Data) <- unlist(Data[1,])
      
      names(Data)[1:2] <- c("Year", "Type")
      
      Data = Data[-1, ]
      
      Data <- as_tibble(Data[c(1,2,5,12,13)])
      
      LowCarbonEconomy <- Data
      
      ### variables
      
      paste("Scotland,", max(LowCarbonEconomy$Year))
    })
  
    output$LowCarbonEconomyPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Low carbon economy", skip = 15, col_names = FALSE)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    Data <- fill(Data, 1)
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1:2] <- c("Year", "Type")
    
    Data = Data[-1, ]
    
    Data <- as_tibble(Data[c(1,2,12,13)])
    
    LowCarbonEconomy <- Data
    
    LowCarbonEconomy <- melt(LowCarbonEconomy, id = c("Year", "Type"))
    
    LowCarbonEconomy$value <- as.numeric(LowCarbonEconomy$value)
    
    LowCarbonEconomy <- LowCarbonEconomy[which(LowCarbonEconomy$Year == max(LowCarbonEconomy$Year) & LowCarbonEconomy$Type == "Employees  (FTE)"),]
    
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
          text = paste("<b>Total Employees</b>:",format(round(sum(LowCarbonEconomy$value), digits = 0), big.mark = ",")),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  
    output$LowCarbonEconomyTurnoverPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Low carbon economy", skip = 15, col_names = FALSE)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    Data <- fill(Data, 1)
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1:2] <- c("Year", "Type")
    
    Data = Data[-1, ]
    
    Data <- as_tibble(Data[c(1,2,12,13)])
    
    LowCarbonEconomy <- Data
    
    LowCarbonEconomy <- melt(LowCarbonEconomy, id = c("Year", "Type"))
    
    LowCarbonEconomy$value <- as.numeric(LowCarbonEconomy$value)
    
    LowCarbonEconomy <- LowCarbonEconomy[which(LowCarbonEconomy$Year == max(LowCarbonEconomy$Year) & LowCarbonEconomy$Type == "Turnover (\u00A3000s)"),]
    
    p <- plot_ly() %>% 
      add_pie(data = LowCarbonEconomy,
              labels = ~variable,
              values = ~value,
              hole = .5,
              sort = FALSE,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#2b8cbe", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(LowCarbonEconomy$variable,": \u00A3", format(round(LowCarbonEconomy$value/1000000, digits = 2), big.mark = ","), " bn\n", percent((LowCarbonEconomy$value)/ sum(LowCarbonEconomy$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste0("<b>Total Turnover</b>: \u00A3",format(round(sum(LowCarbonEconomy$value/1000000), digits = 2), big.mark = ","), "bn"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
    
    
    
  })
  
    output$LowCarbonEconomyTable = renderDataTable({
    
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Low carbon economy", skip = 15, col_names = FALSE)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      
      Data <- fill(Data, 1)
      
      names(Data) <- unlist(Data[1,])
      
      names(Data)[1:2] <- c("Year","Category")
      
      Data = Data[-1, ]
      
      Data <- Data[c(1,2,5,6,7,8)]
      
      Data <- Data[which(Data$Category == "Employees  (FTE)"),]
      
      Data$Category <- NULL
      
      datatable(
        Data,
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
          title = "Full time equivalent jobs supported by the LCRE sector",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Full time equivalent jobs supported by the LCRE sector',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Full time equivalent jobs supported by the LCRE sector')
          ),
          
          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = -1
        )
      ) %>%
        formatRound(2:4, 0) %>% 
        formatPercentage(5, 1) %>% 
        formatStyle(c(5), fontStyle = "italic") %>% 
        formatStyle(c(4), fontWeight = "bold")
  })
  
    output$LowCarbonEconomyTurnoverTable = renderDataTable({
    
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Low carbon economy", skip = 15, col_names = FALSE)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      
      Data <- fill(Data, 1)
      
      names(Data) <- unlist(Data[1,])
      
      names(Data)[1:2] <- c("Year","Category")
      
      Data = Data[-1, ]
      
      Data <- Data[c(1,2,5,6,7,8)]
      
      Data <- Data[which(substr(Data$Category, 1,8) == "Turnover"),]
      
      Data$Category <- NULL
      
      datatable(
        Data,
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
          title = "Turnover supported by the LCRE sector (u00A3000s)",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Turnover supported by the LCRE sector (u00A3000s)',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Turnover supported by the LCRE sector (u00A3000s)')
          ),
          
          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = -1
        )
      ) %>%
        formatRound(2:4, 0) %>% 
        formatPercentage(5, 1) %>% 
        formatStyle(c(5), fontStyle = "italic") %>% 
        formatStyle(c(4), fontWeight = "bold")
  })
    
    output$LowCarbonEconomyDirectEmploymentTable = renderDataTable({
      
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
            sheet = "Low carbon economy", skip = 15, col_names = FALSE)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    
    Data <- fill(Data, 1)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- c("Category")
    
    Data = Data[-1, ]
    
    Data <- Data[c(1,2,4,6,8)]
    
    Data <- Data[complete.cases(Data),]
    
    Data <- Data[-(1:4), ]
    
    Data[Data == 0] <- NA
    
    LowCarbonEconomy <- Data
    
    datatable(
      LowCarbonEconomy,
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
        title = "Turnover directly supported by the LCRE sector (\u00A3000s)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Turnover directly supported by the LCRE sector (\u00A3000s)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Turnover directly supported by the LCRE sector (\u00A3000s)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatRound(2:5, 0) %>% 
      formatStyle(1,
                  target = 'row',
                  backgroundColor = styleEqual(c('Renewable sector', 'Low carbon'), c('#969696', '#969696' ))
      )  %>% 
      formatStyle(1,
                  target = 'row',
                  backgroundColor = styleEqual(c('Low carbon electricity', 'Low carbon heat', 'Energy from waste and biomass', 'Energy efficient products', 'Low carbon services'), c('#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd'))
      )
    })
    
    output$LowCarbonEconomyDirectTurnoverTable = renderDataTable({
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Low carbon economy", skip = 15, col_names = FALSE)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      
      Data <- fill(Data, 1)
      
      Data <- as_tibble(t(Data))
      
      names(Data) <- unlist(Data[1,])
      
      names(Data)[1] <- c("Category")
      
      Data = Data[-1, ]
      
      Data <- Data[c(1,3,5,7,9)]
      
      Data <- Data[complete.cases(Data),]
      
      Data <- Data[-(1:4), ]
      
      Data[Data == 0] <- NA
      
      LowCarbonEconomy <- Data
      
      datatable(
        LowCarbonEconomy,
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
          title = "Turnover directly supported by the LCRE sector (\u00A3000s)",
          dom = 'ltBp',
          buttons = list(
            list(extend = 'copy'),
            list(
              extend = 'excel',
              title = 'Turnover directly supported by the LCRE sector (\u00A3000s)',
              header = TRUE
            ),
            list(extend = 'csv',
                 title = 'Turnover directly supported by the LCRE sector (\u00A3000s)')
          ),
          
          # customize the length menu
          lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
          ), # end of lengthMenu customization
          pageLength = -1
        )
      ) %>%
        formatRound(2:5, 0) %>% 
        formatStyle(1,
                    target = 'row',
                    backgroundColor = styleEqual(c('Renewable sector', 'Low carbon'), c('#969696', '#969696' ))
        )  %>% 
        formatStyle(1,
                    target = 'row',
                    backgroundColor = styleEqual(c('Low carbon electricity', 'Low carbon heat', 'Energy from waste and biomass', 'Energy efficient products', 'Low carbon services'), c('#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd', '#bdbdbd'))
        )
    })
    
    
    
  
    output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Economy/LowCarbonEconomy.txt")[2])
                     
                   )))
  })
  
    observeEvent(input$ToggleTable, {
    toggle("LowCarbonEconomyTable")
  })
  
    observeEvent(input$ToggleTable2, {
    toggle("LowCarbonEconomyTurnoverTable")
  })
    
    observeEvent(input$ToggleTable3, {
      toggle("LowCarbonEconomyDirectEmploymentTable")
    })
    
    observeEvent(input$ToggleTable4, {
      toggle("LowCarbonEconomyDirectTurnoverTable")
    })
  
    observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
    output$LowCarbonEconomy.png <- downloadHandler(
    filename = "LowCarbonEconomy.png",
    content = function(file) {
      writePNG(readPNG("Structure/2 - Renewables/Emissions/LCREJobs.png"), file) 
    }
    )
    
    output$LowCarbonEconomyTurnover.png <- downloadHandler(
    filename = "LowCarbonEconomyTurnover.png",
    content = function(file) {
      writePNG(readPNG("Structure/2 - Renewables/Emissions/LCRETurnover.png"), file) 
    }
    )
}
