require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

PrimaryHeatingOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Domestic",
    fluidRow(column(8,
                    h3("Primary heating fuel for households", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('PrimaryHeatingSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('PrimaryHeating.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("PrimaryHeatingPlot")),
    plotlyOutput(ns("PrimaryHeatingPlot"), height = "600px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Non-domestic",
             fluidRow(column(8,
                             h3("Primary heating fuel for non-domestic premises", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('PrimaryHeatingNonDomSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('PrimaryHeatingNonDom.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("PrimaryHeatingPlot")),
             plotlyOutput(ns("PrimaryHeatingNonDomPlot"), height = "600px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    tabsetPanel(
      tabPanel("Primary Fuel by Year",
               fluidRow(
    column(10, h3("Data - Primary heating fuel for households", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("PrimaryHeatingTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Primary Fuel by Local Authority",
             fluidRow(
               column(10,uiOutput(ns('PrimaryHeatingDataSubtitle'))),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("PrimaryHeatingLATable"))%>% withSpinner(color="#68c3ea"))),
             p("* Small base and judged to be insufficiently reliable for publication"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Non-domestic premises",
             fluidRow(
               column(10, h3("Data - Primary heating fuel for non-domestic premises", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("PrimaryHeatingNonDomTable"))%>% withSpinner(color="#68c3ea"))),
             p("* Small base and judged to be insufficiently reliable for publication"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
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
        SourceLookup("SGSHCS"),
        SourceLookup("SGNonDomBase")
        
      )
    )
  )
}




###### Server ######
PrimaryHeating <- function(input, output, session) {

  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("PrimaryHeating.R")
  
      PrimaryHeating <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Primary heating fuel", col_names = TRUE, 
                          skip = 15)
      
  output$PrimaryHeatingSubtitle <- renderText({
    


    
    paste(max(as.numeric(names(PrimaryHeating)), na.rm = TRUE))
  })
  
  output$PrimaryHeatingDataSubtitle <- renderUI({
    tagList(h3(paste("Data - Primary heating fuel by Local Authority,", max(as.numeric(names(PrimaryHeating)), na.rm = TRUE)), style = "color: #68c3ea;  font-weight:bold"))
  })
 
  output$PrimaryHeatingPlot <- renderPlotly  ({
    
    PrimaryHeating <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Primary heating fuel",
                               col_names = FALSE,
                               skip = 15,
                               n_max = 4)
    
    PrimaryHeating <- as_tibble(t(PrimaryHeating))
    
    names(PrimaryHeating) <- c("Year", "Gas", "Electricity", "Oil")
    
    PrimaryHeating$`Year` <- as.numeric(PrimaryHeating$`Year`)
    
    PrimaryHeating <- PrimaryHeating[which(PrimaryHeating$`Year` == max(PrimaryHeating$`Year`, na.rm = TRUE)),]
    
    PrimaryHeating <- PrimaryHeating[complete.cases(PrimaryHeating),]
    
    PrimaryHeating$Other <- 1 - as.numeric(PrimaryHeating$Gas) - as.numeric(PrimaryHeating$Electricity) - as.numeric(PrimaryHeating$Oil)
    
    PrimaryHeating <- melt(PrimaryHeating, id = "Year")
    
    PrimaryHeating$value <- as.numeric(PrimaryHeating$value)
    
    PrimaryHeating$variable <- paste0("<b>", PrimaryHeating$variable, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <- c(
      "#bd0026",
      "#e31a1c",
      "#fc4e2a",
      "#fd8d3c",
      "#feb24c",
      "#fed976"
    )
    
    p <- plot_ly(
      data = PrimaryHeating,
      labels = ~variable,
      type = 'pie',
      values = ~value,
      text = paste0(
        PrimaryHeating$variable,
        ": ", percent(PrimaryHeating$value, 1)),
      textposition = 'outside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      marker = list(colors = BarColours,
                    line = list(color = '#FFFFFF', width = 1))
    )  %>% 
      layout(
        barmode = 'stack',
        showlegend = FALSE,
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE),
        xaxis = list(
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
    
    orca(p, "StaticCharts/PrimaryHeatingPie.svg")
    
    p
    
  })
  
  output$PrimaryHeatingTable = renderDataTable({
    
    PrimaryHeating <- read_excel("Structure/CurrentWorking.xlsx",
                                 sheet = "Primary heating fuel",
                                 col_names = FALSE,
                                 skip = 15,
                                   n_max = 9)
    
    PrimaryHeating <- as_tibble(t(PrimaryHeating))
    
    names(PrimaryHeating) <- unlist(PrimaryHeating[1,])
    
    PrimaryHeating[1:9] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    PrimaryHeating <- PrimaryHeating[complete.cases(PrimaryHeating),]
    
    names(PrimaryHeating)[1] <- "Year"
    
    datatable(
      PrimaryHeating,
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
        title = "Primary heating fuel for households",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Primary heating fuel for households',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Primary heating fuel for households')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:9, 0)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/5 - Consumers/PrimaryHeating.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("PrimaryHeatingTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("PrimaryHeatingLATable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$PrimaryHeating.png <- downloadHandler(
    filename = "PrimaryHeating.png",
    content = function(file) {

writePNG(
  readPNG(
  "Structure/5 - Consumers/PrimaryHeatingChart.png"
),
file)
    }
  )
  
  output$PrimaryHeatingLATable = renderDataTable({
    
    PrimaryHeatingLA <- read_excel("Structure/CurrentWorking.xlsx",
                               sheet = "Primary heating fuel", col_names = TRUE, 
                               skip = 15, n_max = 33)[9:14]
    
    names(PrimaryHeatingLA)[1] <- c("Local Authority")
    
   datatable(
      PrimaryHeatingLA,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Primary heating fuel by Local Authority",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Primary heating fuel by Local Authority',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Primary heating fuel by Local Authority')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:ncol(PrimaryHeatingLA), 0)
  })
  
  output$PrimaryHeatingNonDomSubtitle <- renderText({
    
    PrimaryHeatingNonDom <- read_excel("Structure/CurrentWorking.xlsx",
                                       sheet = "Primary heating fuel", col_names = FALSE, 
                                       skip = 14)
    paste0("Scotland,", unlist(strsplit(as.character(PrimaryHeatingNonDom[1,16]), ","))[3])
  })
  
  output$PrimaryHeatingNonDomPlot <- renderPlotly  ({
    
    PrimaryHeatingNonDom <- read_excel("Structure/CurrentWorking.xlsx",
                                       sheet = "Primary heating fuel",
                                       col_names = FALSE,
                                       skip = 15,
                                       n_max = 6)[16:17]
    
    PrimaryHeatingNonDom <- as_tibble(t(PrimaryHeatingNonDom))
    
    names(PrimaryHeatingNonDom) <- unlist(PrimaryHeatingNonDom[1,])
    
    PrimaryHeatingNonDom <- PrimaryHeatingNonDom[-1,]
    
    PrimaryHeatingNonDom[1,1] <- "Proportion"
    
    PrimaryHeatingNonDom[2:6] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    PrimaryHeatingNonDom <- melt(PrimaryHeatingNonDom, id = "Primary heating Fuel")
    
    PrimaryHeatingNonDom$variable <- paste0("<b>", PrimaryHeatingNonDom$variable, "</b>")
    
    ChartColours <- c("#68c3ea", "#FF8500")
    BarColours <- c(
      "#bd0026",
      "#e31a1c",
      "#fc4e2a",
      "#fd8d3c",
      "#feb24c",
      "#fed976"
    )
    
    p <- plot_ly(
      data = PrimaryHeatingNonDom,
      labels = ~variable,
      type = 'pie',
      automargin = TRUE,
      values = ~value,
      text = paste0(
        PrimaryHeatingNonDom$variable,
        ": ", percent(PrimaryHeatingNonDom$value, 1)),
      textposition = 'outside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      marker = list(colors = BarColours,
                    line = list(color = '#FFFFFF', width = 1))
    )  %>% 
      layout(
        barmode = 'stack',
        showlegend = FALSE,
        margin = list(
          l = 50,
          r = 50,
          b = 125,
          t = 10,
          pad = 4
        ),
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     automargin = TRUE,
                     showgrid = FALSE),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = TRUE,
          automargin = TRUE
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
  })
  
  output$PrimaryHeatingNonDomTable = renderDataTable({
    
    PrimaryHeatingNonDom <- read_excel("Structure/CurrentWorking.xlsx",
                                       sheet = "Primary heating fuel",
                                       col_names = TRUE,
                                       skip = 15,
                                       n_max = 5)[16:17]
    
    names(PrimaryHeatingNonDom) <- c("Primary heating fuel", "Proportion")
    
    datatable(
      PrimaryHeatingNonDom,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(1, 'desc')),
        title = "Primary heating fuel for non-domestic premises, Scotland, January 2013 - July 2017",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Primary heating fuel for non-domestic premises, Scotland, January 2013 - July 2017',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Primary heating fuel for non-domestic premises, Scotland, January 2013 - July 2017')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2, 0)
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("PrimaryHeatingNonDomTable")
  })
  
  output$PrimaryHeatingNonDom.png <- downloadHandler(
    filename = "PrimaryHeatingNonDom.png",
    content = function(file) {
      
      writePNG(
        readPNG(
          "Structure/5 - Consumers/PrimaryHeatingNonDomChart.png"
        ),
        file)
    }
  )
}

