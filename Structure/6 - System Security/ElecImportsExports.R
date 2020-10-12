require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



ElecImportsExportsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Imports and Exports Quarterly",
               fluidRow(column(8,
                               h3("Quarterly Electricity imports and exports", style = "color: #5d8be1;  font-weight:bold"),
                               h4(textOutput(ns('QuarterlyElecImportsExportsSubtitle')), style = "color: #5d8be1;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('QuarterlyElecImportsExports.png'), 'Download Graph', style="float:right")
               )),
               fluidRow(column(6,selectInput(ns("YearSelect"), "Period:", c("Last Year", "Last 2 Years", "Last 5 Years", "All Years"), selected = "Last Year", multiple = FALSE,
                                             selectize = TRUE, width = NULL, size = NULL))),
               uiOutput(ns("QuarterlyUI"))%>% withSpinner(color="#5d8be1"),
               tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Imports and Exports Annual",
             fluidRow(column(8,
                             h3("Annual Electricity imports and exports", style = "color: #5d8be1;  font-weight:bold"),
                             h4(textOutput(ns('ElecImportsExportsSubtitle')), style = "color: #5d8be1;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecImportsExports.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
             #dygraphOutput(ns("ElecImportsExportsPlot")),
             plotlyOutput(ns("ElecImportsExportsPlot"))%>% withSpinner(color="#5d8be1"),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Wholesale Export Value",
    fluidRow(column(8,
                    h3("Wholesale value of electricity exports", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('WholesaleExportsSubtitle')), style = "color: #5d8be1;")
    ),
    column(
      4, style = 'padding:15px;',
      downloadButton(ns('WholesaleExports.png'), 'Download Graph', style="float:right")
    )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("WholesaleExportsPlot")),
    plotlyOutput(ns("WholesaleExportsPlot"))%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    tabsetPanel(
      tabPanel("Annual",
    fluidRow(
    column(10, h3("Data - Electricity imports and exports (GWh)", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ElecImportsExportsTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Quarterly",
             fluidRow(
               column(10, h3("Data - Electricity imports and exports (GWh)", style = "color: #5d8be1;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ElecImportsExportsQuarterTable"))%>% withSpinner(color="#5d8be1"))),
             tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;")),
    tabPanel("Wholesale exports",
    fluidRow(
      column(10, h3("Data - Wholesale value of electricity exports", style = "color: #5d8be1;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("WholesaleExportsTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISImportExport"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISImportExport")
        
      )
    )
  )
}




###### Server ######
ElecImportsExports <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ElecImportsExports.R")

  
  output$QuarterlyUI <- renderUI({
    
    if(as.character(input$YearSelect) == "Last Year"){ChartHeight <- "500px"}
    
    if(as.character(input$YearSelect) == "Last 2 Years"){ChartHeight <- "1000px"}
    
    if(as.character(input$YearSelect) == "Last 5 Years"){ChartHeight <- "1500px"}
    
    if(as.character(input$YearSelect) == "All Years"){ChartHeight <- "2000px"}
    
    plotlyOutput(session$ns("QuarterlyElecImportsExportsPlot"), height = ChartHeight) %>% withSpinner(color="#5d8be1")

  })
  
  output$ElecImportsExportsSubtitle <- renderText({
    

    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec imports & exports", skip = 12)[c(10,15,16)]
    
    Data <- Data[complete.cases(Data),]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "Exports", "Imports")
    
    ElecImportsExports <- as_tibble(Data)
    
    
    paste("Scotland,", min(ElecImportsExports$Year),"-", max(ElecImportsExports$Year))
    
  })
  
  output$ElecImportsExportsPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec imports & exports", skip = 12)[c(10,15,16)]
    
    Data <- Data[complete.cases(Data),]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "Exports", "Imports")
    
    ElecImportsExports <- as_tibble(Data)
    
    ### variables
    ChartColours <- c("#5d8be1", "#225ea8", "#41b6c4", "#8da0cb")
    sourcecaption = "Source: BEIS"
    plottitle = "Electricity imports and exports"
    
    ElecImportsExports$Year <- paste0("01/01/", ElecImportsExports$Year)
    
    ElecImportsExports$Year <- dmy(ElecImportsExports$Year)
    
    
    p <-  plot_ly(ElecImportsExports,x = ~ Year ) %>% 
      add_trace(data = ElecImportsExports,
                x = ~ Year,
                y = ~ Exports,
                name = "Electricity Exports",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Electricity Exports: ",
                  format(round(ElecImportsExports$Exports, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(ElecImportsExports$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecImportsExports[which(ElecImportsExports$Exports > 0 | ElecImportsExports$Exports < 0),], 1),
        x = ~ Year,
        y = ~ `Exports`,
        legendgroup = "1",
        name = "Electricity Exports",
        text = paste0(
          "Electricity Exports: ",
          format(round(ElecImportsExports[which(ElecImportsExports$Exports > 0 | ElecImportsExports$Exports < 0),][-1,]$Exports, digits = 0), big.mark = ","),
          " GWh\nYear: ",
          format(ElecImportsExports[which(ElecImportsExports$Exports > 0 | ElecImportsExports$Exports < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = ElecImportsExports,
                x = ~ Year,
                y = ~ Imports,
                name = "Electricity Imports",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Electricity Imports: ",
                  format(round(ElecImportsExports$Imports, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(ElecImportsExports$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1),
        x = ~ Year,
        y = ~ `Imports`,
        legendgroup = "2",
        name = "Electricity Imports",
        text = paste0(
          "Electricity Imports: ",
          format(round(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),][-1,]$Imports, digits = 0), big.mark = ","),
          " GWh\nYear: ",
          format(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      add_annotations( x = tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1)$Year,
                       y = tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1)$Exports-500,
                       xref = "x", yref = "y",
                       axref = "x", ayref = "y",
                       text = "",
                       showarrow = T,
                       ax = tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1)$Year,
                       ay = tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1)$Imports+500,
                       arrowcolor = ChartColours[1]
                       ) %>% 
     add_annotations(
       x = tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1)$Year - 500,
       y = ((tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1)$Exports - tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1)$Imports)/2)+tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1)$Imports,
       text = paste("<b>Net Exports:\n", 
                    format(round((tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1)$Exports - tail(ElecImportsExports[which(ElecImportsExports$Imports > 0 | ElecImportsExports$Imports < 0),], 1)$Imports), digits = 0), big.mark = ","), "GWh</b>"
                    ),
       showarrow = FALSE,
       font = list(color = ChartColours[1],
                   size = 15)
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
                     range = c(min(ElecImportsExports$Year)-100, max(ElecImportsExports$Year)+100)),
        yaxis = list(
          title = "GWh",
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
  
  ###
  output$QuarterlyElecImportsExportsSubtitle <- renderText({
    
    ImportsExports <- read_csv("Processed Data/Output/Imports and Exports/ImportsExports.csv")
    
    
    if(as.character(input$YearSelect) == "Last Year"){DataLength <- 4}
    
    if(as.character(input$YearSelect) == "Last 2 Years"){DataLength <- 8}
    
    if(as.character(input$YearSelect) == "Last 5 Years"){DataLength <- 20}
    
    if(as.character(input$YearSelect) == "All Years"){DataLength <- nrow(ImportsExports)}
    
    ImportsExports <- tail(ImportsExports,DataLength)
    
    
    paste("Scotland,", head(ImportsExports,1)[1],"-", tail(ImportsExports,1)[1])
  })
  
  output$QuarterlyElecImportsExportsPlot <- renderPlotly  ({
    
    #"Last Year", "Last 2 Years" "Last 5 Years", "All Years"
    
    Period = as.numeric(input$YearSelect)
    
    i
    
    ImportsExports <- read_csv("Processed Data/Output/Imports and Exports/ImportsExports.csv")
    
    if(as.character(input$YearSelect) == "Last Year"){DataLength <- 4}
    
    if(as.character(input$YearSelect) == "Last 2 Years"){DataLength <- 8}
    
    if(as.character(input$YearSelect) == "Last 5 Years"){DataLength <- 20}
    
    if(as.character(input$YearSelect) == "All Years"){DataLength <- nrow(ImportsExports)}
    
    ImportsExports <- tail(ImportsExports,DataLength)
    
    ImportsExports$ScotlandImports <- -ImportsExports$`Scotland - England Imports` - ImportsExports$`Scotland - NI Imports`
    
    ImportsExports$ScotlandExports <- +ImportsExports$`Scotland - England Exports` + ImportsExports$`Scotland - NI Exports`
    
    ImportsExports <- ImportsExports[c(1,18,19)] 
    
    ImportsExports <- ImportsExports[which(as.numeric(substr(ImportsExports$Quarter,1,4)) >= 2000),]
    
    ChartColours <- c("#39ab2c", "#FF8500")
    
    BarColours <-
      c(
        "#08519c",
        "#3182bd",
        "#6baed6",
        "#9ecae1",
        
        "#a63603",
        "#e6550d",
        "#fd8d3c",
        "#fdae6b"
      )
    
    ImportsExports$Year <- paste0("<b>", ImportsExports$Quarter, "</b>")
    
    p <- plot_ly(
      data = ImportsExports,
      y = ~Year,
      x = ~`ScotlandExports`,
      legendgroup = 2,
      text = paste0(
        "Exports: ",
        format(round(ImportsExports$ScotlandExports, digits = 0),big.mark = ","),
        " GWh\nYear: ",
        ImportsExports$Year
      ),
      name = "Exports",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>% 
      add_trace(
        data = ImportsExports,
        y = ~Year,
        x = ~`ScotlandImports`,
        legendgroup = 1,
        text = paste0(
          "QImports: ",
          format(round(-ImportsExports$`ScotlandImports`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          ImportsExports$Year
        ),
        name = "Imports",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[5])
      ) %>% 
      layout(
        barmode = 'relative',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     dtick = 1),
        xaxis = list(
          title = "",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          range = c(-2000, 20000),
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
    
  })
  
  ###
  
  
  output$ElecImportsExportsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec imports & exports", col_names = FALSE, skip = 12)[10:17]
    
    
    names(Data) <- as.character(unlist(Data[1,]))
    
    Data <- Data[-1,]
    Data[1:8] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    ElecImportsExports <- as_tibble(Data[c(1,6:8,2:5)])
    
    
    datatable(
      ElecImportsExports[complete.cases(ElecImportsExports),],
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
        title = "Electricity imports and exports (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Electricity imports and exports (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Electricity imports and exports (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:8, 0)
  })
  
  output$ElecImportsExportsQuarterTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec imports & exports", col_names = FALSE, skip = 12)[1:8]
    
    
    names(Data) <- as.character(unlist(Data[1,]))
    
    Data <- Data[-1,]
    Data[2:8] %<>% lapply(function(x) as.numeric(as.character(x)))

    ElecImportsExports <- as_tibble(Data[c(1,6:8,2:5)])
    
    datatable(
      ElecImportsExports,
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
        title = "Quarterly Electricity imports and exports (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Quarterly Electricity imports and exports (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Quarterly Electricity imports and exports (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:8, 0)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/6 - System Security/ElecImportsExports.txt")[2])
                     
                   )))
  })
 
  observeEvent(input$ToggleTable1, {
    toggle("ElecImportsExportsTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ElecImportsExportsQuarterTable")
  })
  
  
  observeEvent(input$ToggleTable3, {
    toggle("WholesaleExportsTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ElecImportsExports.png <- downloadHandler(
    filename = "ElecImportsExports.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Elec imports & exports", skip = 12)[c(10,15,16)]
      
      Data <- Data[complete.cases(Data),]
      
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      
      names(Data) <- c("Year", "Exports", "Imports")
      
      ElecImportsExports <- as_tibble(Data)
      
      ### variables
      ChartColours <- c("#5d8be1", "#225ea8", "#41b6c4", "#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = "Electricity imports and exports"
      
      #ElecImportsExports$ExportsPercentage <- PercentLabel(ElecImportsExports$Exports)
      
      
      ElecImportsExportsChart <- ElecImportsExports %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = Exports,
            label = percent(Exports)
          ),
          size = 1.5,
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Exports,
            label = ifelse(Year == min(Year), paste0(format(round(Exports, digits = 0),big.mark=",")," GWh"), ""),
            hjust = 0.5,
            vjust = -1,
            
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Exports,
            label = ifelse(Year == max(Year), paste0(format(round(Exports, digits = 0),big.mark=",")," GWh"), ""),
            vjust = -1,
            
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ElecImportsExports, 1),
          aes(
            x = Year,
            y = Exports,
            
            show_guide = FALSE
          ),
          size = 4,
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(Exports),
            label = "Exports",
            hjust = 0.5,
            vjust = -4,
            
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `Imports`,
            
            label = paste0(`Imports` * 100, "%")
          ),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Imports`,
            label = ifelse(Year == min(Year), paste0(format(round(`Imports`, digits = 0),big.mark=","), " GWh"), ""),
            hjust = 0.5,
            vjust = -1.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Imports`,
            label = ifelse(Year == max(Year), paste0(format(round(`Imports`, digits = 0),big.mark=","), " GWh"), ""),
            vjust = 2,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ElecImportsExports, 1),
          aes(
            x = Year,
            y = `Imports`,
            
            show_guide = FALSE
          ),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(`Imports`),
            label = "Imports",
            hjust = 0.5,
            vjust = -4,
            
            fontface = 2
          ),
          colour = ChartColours[3],
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
        ) +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )+
        geom_segment(
          aes(
            y = ElecImportsExports[which(ElecImportsExports$Year == max(ElecImportsExports$Year)),]$Exports - 300,
            yend = ElecImportsExports[which(ElecImportsExports$Year == max(ElecImportsExports$Year)),]$Imports + 300,
            x = ElecImportsExports[which(ElecImportsExports$Year == max(ElecImportsExports$Year)),]$Year,
            xend = ElecImportsExports[which(ElecImportsExports$Year == max(ElecImportsExports$Year)),]$Year
          ),
          arrow = arrow(length = unit(0.3, "cm"), 
                        ends = "both"),
          colour = ChartColours[1],
          family = "Century Gothic"
        )+ 
        geom_text(
          aes(
            x = ElecImportsExports[which(ElecImportsExports$Year == max(ElecImportsExports$Year)),]$Year - 3,
            y = ((ElecImportsExports[which(ElecImportsExports$Year == max(ElecImportsExports$Year)),]$Exports - ElecImportsExports[which(ElecImportsExports$Year == max(ElecImportsExports$Year)),]$Imports)/2)+ElecImportsExports[which(ElecImportsExports$Year == max(ElecImportsExports$Year)),]$Imports,
            label = paste0("Net exports:\n",
                           format(round(ElecImportsExports$Exports[which(ElecImportsExports$Year == max(ElecImportsExports$Year))]-ElecImportsExports$Imports[which(ElecImportsExports$Year == max(ElecImportsExports$Year))], digits = 0), big.mark = ","),
                           " GWh")
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          fontface = 2
        )
      
      ElecImportsExports[which((ElecImportsExports$Exports - ElecImportsExports$Imports) == max(ElecImportsExports$Exports - ElecImportsExports$Imports)),]$Exports
      ElecImportsExportsChart <-
        TimeChart(ElecImportsExportsChart,
                  ElecImportsExports,
                  plottitle,
                  sourcecaption,
                  ChartColours)
      
      ElecImportsExportsChart <- ElecImportsExportsChart +
        xlim(1999,2020) +
        ylim(0,(max(ElecImportsExports$Exports)*1.02))+
        labs(subtitle = paste("Scotland,", min(ElecImportsExports$Year), "-", max(ElecImportsExports$Year)))
      
      ElecImportsExportsChart
      ggsave(
        file,
        plot =  ElecImportsExportsChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
  
  WholesaleExportsData <- {
    WholesaleValue <- read_delim("Processed Data/Output/Exports/WholesaleValue.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    WholesaleValue$`Month Year` <- (as.yearmon(WholesaleValue$`Month Year`))
    
    DataFullYearStart <- ceiling_date(as.Date(min(WholesaleValue$`Month Year`)), unit = "year",week_start = getOption("lubridate.week.start", 1) )
    DataFullYearEnd <- floor_date(as.Date(max(WholesaleValue$`Month Year`)), unit = "year",week_start = getOption("lubridate.week.start", 1) ) - 1
    
    WholesaleValue <- WholesaleValue[which(as.Date(WholesaleValue$`Month Year`) >= DataFullYearStart),]
    
    WholesaleValue <- WholesaleValue[which(as.Date(WholesaleValue$`Month Year`) <= DataFullYearEnd),]
    
    WholesaleValue$Year <- as.numeric(substr(WholesaleValue$`Month Year`,5,9))
    
    WholesaleValue$WholesaleValue <- WholesaleValue$WholesaleValue/ 1000000
    
    WholesaleValue[2:6] %>%  group_by(`Year`) %>%  summarise_all(sum)
    
  }
  
  
  output$WholesaleExportsSubtitle <- renderText({
    
    paste("Scotland,", min(WholesaleExportsData$Year),"-", max(WholesaleExportsData$Year))
  })
  
  output$WholesaleExportsPlot <- renderPlotly  ({
    
    
    WholesaleExports <- WholesaleExportsData
    
    ### variables
    ChartColours <- c("#5d8be1", "#225ea8", "#41b6c4", "#8da0cb")
    sourcecaption = "Source: BEIS"
    plottitle = "Electricity imports and exports"
    
    WholesaleExports$Year <- paste0("01/01/", WholesaleExports$Year)
    
    WholesaleExports$Year <- dmy(WholesaleExports$Year)
    
    
    p <-  plot_ly(WholesaleExports,x = ~ Year ) %>% 
      add_trace(data = WholesaleExports,
                x = ~ Year,
                y = ~ WholesaleValue,
                name = "Electricity WholesaleValue",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Wholesale value of electricity exported: \u00A3",
                  format(round(WholesaleExports$WholesaleValue, digits = 0), big.mark = ","),
                  " million \nYear: ",
                  format(WholesaleExports$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(WholesaleExports[which(WholesaleExports$WholesaleValue > 0 | WholesaleExports$WholesaleValue < 0),], 1),
        x = ~ Year,
        y = ~ `WholesaleValue`,
        legendgroup = "1",
        name = "Electricity WholesaleValue",
        text = paste0(
          "Wholesale value of electricity exported: \u00A3",
          format(round(WholesaleExports[which(WholesaleExports$WholesaleValue > 0 | WholesaleExports$WholesaleValue < 0),][-1,]$WholesaleValue, digits = 0), big.mark = ","),
          " million \nYear: ",
          format(WholesaleExports[which(WholesaleExports$WholesaleValue > 0 | WholesaleExports$WholesaleValue < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
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
                     range = c(min(WholesaleExports$Year)-100, max(WholesaleExports$Year)+100)),
        yaxis = list(
          title = "\u00A3 Million",
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
  
  
  output$WholesaleExportsTable = renderDataTable({
    
    WholesaleExportsTableData <- WholesaleExportsData
    
    names(WholesaleExportsTableData) <- c("Year", "Exports to GB (GWh)", "Exports to NI (GWh)", "Electricity Price (\u00A3/GWh)", "Wholesale value of exports (\u00A3 million)")
    
    
    datatable(
      WholesaleExportsTableData[c(1,2,3,5)],
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
        title = "Wholesale value of electricity exports",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Wholesale value of electricity exports',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Wholesale value of electricity exports')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:4, 0)
  }) 
  
  
  output$WholesaleExports.png <- downloadHandler(
    filename = "WholesaleExports.png",
    content = function(file) {
      
      WholesaleExports <- WholesaleExportsData
      
      WholesaleExports$Year <- as.numeric(substr(WholesaleExports$Year,1,4))
      
      ### variables
      ChartColours <- c("#5d8be1", "#225ea8", "#41b6c4", "#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = "Wholesale value of electricity exports"
      
      #WholesaleExports$ExportsPercentage <- PercentLabel(WholesaleExports$Exports)
      
      
      WholesaleExportsChart <- WholesaleExports %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = WholesaleValue,
            label = percent(WholesaleValue)
          ),
          size = 1.5,
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = WholesaleValue,
            label = ifelse(Year == min(Year), paste0("\u00A3",format(round(WholesaleValue, digits = 0),big.mark=",")," million"), ""),
            hjust = 0.5,
            vjust = -1,
            
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = WholesaleValue,
            label = ifelse(Year == max(Year), paste0("\u00A3",format(round(WholesaleValue, digits = 0),big.mark=",")," million"), ""),
            vjust = 2,
            
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(WholesaleExports, 1),
          aes(
            x = Year,
            y = WholesaleValue,
            
            show_guide = FALSE
          ),
          size = 4,
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year) | Year == min(Year), Year, ""),
            vjust = 1.6,
            
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      WholesaleExportsChart <-
        LinePercentChart(WholesaleExportsChart,
                         WholesaleExports,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      WholesaleExportsChart <- WholesaleExportsChart +
        labs(subtitle = paste("Scotland,", min(WholesaleExports$Year), "-", max(WholesaleExports$Year))) +
        xlim(2010,2020) + 
        ylim(-10,880)
      
      WholesaleExportsChart
      ggsave(
        file,
        plot =  WholesaleExportsChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
  
  
  

