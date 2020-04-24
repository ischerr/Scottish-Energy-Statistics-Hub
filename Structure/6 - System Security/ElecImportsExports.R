require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ElecImportsExportsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Electricity imports and exports", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('ElecImportsExportsSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecImportsExports.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("ElecImportsExportsPlot")),
    plotlyOutput(ns("ElecImportsExportsPlot"))%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
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
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
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
 
  observeEvent(input$ToggleTable, {
    toggle("ElecImportsExportsTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ElecImportsExportsQuarterTable")
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
}
