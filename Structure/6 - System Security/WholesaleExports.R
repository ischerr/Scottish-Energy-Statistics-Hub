require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

WholesaleExportsOutput <- function(id) {
  ns <- NS(id)
  tagList(
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
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
      column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
      column(10, h3("Data - Wholesale value of electricity exports", style = "color: #5d8be1;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("WholesaleExportsTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
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
WholesaleExports <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("WholesaleExports.R")
  
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/6 - System Security/WholesaleExports.txt")[2])
                     
                   )))
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
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
      formatRound(2:8, 0)
  }) 
  
  observeEvent(input$ToggleTable3, {
    toggle("WholesaleExportsTable")
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
