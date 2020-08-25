require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



SmartMetersOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Current installations",
    fluidRow(column(8,
                    h3("Smart meter installations", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('SmartMetersSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('SmartMeters.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("SmartMetersPlot")),
    imageOutput(ns("SmartMetersPlot"), height = "500px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Proportion time series",
             fluidRow(column(8,
                             h3("Proportion of installed meters which are smart meters", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('SmartMetersTimeSeriesSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('SmartMetersTimeSeries.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("SmartMetersPlot")),
             plotlyOutput(ns("SmartMetersTimeSeriesPlot"))%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
    column(10, h3("Data", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("SmartMetersTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("ElectralinkMeters", "BEISLSOA"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("ElectralinkMeters"),
        SourceLookup("BEISLSOA")
        
      )
    )
  )
}




###### Server ######
SmartMeters <- function(input, output, session) {

  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("SmartMeters.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$SmartMetersSubtitle <- renderText({
    
    paste("Scotland")
  })
  
  output$SmartMetersPlot <- renderImage({
    
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
   
     writePNG(readPNG("Structure/5 - Consumers/SmartMetersOutput.png"),outfile) 
    
    # Generate a png
    
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$SmartMetersTable = renderDataTable({
    
    SmartMeters <- read_delim("Processed Data/Output/Electricity Meters/SmartMeters.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    names(SmartMeters) <- c("Date","Total Scotland - Installations (Cumulative)", "Total Scotland - Proportion of smart meters", "North Scotland - Installations (Cumulative)", "North Scotland - Proportion of smart meters", "South Scotland - Installations (Cumulative)", "South Scotland - Proportion of smart meters")

    SmartMeters$Date <- format(SmartMeters$Date, "%b %Y")
    
    datatable(
      SmartMeters[nrow(SmartMeters):1,],
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Smart meter installations",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Smart meter installations',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Smart meter installations')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(3,5,7), 1) %>% 
      formatRound(c(2,4,6), 0)
  })
  
      SmartMeters <- read_delim("Processed Data/Output/Electricity Meters/SmartMeters.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(SmartMeters)[1] <- "Quarter"#
    
    SmartMeters <- SmartMeters[complete.cases(SmartMeters),]
    
    SmartMeters$Quarter <- as.Date(as.numeric(SmartMeters$Quarter), origin = "1899-12-30")
    
    SmartMeters$Quarter <- as.character(as.yearqtr(SmartMeters$Quarter))
    
  output$SmartMetersTimeSeriesTable = renderDataTable({
    

    
    
    datatable(
      SmartMeters,
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
        title = "Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of households not on the gas grid by local authority (estimates), Scotland, 2017')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:9, 1)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/SmartMeters.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("SmartMetersTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("SmartMetersTimeSeriesTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$SmartMeters.png <- downloadHandler(
    filename = "SmartMeters.png",
    content = function(file) {
      writePNG(readPNG("Structure/5 - Consumers/SmartMetersChart.png"), file) 
    }
  )
  
  output$SmartMetersTimeSeries.png <- downloadHandler(
    filename = "SmartMetersTimeSeries.png",
    content = function(file) {
      
      SmartMeters <- read_delim("Processed Data/Output/Electricity Meters/SmartMeters.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
      
      names(SmartMeters) <- c("Year","Total Scotland - Proportion of smart meters","Total",  "North Scotland - Proportion of smart meters","NorthScotland", "South Scotland - Proportion of smart meters", "SouthScotland" )
      
      
      
      ChartColours <- c("#68c3ea", "#FF8500")
      LineColours <-
        c( "#68c3ea",   "#7bccc4","#0868ac","#43a2ca"
        )
      
      
      ### variables
      ChartColours <- c("#68c3ea", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: Electralink, BEIS"
      plottitle = "Proportion of installed meters which are\nsmart meters"
      
      #SmartMeters$TotalPercentage <- PercentLabel(SmartMeters$Total)
      
      
      SmartMetersChart <- SmartMeters %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = Total,
            label = percent(Total)
          ),
          size = 1.5,
          family = "Century Gothic",
          colour = LineColours[2]
        ) +
        geom_text(
          aes(
            x = Year - 1,
            y = Total,
            label = ifelse(Year == min(Year), percent(`Total`, 0.1) , ""),
            hjust =1.1,
            
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.2,
            y = Total,
            label = ifelse(Year == max(Year), percent(`Total`, 0.1) , ""),
            hjust = -0.2,
            
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(SmartMeters, 1),
          aes(
            x = Year,
            y = Total,
            
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(Total),
            label = "Total\nScotland",
            hjust = 0.5,
            vjust = 1.5,
            
            fontface = 2
          ),
          colour = LineColours[2],
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `NorthScotland`,
            
            label = paste0(`NorthScotland` * 100, "%")
          ),
          linetype = "dashed",
          size = 1.5,
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1,
            y = `NorthScotland`,
            label = ifelse(Year == min(Year), percent(`NorthScotland`, 0.1) , ""),
            hjust = 1.1,
            
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.2,
            y = `NorthScotland`,
            label = ifelse(Year == max(Year), percent(`NorthScotland`, 0.1) , ""),
            hjust = -.2,
            
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(SmartMeters, 1),
          aes(
            x = Year,
            y = `NorthScotland`,
            show_guide = FALSE
          ),
          size = 4,
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(`NorthScotland`),
            label = "North\nScotland",
            hjust = 0.5,
            vjust = 1.5,
            
            fontface = 2
          ),
          colour = LineColours[3],
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `SouthScotland`,
            label = paste0(`SouthScotland` * 100, "%")
          ),
          linetype = "dashed",
          size = 1.5,
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1,
            y = `SouthScotland`,
            label = ifelse(Year == min(Year), percent(`SouthScotland`, 0.1) , ""),
            hjust = 1.1,
            fontface = 2
          ),
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.2,
            y = `SouthScotland`,
            label = ifelse(Year == max(Year), percent(`SouthScotland`, 0.1) , ""),
            hjust = -0.2,
            fontface = 2
          ),
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(SmartMeters, 1),
          aes(
            x = Year,
            y = `SouthScotland`,
            show_guide = FALSE
          ),
          colour = LineColours[4],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(`SouthScotland`),
            label = "South\nScotland",
            hjust = 0.5,
            vjust = -2,
            fontface = 2
          ),
          colour = LineColours[4],
          family = "Century Gothic"
        ) +
        
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year) |
                             Year == min(Year), format(Year, "%b %Y"), ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = LineColours[1],
          family = "Century Gothic"
        )
      
      
      SmartMetersChart <-
        DailyChart(SmartMetersChart,
                   SmartMeters,
                   plottitle,
                   sourcecaption,
                   LineColours)
      
      length <- max(SmartMeters$Year)-min(SmartMeters$Year)
      
      SmartMetersChart <- SmartMetersChart +
        coord_cartesian(xlim = c(min(SmartMeters$Year) - length*0.06, max(SmartMeters$Year)+ length*0.08)) +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )
      
      SmartMetersChart
      
      ggsave(
        file,
        plot =  SmartMetersChart,
        width = 16,
        height = 16,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  
  output$SmartMetersTimeSeriesSubtitle <- renderText({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
    
    print("Energy daily demand")
    SmartMeters <- read_delim("Processed Data/Output/Electricity Meters/SmartMeters.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(SmartMeters) <- c("Date","Total Scotland - Installations (Cumulative)", "Total Scotland - Proportion of smart meters", "North Scotland - Installations (Cumulative)", "North Scotland - Proportion of smart meters", "South Scotland - Installations (Cumulative)", "South Scotland - Proportion of smart meters")
    
    paste("Scotland,", format(min(SmartMeters$Date),"%B %Y"),"-", format(max(SmartMeters$Date),"%B %Y"))
  })
  
  output$SmartMetersTimeSeriesPlot <- renderPlotly  ({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
    
    SmartMeters <- read_delim("Processed Data/Output/Electricity Meters/SmartMeters.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(SmartMeters) <- c("Date","Total Scotland - Installations (Cumulative)", "Total Scotland - Proportion of smart meters", "North Scotland - Installations (Cumulative)", "North Scotland - Proportion of smart meters", "South Scotland - Installations (Cumulative)", "South Scotland - Proportion of smart meters")
    
    
    
    ChartColours <- c("#68c3ea", "#FF8500")
    LineColours <-
      c(    "#7bccc4","#0868ac","#43a2ca"
      )
    
    p <-  plot_ly(SmartMeters, x = ~ Date ) %>%  
      add_trace(y = ~ `Total Scotland - Proportion of smart meters`, 
                name = "Total Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Total Scotland - Proportion: ",
                  percent(SmartMeters$`Total Scotland - Proportion of smart meters`, 0.1),
                  "\nDate: ",
                  format(SmartMeters$Date, "%B %Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[1], dash = "none")
      ) %>% 
      add_trace(y = ~ `North Scotland - Proportion of smart meters`,
                name = "North Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "North Scotland - Proportion: ",
                  percent(SmartMeters$`North Scotland - Proportion of smart meters`, 0.1),
                  "\nDate: ",
                  format(SmartMeters$Date, "%B %Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[2], dash = "dash")
      ) %>% 
      add_trace(y = ~ `South Scotland - Proportion of smart meters`, 
                name = "South Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "South Scotland - Proportion: ",
                  percent(SmartMeters$`South Scotland - Proportion of smart meters`, 0.1),
                  "\nDate: ",
                  format(SmartMeters$Date, "%B %Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = LineColours[3], dash = "dash")
      ) %>% 
      add_trace(
        data = tail(SmartMeters[which(SmartMeters$`Total Scotland - Proportion of smart meters` != 0),], 1),
        x = ~ Date,
        y = ~ `Total Scotland - Proportion of smart meters`,
        name = "Total Scotland",
        legendgroup = "1",
        text = paste0(
          "Total Scotland - Proportion: ",
          percent(tail(SmartMeters[which(SmartMeters$`Total Scotland - Proportion of smart meters` != 0),], 1)$`Total Scotland - Proportion of smart meters`, 0.1),
          "\nDate: ",
          format(tail(SmartMeters[which(SmartMeters$`Total Scotland - Proportion of smart meters` != 0),], 1)$Date, "%B %Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[1])
      ) %>%
      add_trace(
        data = tail(SmartMeters[which(SmartMeters$`North Scotland - Proportion of smart meters` != 0),], 1),
        x = ~ Date,
        y = ~ `North Scotland - Proportion of smart meters`,
        name = "North Scotland",
        legendgroup = "2",
        text = paste0(
          "North Scotland - Proportion: ",
          percent(tail(SmartMeters[which(SmartMeters$`North Scotland - Proportion of smart meters` != 0),], 1)$`North Scotland - Proportion of smart meters`, 0.1),
          "\nDate: ",
          format(tail(SmartMeters[which(SmartMeters$`North Scotland - Proportion of smart meters` != 0),], 1)$Date, "%B %Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[2])
      ) %>% 
      add_trace(
        data = tail(SmartMeters[which(SmartMeters$`South Scotland - Proportion of smart meters` != 0),], 1),
        x = ~ Date,
        y = ~ `South Scotland - Proportion of smart meters`,
        name = "South Scotland",
        legendgroup = "3",
        text = paste0(
          "South Scotland - Proportion: ",
          percent(tail(SmartMeters[which(SmartMeters$`South Scotland - Proportion of smart meters` != 0),], 1)$`South Scotland - Proportion of smart meters`, 0.1),
          "\nDate: ",
          format(tail(SmartMeters[which(SmartMeters$`South Scotland - Proportion of smart meters` != 0),], 1)$Date, "%B %Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = LineColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#68c3ea"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE),
        yaxis = list(
          title = "",
          tickformat = "%",
          tickprefix = "",
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
  
  
}
