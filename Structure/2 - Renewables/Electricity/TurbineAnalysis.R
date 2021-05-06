require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



TurbineAnalysisOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Sites",
               fluidRow(column(8,
                               h3("Number of wind generating sites by planning status", style = "color: #39ab2c;  font-weight:bold"),
                               h4(textOutput(ns('SitesTimeSeriesSubtitle')), style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('SitesTimeSeries.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               plotlyOutput(ns("SitesTimeSeriesPlot"))%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Turbines",
             fluidRow(column(8,
                             h3("Number of wind turbines by planning status", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('TurbinesTimeSeriesSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('TurbinesTimeSeries.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("EUWindPlot")),
             plotlyOutput(ns("TurbinesTimeSeriesPlot"))%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Capacity",
             fluidRow(column(8,
                             h3("Total wind capacity by planning status", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('CapacityTimeSeriesSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('CapacityTimeSeries.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("EUHydroPlot")),
             plotlyOutput(ns("CapacityTimeSeriesPlot"))%>% withSpinner(color="#39ab2c"),
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
      tabPanel("Summary",
               fluidRow(
                 column(10, uiOutput(ns("TurbineDataDate"))),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Tables", style = "float:right; "))
               ),
               fluidRow(
                 column(10, h4("All Wind", style = "color: #39ab2c;  font-weight:bold"))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("AllWindTable"))%>% withSpinner(color="#39ab2c"))),
               fluidRow(
                 column(10, h4("Onshore Wind", style = "color: #39ab2c;  font-weight:bold"))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("OnshoreWindTable"))%>% withSpinner(color="#39ab2c"))),
               fluidRow(
                 column(10, h4("Offshore Wind", style = "color: #39ab2c;  font-weight:bold"))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("OffshoreWindTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Sites",
               fluidRow(
                 column(10, h3("Data - Number of wind generating sites by planning status", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("SitesTimeSeriesTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Turbines",
             fluidRow(
               column(10, h3("Data - Number of wind turbines by planning status", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("TurbinesTimeSeriesTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Capacity",
             fluidRow(
               column(10, h3("Data - Total wind capacity by planning status", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("CapacityTimeSeriesTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))

    ),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("BEISREPD"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("BEISREPD"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(
        8,
        align = "right",
        SourceLookup("BEISREPD")
        
      )
    )
  )
}




###### Server ######
TurbineAnalysis <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecFuel.R")
  ###### Renewable Energy ###### ######
  
  
  output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/TurbineAnalysis.txt")[2])
                    
                  )))
 })

 output$TurbineDataDate <- renderUI({
  tagList(
    h3(paste("Data - Wind summary,", format(max(SitesTimeSeries$Month), "%B %Y")), style = "color: #39ab2c;  font-weight:bold")
  )
 }) 

  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  SitesTimeSeries <- {
    
    Data <- list.files(path="Processed Data/Output/Turbine Analysis/Time Series/Sites/", full.names = TRUE) %>% 
      lapply(read_csv) %>% 
      bind_rows 
  
    Data$Month <- dmy(paste0("01-",Data$Month)) 
    
    Data <- Data[rev(order(Data$Month)),]
   
   Data[c(1,2,3,5,4)]
    
  }
  
  observeEvent(input$ToggleTable1, {
    toggle("SitesTimeSeriesTable")
  })

  output$SitesTimeSeriesTable = renderDataTable({
    
    SitesTimeSeries$Month <- format(SitesTimeSeries$Month, "%b-%y")
    
    datatable(
      SitesTimeSeries,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Number of wind generating sites by planning status",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Number of wind generating sites by planning status',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Number of wind generating sites by planning status')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    )  %>% 
      formatRound(2:5, 0)
    
    
    
  })
  
  output$SitesTimeSeriesPlot = renderPlotly({
    ChartColours <- c("#0868ac", "#4eb3d3", "#a8ddb5", "#39ab2c")
    
    
    
    p <- plot_ly(
      SitesTimeSeries,
      x = ~ Month,
      y = ~ `Application Submitted`,
      legendgroup = "1",
      name = "Application Submitted",
      type = 'scatter',
      mode = 'lines',
      text = paste0("Sites: ",SitesTimeSeries$`Application Submitted`,"\nQuarter: ", as.yearqtr(SitesTimeSeries$Month)
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1])
    )  %>%
      add_trace(
        y = ~ `Awaiting Construction`,
        legendgroup = "2",
        name = "Awaiting Construction",
        type = 'scatter',
        mode = 'lines',
        text = paste0("Sites: ",SitesTimeSeries$`Awaiting Construction`,"\nQuarter: ", as.yearqtr(SitesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[2])
      ) %>% 
      add_trace(
        y = ~ `Under Construction`,
        legendgroup = "3",
        name = "Under Construction",
        type = 'scatter',
        mode = 'lines',
        text = paste0("Sites: ",SitesTimeSeries$`Under Construction`,"\nQuarter: ", as.yearqtr(SitesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[3])
      ) %>% 
      add_trace(
        y = ~ `Operational`,
        legendgroup = "4",
        name = "Operational",
        type = 'scatter',
        mode = 'lines',
        text = paste0("Sites: ",SitesTimeSeries$`Operational`,"\nQuarter: ", as.yearqtr(SitesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[4])
      ) %>% 
      add_trace(
        data = SitesTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Application Submitted`,
        name = "Application Submitted",
        legendgroup = "1",
        text = paste0("Sites: ",SitesTimeSeries$`Application Submitted`,"\nQuarter: ", as.yearqtr(SitesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[1])
      ) %>%
      add_trace(
        data = SitesTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Awaiting Construction`,
        name = "Awaiting Construction",
        legendgroup = "2",
        text = paste0("Sites: ",SitesTimeSeries$`Awaiting Construction`,"\nQuarter: ", as.yearqtr(SitesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[2])
      ) %>%
      add_trace(
        data = SitesTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Under Construction`,
        name = "Under Construction",
        legendgroup = "3",
        text = paste0("Sites: ",SitesTimeSeries$`Under Construction`,"\nQuarter: ", as.yearqtr(SitesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[3])
      ) %>%
      add_trace(
        data = SitesTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Operational`,
        name = "Operational",
        legendgroup = "4",
        text = paste0("Sites: ",SitesTimeSeries$`Operational`,"\nQuarter: ", as.yearqtr(SitesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[4])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(SitesTimeSeries$Month)-100, max(SitesTimeSeries$Month)+100)),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[4],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F)
    p
  })
  
  output$SitesTimeSeries.png <- downloadHandler(
    filename = "SitesTimeSeries.png",
    content = function(file) {
    ### variables
    ChartColours <- c( "#39ab2c", "#a8ddb5", "#4eb3d3", "#0868ac")
    sourcecaption = "Source: BEIS"
    plottitle = "Number of wind generating sites by planning status"
    
    #SitesTimeSeries$`Application Submitted`Percentage <- PercentLabel(SitesTimeSeries$`Application Submitted`)
    
    SitesTimeSeries$Year <- ymd(SitesTimeSeries$Month)
    
    SitesTimeSeries$Quarter <- as.yearqtr(SitesTimeSeries$Month)
    
    Length <- max(SitesTimeSeries$Month) - min(SitesTimeSeries$Month)
    
    SitesTimeSeries<- SitesTimeSeries[seq(dim(SitesTimeSeries)[1],1),]
    
    SitesTimeSeriesChart <- SitesTimeSeries %>%
      ggplot(aes(x = Month), family = "Century Gothic") +
      
      geom_line(
        aes(y = `Application Submitted`,
            label = `Application Submitted`),
        colour = ChartColours[4],
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Month - .5,
          y = `Application Submitted`,
          label = ifelse(Month == min(Month), `Application Submitted`, ""),
          hjust = 1.3,
          vjust = .3,
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Month +Length*0.02,
          y = `Application Submitted`,
          label = ifelse(Month == max(Month), `Application Submitted`, ""),
          hjust = 0,
          
          fontface = 2
        ),
        colour = ChartColours[4],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(SitesTimeSeries, 1),
        aes(x = Month,
            y = `Application Submitted`,
            show_guide = FALSE),
        colour = ChartColours[4],
        size = 4,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(SitesTimeSeries$Month),
        y = mean(SitesTimeSeries$`Application Submitted`),
        label = "Application submitted",
        hjust = 0.5,
        vjust = -.3,
        colour = ChartColours[4],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = `Awaiting Construction`,
            label = paste0(`Awaiting Construction` * 100, "%")),
        colour = ChartColours[3],
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Month - .5,
          y = `Awaiting Construction`,
          label = ifelse(Month == min(Month), `Awaiting Construction`, ""),
          hjust = 1.3,
          vjust = 1,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Month +Length*0.02,
          y = `Awaiting Construction`,
          label = ifelse(Month == max(Month), `Awaiting Construction`, ""),
          hjust = 0,
          fontface = 2
        ),
        colour = ChartColours[3],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(SitesTimeSeries, 1),
        aes(x = Month,
            y = `Awaiting Construction`,
            show_guide = FALSE),
        colour = ChartColours[3],
        size = 4,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(SitesTimeSeries$Month),
        y = mean(SitesTimeSeries$`Awaiting Construction`),
        label = "Awaiting construction",
        hjust = 0.5,
        vjust = -.5,
        colour = ChartColours[3],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = `Under Construction`,
            label = paste0(`Under Construction` * 100, "%")),
        colour = ChartColours[2],
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Month - .5,
          y = `Under Construction`,
          label = ifelse(Month == min(Month), `Under Construction`, ""),
          hjust = 1.3,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Month +Length*0.02,
          y = `Under Construction`,
          label = ifelse(Month == max(Month), `Under Construction`, ""),
          hjust = 0,
          fontface = 2
        ),
        colour = ChartColours[2],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(SitesTimeSeries, 1),
        aes(x = Month,
            y = `Under Construction`,
            
            show_guide = FALSE),
        size = 4,
        colour = ChartColours[2],
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(SitesTimeSeries$Month),
        y = mean(SitesTimeSeries$`Under Construction`),
        label = "Under Construction",
        hjust = 0.5,
        vjust = 1.7,
        colour = ChartColours[2],
        fontface = 2,
        family = "Century Gothic"
      ) +
      geom_line(
        aes(y = `Operational`,
            label = `Operational`),
        colour = ChartColours[1],
        size = 1.5,
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Month - .5,
          y = `Operational`,
          label = ifelse(Month == min(Month), `Operational`, ""),
          hjust = 1.3,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          x = Month+Length*0.02,
          y = `Operational`,
          label = ifelse(Month == max(Month), `Operational`, ""),
          hjust= 0,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      ) +
      geom_point(
        data = tail(SitesTimeSeries, 1),
        aes(x = Month,
            y = `Operational`,
            show_guide = FALSE),
        colour = ChartColours[1],
        size = 4,
        family = "Century Gothic"
      ) +
      annotate(
        "text",
        x = mean(SitesTimeSeries$Month),
        y = mean(SitesTimeSeries$`Operational`),
        label = "Operational",
        hjust = 0.5,
        vjust = 1.2,
        colour = ChartColours[1],
        fontface = 2,
        family = "Century Gothic"
      )+
      geom_text(
        aes(
          x = Month,
          y = 0,
          label = ifelse(
            Month == max(Month) |
              Month == min(Month),
            format(Quarter, "%Y Q%q"),
            ""
          ),
          hjust = 0.5,
          vjust = 1.5,
          fontface = 2
        ),
        colour = ChartColours[1],
        family = "Century Gothic"
      )
    
    SitesTimeSeriesChart
    
    SitesTimeSeriesChart <-
      DailyChart(SitesTimeSeriesChart,
                 SitesTimeSeries,
                 plottitle,
                 sourcecaption,
                 ChartColours)
    
    SitesTimeSeriesChart <- SitesTimeSeriesChart +
      coord_cartesian(xlim = c(min(SitesTimeSeries$Year)-(Length*0.03), max(SitesTimeSeries$Year)+(Length*0.03))) +
      ylim(
        -(max(SitesTimeSeries[2:5])*0.03),
        max(SitesTimeSeries[2:5])*1.01
      )+     
      geom_hline(
        yintercept = 0,
        color = "grey",
        alpha = 0.7,
        linetype = 2
      ) 
    
    SitesTimeSeriesChart
    
    ggsave(
      file,
      plot =  SitesTimeSeriesChart,
      width = 18,
      height = 10,
      units = "cm",
      dpi = 300
    )
  })
  
   output$SitesTimeSeriesSubtitle <- renderText({
    
    paste("Scotland,", format(min(SitesTimeSeries$Month), "%B %Y"),"-", format(max(SitesTimeSeries$Month), "%B %Y"))
    
  })
   
  TurbinesTimeSeries <- {
    
    Data <- list.files(path="Processed Data/Output/Turbine Analysis/Time Series/Turbines/", full.names = TRUE) %>% 
      lapply(read_csv) %>% 
      bind_rows 
    
    Data$Month <- dmy(paste0("01-",Data$Month)) 
    
    Data <- Data[rev(order(Data$Month)),]
    
    
    
    Data[c(1,2,3,5,4)]
    
  }
  
  
 
  
  observeEvent(input$ToggleTable2, {
    toggle("TurbinesTimeSeriesTable")
  })
  
  output$TurbinesTimeSeriesTable = renderDataTable({
    
    TurbinesTimeSeries$Month <- format(TurbinesTimeSeries$Month, "%b-%y")
    
    datatable(
      TurbinesTimeSeries,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Number of wind turbines by planning status",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Number of wind turbines by planning status',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Number of wind turbines by planning status')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:5, 0)
    
    
    
  })
  
  output$TurbinesTimeSeriesPlot = renderPlotly({
    ChartColours <- c("#0868ac", "#4eb3d3", "#a8ddb5", "#39ab2c")
    
    
    
    p <- plot_ly(
      TurbinesTimeSeries,
      x = ~ Month,
      y = ~ `Application Submitted`,
      legendgroup = "1",
      name = "Application Submitted",
      type = 'scatter',
      mode = 'lines',
      text = paste0("Turbines: ",TurbinesTimeSeries$`Application Submitted`,"\nQuarter: ", as.yearqtr(TurbinesTimeSeries$Month)
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1])
    )  %>%
      add_trace(
        y = ~ `Awaiting Construction`,
        legendgroup = "2",
        name = "Awaiting Construction",
        type = 'scatter',
        mode = 'lines',
        text = paste0("Turbines: ",TurbinesTimeSeries$`Awaiting Construction`,"\nQuarter: ", as.yearqtr(TurbinesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[2])
      ) %>% 
      add_trace(
        y = ~ `Under Construction`,
        legendgroup = "3",
        name = "Under Construction",
        type = 'scatter',
        mode = 'lines',
        text = paste0("Turbines: ",TurbinesTimeSeries$`Under Construction`,"\nQuarter: ", as.yearqtr(TurbinesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[3])
      ) %>% 
      add_trace(
        y = ~ `Operational`,
        legendgroup = "4",
        name = "Operational",
        type = 'scatter',
        mode = 'lines',
        text = paste0("Turbines: ",TurbinesTimeSeries$`Operational`,"\nQuarter: ", as.yearqtr(TurbinesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[4])
      ) %>% 
      add_trace(
        data = TurbinesTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Application Submitted`,
        name = "Application Submitted",
        legendgroup = "1",
        text = paste0("Turbines: ",TurbinesTimeSeries$`Application Submitted`,"\nQuarter: ", as.yearqtr(TurbinesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[1])
      ) %>%
      add_trace(
        data = TurbinesTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Awaiting Construction`,
        name = "Awaiting Construction",
        legendgroup = "2",
        text = paste0("Turbines: ",TurbinesTimeSeries$`Awaiting Construction`,"\nQuarter: ", as.yearqtr(TurbinesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[2])
      ) %>%
      add_trace(
        data = TurbinesTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Under Construction`,
        name = "Under Construction",
        legendgroup = "3",
        text = paste0("Turbines: ",TurbinesTimeSeries$`Under Construction`,"\nQuarter: ", as.yearqtr(TurbinesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[3])
      ) %>%
      add_trace(
        data = TurbinesTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Operational`,
        name = "Operational",
        legendgroup = "4",
        text = paste0("Turbines: ",TurbinesTimeSeries$`Operational`,"\nQuarter: ", as.yearqtr(TurbinesTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[4])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(TurbinesTimeSeries$Month)-100, max(TurbinesTimeSeries$Month)+100)),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[4],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F)
    p
  })
  
  output$TurbinesTimeSeries.png <- downloadHandler(
    filename = "TurbinesTimeSeries.png",
    content = function(file) {
      ### variables
      ChartColours <- c( "#39ab2c", "#a8ddb5", "#4eb3d3", "#0868ac")
      sourcecaption = "Source: BEIS"
      plottitle = "Number of wind turbines by planning status"
      
      #TurbinesTimeSeries$`Application Submitted`Percentage <- PercentLabel(TurbinesTimeSeries$`Application Submitted`)
      
      TurbinesTimeSeries$Year <- ymd(TurbinesTimeSeries$Month)
      
      TurbinesTimeSeries$Quarter <- as.yearqtr(TurbinesTimeSeries$Month)
      
      Length <- max(TurbinesTimeSeries$Month) - min(TurbinesTimeSeries$Month)
      
      TurbinesTimeSeries<- TurbinesTimeSeries[seq(dim(TurbinesTimeSeries)[1],1),]
      
      TurbinesTimeSeriesChart <- TurbinesTimeSeries %>%
        ggplot(aes(x = Month), family = "Century Gothic") +
        
        geom_line(
          aes(y = `Application Submitted`,
              label = `Application Submitted`),
          colour = ChartColours[4],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month - .5,
            y = `Application Submitted`,
            label = ifelse(Month == min(Month), format(`Application Submitted`, big.mark = ",", trim = TRUE), ""),
            hjust = 1.3,
            vjust = 1,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month +Length*0.02,
            y = `Application Submitted`,
            label = ifelse(Month == max(Month), format(`Application Submitted`, big.mark = ",", trim = TRUE), ""),
            hjust = 0,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(TurbinesTimeSeries, 1),
          aes(x = Month,
              y = `Application Submitted`,
              show_guide = FALSE),
          colour = ChartColours[4],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(TurbinesTimeSeries$Month),
          y = mean(TurbinesTimeSeries$`Application Submitted`),
          label = "Application submitted",
          hjust = 0.5,
          vjust = -.3,
          colour = ChartColours[4],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Awaiting Construction`,
              label = paste0(`Awaiting Construction` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month - .5,
            y = `Awaiting Construction`,
            label = ifelse(Month == min(Month), format(`Awaiting Construction`, big.mark = ",", trim = TRUE), ""),
            hjust = 1.3,
            vjust = 0,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month +Length*0.02,
            y = `Awaiting Construction`,
            label = ifelse(Month == max(Month), format(`Awaiting Construction`, big.mark = ",", trim = TRUE), ""),
            hjust = 0,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(TurbinesTimeSeries, 1),
          aes(x = Month,
              y = `Awaiting Construction`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(TurbinesTimeSeries$Month),
          y = mean(TurbinesTimeSeries$`Awaiting Construction`),
          label = "Awaiting construction",
          hjust = 0.5,
          vjust = -1.1,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Under Construction`,
              label = paste0(`Under Construction` * 100, "%")),
          colour = ChartColours[2],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month - .5,
            y = `Under Construction`,
            label = ifelse(Month == min(Month), format(`Under Construction`, big.mark = ",", trim = TRUE), ""),
            hjust = 1.3,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month +Length*0.02,
            y = `Under Construction`,
            label = ifelse(Month == max(Month), format(`Under Construction`, big.mark = ",", trim = TRUE), ""),
            hjust = 0,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(TurbinesTimeSeries, 1),
          aes(x = Month,
              y = `Under Construction`,
              
              show_guide = FALSE),
          size = 4,
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(TurbinesTimeSeries$Month),
          y = mean(TurbinesTimeSeries$`Under Construction`),
          label = "Under Construction",
          hjust = 0.5,
          vjust = 2,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Operational`,
              label = `Operational`),
          colour = ChartColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month - .5,
            y = `Operational`,
            label = ifelse(Month == min(Month), format(`Operational`, big.mark = ",", trim = TRUE), ""),
            hjust = 1.3,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month+Length*0.02,
            y = `Operational`,
            label = ifelse(Month == max(Month), format(`Operational`, big.mark = ",", trim = TRUE), ""),
            hjust= 0,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(TurbinesTimeSeries, 1),
          aes(x = Month,
              y = `Operational`,
              show_guide = FALSE),
          colour = ChartColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(TurbinesTimeSeries$Month),
          y = mean(TurbinesTimeSeries$`Operational`),
          label = "Operational",
          hjust = 0.5,
          vjust = 1.3,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Month,
            y = 0,
            label = ifelse(
              Month == max(Month) |
                Month == min(Month),
              format(Quarter, "%Y Q%q"),
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      TurbinesTimeSeriesChart
      
      TurbinesTimeSeriesChart <-
        DailyChart(TurbinesTimeSeriesChart,
                   TurbinesTimeSeries,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      TurbinesTimeSeriesChart <- TurbinesTimeSeriesChart +
        coord_cartesian(xlim = c(min(TurbinesTimeSeries$Year)-(Length*0.04), max(TurbinesTimeSeries$Year)+(Length*0.04))) +
        ylim(
          -(max(TurbinesTimeSeries[2:5])*0.03),
          max(TurbinesTimeSeries[2:5])*1.01
        )+     
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        ) 
      
      TurbinesTimeSeriesChart
      
      ggsave(
        file,
        plot =  TurbinesTimeSeriesChart,
        width = 18,
        height = 10,
        units = "cm",
        dpi = 300
      )
    })
  
  output$TurbinesTimeSeriesSubtitle <- renderText({
    
    paste("Scotland,", format(min(TurbinesTimeSeries$Month), "%B %Y"),"-", format(max(TurbinesTimeSeries$Month), "%B %Y"))
    
  })
  
  CapacityTimeSeries <- {
    
    Data <- list.files(path="Processed Data/Output/Turbine Analysis/Time Series/Capacity/", full.names = TRUE) %>% 
      lapply(read_csv) %>% 
      bind_rows 
    
    Data$Month <- dmy(paste0("01-",Data$Month)) 
    
    Data <- Data[rev(order(Data$Month)),]
    
    
    
    Data[c(1,2,3,5,4)]
    
  }
  
  observeEvent(input$ToggleTable3, {
    toggle("CapacityTimeSeriesTable")
  })
  
  output$CapacityTimeSeriesTable = renderDataTable({
    
    CapacityTimeSeries$Month <- format(CapacityTimeSeries$Month, "%b-%y")
    
    datatable(
      CapacityTimeSeries,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Wind capacity (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Wind capacity (MW)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Total wind capacity by planning status (MW)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:5, 0)
    
    
    
  })
  
  output$CapacityTimeSeriesPlot = renderPlotly({
    ChartColours <- c("#0868ac", "#4eb3d3", "#a8ddb5", "#39ab2c")
    
    
    
    p <- plot_ly(
      CapacityTimeSeries,
      x = ~ Month,
      y = ~ `Application Submitted`,
      legendgroup = "1",
      name = "Application Submitted",
      type = 'scatter',
      mode = 'lines',
      text = paste0("Capacity: ",CapacityTimeSeries$`Application Submitted`,"\nQuarter: ", as.yearqtr(CapacityTimeSeries$Month)
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1])
    )  %>%
      add_trace(
        y = ~ `Awaiting Construction`,
        legendgroup = "2",
        name = "Awaiting Construction",
        type = 'scatter',
        mode = 'lines',
        text = paste0("Capacity: ",CapacityTimeSeries$`Awaiting Construction`,"\nQuarter: ", as.yearqtr(CapacityTimeSeries$Month)
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[2])
      ) %>% 
      add_trace(
        y = ~ `Under Construction`,
        legendgroup = "3",
        name = "Under Construction",
        type = 'scatter',
        mode = 'lines',
        text = paste0("Capacity: ",CapacityTimeSeries$`Under Construction`,"\nQuarter: ", as.yearqtr(CapacityTimeSeries$Month)
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[3])
      ) %>% 
      add_trace(
        y = ~ `Operational`,
        legendgroup = "4",
        name = "Operational",
        type = 'scatter',
        mode = 'lines',
        text = paste0("Capacity: ",CapacityTimeSeries$`Operational`,"\nQuarter: ", as.yearqtr(CapacityTimeSeries$Month)
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[4])
      ) %>% 
      add_trace(
        data = CapacityTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Application Submitted`,
        name = "Application Submitted",
        legendgroup = "1",
        text = paste0("Capacity: ",CapacityTimeSeries$`Application Submitted`,"\nQuarter: ", as.yearqtr(CapacityTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[1])
      ) %>%
      add_trace(
        data = CapacityTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Awaiting Construction`,
        name = "Awaiting Construction",
        legendgroup = "2",
        text = paste0("Capacity: ",CapacityTimeSeries$`Awaiting Construction`,"\nQuarter: ", as.yearqtr(CapacityTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[2])
      ) %>%
      add_trace(
        data = CapacityTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Under Construction`,
        name = "Under Construction",
        legendgroup = "3",
        text = paste0("Capacity: ",CapacityTimeSeries$`Under Construction`,"\nQuarter: ", as.yearqtr(CapacityTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[3])
      ) %>%
      add_trace(
        data = CapacityTimeSeries[1, ],
        x = ~ Month,
        y = ~ `Operational`,
        name = "Operational",
        legendgroup = "4",
        text = paste0("Capacity: ",CapacityTimeSeries$`Operational`,"\nQuarter: ", as.yearqtr(CapacityTimeSeries$Month)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[4])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(CapacityTimeSeries$Month)-100, max(CapacityTimeSeries$Month)+100)),
        yaxis = list(
          title = "",
          showgrid = TRUE,
          zeroline = TRUE,
          zerolinecolor = ChartColours[4],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F)
    p
  })
  
  output$CapacityTimeSeries.png <- downloadHandler(
    filename = "CapacityTimeSeries.png",
    content = function(file) {
      ### variables
      ChartColours <- c( "#39ab2c", "#a8ddb5", "#4eb3d3", "#0868ac")
      sourcecaption = "Source: BEIS"
      plottitle = "Total wind capacity by planning status"
      
      #CapacityTimeSeries$`Application Submitted`Percentage <- PercentLabel(CapacityTimeSeries$`Application Submitted`)
      
      CapacityTimeSeries$Year <- ymd(CapacityTimeSeries$Month)
      
      CapacityTimeSeries$Quarter <- as.yearqtr(CapacityTimeSeries$Month)
      
      Length <- max(CapacityTimeSeries$Month) - min(CapacityTimeSeries$Month)
      
      CapacityTimeSeries<- CapacityTimeSeries[seq(dim(CapacityTimeSeries)[1],1),]
      
      CapacityTimeSeriesChart <- CapacityTimeSeries %>%
        ggplot(aes(x = Month), family = "Century Gothic") +
        
        geom_line(
          aes(y = `Application Submitted`,
              label = `Application Submitted`),
          colour = ChartColours[4],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month - Length*0.01,
            y = `Application Submitted`,
            label = ifelse(Month == min(Month), paste(format(round(`Application Submitted`, 0.1), big.mark = ",", trim = TRUE), "MW"), ""),
            hjust = 1,
            
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month +Length*0.02,
            y = `Application Submitted`,
            label = ifelse(Month == max(Month), paste(format(round(`Application Submitted`, 0.1), big.mark = ",", trim = TRUE), "MW"), ""),
            hjust = 0,
            
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(CapacityTimeSeries, 1),
          aes(x = Month,
              y = `Application Submitted`,
              show_guide = FALSE),
          colour = ChartColours[4],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(CapacityTimeSeries$Month),
          y = mean(CapacityTimeSeries$`Application Submitted`),
          label = "Application submitted",
          hjust = 0.5,
          vjust = -.5,
          colour = ChartColours[4],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Awaiting Construction`,
              label = paste0(`Awaiting Construction` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month - Length*0.01,
            y = `Awaiting Construction`,
            label = ifelse(Month == min(Month), paste(format(round(`Awaiting Construction`, 0.1), big.mark = ",", trim = TRUE), "MW"), ""),
            hjust = 1,
            
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month +Length*0.02,
            y = `Awaiting Construction`,
            label = ifelse(Month == max(Month), paste(format(round(`Awaiting Construction`, 0.1), big.mark = ",", trim = TRUE), "MW"), ""),
            hjust = 0,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(CapacityTimeSeries, 1),
          aes(x = Month,
              y = `Awaiting Construction`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(CapacityTimeSeries$Month),
          y = mean(CapacityTimeSeries$`Awaiting Construction`),
          label = "Awaiting construction",
          hjust = 0.5,
          vjust = 1.5,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Under Construction`,
              label = paste0(`Under Construction` * 100, "%")),
          colour = ChartColours[2],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month - Length*0.01,
            y = `Under Construction`,
            label = ifelse(Month == min(Month), paste(format(round(`Under Construction`, 0.1), big.mark = ",", trim = TRUE), "MW"), ""),
            hjust = 1,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month +Length*0.02,
            y = `Under Construction`,
            label = ifelse(Month == max(Month), paste(format(round(`Under Construction`, 0.1), big.mark = ",", trim = TRUE), "MW"), ""),
            hjust = 0,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(CapacityTimeSeries, 1),
          aes(x = Month,
              y = `Under Construction`,
              
              show_guide = FALSE),
          size = 4,
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(CapacityTimeSeries$Month),
          y = mean(CapacityTimeSeries$`Under Construction`),
          label = "Under Construction",
          hjust = 0.5,
          vjust = 2,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Operational`,
              label = `Operational`),
          colour = ChartColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month - Length*0.01,
            y = `Operational`,
            label = ifelse(Month == min(Month), paste(format(round(`Operational`, 0.1), big.mark = ",", trim = TRUE), "MW"), ""),
            hjust = 1,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Month+Length*0.02,
            y = `Operational`,
            label = ifelse(Month == max(Month), paste(format(round(`Operational`, 0.1), big.mark = ",", trim = TRUE), "MW"), ""),
            hjust= 0,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(CapacityTimeSeries, 1),
          aes(x = Month,
              y = `Operational`,
              show_guide = FALSE),
          colour = ChartColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(CapacityTimeSeries$Month),
          y = mean(CapacityTimeSeries$`Operational`),
          label = "Operational",
          hjust = 0.5,
          vjust = -2.5,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Month,
            y = 0,
            label = ifelse(
              Month == max(Month) |
                Month == min(Month),
              format(Quarter, "%Y Q%q"),
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      CapacityTimeSeriesChart
      
      CapacityTimeSeriesChart <-
        DailyChart(CapacityTimeSeriesChart,
                   CapacityTimeSeries,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      CapacityTimeSeriesChart <- CapacityTimeSeriesChart +
        coord_cartesian(xlim = c(min(CapacityTimeSeries$Year)-(Length*0.11), max(CapacityTimeSeries$Year)+(Length*0.11))) +
        ylim(
          -(max(CapacityTimeSeries[2:5])*0.03),
          max(CapacityTimeSeries[2:5])*1.01
        )+     
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        ) 
      
      CapacityTimeSeriesChart
      
      ggsave(
        file,
        plot =  CapacityTimeSeriesChart,
        width = 18,
        height = 10,
        units = "cm",
        dpi = 300
      )
    })
  
  output$CapacityTimeSeriesSubtitle <- renderText({
    
    paste("Scotland,", format(min(CapacityTimeSeries$Month), "%B %Y"),"-", format(max(CapacityTimeSeries$Month), "%B %Y"))
    
  })
  
  output$AllWindTable = renderDataTable({
    
    CurrentAll <- read_delim("Processed Data/Output/Turbine Analysis/Quarterly/CurrentAll.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)
    
    
    
    PreviousAll <- read_delim("Processed Data/Output/Turbine Analysis/Quarterly/PreviousAll.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ChangeAll <- CurrentAll
    
    ChangeAll[2:4] <- CurrentAll[2:4]-PreviousAll[2:4]
    
    
    names(ChangeAll) <- c("Status", "Change in sites since last quarter","Change in turbines since last quarter","Change in capacity since last quarter (MW)")
    
    names(CurrentAll) <- c("Status", "Sites","Turbines","Capacity (MW)")
    
    Table <- merge(CurrentAll, ChangeAll)
    
    Table <- Table[c(1,2,4,3),c(1,2,5,3,6,4,7)]
    
    
    datatable(
      Table,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "All wind",
        dom = 'tB',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'All wind',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'All wind')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:7, 0) %>% 
      formatStyle(c(3,5,7), fontStyle = "italic")
    
  })
  
  output$OnshoreWindTable = renderDataTable({
    
    Dummy <- read_delim("Processed Data/Output/Turbine Analysis/Quarterly/CurrentAll.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)
    
    Dummy[2:4] <- 0
    
    CurrentOnshore <- read_delim("Processed Data/Output/Turbine Analysis/Quarterly/CurrentOnshore.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ### Ensure there is a full data frame with every status
    
    CurrentOnshore <- rbind(CurrentOnshore,Dummy)
    
    CurrentOnshore <- CurrentOnshore[!duplicated(CurrentOnshore[,c('Status')]),]
    
    CurrentOnshore <- CurrentOnshore[order(CurrentOnshore$Status),]
    
    PreviousOnshore <- read_delim("Processed Data/Output/Turbine Analysis/Quarterly/PreviousOnshore.txt", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ### Ensure there is a full data frame with every status
    
    PreviousOnshore <- rbind(PreviousOnshore,Dummy)
    
    PreviousOnshore <- PreviousOnshore[!duplicated(PreviousOnshore[,c('Status')]),]
    
    PreviousOnshore <- PreviousOnshore[order(PreviousOnshore$Status),]
    
    
    
    ChangeOnshore <- CurrentOnshore
    
    ChangeOnshore[2:4] <- CurrentOnshore[2:4]-PreviousOnshore[2:4]
    
    names(ChangeOnshore) <- c("Status", "Change in sites since last quarter","Change in turbines since last quarter","Change in capacity since last quarter (MW)")
    
    names(CurrentOnshore) <- c("Status", "Sites","Turbines","Capacity (MW)")
    
    Table <- merge(CurrentOnshore, ChangeOnshore)
    
    Table <- Table[c(1,2,4,3),c(1,2,5,3,6,4,7)]
    
    datatable(
      Table,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Onshore wind",
        dom = 'tB',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Onshore wind',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Onshore wind')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "Onshore") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:7, 0) %>% 
      formatStyle(c(3,5,7), fontStyle = "italic")
    
    
  })
  
  output$OffshoreWindTable = renderDataTable({
    
    Dummy <- read_delim("Processed Data/Output/Turbine Analysis/Quarterly/CurrentAll.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)
    
    Dummy[2:4] <- 0
    
    CurrentOffshore <- read_delim("Processed Data/Output/Turbine Analysis/Quarterly/CurrentOffshore.txt", 
                                  "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ### Ensure there is a full data frame with every status
    
    CurrentOffshore <- rbind(CurrentOffshore,Dummy)
    
    CurrentOffshore <- CurrentOffshore[!duplicated(CurrentOffshore[,c('Status')]),]
    
    CurrentOffshore <- CurrentOffshore[order(CurrentOffshore$Status),]
    
    PreviousOffshore <- read_delim("Processed Data/Output/Turbine Analysis/Quarterly/PreviousOffshore.txt", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ### Ensure there is a full data frame with every status
    
    PreviousOffshore <- rbind(PreviousOffshore,Dummy)
    
    PreviousOffshore <- PreviousOffshore[!duplicated(PreviousOffshore[,c('Status')]),]
    
    PreviousOffshore <- PreviousOffshore[order(PreviousOffshore$Status),]
    
    
    
    ChangeOffshore <- CurrentOffshore
    
    ChangeOffshore[2:4] <- CurrentOffshore[2:4]-PreviousOffshore[2:4]
    
    
    names(ChangeOffshore) <- c("Status", "Change in sites since last quarter","Change in turbines since last quarter","Change in capacity since last quarter (MW)")
    
    names(CurrentOffshore) <- c("Status", "Sites","Turbines","Capacity (MW)")
    
    Table <- merge(CurrentOffshore, ChangeOffshore)
    
    Table <- Table[c(1,2,4,3),c(1,2,5,3,6,4,7)]
    
    datatable(
      Table,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Offshore wind",
        dom = 'tB',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Offshore wind',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Offshore wind')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "Offshore") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>% 
      formatRound(2:7, 0) %>% 
      formatStyle(c(3,5,7), fontStyle = "italic")
    
  })
}
