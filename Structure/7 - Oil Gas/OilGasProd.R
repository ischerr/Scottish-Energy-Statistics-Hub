require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

OilGasProdOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Oil and gas production", style = "color: #126992;  font-weight:bold"),
                    h4(textOutput(ns('OilGasProdSubtitle')), style = "color: #;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('OilGasProd.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    #dygraphOutput(ns("OilGasProdPlot")),
    plotlyOutput(ns("OilGasProdPlot"))%>% withSpinner(color="#126992"),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #126992;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
               fluidRow(
    column(10, h3("Data", style = "color: #126992;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("OilGasProdTable"))%>% withSpinner(color="#126992"))),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
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
        p("Energy Balance")
        
      )
    )
  )
}




###### Server ######
OilGasProd <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("OilGasProd.R")

  
  output$OilGasProdSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish oil and gas production", skip = 13)[c(1,4,6)]
    
    names(Data) <- c("Year", "Oil", "Gas")
    
    OilGasProd <- Data
    
    
    paste("Scotland,", min(OilGasProd$Year),"-", max(OilGasProd$Year))
  })
  
  output$OilGasProdPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish oil and gas production", skip = 13)[c(1,4,6)]
    
    names(Data) <- c("Year", "Oil", "Gas")
    
    OilGasProd <- Data
    
    ### variables
    ChartColours <- c("#126992", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: Scottish Government"
    plottitle = "Oil and gas production"
    
    OilGasProd$Year <- paste0("01/01/", OilGasProd$Year)
    
    OilGasProd$Year <- dmy(OilGasProd$Year)
    
    
    p <-  plot_ly(OilGasProd,x = ~ Year ) %>% 
      add_trace(data = OilGasProd,
                x = ~ Year,
                y = ~ Oil,
                name = "Oil",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Oil: ",
                  round(OilGasProd$Oil, digits = 1),
                  " mtoe\nYear: ",
                  format(OilGasProd$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(OilGasProd[which(OilGasProd$Oil > 0 | OilGasProd$Oil < 0),], 1),
        x = ~ Year,
        y = ~ Oil,
        legendgroup = "1",
        name = "Oil",
        text = paste0(
          "Oil: ",
          round(OilGasProd[which(OilGasProd$Oil > 0 | OilGasProd$Oil < 0),][-1,]$Oil, digits = 1),
          " mtoe\nYear: ",
          format(OilGasProd[which(OilGasProd$Oil > 0 | OilGasProd$Oil < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(data = OilGasProd,
                x = ~ Year,
                y = ~ Gas,
                name = "Gas",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Gas: ",
                  round(OilGasProd$Gas, digits = 1),
                  " mtoe\nYear: ",
                  format(OilGasProd$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(OilGasProd[which(OilGasProd$Gas > 0 | OilGasProd$Gas < 0),], 1),
        x = ~ Year,
        y = ~ Gas,
        legendgroup = "2",
        name = "Gas",
        text = paste0(
          "Gas: ",
          round(OilGasProd[which(OilGasProd$Gas > 0 | OilGasProd$Gas < 0),][-1,]$Gas, digits = 1),
          " mtoe\nYear: ",
          format(OilGasProd[which(OilGasProd$Gas > 0 | OilGasProd$Gas < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',

        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(OilGasProd$Year)-100, max(OilGasProd$Year)+100)),
        yaxis = list(
          title = "mtoe",
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
  
  
  output$OilGasProdTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish oil and gas production", skip = 13)
    
    names(Data) <- c("Year", "Total Production (mtoe)", "Total Production proportion of U.K.", "Crude Oil and NGL (mtoe)", "Crude Oil and NGL proportion of U.K.", "Natural Gas (mtoe)", "Natural Gas proportion of U.K.")
    
    OilGasProd <- Data
    
    datatable(
      OilGasProd,
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
        title = "Oil and gas production",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Oil and gas production',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Oil and gas production')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(3,5,7), 1) %>% 
      formatRound(c(2,4,6), 1)
  })
  

 output$Text <- renderUI({
   tagList(column(12,
                                   
                                     HTML(
                                       paste(readtext("Structure/7 - Oil Gas/OilGasProd.txt")[2])
                                     
                                   )))
 })
 
 
  observeEvent(input$ToggleTable, {
    toggle("OilGasProdTable")
  })
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$OilGasProd.png <- downloadHandler(
    filename = "OilGasProd.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Scottish oil and gas production", skip = 13)[c(1,4,6)]
      
      names(Data) <- c("Year", "Oil", "Gas")
      
      OilGasProd <- Data
      
      ### variables
      ChartColours <- c("#126992", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: Scottish Government"
      plottitle = "Oil and gas production"
      
      #OilGasProd$OilPercentage <- PercentLabel(OilGasProd$Oil)
      
      
      OilGasProdChart <- OilGasProd %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = Oil,
            colour = ChartColours[2],
            label = percent(Oil)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Oil,
            label = ifelse(Year == min(Year), paste0(round(`Oil`, digits = 1), " mtoe"), ""),
            hjust = 0.5,
            vjust = 2.2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Oil,
            label = ifelse(Year == max(Year), paste0(round(`Oil`, digits = 1), " mtoe"), ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(OilGasProd, 1),
          aes(
            x = Year,
            y = Oil,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(Oil),
            label = "Oil",
            hjust = 0.5,
            vjust = -.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `Gas`,
            colour = ChartColours[3],
            label = paste0(`Gas` * 100, "%")
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Gas`,
            label = ifelse(Year == min(Year), paste0(round(`Gas`, digits = 1)," mtoe"), ""),
            hjust = 0.5,
            vjust = -1.9,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Gas`,
            label = ifelse(Year == max(Year), paste0(round(`Gas`, digits = 1), " mtoe"), ""),
            hjust = 0.5,
            vjust = -1.5,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(OilGasProd, 1),
          aes(
            x = Year,
            y = `Gas`,
            colour = ChartColours[3],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(`Gas`),
            label = "Gas",
            hjust = 0.5,
            vjust = -2,
            colour = ChartColours[3],
            fontface = 2
          ),
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
        )
      
      
      OilGasProdChart <-
        LinePercentChart(OilGasProdChart,
                         OilGasProd,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      OilGasProdChart <- OilGasProdChart +
        xlim(min(OilGasProd$Year) -1 , max(OilGasProd$Year) +1)
      
      OilGasProdChart
      
      ggsave(
        file,
        plot =  OilGasProdChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
