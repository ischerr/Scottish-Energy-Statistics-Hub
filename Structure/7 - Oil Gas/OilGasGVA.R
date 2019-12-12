require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

OilGasGVAOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("GVA associated with oil and gas production", style = "color: #126992;  font-weight:bold"),
                    h4(textOutput(ns('OilGasGVASubtitle')), style = "color: #126992;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('OilGasGVA.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    #dygraphOutput(ns("OilGasGVAPlot")),
    plotlyOutput(ns("OilGasGVAPlot"))%>% withSpinner(color="#126992"),
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
      column(12, dataTableOutput(ns("OilGasGVATable"))%>% withSpinner(color="#126992"))),
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
        SourceLookup("BEISFinalConsump"),
        SourceLookup("ETElecGen"),
        SourceLookup("ESTRenHeat")
        
      )
    )
  )
}




###### Server ######
OilGasGVA <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("OilGasGVA.R")

  
  output$OilGasGVASubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Oil and gas GVA", skip = 13, col_names = FALSE)
    
    Data <- as_tibble(t(Data))[1:2]
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "GVA")
    
    OilGasGVA <- as_tibble(Data)
    
    ### variables
    ChartColours <- c("#126992", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: SG"
    plottitle = "GVA associated with oil and gas production"

    paste0("Scotland, ", min(OilGasGVA$Year)," - ",  max(OilGasGVA$Year))
  })
  
  output$OilGasGVAPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Oil and gas GVA", skip = 13, col_names = FALSE)
    
    Data <- as_tibble(t(Data))[1:2]
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "GVA")
    
    OilGasGVA <- as_tibble(Data)
    
    ### variables
    ChartColours <- c("#126992", "#66c2a5", "#fc8d62", "#8da0cb")
    sourcecaption = "Source: SG"
    plottitle = "GVA associated with oil and gas production"
    
    OilGasGVA$Year <- paste0("01/01/", OilGasGVA$Year)
    
    OilGasGVA$Year <- dmy(OilGasGVA$Year)
    
    
    p <-  plot_ly(OilGasGVA,x = ~ Year ) %>% 
      add_trace(data = OilGasGVA,
                x = ~ Year,
                y = ~ GVA,
                name = "GVA",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "GVA: \u00A3",
                  round(OilGasGVA$GVA, digits = 3),
                  " billion\nYear: ",
                  format(OilGasGVA$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      )  %>% 
      add_trace(
        data = tail(OilGasGVA[which(OilGasGVA$GVA > 0 | OilGasGVA$GVA < 0),], 1),
        x = ~ Year,
        y = ~ GVA,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "GVA: \u00A3",
          round(OilGasGVA[which(OilGasGVA$GVA > 0 | OilGasGVA$GVA < 0),][-1,]$GVA, digits = 3),
          " billion\nYear: ",
          format(OilGasGVA[which(OilGasGVA$GVA > 0 | OilGasGVA$GVA < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      )  %>%  
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',

        xaxis = list(title = "",
                     showgrid = FALSE),
        yaxis = list(
          title = "\u00A3 Billion",
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
  
  
  output$OilGasGVATable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Oil and gas GVA", skip = 13, col_names = FALSE)
    
    Data <- as_tibble(t(Data))[1:3]
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "Oil and Gas GVA (\u00A3bn)", "% of GDP")
    
    OilGasGVA <- as_tibble(Data)
    
    datatable(
      OilGasGVA,
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
        title = "GVA associated with oil and gas production (\u00A3 billion)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'GVA associated with oil and gas production (\u00A3 billion)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'GVA associated with oil and gas production (\u00A3 billion)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2), 3) %>% 
      formatPercentage(3,1)
  })
  

 output$Text <- renderUI({
   tagList(column(12,
                                   
                                     HTML(
                                       paste(readtext("Structure/7 - Oil Gas/OilGasGVA.txt")[2])
                                     
                                   )))
 })
 
 
  observeEvent(input$ToggleTable, {
    toggle("OilGasGVATable")
  })
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$OilGasGVA.png <- downloadHandler(
    filename = "OilGasGVA.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Oil and gas GVA", skip = 13, col_names = FALSE)
      
      Data <- as_tibble(t(Data))[1:2]
      
      Data <- Data[-1,]
      
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      
      names(Data) <- c("Year", "GVA")
      
      OilGasGVA <- as_tibble(Data)
      
      OilGasGVA$Year <- as.numeric(substr(OilGasGVA$Year, 1,4))
      ### variables
      ChartColours <- c("#126992", "#66c2a5", "#fc8d62", "#8da0cb")
      sourcecaption = "Source: SG"
      plottitle = "GVA associated with oil and gas production"
      
      #OilGasGVA$OilPercentage <- PercentLabel(OilGasGVA$Oil)
      
      
      OilGasGVAChart <- OilGasGVA %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = GVA,
            colour = ChartColours[2],
            label = percent(GVA, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = GVA,
            label = ifelse(Year == min(Year), paste0("\u00A3", format(round(GVA, digits = 3),nsmall = 3, trim = TRUE), "\nbillion"), ""),
            hjust = 0.5,
            vjust = 1.4,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = GVA,
            label = ifelse(Year == max(Year), paste0("\u00A3", format(round(GVA, digits = 3),nsmall = 3, trim = TRUE), "\nbillion"), ""),
            hjust = 0.5,
            vjust = -.4,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(OilGasGVA, 1),
          aes(
            x = Year,
            y = GVA,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(GVA),
            label = "GVA",
            hjust = 0.5,
            vjust = -.5,
            colour = ChartColours[2],
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
      
      
      OilGasGVAChart <-
        LinePercentChart(OilGasGVAChart,
                         OilGasGVA,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      OilGasGVAChart <- OilGasGVAChart +
        xlim(min(OilGasGVA$Year) -1 , max(OilGasGVA$Year) +1)
      
      OilGasGVAChart
      
      ggsave(
        file,
        plot =  OilGasGVAChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
