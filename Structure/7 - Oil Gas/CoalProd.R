require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

CoalProdOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Coal production", style = "color: #126992;  font-weight:bold"),
                    h4(textOutput(ns('CoalProdSubtitle')), style = "color: #126992;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('CoalProd.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    #dygraphOutput(ns("CoalProdPlot")),
    plotlyOutput(ns("CoalProdPlot"))%>% withSpinner(color="#126992"),
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
      column(12, dataTableOutput(ns("CoalProdTable"))%>% withSpinner(color="#126992"))),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISDUKESCoal"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISDUKESCoal")
        
      )
    )
  )
}




###### Server ######
CoalProd <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("CoalProd.R")

  
  output$CoalProdSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Coal production", skip = 12, col_names = FALSE)
    
    Data <- as_tibble(t(Data))[1:3]
    
    Data <- Data[-1,]
    
    names(Data) <- c("Year", "Prod", "Proportion")
    
    Data$Year <- substr(Data$Year, 1,4)
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    CoalProd <- as_tibble(Data)
    
    CoalProd$Year <- as.numeric(substr(CoalProd$Year, 1,4))

    paste0("Scotland, ", min(CoalProd$Year),"/",substr(min(CoalProd$Year)+1,3,4)," - ",  max(CoalProd$Year))
  })
  
  output$CoalProdPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Coal production", skip = 12, col_names = FALSE)
    
    Data <- as_tibble(t(Data))[1:3]
    
    Data <- Data[-1,]
    
    names(Data) <- c("Year", "Prod", "Proportion")
    
    Data$Year2 <- substr(Data$Year, 1,4)
    
    Data[2:4] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- ifelse(nchar(Data$Year) > 8, substr(Data$Year,1,4), substr(Data$Year,1,7))
    
    Data$Year <- factor(Data$Year, ordered = TRUE)
    
    CoalProd <- as_tibble(Data)
    
    CoalProd$Year <- as.character(CoalProd$Year)
    
    plottitle <-
      "Coal production (million tonnes)"
    sourcecaption <- "Source: BEIS"
    
    ChartColours <- c("#126992", "#66c2a5", "#fc8d62", "#8da0cb")
    BarColours <-
      c(    "#0868ac","#43a2ca","#7bccc4"
      )
    
    CoalProd$Year2 <- paste0("01/01/", CoalProd$Year2)
    
    CoalProd$Year2 <- dmy(CoalProd$Year2)
    
    p <-  plot_ly(CoalProd,x = ~ Year2 ) %>% 
      add_trace(data = CoalProd,
                x = ~ Year2,
                y = ~ Prod,
                name = "Prod",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Production: ",
                  round(CoalProd$Prod, digits = 3),
                  " million tonnes\nYear: ",
                  paste(CoalProd$Year)
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      )  %>% 
      add_trace(
        data = tail(CoalProd[which(CoalProd$Prod > 0 | CoalProd$Prod < 0),], 1),
        x = ~ Year2,
        y = ~ Prod,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "Production: ",
          round(CoalProd[which(CoalProd$Prod > 0 | CoalProd$Prod < 0),][-1,]$Prod, digits = 3),
          " million tonnes\nYear: ",
          paste(CoalProd[which(CoalProd$Prod > 0 | CoalProd$Prod < 0),][-1,]$Year)
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
          title = "million tonnes",
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
  
  
  output$CoalProdTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Coal production", skip = 12, col_names = FALSE)
    
    Data <- as_tibble(t(Data))[1:3]
    
    Data <- Data[-1,]
    
    names(Data) <- c("Year", "Scottish Production (million tonnes)", "Scotland's share of GB")
    
    Data[2:3] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data$Year <- ifelse(nchar(Data$Year) > 8, substr(Data$Year,1,4), substr(Data$Year,1,7))
    
    Data$Year <- factor(Data$Year, ordered = TRUE)
    
    CoalProd <- as_tibble(Data)
    
    datatable(
      CoalProd,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
		scrollX = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        title = "Scottish Coal Production",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Scottish Coal Production',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Scottish Coal Production')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2), 1) %>% 
      formatPercentage(3,1)
  })
  

 output$Text <- renderUI({
   tagList(column(12,
                                   
                                     HTML(
                                       paste(readtext("Structure/7 - Oil Gas/CoalProd.txt")[2])
                                     
                                   )))
 })
 
 
  observeEvent(input$ToggleTable, {
    toggle("CoalProdTable")
  })
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$CoalProd.png <- downloadHandler(
    filename = "CoalProd.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Coal production", skip = 12, col_names = FALSE)
      
      Data <- as_tibble(t(Data))[1:3]
      
      Data <- Data[-1,]
      
      names(Data) <- c("Year", "Prod", "Proportion")
      
      Data$Year <- substr(Data$Year, 1,4)
      
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      
      CoalProd <- as_tibble(Data)
      
      
      plottitle <-
        "Coal production (million tonnes)"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#126992", "#66c2a5", "#fc8d62", "#8da0cb")
      BarColours <-
        c(    "#0868ac","#43a2ca","#7bccc4"
        )
      
      
      CoalProdChart <- CoalProd %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = Prod,
            colour = ChartColours[2],
            label = percent(Prod, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Prod,
            label = ifelse(Year == min(Year), round(Prod, digits = 1), ""),
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
            y = Prod,
            label = ifelse(Year == max(Year), round(Prod, digits = 1), ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(CoalProd, 1),
          aes(
            x = Year,
            y = Prod,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
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
      
      CoalProdChart <-
        LinePercentChart(CoalProdChart,
                         CoalProd,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      CoalProdChart
      
      ggsave(
        file,
        plot = CoalProdChart,
        width = 12.5,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
}
