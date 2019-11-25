require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ScotRenGenOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Renewable generation as proportion of U.K.", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('ScotRenGenSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ScotRenGen.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("ScotRenGenPlot")),
    plotlyOutput(ns("ScotRenGenPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ScotRenGenTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
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
ScotRenGen <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenGen.R")

  
  output$ScotRenGenSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                          sheet = "Scottish renewables generation", col_names = TRUE, 
                          skip = 13)
    
    Data <- Data[c(1,4,7,10)]
    
    names(Data) <- c("Year", "Renewables", "Wind", "Hydro")
    
    ScotRenGen <- Data
    ### variables
    
    paste("Scotland,", min(ScotRenGen$Year),"-", max(ScotRenGen$Year))
  })
  
  output$ScotRenGenPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish renewables generation", col_names = TRUE, 
                       skip = 13)
    
    Data <- Data[c(1,4,7,10)]
    
    names(Data) <- c("Year", "Renewables", "Wind", "Hydro")
    
    ScotRenGen <- Data
    
    
    ChartColours <- c("#39ab2c",  "#fdb462", "#34d1a3", "#66c2a5","#8da0cb")
    sourcecaption = "Source: BEIS"
    plottitle = "Renewable generation as proportion of U.K."
    
    ScotRenGen$Year <- paste0("01/01/", ScotRenGen$Year)
    
    ScotRenGen$Year <- dmy(ScotRenGen$Year)
    
    
    p <-  plot_ly(data = ScotRenGen,
                  x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "Renewables",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "All Renewables: ",
                  percent(ScotRenGen$Renewables, accuracy = 0.1),
                  "\nYear: ",
                  format(ScotRenGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ScotRenGen[which(ScotRenGen$Renewables > 0 | ScotRenGen$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewable Electricity",
        text = paste0(
          "All Renewables: ",
          percent(ScotRenGen[which(ScotRenGen$Renewables > 0 | ScotRenGen$Renewables < 0),][-1,]$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(ScotRenGen[which(ScotRenGen$Renewables > 0 | ScotRenGen$Renewables < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        legendgroup = "1",
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(data = ScotRenGen,
                x = ~ Year,
                y = ~ Wind,
                name = "Wind",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Wind: ",
                  percent(ScotRenGen$Wind, accuracy = 0.1),
                  "\nYear: ",
                  format(ScotRenGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ScotRenGen[which(ScotRenGen$Wind > 0 | ScotRenGen$Wind < 0),], 1),
        x = ~ Year,
        y = ~ `Wind`,
        name = "Wind",
        legendgroup = "2",
        text = paste0(
          "Wind: ",
          percent(ScotRenGen[which(ScotRenGen$Wind > 0 | ScotRenGen$Wind < 0),][-1,]$Wind, accuracy = 0.1),
          "\nYear: ",
          format(ScotRenGen[which(ScotRenGen$Wind > 0 | ScotRenGen$Wind < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      add_trace(data = ScotRenGen,
                x = ~ Year,
                y = ~ Hydro,
                name = "Hydro",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Hydro: ",
                  percent(ScotRenGen$Hydro, accuracy = 0.1),
                  "\nYear: ",
                  format(ScotRenGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[5], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ScotRenGen[which(ScotRenGen$Hydro > 0 | ScotRenGen$Hydro < 0),], 1),
        x = ~ Year,
        y = ~ `Hydro`,
        name = "Hydro",
        legendgroup = "3",
        text = paste0(
          "Hydro: ",
          percent(ScotRenGen[which(ScotRenGen$Hydro > 0 | ScotRenGen$Hydro < 0),][-1,]$Hydro, accuracy = 0.1),
          "\nYear: ",
          format(ScotRenGen[which(ScotRenGen$Hydro > 0 | ScotRenGen$Hydro < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[5])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(ScotRenGen$Year)-100, max(ScotRenGen$Year)+100)),
        yaxis = list(
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
    p
    
    
    
  })
  
  
  output$ScotRenGenTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish renewables generation", col_names = TRUE, 
                       skip = 13)
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    ScotRenGen <- as_tibble(Data)
    
    names(ScotRenGen) <- c("Year", "Renewables - Scotland", "Renewables - UK", "Renewables - Scottish proportion of UK Total", 
                           "Wind - Scotland", "Wind - UK", "Wind - Scottish proportion of UK Total",
                           "Hydro - Scotland", "Hydro - UK", "Hydro - Scottish proportion of UK Total")
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Year'),
          th(colspan = 3, 'Total Renewables'),
          th(colspan = 3, 'Wind'),
          th(colspan = 3, 'Hydro')
        ),
        tr(
          lapply(rep(c('Scotland', 'UK', "% Scotland of UK Total"), 3), th)
        )
      )
    ))
    
    print(sketch)
    datatable(
      ScotRenGen,
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
        order = list(list(0, 'desc')),
        title = "Scottish Renewable Generation",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Scottish Renewable Generation',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Scottish Renewable Generation')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatPercentage(c(4,7,10), 1) %>% 
      formatRound(c(2:3, 5:6,8:9), 0)
  })
  
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/RenGen.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("ScotRenGenTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ScotRenGen.png <- downloadHandler(
    filename = "ScotRenGen.png",
    content = function(file) {

      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Scottish renewables generation", col_names = TRUE, 
                         skip = 13)
      
      Data <- Data[c(1,4,7,10)]
      
      names(Data) <- c("Year", "Renewables", "Wind", "Hydro")
      
      ScotRenGen <- Data
      ### variables
      ChartColours <- c("#39ab2c",  "#fdb462", "#34d1a3", "#66c2a5","#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = "Renewable generation as proportion of U.K."
      
      #ScotRenGen$`Renewables`Percentage <- PercentLabel(ScotRenGen$`Renewables`)
      
      
      ScotRenGenChart <- ScotRenGen %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = `Renewables`,
              label = percent(`Renewables`)),
          colour = ChartColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1.4,
            y = `Renewables`,
            label = ifelse(Year == min(Year), percent(`Renewables`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.65,
            y = `Renewables`,
            label = ifelse(Year == max(Year), percent(`Renewables`, accuracy = .1), ""),
            hjust = 0.5,
            
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotRenGen, 1),
          aes(x = Year,
              y = `Renewables`,
              show_guide = FALSE),
          colour = ChartColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotRenGen$Year),
          y = mean(ScotRenGen$`Renewables`),
          label = "All renewables",
          hjust = 0.5,
          vjust = 2.5,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Wind`,
              label = paste0(`Wind` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1.4,
            y = `Wind`,
            label = ifelse(Year == min(Year), percent(`Wind`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = -0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.65,
            y = `Wind`,
            label = ifelse(Year == max(Year), percent(`Wind`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotRenGen, 1),
          aes(x = Year,
              y = `Wind`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotRenGen$Year),
          y = mean(ScotRenGen$`Wind`),
          label = "Wind",
          hjust = 0.5,
          vjust = -3,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Hydro`,
              label = percent(`Hydro`)),
          colour = ChartColours[5],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1.4,
            y = `Hydro`,
            label = ifelse(Year == min(Year), percent(`Hydro`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[5],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.65,
            y = `Hydro`,
            label = ifelse(Year == max(Year), percent(`Hydro`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = 0,
            fontface = 2
          ),
          colour = ChartColours[5],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotRenGen, 1),
          aes(x = Year,
              y = `Hydro`,
              show_guide = FALSE),
          colour = ChartColours[5],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotRenGen$Year),
          y = mean(ScotRenGen$`Hydro`),
          label = "Hydro",
          hjust = 0.5,
          vjust = -1,
          colour = ChartColours[5],
          fontface = 2,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              Year,
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      ScotRenGenChart
      
      ScotRenGenChart <-
        StackedArea(ScotRenGenChart,
                    ScotRenGen,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ScotRenGenChart <- ScotRenGenChart
      
      ScotRenGenChart
      
      ggsave(
        file,
        plot =  ScotRenGenChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
}
