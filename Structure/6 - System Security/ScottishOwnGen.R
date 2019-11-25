require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ScotOwnGenOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Electricity generated and consumed", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('ScotOwnGenSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ScotOwnGen.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("ScotOwnGenPlot")),
    plotlyOutput(ns("ScotOwnGenPlot"))%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
               fluidRow(
    column(10, h3("Data", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ScotOwnGenTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
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
ScotOwnGen <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ScottishOwnGen.R")

  
  output$ScotOwnGenSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish generation", skip = 14)
    
    names(Data) <- c("Year", "ScotlandOnly", "ScotGenNonWind", "Renewables", "LowCarbon")
    ScotOwnGen <- Data
    
    
    paste("Scotland,", min(ScotOwnGen$Year),"-", max(ScotOwnGen$Year))
  })
  
  output$ScotOwnGenPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish generation", skip = 14)
    
    names(Data) <- c("Year", "ScotlandOnly", "ScotGenNonWind", "Renewables", "LowCarbon")
    ScotOwnGen <- Data
    
    ### variables
    ChartColours <- c("#5d8be1",  "#fdb462", "#fc8d62", "#66c2a5","#8da0cb")
    sourcecaption = "Source: Elexon, National Grid"
    plottitle = "Proportion of time Scotland is capable of meeting\ndemand from Scottish generation"
    
    ScotOwnGen$Year <- paste0("01/01/", ScotOwnGen$Year)
    
    ScotOwnGen$Year <- dmy(ScotOwnGen$Year)
    
    
    p <-  plot_ly(ScotOwnGen,x = ~ Year ) %>% 
      add_trace(data = ScotOwnGen,
                x = ~ Year,
                y = ~ ScotlandOnly,
                name = "All Scottish Generation",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "All Scottish Generation: ",
                  percent(ScotOwnGen$ScotlandOnly, accuracy = 0.1),
                  "\nYear: ",
                  format(ScotOwnGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ScotOwnGen[which(ScotOwnGen$ScotlandOnly > 0 | ScotOwnGen$ScotlandOnly < 0),], 1),
        x = ~ Year,
        y = ~ ScotlandOnly,
        legendgroup = "1",
        name = "All Scottish Generation",
        text = paste0(
          "All Scottish Generation: ",
          percent(ScotOwnGen[which(ScotOwnGen$ScotlandOnly > 0 | ScotOwnGen$ScotlandOnly < 0),][-1,]$ScotlandOnly, accuracy = 0.1),
          "\nYear: ",
          format(ScotOwnGen[which(ScotOwnGen$ScotlandOnly > 0 | ScotOwnGen$ScotlandOnly < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = ScotOwnGen,
                x = ~ Year,
                y = ~ ScotGenNonWind,
                name = "Scottish Generation - Excluding Wind",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Scottish Generation - Excluding Wind: ",
                  percent(ScotOwnGen$ScotGenNonWind, accuracy = 0.1),
                  "\nYear: ",
                  format(ScotOwnGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ScotOwnGen[which(ScotOwnGen$ScotGenNonWind > 0 | ScotOwnGen$ScotGenNonWind < 0),], 1),
        x = ~ Year,
        y = ~ ScotGenNonWind,
        legendgroup = "2",
        name = "Scottish Generation - Excluding Wind",
        text = paste0(
          "Scottish Generation - Excluding Wind: ",
          percent(ScotOwnGen[which(ScotOwnGen$ScotGenNonWind > 0 | ScotOwnGen$ScotGenNonWind < 0),][-1,]$ScotGenNonWind, accuracy = 0.1),
          "\nYear: ",
          format(ScotOwnGen[which(ScotOwnGen$ScotGenNonWind > 0 | ScotOwnGen$ScotGenNonWind < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      add_trace(data = ScotOwnGen,
                x = ~ Year,
                y = ~ Renewables,
                name = "Scottish Renewable Generation Only",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Scottish Renewable Generation Only: ",
                  percent(ScotOwnGen$Renewables, accuracy = 0.1),
                  "\nYear: ",
                  format(ScotOwnGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[4], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ScotOwnGen[which(ScotOwnGen$Renewables > 0 | ScotOwnGen$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ Renewables,
        legendgroup = "3",
        name = "Scottish Renewable Generation Only",
        text = paste0(
          "Scottish Renewable Generation Only: ",
          percent(ScotOwnGen[which(ScotOwnGen$Renewables > 0 | ScotOwnGen$Renewables < 0),][-1,]$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(ScotOwnGen[which(ScotOwnGen$Renewables > 0 | ScotOwnGen$Renewables < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[4])
      ) %>% 
      add_trace(data = ScotOwnGen,
                x = ~ Year,
                y = ~ LowCarbon,
                name = "Scottish Low Carbon Generation",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "4",
                text = paste0(
                  "Scottish Low Carbon Generation: ",
                  percent(ScotOwnGen$LowCarbon, accuracy = 0.1),
                  "\nYear: ",
                  format(ScotOwnGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[5], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ScotOwnGen[which(ScotOwnGen$LowCarbon > 0 | ScotOwnGen$LowCarbon < 0),], 1),
        x = ~ Year,
        y = ~ LowCarbon,
        legendgroup = "4",
        name = "Scottish Low Carbon Generation",
        text = paste0(
          "Scottish Low Carbon Generation: ",
          percent(ScotOwnGen[which(ScotOwnGen$LowCarbon > 0 | ScotOwnGen$LowCarbon < 0),][-1,]$LowCarbon, accuracy = 0.1),
          "\nYear: ",
          format(ScotOwnGen[which(ScotOwnGen$LowCarbon > 0 | ScotOwnGen$LowCarbon < 0),][-1,]$Year, "%Y")
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
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',

        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(ScotOwnGen$Year)-100, max(ScotOwnGen$Year)+100)),
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
  
  
  output$ScotOwnGenTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish generation", skip = 14)
    
    names(Data) <- c("Year", "All Scottish Generation", "Scottish Generation Excluding Wind", "Scottish Renewable Generation Only", "Scottish Low Carbon Generation")
    ScotOwnGeneration <- Data
    
    datatable(
      ScotOwnGeneration,
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
        title = "Proportion of time Scotland is capable of meeting demand from Scottish generation",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of time Scotland is capable of meeting demand from Scottish generation',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of time Scotland is capable of meeting demand from Scottish generation')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:5, 1)
  })
  

 output$Text <- renderUI({
   tagList(column(12,
                                   
                                     HTML(
                                       paste(readtext("Structure/6 - System Security/ScottishOwnGen.txt")[2])
                                     
                                   )))
 })
 
 
  observeEvent(input$ToggleTable, {
    toggle("ScotOwnGenTable")
  })
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ScotOwnGen.png <- downloadHandler(
    filename = "ScotOwnGen.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Scottish generation", skip = 14)
      
      names(Data) <- c("Year", "ScotlandOnly", "ScotGenNonWind", "Renewables", "LowCarbon")
      ScotOwnGeneration <- Data
      
      ### variables
      ChartColours <- c("#5d8be1",  "#fdb462", "#fc8d62", "#66c2a5","#8da0cb")
      sourcecaption = "Source: Elexon, National Grid"
      plottitle = "Proportion of time Scotland is capable of meeting\ndemand from Scottish generation"
      
      #ScotOwnGeneration$`ScotlandOnly`Percentage <- PercentLabel(ScotOwnGeneration$`ScotlandOnly`)
      
      
      ScotOwnGenerationChart <- ScotOwnGeneration %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = `ScotlandOnly`,
              label = percent(`ScotlandOnly`)),
          colour = ChartColours[2],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .4,
            y = `ScotlandOnly`,
            label = ifelse(Year == min(Year), percent(`ScotlandOnly`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = -0.02,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .45,
            y = `ScotlandOnly`,
            label = ifelse(Year == max(Year), percent(`ScotlandOnly`, accuracy = .1), ""),
            hjust = 0.5,
            
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotOwnGeneration, 1),
          aes(x = Year,
              y = `ScotlandOnly`,
              show_guide = FALSE),
          colour = ChartColours[2],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotOwnGeneration$Year),
          y = mean(ScotOwnGeneration$`ScotlandOnly`),
          label = "All generation",
          hjust = 0.5,
          vjust = -1,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `ScotGenNonWind`,
              label = paste0(`ScotGenNonWind` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .4,
            y = `ScotGenNonWind`,
            label = ifelse(Year == min(Year), percent(`ScotGenNonWind`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = 1.02,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .45,
            y = `ScotGenNonWind`,
            label = ifelse(Year == max(Year), percent(`ScotGenNonWind`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotOwnGeneration, 1),
          aes(x = Year,
              y = `ScotGenNonWind`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotOwnGeneration$Year),
          y = mean(ScotOwnGeneration$`ScotGenNonWind`),
          label = "Excluding wind",
          hjust = 0.5,
          vjust = 0.7,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Renewables`,
              label = paste0(`Renewables` * 100, "%")),
          colour = ChartColours[4],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .4,
            y = `Renewables`,
            label = ifelse(Year == min(Year), percent(`Renewables`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = -0.1,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .45,
            y = `Renewables`,
            label = ifelse(Year == max(Year), percent(`Renewables`,  accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotOwnGeneration, 1),
          aes(x = Year,
              y = `Renewables`,
              
              show_guide = FALSE),
          size = 4,
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotOwnGeneration$Year),
          y = mean(ScotOwnGeneration$`Renewables`),
          label = "Renewables only",
          hjust = 0.5,
          vjust = -4,
          colour = ChartColours[4],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `LowCarbon`,
              label = percent(`LowCarbon`)),
          colour = ChartColours[5],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .4,
            y = `LowCarbon`,
            label = ifelse(Year == min(Year), percent(`LowCarbon`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[5],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .45,
            y = `LowCarbon`,
            label = ifelse(Year == max(Year), percent(`LowCarbon`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = 0,
            fontface = 2
          ),
          colour = ChartColours[5],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotOwnGeneration, 1),
          aes(x = Year,
              y = `LowCarbon`,
              show_guide = FALSE),
          colour = ChartColours[5],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotOwnGeneration$Year),
          y = mean(ScotOwnGeneration$`LowCarbon`),
          label = "Low carbon\n generation",
          hjust = 0.5,
          vjust = 0,
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
      
      ScotOwnGenerationChart
      
      ScotOwnGenerationChart <-
        StackedArea(ScotOwnGenerationChart,
                    ScotOwnGeneration,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ScotOwnGenerationChart <- ScotOwnGenerationChart +
        ylim(-.03,1.06)
      
      ScotOwnGenerationChart
      
      ggsave(
        file,
        plot =  ScotOwnGenerationChart,
        width = 18,
        height = 10,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
