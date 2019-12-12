require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ElecGenerationOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Electricity generated and consumed", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('ElecGenerationSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecGeneration.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("ElecGenerationPlot")),
    plotlyOutput(ns("ElecGenerationPlot"))%>% withSpinner(color="#5d8be1"),
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
      column(12, dataTableOutput(ns("ElecGenerationTable"))%>% withSpinner(color="#5d8be1"))),
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
        SourceLookup("ETElecGeneration"),
        SourceLookup("ESTRenHeat")
        
      )
    )
  )
}




###### Server ######
ElecGeneration <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ElecGeneration.R")

  
  output$ElecGenerationSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec generation", skip = 12)[c(1,2,5,6)]
    
    Data <- Data[complete.cases(Data),]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "Generated", "Gross", "Total")
    
    ElecGeneration <- as_tibble(Data)
    
    
    paste("Scotland,", min(ElecGeneration$Year),"-", max(ElecGeneration$Year))
  })
  
  output$ElecGenerationPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec generation", skip = 12)[c(1,2,5,6)]
    
    Data <- Data[complete.cases(Data),]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "Generated", "Gross", "Total")
    
    ElecGeneration <- as_tibble(Data)
    
    ### variables
    ChartColours <- c("#5d8be1", "#1d91c0",  "#5d8be1", "#253494" )
    sourcecaption = "Source: BEIS"
    plottitle = "Electricity generated and consumed"
    
    ElecGeneration$Year <- paste0("01/01/", ElecGeneration$Year)
    
    ElecGeneration$Year <- dmy(ElecGeneration$Year)
    
    
    p <-  plot_ly(ElecGeneration,x = ~ Year ) %>% 
      add_trace(data = ElecGeneration,
                x = ~ Year,
                y = ~ Generated,
                name = "Electricity Generated",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Electricity Generated: ",
                  round(ElecGeneration$Generated, digits = 0),
                  " GWh\nYear: ",
                  format(ElecGeneration$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecGeneration[which(ElecGeneration$Generated > 0 | ElecGeneration$Generated < 0),], 1),
        x = ~ Year,
        y = ~ `Generated`,
        legendgroup = "1",
        name = "Electricity Generated",
        text = paste0(
          "Electricity Generated: ",
          round(ElecGeneration[which(ElecGeneration$Generated > 0 | ElecGeneration$Generated < 0),][-1,]$Generated, digits = 0),
          " GWh\nYear: ",
          format(ElecGeneration[which(ElecGeneration$Generated > 0 | ElecGeneration$Generated < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(data = ElecGeneration,
                x = ~ Year,
                y = ~ Gross,
                name = "Gross electricity consumption",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Gross electricity consumption: ",
                  round(ElecGeneration$Gross, digits = 0),
                  " GWh\nYear: ",
                  format(ElecGeneration$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[4], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecGeneration[which(ElecGeneration$Gross > 0 | ElecGeneration$Gross < 0),], 1),
        x = ~ Year,
        y = ~ `Gross`,
        legendgroup = "2",
        name = "Gross electricity consumption",
        text = paste0(
          "Gross electricity consumption: ",
          round(ElecGeneration[which(ElecGeneration$Gross > 0 | ElecGeneration$Gross < 0),][-1,]$Gross, digits = 0),
          " GWh\nYear: ",
          format(ElecGeneration[which(ElecGeneration$Gross > 0 | ElecGeneration$Gross < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[4])
      ) %>% 
      add_trace(data = ElecGeneration,
                x = ~ Year,
                y = ~ Total,
                name = "Total electricity consumption",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Total electricity consumption: ",
                  round(ElecGeneration$Total, digits = 0),
                  " GWh\nYear: ",
                  format(ElecGeneration$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecGeneration[which(ElecGeneration$Total > 0 | ElecGeneration$Total < 0),], 1),
        x = ~ Year,
        y = ~ `Total`,
        legendgroup = "3",
        name = "Total electricity consumption",
        text = paste0(
          "Total electricity consumption: ",
          round(ElecGeneration[which(ElecGeneration$Total > 0 | ElecGeneration$Total < 0),][-1,]$Total, digits = 0),
          " GWh\nYear: ",
          format(ElecGeneration[which(ElecGeneration$Total > 0 | ElecGeneration$Total < 0),][-1,]$Year, "%Y")
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
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',

        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(ElecGeneration$Year)-100, max(ElecGeneration$Year)+100)),
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
  
  
  output$ElecGenerationTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec generation", skip = 12)
    
    Data <- Data[complete.cases(Data),]

    ElecGeneration <- as_tibble(Data)
    
    datatable(
      ElecGeneration,
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
        title = "Electricity generated and consumed",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Electricity generated and consumed',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Electricity generated and consumed')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:7, 0)
  })
  
  output$ElecGenerationImpactTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Wall insulation", skip = 20,  col_names = FALSE)
    
    Data <- as_tibble(t(Data))[c(1:3,7:8)]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "Median Saving in Consumption (kWh) - Cavity Wall Insulation", "Median percentage saving - Cavity Wall", "Median Saving in Consumption (kWh) - Solid Wall Insulation", "Median percentage saving - Solid Wall")
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    ElecGeneration <- Data
    
    datatable(
      ElecGeneration,
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
        title = "Impact of Measures - Wall Insulation",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Impact of Measures - Wall Insulation',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Impact of Measures - Wall Insulation')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(3,5), 1) %>% 
      formatRound(c(2,4), 0)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/6 - System Security/ElecGeneration.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("ElecGenerationTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ElecGenerationImpactTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ElecGeneration.png <- downloadHandler(
    filename = "ElecGeneration.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Elec generation", skip = 12)[c(1,2,5,6)]
      
      Data <- Data[complete.cases(Data),]
      
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      
      names(Data) <- c("Year", "Generated", "Gross", "Total")
      
      ElecGeneration <- as_tibble(Data)
      
      ### variables
      ChartColours <- c("#5d8be1", "#1d91c0",  "#5d8be1", "#253494" )
      sourcecaption = "Source: BEIS"
      plottitle = "Electricity generated and consumed"
      
      #ElecGeneration$TotalPercentage <- PercentLabel(ElecGeneration$Total)
      
      
      ElecGenerationChart <- ElecGeneration %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = Total,
              label = Total),
          colour = ChartColours[2],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecGeneration$Year),
          y = max(ElecGeneration$Total),
          label = "Total electricity consumption",
          hjust = 0.5,
          vjust = 4,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Generated,
              label = paste0(Generated * 100, "%")),
          colour = ChartColours[3],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecGeneration$Year),
          y = mean(ElecGeneration$Generated),
          label = "Electricity generated",
          hjust = 0.5,
          vjust = -1.75,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = Gross,
              label = paste0(Gross * 100, "%")),
          colour = ChartColours[4],
          size = 1,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ElecGeneration$Year),
          y = mean(ElecGeneration$Gross),
          label = "Gross electricity consumption",
          hjust = 0.5,
          vjust = -1.5,
          colour = ChartColours[4],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              format(Year, format = "%b %Y"),
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = min(Year)-1,
            y = ElecGeneration$Total[which(ElecGeneration$Year == min(ElecGeneration$Year))],
            label = paste0(format(round(ElecGeneration$Total[which(ElecGeneration$Year == min(ElecGeneration$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = max(Year)+1,
            y = ElecGeneration$Total[which(ElecGeneration$Year == max(ElecGeneration$Year))],
            label = paste0(format(round(ElecGeneration$Total[which(ElecGeneration$Year == max(ElecGeneration$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        )+
        geom_point(
          aes(
            x = max(Year),
            y = ElecGeneration$Total[which(ElecGeneration$Year == max(ElecGeneration$Year))]
          ),
          colour = ChartColours[2],
          size = 4,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = min(Year)-1,
            y = ElecGeneration$Generated[which(ElecGeneration$Year == min(ElecGeneration$Year))],
            label = paste0(format(round(ElecGeneration$Generated[which(ElecGeneration$Year == min(ElecGeneration$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = max(Year)+1,
            y = ElecGeneration$Generated[which(ElecGeneration$Year == max(ElecGeneration$Year))],
            label = paste0(format(round(ElecGeneration$Generated[which(ElecGeneration$Year == max(ElecGeneration$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        )+
        geom_point(
          aes(
            x = max(Year),
            y = ElecGeneration$Generated[which(ElecGeneration$Year == max(ElecGeneration$Year))]
          ),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = min(Year)-1,
            y = ElecGeneration$Gross[which(ElecGeneration$Year == min(ElecGeneration$Year))],
            label = paste0(format(round(ElecGeneration$Gross[which(ElecGeneration$Year == min(ElecGeneration$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = max(Year)+1,
            y = ElecGeneration$Gross[which(ElecGeneration$Year == max(ElecGeneration$Year))],
            label = paste0(format(round(ElecGeneration$Gross[which(ElecGeneration$Year == max(ElecGeneration$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            vjust = 0,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        )+
        geom_point(
          aes(
            x = max(Year),
            y = ElecGeneration$Gross[which(ElecGeneration$Year == max(ElecGeneration$Year))]
          ),
          colour = ChartColours[4],
          family = "Century Gothic",
          size = 4
        )
      
      
      ElecGenerationChart
      
      ElecGenerationChart <-
        LinePercentChart(ElecGenerationChart,
                         ElecGeneration,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      ElecGenerationChart <- ElecGenerationChart +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )+
        ylim(-1000, max(ElecGeneration$Generated))
      
      
      ElecGenerationChart
      
      ggsave(
        file,
        plot =  ElecGenerationChart,
        width = 18,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
