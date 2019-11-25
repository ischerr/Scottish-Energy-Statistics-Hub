require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ElecGenOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Electricity generated and consumed", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('ElecGenSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ElecGen.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("ElecGenPlot")),
    plotlyOutput(ns("ElecGenPlot"))%>% withSpinner(color="#5d8be1"),
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
      column(12, dataTableOutput(ns("ElecGenTable"))%>% withSpinner(color="#5d8be1"))),
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
ElecGen <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ElecGen.R")

  
  output$ElecGenSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec generation", skip = 12)[c(1,2,5,6)]
    
    Data <- Data[complete.cases(Data),]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "Generated", "Gross", "Total")
    
    ElecGen <- as_tibble(Data)
    
    
    paste("Scotland,", min(ElecGen$Year),"-", max(ElecGen$Year))
  })
  
  output$ElecGenPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec generation", skip = 12)[c(1,2,5,6)]
    
    Data <- Data[complete.cases(Data),]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "Generated", "Gross", "Total")
    
    ElecGen <- as_tibble(Data)
    
    ### variables
    ChartColours <- c("#5d8be1", "#1d91c0",  "#5d8be1", "#253494" )
    sourcecaption = "Source: BEIS"
    plottitle = "Electricity generated and consumed"
    
    ElecGen$Year <- paste0("01/01/", ElecGen$Year)
    
    ElecGen$Year <- dmy(ElecGen$Year)
    
    
    p <-  plot_ly(ElecGen,x = ~ Year ) %>% 
      add_trace(data = ElecGen,
                x = ~ Year,
                y = ~ Generated,
                name = "Electricity Generated",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Electricity Generated: ",
                  round(ElecGen$Generated, digits = 0),
                  " GWh\nYear: ",
                  format(ElecGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecGen[which(ElecGen$Generated > 0 | ElecGen$Generated < 0),], 1),
        x = ~ Year,
        y = ~ `Generated`,
        legendgroup = "1",
        name = "Electricity Generated",
        text = paste0(
          "Electricity Generated: ",
          round(ElecGen[which(ElecGen$Generated > 0 | ElecGen$Generated < 0),][-1,]$Generated, digits = 0),
          " GWh\nYear: ",
          format(ElecGen[which(ElecGen$Generated > 0 | ElecGen$Generated < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(data = ElecGen,
                x = ~ Year,
                y = ~ Gross,
                name = "Gross electricity consumption",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Gross electricity consumption: ",
                  round(ElecGen$Gross, digits = 0),
                  " GWh\nYear: ",
                  format(ElecGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[4], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecGen[which(ElecGen$Gross > 0 | ElecGen$Gross < 0),], 1),
        x = ~ Year,
        y = ~ `Gross`,
        legendgroup = "2",
        name = "Gross electricity consumption",
        text = paste0(
          "Gross electricity consumption: ",
          round(ElecGen[which(ElecGen$Gross > 0 | ElecGen$Gross < 0),][-1,]$Gross, digits = 0),
          " GWh\nYear: ",
          format(ElecGen[which(ElecGen$Gross > 0 | ElecGen$Gross < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[4])
      ) %>% 
      add_trace(data = ElecGen,
                x = ~ Year,
                y = ~ Total,
                name = "Total electricity consumption",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Total electricity consumption: ",
                  round(ElecGen$Total, digits = 0),
                  " GWh\nYear: ",
                  format(ElecGen$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecGen[which(ElecGen$Total > 0 | ElecGen$Total < 0),], 1),
        x = ~ Year,
        y = ~ `Total`,
        legendgroup = "3",
        name = "Total electricity consumption",
        text = paste0(
          "Total electricity consumption: ",
          round(ElecGen[which(ElecGen$Total > 0 | ElecGen$Total < 0),][-1,]$Total, digits = 0),
          " GWh\nYear: ",
          format(ElecGen[which(ElecGen$Total > 0 | ElecGen$Total < 0),][-1,]$Year, "%Y")
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
                     range = c(min(ElecGen$Year)-100, max(ElecGen$Year)+100)),
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
  
  
  output$ElecGenTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec generation", skip = 12)
    
    Data <- Data[complete.cases(Data),]

    ElecGen <- as_tibble(Data)
    
    datatable(
      ElecGen,
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
        title = "Proportion of eligible homes with Wall Insulation",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of eligible homes with Wall Insulation',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of eligible homes with Wall Insulation')
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
  
  output$ElecGenImpactTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Wall insulation", skip = 20,  col_names = FALSE)
    
    Data <- as_tibble(t(Data))[c(1:3,7:8)]
    
    Data <- tail(Data, -1)
    
    names(Data) <- c("Year", "Median Saving in Consumption (kWh) - Cavity Wall Insulation", "Median percentage saving - Cavity Wall", "Median Saving in Consumption (kWh) - Solid Wall Insulation", "Median percentage saving - Solid Wall")
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    ElecGen <- Data
    
    datatable(
      ElecGen,
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
                                   tags$p(
                                     HTML(
                                       "<p>Electricity generated and consumed</p>"
                                     )
                                   )))
 })
  observeEvent(input$ToggleTable, {
    toggle("ElecGenTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ElecGenImpactTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ElecGen.png <- downloadHandler(
    filename = "ElecGen.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Elec generation", skip = 12)[c(1,2,5,6)]
      
      Data <- Data[complete.cases(Data),]
      
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      
      names(Data) <- c("Year", "Generated", "Gross", "Total")
      
      ElecGen <- as_tibble(Data)
      
      ### variables
      ChartColours <- c("#5d8be1", "#1d91c0",  "#5d8be1", "#253494" )
      sourcecaption = "Source: BEIS"
      plottitle = "Electricity generated and consumed"
      
      #ElecGen$TotalPercentage <- PercentLabel(ElecGen$Total)
      
      
      ElecGenChart <- ElecGen %>%
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
          x = mean(ElecGen$Year),
          y = max(ElecGen$Total),
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
          x = mean(ElecGen$Year),
          y = mean(ElecGen$Generated),
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
          x = mean(ElecGen$Year),
          y = mean(ElecGen$Gross),
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
            y = ElecGen$Total[which(ElecGen$Year == min(ElecGen$Year))],
            label = paste0(format(round(ElecGen$Total[which(ElecGen$Year == min(ElecGen$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = max(Year)+1,
            y = ElecGen$Total[which(ElecGen$Year == max(ElecGen$Year))],
            label = paste0(format(round(ElecGen$Total[which(ElecGen$Year == max(ElecGen$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        )+
        geom_point(
          aes(
            x = max(Year),
            y = ElecGen$Total[which(ElecGen$Year == max(ElecGen$Year))]
          ),
          colour = ChartColours[2],
          size = 4,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = min(Year)-1,
            y = ElecGen$Generated[which(ElecGen$Year == min(ElecGen$Year))],
            label = paste0(format(round(ElecGen$Generated[which(ElecGen$Year == min(ElecGen$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = max(Year)+1,
            y = ElecGen$Generated[which(ElecGen$Year == max(ElecGen$Year))],
            label = paste0(format(round(ElecGen$Generated[which(ElecGen$Year == max(ElecGen$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        )+
        geom_point(
          aes(
            x = max(Year),
            y = ElecGen$Generated[which(ElecGen$Year == max(ElecGen$Year))]
          ),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = min(Year)-1,
            y = ElecGen$Gross[which(ElecGen$Year == min(ElecGen$Year))],
            label = paste0(format(round(ElecGen$Gross[which(ElecGen$Year == min(ElecGen$Year))], digits = 0), big.mark = ","), "\nGWh"),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = max(Year)+1,
            y = ElecGen$Gross[which(ElecGen$Year == max(ElecGen$Year))],
            label = paste0(format(round(ElecGen$Gross[which(ElecGen$Year == max(ElecGen$Year))], digits = 0), big.mark = ","), "\nGWh"),
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
            y = ElecGen$Gross[which(ElecGen$Year == max(ElecGen$Year))]
          ),
          colour = ChartColours[4],
          family = "Century Gothic",
          size = 4
        )
      
      
      ElecGenChart
      
      ElecGenChart <-
        LinePercentChart(ElecGenChart,
                         ElecGen,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      ElecGenChart <- ElecGenChart +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )+
        ylim(-1000, max(ElecGen$Generated))
      
      
      ElecGenChart
      
      ggsave(
        file,
        plot =  ElecGenChart,
        width = 18,
        height = 12,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
