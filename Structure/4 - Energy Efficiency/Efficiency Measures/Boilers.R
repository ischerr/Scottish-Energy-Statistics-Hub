require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



BoilersOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Proportion of homes with boiler improvements", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('BoilersSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('Boilers.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("BoilersPlot")),
    plotlyOutput(ns("BoilersPlot"))%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    tabsetPanel(
      tabPanel("Proportion with Boiler Improvements",
               fluidRow(
    column(10, h3("Data - Proportion with Boiler Improvements", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("BoilersTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;")),
    tabPanel("Impact of Measures",
             fluidRow(
               column(10, h3("Data - Impact of Measures", style = "color: #34d1a3;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("BoilersImpactTable"))%>% withSpinner(color="#34d1a3"))),
             tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("SGSHCS", "BEISNEED"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("SGSHCS"),
        SourceLookup("BEISNEED")
        
      )
    )
  )
}




###### Server ######
Boilers <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("Boilers.R")

  
  output$BoilersSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Boilers", skip = 17, col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    Data <- Data[c(1,4,5,6)]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    names(Data) <- c("Year", "New Boilers (post-1998)", "Condensing Boilers", "Standards Compliant Boilers")
    
    Data <- subset(Data, Data$Year >= 2009)
    
    Boilers <- Data
    
    paste("Scotland,", min(Boilers$Year),"-", max(Boilers$Year))
  })
  
  output$BoilersPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Boilers", skip = 17, col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    Data <- Data[c(1,4,5,6)]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    names(Data) <- c("Year", "New Boilers (post-1998)", "Condensing Boilers", "Standards Compliant Boilers")
    
    Data <- subset(Data, Data$Year >= 2009)
    
    Boilers <- Data
    
    ### variables
    ChartColours <- c("#34d1a3", "#0868ac", "#4eb3d3", "#a8ddb5")
    sourcecaption = "Source: SG"
    plottitle = "Proportion of homes with boiler improvements"
    
    Boilers$Year <- paste0("01/01/", Boilers$Year)
    
    Boilers$Year <- dmy(Boilers$Year)
    
    
    p <-  plot_ly(Boilers,x = ~ Year ) %>% 
      add_trace(data = Boilers,
                x = ~ Year,
                y = ~ `New Boilers (post-1998)`,
                name = "New Boilers (post-1998)",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "New Boilers (post-1998): ",
                  percent(Boilers$`New Boilers (post-1998)`, accuracy = 0.1),
                  "\nYear: ",
                  format(Boilers$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(Boilers[which(Boilers$`New Boilers (post-1998)` > 0 | Boilers$`New Boilers (post-1998)` < 0),], 1),
        x = ~ Year,
        y = ~ `New Boilers (post-1998)`,
        legendgroup = "1",
        name = "New Boilers (post-1998)",
        text = paste0(
          "New Boilers (post-1998): ",
          percent(Boilers[which(Boilers$`New Boilers (post-1998)` > 0 | Boilers$`New Boilers (post-1998)` < 0),][-1,]$`New Boilers (post-1998)`, accuracy = 0.1),
          "\nYear: ",
          format(Boilers[which(Boilers$`New Boilers (post-1998)` > 0 | Boilers$`New Boilers (post-1998)` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>% 
      add_trace(data = Boilers,
                x = ~ Year,
                y = ~ `Condensing Boilers`,
                name = "Condensing Boilers",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Condensing Boilers: ",
                  percent(Boilers$`Condensing Boilers`, accuracy = 0.1),
                  "\nYear: ",
                  format(Boilers$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(Boilers[which(Boilers$`Condensing Boilers` > 0 | Boilers$`Condensing Boilers` < 0),], 1),
        x = ~ Year,
        y = ~ `Condensing Boilers`,
        legendgroup = "2",
        name = "Condensing Boilers",
        text = paste0(
          "Condensing Boilers: ",
          percent(Boilers[which(Boilers$`Condensing Boilers` > 0 | Boilers$`Condensing Boilers` < 0),][-1,]$`Condensing Boilers`, accuracy = 0.1),
          "\nYear: ",
          format(Boilers[which(Boilers$`Condensing Boilers` > 0 | Boilers$`Condensing Boilers` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = Boilers,
                x = ~ Year,
                y = ~ `Standards Compliant Boilers`,
                name = "Standards Compliant Boilers",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "Standards Compliant Boilers: ",
                  percent(Boilers$`Standards Compliant Boilers`, accuracy = 0.1),
                  "\nYear: ",
                  format(Boilers$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(Boilers[which(Boilers$`Standards Compliant Boilers` > 0 | Boilers$`Standards Compliant Boilers` < 0),], 1),
        x = ~ Year,
        y = ~ `Standards Compliant Boilers`,
        legendgroup = "3",
        name = "Standards Compliant Boilers",
        text = paste0(
          "Standards Compliant Boilers: ",
          percent(Boilers[which(Boilers$`Standards Compliant Boilers` > 0 | Boilers$`Standards Compliant Boilers` < 0),][-1,]$`Standards Compliant Boilers`, accuracy = 0.1),
          "\nYear: ",
          format(Boilers[which(Boilers$`Standards Compliant Boilers` > 0 | Boilers$`Standards Compliant Boilers` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#34d1a3"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',

        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(Boilers$Year)-100, max(Boilers$Year)+100)),
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
  
  
  output$BoilersTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Boilers", skip = 17, col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    Data <- Data[c(1,2,3,4,5,6)]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    names(Data) <- c("Year", "Proportion of homes using a gas or oil boiler for heating" ,"...of which...","New Boilers (post-1998)", "Condensing Boilers", "Standards Compliant Boilers")
    
    Data <- subset(Data, Data$Year >= 2009)
    
    Boilers <- Data
    
    datatable(
      Boilers,
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
        title = "Proportion of eligible homes with Boiler Improvements",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Proportion of eligible homes with Boiler Improvements',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Proportion of eligible homes with Boiler Improvements')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:6, 0)
  })
  
  output$BoilersImpactTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Boilers", skip = 27, col_names = FALSE)
    
    Data <- as_tibble(t(Data))
    
    Data[1,1] <- "Year"
    
    names(Data)<-  as.character(unlist(Data[1,]))
    
    Data <- Data[-1,]
    
    Boilers <- as_tibble(sapply( Data, as.numeric ))
    
    datatable(
      Boilers,
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
        title = "Impact of Measures - Boiler Improvements",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Impact of Measures - Boiler Improvements',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Impact of Measures - Boiler Improvements')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(3, 1) %>% 
      formatRound(2, 0)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/Efficiency Measures/Boilers.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("BoilersTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("BoilersImpactTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$Boilers.png <- downloadHandler(
    filename = "Boilers.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Boilers", skip = 17, col_names = FALSE)
      
      Data <- as_tibble(t(Data))
      
      Data <- Data[c(1,4,5,6)]
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      names(Data) <- c("Year", "New Boilers (post-1998)", "Condensing Boilers", "Standards Compliant Boilers")
      
      Data <- subset(Data, Data$Year >= 2009)
      
      BoilerProportion <- Data
      
      ### variables
      ChartColours <- c("#34d1a3", "#0868ac", "#4eb3d3", "#a8ddb5")
      sourcecaption = "Source: SG"
      plottitle = "Proportion of homes with boiler improvements"
      
      #BoilerProportion$`Low Carbon`Percentage <- PercentLabel(BoilerProportion$`Low Carbon`)
      
      
      BoilerProportionChart <- BoilerProportion %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = `New Boilers (post-1998)`,
            colour = ChartColours[2],
            label = percent(`New Boilers (post-1998)`)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `New Boilers (post-1998)`,
            label = ifelse(Year == min(Year[which(BoilerProportion$`New Boilers (post-1998)` > 0)]), percent(`New Boilers (post-1998)`,1), ""),
            hjust = 0.5,
            vjust = -.8,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `New Boilers (post-1998)`,
            label = ifelse(Year == max(Year), percent(`New Boilers (post-1998)`,1), ""),
            hjust = .5,
            vjust = 1.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(BoilerProportion, 1),
          aes(
            x = Year,
            y = `New Boilers (post-1998)`,
            colour = ChartColours[2],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year[which(BoilerProportion$`New Boilers (post-1998)` > 0)]),
            y = mean(`New Boilers (post-1998)`, na.rm = TRUE),
            label = "New Boilers\n(post-1998)",
            hjust = .7,
            vjust = -0.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `Condensing Boilers`,
            colour = ChartColours[3],
            label = paste0(`Condensing Boilers` * 100, "%")
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Condensing Boilers`,
            label = ifelse(Year == min(Year), percent(`Condensing Boilers`, 1), ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Condensing Boilers`,
            label = ifelse(Year == max(Year), percent(`Condensing Boilers`, 1), ""),
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(BoilerProportion, 1),
          aes(
            x = Year,
            y = `Condensing Boilers`,
            colour = ChartColours[3],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year[which(BoilerProportion$`Condensing Boilers` > 0)]),
            y = mean(`Condensing Boilers`, na.rm = TRUE),
            label = "Condensing\nBoilers",
            hjust = 0.5,
            vjust = -.5,
            colour = ChartColours[3],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `Standards Compliant Boilers`,
            colour = ChartColours[4],
            label = paste0(`Standards Compliant Boilers` * 100, "%")
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Standards Compliant Boilers`,
            label = ifelse(Year == min(Year[which(BoilerProportion$`Standards Compliant Boilers` > 0)]), percent(`Standards Compliant Boilers`,1), ""),
            hjust = 0.5,
            vjust = 1.2,
            colour = ChartColours[4],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Standards Compliant Boilers`,
            label = ifelse(Year == max(Year), percent(`Standards Compliant Boilers`,1), ""),
            hjust = 0.5,
            vjust = 2,
            colour = ChartColours[4],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(BoilerProportion, 1),
          aes(
            x = Year,
            y = `Standards Compliant Boilers`,
            colour = ChartColours[4],
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year[which(BoilerProportion$`Standards Compliant Boilers` > 0)]+1),
            y = mean(`Standards Compliant Boilers`, na.rm = TRUE),
            label = "Standards Compliant\nBoilers",
            hjust = 0.5,
            vjust = 2.2,
            colour = ChartColours[4],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = BoilerProportion$Year,
          y = 0,
          label = ifelse(
            BoilerProportion$Year == max(BoilerProportion$Year) |
              BoilerProportion$Year == min(BoilerProportion$Year[which(BoilerProportion$`New Boilers (post-1998)` > 0)]) |
              BoilerProportion$Year == min(BoilerProportion$Year[which(BoilerProportion$`Condensing Boilers` > 0)]) |
              BoilerProportion$Year == min(BoilerProportion$Year[which(BoilerProportion$`Standards Compliant Boilers` > 0)]),
            BoilerProportion$Year,
            ""
          ),
          hjust = 0.5,
          vjust = 1.5,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        )
      
      
      BoilerProportionChart <-
        LinePercentChart(BoilerProportionChart,
                         BoilerProportion,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      BoilerProportionChart
      
      ggsave(
        file,
        plot =  BoilerProportionChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
