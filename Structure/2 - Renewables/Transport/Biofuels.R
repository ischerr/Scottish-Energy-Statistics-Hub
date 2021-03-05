require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



BiofuelsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Share of biofuels in transport petrol and diesel consumption", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('BiofuelsSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('Biofuels.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("BiofuelsPlot")),
    plotlyOutput(ns("BiofuelsPlot"))%>% withSpinner(color="#39ab2c"),
    p("*A Scotland only figure is not available."),
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
      column(12, dataTableOutput(ns("BiofuelsTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("DFTRenewable"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("DFTRenewable"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("DFTRenewable")
        
      )
    )
  )
}




###### Server ######
Biofuels <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("Biofuels.R")
  
  output$BiofuelsSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                           sheet = "Biofuels in transport", col_names = FALSE, 
                           skip = 12, n_max = 15)
    
    Data <- as.data.frame(t(Data))
    
    Data <- tail(Data[c(1,4)],-1)
    
    names(Data) <- c("Year", "Renewables")
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Biofuels <- as_tibble(Data)
    
    paste("United Kingdom*,", min(Biofuels$Year),"-", max(Biofuels$Year[which(Biofuels$Renewables != 0)]))
  })
  
  output$BiofuelsPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Biofuels in transport", col_names = FALSE, 
                       skip = 12, n_max = 15)
    
    Data <- as.data.frame(t(Data))
    
    Data <- tail(Data[c(1,4)],-1)
    
    names(Data) <- c("Year", "Renewables")
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Biofuels <- as_tibble(Data)
    
    plottitle <- "Share of biofuels in transport petrol\nand diesel consumption"
    sourcecaption <- "Source: DfT, HMRC"
    ChartColours <- c("#39ab2c", "#FF8500")
    
    Biofuels$Year <- paste0("01/01/", Biofuels$Year)
    
    Biofuels$Year <- dmy(Biofuels$Year)
    
    
    p <-  plot_ly(Biofuels,x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "Biofuels",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Proportion: ",
                  percent(Biofuels$Renewables, accuracy = 0.01),
                  "\nYear: ",
                  format(Biofuels$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(Biofuels[which(Biofuels$Renewables > 0 | Biofuels$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Biofuels",
        legendgroup = "1",
        text = paste0(
          "Proportion: ",
          percent(Biofuels[which(Biofuels$Renewables > 0 | Biofuels$Renewables < 0),][-1,]$Renewables, accuracy = 0.01),
          "\nYear: ",
          format(Biofuels[which(Biofuels$Renewables > 0 | Biofuels$Renewables < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
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
                     range = c(min(Biofuels$Year)-100, max(Biofuels$Year)+100)),
        yaxis = list(
          title = "",
          tickformat = ".1%",
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
  
  
  output$BiofuelsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Biofuels in transport", col_names = FALSE, 
                       skip = 12, n_max = 15)
    
    Data <- as.data.frame(t(Data))
    
    Data <- tail(Data[c(1,4)],-1)
    
    names(Data) <- c("Year", "% of Biofuels")
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Biofuels <- as_tibble(Data)
    
    datatable(
      Biofuels,
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
        title = "Share of biofuels in transport petrol and diesel consumption",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Share of biofuels in transport petrol and diesel consumption',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Share of biofuels in transport petrol and diesel consumption')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2, 2) 
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Transport/Biofuels.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("BiofuelsTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$Biofuels.png <- downloadHandler(
    filename = "Biofuels.png",
    content = function(file) {

      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                             sheet = "Biofuels in transport", col_names = FALSE, 
                             skip = 12, n_max = 15)
      
      Data <- as.data.frame(t(Data))
      
      Data <- tail(Data[c(1,4)],-1)
      
      names(Data) <- c("Year", "Renewables")
      
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      
      Biofuels <- as_tibble(Data)
      
      plottitle <- "Share of biofuels in transport petrol\nand diesel consumption"
      sourcecaption <- "Source: DfT"
      ChartColours <- c("#39ab2c", "#FF8500")
      
      BiofuelsChart <-
        Biofuels %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(
            y = Renewables,
            colour = ChartColours[1],
            label = percent(Renewables, 0.01)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == min(Year), percent(Renewables, 0.01), ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == max(Year), percent(Renewables, 0.01), ""),
            hjust = 0.5,
            vjust = -1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(Biofuels, 1),
          aes(
            x = Year,
            y = Renewables,
            colour = ChartColours[1],
            label = percent(Renewables, 0.01),
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
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      BiofuelsChart <-
        LinePercentChart(BiofuelsChart,
                         Biofuels,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      BiofuelsChart <- BiofuelsChart +
        ylim(0, .1) +
        labs(subtitle = paste("United Kingdom,", min(Biofuels$Year), "-", max(Biofuels$Year)))
      
      BiofuelsChart

      ggsave(
        file,
        plot = BiofuelsChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
}
