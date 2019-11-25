require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

ImportsExportsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("International sales from oil and gas supply chain", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('ImportsExportsSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ImportsExports.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("ImportsExportsPlot")),
    plotlyOutput(ns("ImportsExportsPlot"))%>% withSpinner(color="#5d8be1"),
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
      column(12, dataTableOutput(ns("ImportsExportsTable"))%>% withSpinner(color="#5d8be1"))),
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
ImportsExports <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ImportsExports.R")

  
  output$ImportsExportsSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec imports & exports", skip = 12)[c(10,15,16)]
    
    Data <- Data[complete.cases(Data),]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "Exports", "Imports")
    
    ImportsExports <- as_tibble(Data)
    
    
    paste("Scotland,", min(ImportsExports$Year),"-", max(ImportsExports$Year))
  })
  
  output$ImportsExportsPlot <- renderPlotly  ({
    
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec imports & exports", skip = 12)[c(10,15,16)]
    
    Data <- Data[complete.cases(Data),]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    names(Data) <- c("Year", "Exports", "Imports")
    
    ImportsExports <- as_tibble(Data)
    
    ### variables
    ChartColours <- c("#5d8be1", "#225ea8", "#41b6c4", "#8da0cb")
    sourcecaption = "Source: BEIS"
    plottitle = "Electricity imports and exports"
    
    ImportsExports$Year <- paste0("01/01/", ImportsExports$Year)
    
    ImportsExports$Year <- dmy(ImportsExports$Year)
    
    
    p <-  plot_ly(ImportsExports,x = ~ Year ) %>% 
      add_trace(data = ImportsExports,
                x = ~ Year,
                y = ~ Exports,
                name = "Electricity Exports",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Electricity Exports: ",
                  format(round(ImportsExports$Exports, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(ImportsExports$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ImportsExports[which(ImportsExports$Exports > 0 | ImportsExports$Exports < 0),], 1),
        x = ~ Year,
        y = ~ `Exports`,
        legendgroup = "1",
        name = "Electricity Exports",
        text = paste0(
          "Electricity Exports: ",
          format(round(ImportsExports[which(ImportsExports$Exports > 0 | ImportsExports$Exports < 0),][-1,]$Exports, digits = 0), big.mark = ","),
          " GWh\nYear: ",
          format(ImportsExports[which(ImportsExports$Exports > 0 | ImportsExports$Exports < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = ImportsExports,
                x = ~ Year,
                y = ~ Imports,
                name = "Electricity Imports",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "Electricity Imports: ",
                  format(round(ImportsExports$Imports, digits = 0), big.mark = ","),
                  " GWh\nYear: ",
                  format(ImportsExports$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1),
        x = ~ Year,
        y = ~ `Imports`,
        legendgroup = "2",
        name = "Electricity Imports",
        text = paste0(
          "Electricity Imports: ",
          format(round(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),][-1,]$Imports, digits = 0), big.mark = ","),
          " GWh\nYear: ",
          format(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      add_annotations( x = tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1)$Year,
                       y = tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1)$Exports-500,
                       xref = "x", yref = "y",
                       axref = "x", ayref = "y",
                       text = "",
                       showarrow = T,
                       ax = tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1)$Year,
                       ay = tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1)$Imports+500,
                       arrowcolor = ChartColours[1]
                       ) %>% 
     add_annotations(
       x = tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1)$Year - 500,
       y = ((tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1)$Exports - tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1)$Imports)/2)+tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1)$Imports,
       text = paste("<b>Net Exports:\n", 
                    format(round((tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1)$Exports - tail(ImportsExports[which(ImportsExports$Imports > 0 | ImportsExports$Imports < 0),], 1)$Imports), digits = 0), big.mark = ","), "GWh</b>"
                    ),
       showarrow = FALSE,
       font = list(color = ChartColours[1],
                   size = 15)
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
                     range = c(min(ImportsExports$Year)-100, max(ImportsExports$Year)+100)),
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
  
  
  output$ImportsExportsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec imports & exports", col_names = FALSE, skip = 12)[10:17]
    
    
    names(Data) <- as.character(unlist(Data[1,]))
    
    Data <- Data[-1,]
    Data[1:8] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    ImportsExports <- as_tibble(Data)
    
    
    datatable(
      ImportsExports[complete.cases(ImportsExports),],
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
        title = "Electricity imports and exports (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Electricity imports and exports (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Electricity imports and exports (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:8, 0)
  })
  
  output$ImportsExportsQuarterTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Elec imports & exports", col_names = FALSE, skip = 12)[1:8]
    
    
    names(Data) <- as.character(unlist(Data[1,]))
    
    Data <- Data[-1,]
    Data[2:8] %<>% lapply(function(x) as.numeric(as.character(x)))

    ImportsExports <- as_tibble(Data)
    
    datatable(
      ImportsExports,
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
        title = "Quarterly Electricity imports and exports (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Quarterly Electricity imports and exports (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Quarterly Electricity imports and exports (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:8, 0)
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
    toggle("ImportsExportsTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ImportsExportsQuarterTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ImportsExports.png <- downloadHandler(
    filename = "ImportsExports.png",
    content = function(file) {


      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Elec imports & exports", skip = 12)[c(10,15,16)]
      
      Data <- Data[complete.cases(Data),]
      
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      
      names(Data) <- c("Year", "Exports", "Imports")
      
      ImportsExports <- as_tibble(Data)
      
      ### variables
      ChartColours <- c("#5d8be1", "#225ea8", "#41b6c4", "#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = "Electricity imports and exports"
      
      #ImportsExports$ExportsPercentage <- PercentLabel(ImportsExports$Exports)
      
      
      ImportsExportsChart <- ImportsExports %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = Exports,
            label = percent(Exports)
          ),
          size = 1.5,
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Exports,
            label = ifelse(Year == min(Year), paste0(format(round(Exports, digits = 0),big.mark=",")," GWh"), ""),
            hjust = 0.5,
            vjust = -1,
            
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Exports,
            label = ifelse(Year == max(Year), paste0(format(round(Exports, digits = 0),big.mark=",")," GWh"), ""),
            vjust = -1,
            
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ImportsExports, 1),
          aes(
            x = Year,
            y = Exports,
            
            show_guide = FALSE
          ),
          size = 4,
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(Exports),
            label = "Exports",
            hjust = 0.5,
            vjust = -4,
            
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_line(
          aes(
            y = `Imports`,
            
            label = paste0(`Imports` * 100, "%")
          ),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Imports`,
            label = ifelse(Year == min(Year), paste0(format(round(`Imports`, digits = 0),big.mark=","), " GWh"), ""),
            hjust = 0.5,
            vjust = -1.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = `Imports`,
            label = ifelse(Year == max(Year), paste0(format(round(`Imports`, digits = 0),big.mark=","), " GWh"), ""),
            vjust = 2,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ImportsExports, 1),
          aes(
            x = Year,
            y = `Imports`,
            
            show_guide = FALSE
          ),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(`Imports`),
            label = "Imports",
            hjust = 0.5,
            vjust = -4,
            
            fontface = 2
          ),
          colour = ChartColours[3],
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
        ) +
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        )+
        geom_segment(
          aes(
            y = ImportsExports[which(ImportsExports$Year == max(ImportsExports$Year)),]$Exports - 300,
            yend = ImportsExports[which(ImportsExports$Year == max(ImportsExports$Year)),]$Imports + 300,
            x = ImportsExports[which(ImportsExports$Year == max(ImportsExports$Year)),]$Year,
            xend = ImportsExports[which(ImportsExports$Year == max(ImportsExports$Year)),]$Year
          ),
          arrow = arrow(length = unit(0.3, "cm"), 
                        ends = "both"),
          colour = ChartColours[1],
          family = "Century Gothic"
        )+ 
        geom_text(
          aes(
            x = ImportsExports[which(ImportsExports$Year == max(ImportsExports$Year)),]$Year - 2,
            y = ((ImportsExports[which(ImportsExports$Year == max(ImportsExports$Year)),]$Exports - ImportsExports[which(ImportsExports$Year == max(ImportsExports$Year)),]$Imports)/2)+ImportsExports[which(ImportsExports$Year == max(ImportsExports$Year)),]$Imports,
            label = paste0("Net exports:\n",
                           format(round(ImportsExports$Exports[which(ImportsExports$Year == max(ImportsExports$Year))]-ImportsExports$Imports[which(ImportsExports$Year == max(ImportsExports$Year))], digits = 0), big.mark = ","),
                           " GWh")
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          fontface = 2
        )
      
      ImportsExports[which((ImportsExports$Exports - ImportsExports$Imports) == max(ImportsExports$Exports - ImportsExports$Imports)),]$Exports
      ImportsExportsChart <-
        TimeChart(ImportsExportsChart,
                  ImportsExports,
                  plottitle,
                  sourcecaption,
                  ChartColours)
      
      ImportsExportsChart <- ImportsExportsChart +
        xlim(1999,2019) +
        labs(subtitle = paste("Scotland,", min(ImportsExports$Year), "-", max(ImportsExports$Year)))
      
      ImportsExportsChart
      ggsave(
        file,
        plot =  ImportsExportsChart,
        width = 14,
        height = 14,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}
