require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

CarbonProdOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
                     column(8,
                      h3("Carbon productivity", style = "color: #1A5D38;  font-weight:bold"),
                      h4(textOutput(ns('CarbonProdSubtitle')), style = "color: #1A5D38;")
                     ),
                      column(
                           4, style = 'padding:15px;',
                           downloadButton(ns('CarbonProd.png'), 'Download Graph', style="float:right")
                         )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("RenEnTgtPlot")),
             plotlyOutput(ns("CarbonProdPlot"))%>% withSpinner(color="#1A5D38"),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    fluidRow(
      column(10,h3("Commentary", style = "color: #1A5D38;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    fluidRow(
      column(10, h3("Data", style = "color: #1A5D38;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("CarbonProdTable"))%>% withSpinner(color="#1A5D38"))),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("SGQNAS", "BEISSubNatEnergy", "BEISSubNatElec", "BEISSubNatGas", "BEISLocalRoad"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("SGQNAS"),
        SourceLookup("BEISSubNatEnergy"),
        SourceLookup("BEISSubNatElec"),
        SourceLookup("BEISSubNatGas"),
        SourceLookup("BEISLocalRoad")
        
        
      )
    )
  )
}




###### Server ######
CarbonProd <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("CarbonProd.R")


#####################################################################
  output$CarbonProdPlot <- renderPlotly  ({
    
    CarbonProd <- read_delim("Processed Data/Output/Carbon Productivity/Carbon Productivity.txt", 
                                      "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,4)]
    
    names(CarbonProd) <- c("Year", "Renewables")
    
    CarbonProd %<>% lapply(function(x) as.numeric(as.character(x)))
    
    CarbonProd <- as_tibble(CarbonProd)
    
    
    ### Variables
    ChartColours <- c("#1a5d38", "#FF8500")
    sourcecaption = "Source: BEIS, SG"
    plottitle = "Energy productivity target progress"
    
    #CarbonProd$OilPercentage <- PercentLabel(CarbonProd$Oil)
    
    CarbonProd$Year <-
      paste0("01/01/", CarbonProd$Year)
    
    CarbonProd$Year <- dmy(CarbonProd$Year)
    
    p <-  plot_ly(CarbonProd, x = ~ Year) %>% 
      add_trace(
      y = ~ Renewables,
      name = "Renewables",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "Carbon Productivity: ",
        format(round(CarbonProd$Renewables, 0), big.mark = ","),
        " \u00A3 GVA per tonne of CO2e\nYear: ",
        format(CarbonProd$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = tail(CarbonProd[which(CarbonProd$Renewables > 0 | CarbonProd$Renewables < 0),],1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewables",
        text = paste0(
          "Carbon Productivity: ",
          format(round(tail(CarbonProd[which(CarbonProd$Renewables > 0 | CarbonProd$Renewables < 0),],1)$Renewables, 0), big.mark = ","),
          " \u00A3 GVA per tonne of CO2e\nYear: ",
          format(tail(CarbonProd[which(CarbonProd$Renewables > 0 | CarbonProd$Renewables < 0),],1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(CarbonProd$Year)-100, max(CarbonProd$Year)+100)),
        yaxis = list(
          title = "\u00A3 GVA per tonne CO2e",
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
  
  output$CarbonProdSubtitle <- renderText({
    
    CarbonProd <- read_delim("Processed Data/Output/Carbon Productivity/Carbon Productivity.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)[c(1:4)]
    
    names(CarbonProd) <- c("Year", "Renewables")
    
    CarbonProd %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(CarbonProd$Year),"-", max(CarbonProd$Year))
    
    
    
  })

  output$CarbonProdTable = renderDataTable({
    
    CarbonProdData <- read_delim("Processed Data/Output/Carbon Productivity/Carbon Productivity.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)[c(1:4)]
    
    CarbonProdData %<>% lapply(function(x) as.numeric(as.character(x)))
    
    CarbonProdData <- as_tibble(CarbonProdData)
    
    names(CarbonProdData) <- c("Year", "GVA (\u00A3 million)", "Emissions (Million tonnes of CO2 equivalent)", "Carbon Productivity (\u00A3GVA per tonne of CO2e)")
    
    datatable(
      CarbonProdData,
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
        title = "Carbon Productivity",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Carbon Productivity',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Carbon Productivity')
        ),
        
        # customize the length menu
      lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:4, 0) %>% 
      formatRound(3, 2)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/1 - Whole System/CarbonProd.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("CarbonProdTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$CarbonProd.png <- downloadHandler(
    filename = "CarbonProd.png",
    content = function(file) {
      
      CarbonProd <- read_delim("Processed Data/Output/Carbon Productivity/Carbon Productivity.txt", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,4)]
      
      names(CarbonProd) <- c("Year", "Renewables")
      
      CarbonProd %<>% lapply(function(x) as.numeric(as.character(x)))
      
      CarbonProd <- as_tibble(CarbonProd)
      
      ### Variables
      ChartColours <- c("#1a5d38", "#FF8500")
      sourcecaption = "Source: SG"
      plottitle = "Carbon productivity"
      
      
      CarbonProdChart <- CarbonProd %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(
            y = Renewables,
            colour = ChartColours[2],
            label = percent(Renewables, 0.1)
          ),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == min(Year), paste0(format(round(Renewables, digits = 0), big.mark = ",", trim = TRUE), " \u00A3 GVA\ntonne of CO2e"), ""),
            hjust = 0.5,
            vjust = 1.1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == max(Year), paste0(format(round(Renewables, digits = 0), big.mark = ",", trim = TRUE), " \u00A3 GVA\ntonne of CO2e"), ""),
            hjust = 0.5,
            vjust = -.6,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(CarbonProd, 1),
          aes(
            x = Year,
            y = Renewables,
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
                             Year == min(Year), paste0(Year), ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      CarbonProdChart <-
        LinePercentChart(CarbonProdChart,
                         CarbonProd,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      CarbonProdChart <- CarbonProdChart +
        xlim(min(CarbonProd$Year) -1 , max(CarbonProd$Year) +1)+
        ylim(-30,max(CarbonProd$Renewables)*1.16)+
        labs(subtitle = paste0("Scotland, ",min(CarbonProd$Year)," - ", max(CarbonProd$Year)))
             
             CarbonProdChart
             
             ggsave(
               file,
               plot =  CarbonProdChart,
               width = 26,
               height = 12,
               units = "cm",
               dpi = 300
             )
             
             
    }
        )
}