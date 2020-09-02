require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

GHGElecHeatTransportOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
                     column(8,
                      h3("Electricity", style = "color: #1A5D38;  font-weight:bold"),
                      h4(textOutput(ns('GHGElectricitySubtitle')), style = "color: #1A5D38;")
                     ),
                      column(
                           4, style = 'padding:15px;',
                           downloadButton(ns('GHGElectricity.png'), 'Download Graph', style="float:right")
                         )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("RenEnTgtPlot")),
             plotlyOutput(ns("GHGElectricityPlot"))%>% withSpinner(color="#1A5D38"),
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
      column(12, dataTableOutput(ns("GHGElectricityTable"))%>% withSpinner(color="#1A5D38"))),
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
GHGElecHeatTransport <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("GHGElecHeatTransport.R")


#####################################################################
  output$GHGElectricityPlot <- renderPlotly  ({
    
    GHGElectricity <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                       "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,2)]
    
    names(GHGElectricity) <- c("Year", "Renewables")
    
    GHGElectricity %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGElectricity <- as_tibble(GHGElectricity)
    
    GHGElectricity <- GHGElectricity[complete.cases(GHGElectricity),]
    
    ### Variables
    ChartColours <- c("#1a5d38", "#FF8500")
    sourcecaption = "Source: BEIS, SG"
    plottitle = "Energy productivity target progress"
    
    #GHGElectricity$OilPercentage <- PercentLabel(GHGElectricity$Oil)
    
    GHGElectricity$Year <-
      paste0("01/01/", GHGElectricity$Year)
    
    GHGElectricity$Year <- dmy(GHGElectricity$Year)
    
    p <-  plot_ly(GHGElectricity, x = ~ Year) %>% 
      add_trace(
      y = ~ Renewables,
      name = "Renewables",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "Progress: ",
        format(round(GHGElectricity$Renewables, 0), big.mark = ","),
        "\nYear: ",
        format(GHGElectricity$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = tail(GHGElectricity[which(GHGElectricity$Renewables > 0 | GHGElectricity$Renewables < 0),],1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewables",
        text = paste0(
          "Progress: ",
          format(round(tail(GHGElectricity[which(GHGElectricity$Renewables > 0 | GHGElectricity$Renewables < 0),],1)$Renewables, 0), big.mark = ","),
          "\nYear: ",
          format(tail(GHGElectricity[which(GHGElectricity$Renewables > 0 | GHGElectricity$Renewables < 0),],1)$Year, "%Y")
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
                     range = c(min(GHGElectricity$Year)-100, max(GHGElectricity$Year)+100)),
        yaxis = list(
          title = "",
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
  
  output$GHGElectricitySubtitle <- renderText({
    
    GHGElectricity <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,2)]
    
    names(GHGElectricity) <- c("Year", "Renewables")
    
    GHGElectricity$Year <- as.numeric(GHGElectricity$Year)
    
    GHGElectricity <- GHGElectricity[complete.cases(GHGElectricity),]
    
    paste("Scotland,", min(GHGElectricity$Year),"-", max(GHGElectricity$Year))
    
    
    
  })

  output$GHGElectricityTable = renderDataTable({
    
    GHGElectricityData <- read_delim("Processed Data/Output/Carbon Productivity/Carbon Productivity.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)[c(1:4)]
    
    GHGElectricityData %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGElectricityData <- as_tibble(GHGElectricityData)
    
    
    datatable(
      GHGElectricityData,
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
  
    output$GHGElectricity.png <- downloadHandler(
    filename = "GHGElectricity.png",
    content = function(file) {
      
      GHGElectricity <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,2)]
      
      names(GHGElectricity) <- c("Year", "Renewables")
      
      GHGElectricity %<>% lapply(function(x) as.numeric(as.character(x)))
      
      GHGElectricity <- as_tibble(GHGElectricity)
      
      GHGElectricity <- GHGElectricity[complete.cases(GHGElectricity),]
      
      ### Variables
      ChartColours <- c("#1a5d38", "#FF8500")
      sourcecaption = "Source: SG"
      plottitle = "Carbon productivity"
      
      
      GHGElectricityChart <- GHGElectricity %>%
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
            label = ifelse(Year == min(Year), paste0(format(round(Renewables, digits = 0), big.mark = ",", trim = TRUE), " billion"), ""),
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
            y = Renewables,
            label = ifelse(Year == max(Year), paste0(format(round(Renewables, digits = 0), big.mark = ",", trim = TRUE), " billion"), ""),
            hjust = 0.5,
            vjust = -1,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(GHGElectricity, 1),
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
      
      
      GHGElectricityChart <-
        LinePercentChart(GHGElectricityChart,
                         GHGElectricity,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      GHGElectricityChart <- GHGElectricityChart +
        xlim(min(GHGElectricity$Year) -1 , max(GHGElectricity$Year) +1)+
        ylim(-.30,max(GHGElectricity$Renewables)*1.05)+
        labs(subtitle = paste0("Scotland, ",min(GHGElectricity$Year)," - ", max(GHGElectricity$Year)))
             
             GHGElectricityChart
             
             ggsave(
               file,
               plot =  GHGElectricityChart,
               width = 26,
               height = 12,
               units = "cm",
               dpi = 300
             )
             
             
    }
        )
    
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/8 - Greenhouse Gases/GHGElecHeatTransport.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("GHGElectricityTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  

}