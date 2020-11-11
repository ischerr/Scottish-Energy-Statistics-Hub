require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

GHGTransportOutput <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
               column(8,
                      h3("Transport emissions", style = "color: #39ab2c;  font-weight:bold"),
                      h4(textOutput(ns('GHGTransportSubtitle')), style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('GHGTransport.png'), 'Download Graph', style="float:right")
               )),

             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenEnTgtPlot")),
             plotlyOutput(ns("GHGTransportPlot"))%>% withSpinner(color="#39ab2c"),
             HTML("<blockquote><p>*transport refers to transport (excluding international) and international aviation and shipping categories in the greenhouse gas inventory</p></blockquote>"),
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
      column(12, dataTableOutput(ns("GHGTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
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
GHGTransport <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("GHGTransport.R")


#####################################################################
  output$GHGTransportPlot <- renderPlotly  ({
    
    GHGTransport <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1,6)]
    
    names(GHGTransport) <- c("Year", "Renewables")
    
    GHGTransport %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGTransport <- as_tibble(GHGTransport)
    
    GHGTransport <- GHGTransport[complete.cases(GHGTransport),]
    
    GHGTransport <- GHGTransport[which(GHGTransport$Year >= 1998),]
    
    SectorTimeSeries <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
    
    SectorTimeSeries$Total <- rowSums(SectorTimeSeries[2:11])
    
    SectorTimeSeries <- SectorTimeSeries[c(1,12)]
    
    names(SectorTimeSeries)[1] <- "Year"
    
    GHGTransport <- merge(GHGTransport,SectorTimeSeries)
    
    ### Variables
    ChartColours <- c("#39ab2c", "#1c9099")
    sourcecaption = "Source: BEIS, SG"
    plottitle = "Energy productivity target progress"
    
    #GHGTransport$OilPercentage <- PercentLabel(GHGTransport$Oil)
    
    GHGTransport$Year <-
      paste0("01/01/", GHGTransport$Year)
    
    GHGTransport$Year <- dmy(GHGTransport$Year)
    
    p <-  plot_ly(GHGTransport, x = ~ Year) %>% 
      add_trace(
        y = ~ Renewables,
        name = "Renewables",
        type = 'scatter',
        mode = 'lines',
        text = paste0(
          "Transport Emissions: ",
          format(round(GHGTransport$Renewables, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(GHGTransport$Year, "%Y")
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>%
      add_trace(
        data = tail(GHGTransport[which(GHGTransport$Renewables > 0 | GHGTransport$Renewables < 0),],1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewables",
        text = paste0(
          "Transport Emissions: ",
          format(round(tail(GHGTransport[which(GHGTransport$Renewables > 0 | GHGTransport$Renewables < 0),],1)$Renewables, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(tail(GHGTransport[which(GHGTransport$Renewables > 0 | GHGTransport$Renewables < 0),],1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>%
      add_trace(
        data = GHGTransport,
        y = ~ Total,
        name = "Total",
        type = 'scatter',
        mode = 'lines',
        text = paste0(
          "Total Emissions: ",
          format(round(GHGTransport$Total, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(GHGTransport$Year, "%Y")
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[2], dash = "dash")
      ) %>%
      add_trace(
        data = tail(GHGTransport[which(GHGTransport$Total > 0 | GHGTransport$Total < 0),],1),
        x = ~ Year,
        y = ~ `Total`,
        name = "Total Emissions",
        text = paste0(
          "Total Emissions: ",
          format(round(tail(GHGTransport[which(GHGTransport$Total > 0 | GHGTransport$Total < 0),],1)$Total, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(tail(GHGTransport[which(GHGTransport$Total > 0 | GHGTransport$Total < 0),],1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>%
      
      
      layout(
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(GHGTransport$Year)-100, max(GHGTransport$Year)+100)),
        yaxis = list(
          title = "MtCO2e",
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
    
    output$GHGTransportSubtitle <- renderText({
      
      GHGTransport <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1,6)]
      
      names(GHGTransport) <- c("Year", "Renewables")
      
      GHGTransport$Year <- as.numeric(GHGTransport$Year)
      
      GHGTransport <- GHGTransport[complete.cases(GHGTransport),]
      
      GHGTransport <- GHGTransport[which(GHGTransport$Year >= 1998),]
      
      paste("Scotland,", min(GHGTransport$Year),"-", max(GHGTransport$Year))
      
      
      
    })
    
    
    
    output$GHGTransport.png <- downloadHandler(
      filename = "GHGTransport.png",
      content = function(file) {
        
        GHGTransport <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1,6)]
        
        names(GHGTransport) <- c("Year", "Renewables")
        
        GHGTransport %<>% lapply(function(x) as.numeric(as.character(x)))
        
        GHGTransport <- as_tibble(GHGTransport)
        
        GHGTransport <- GHGTransport[complete.cases(GHGTransport),]
        
        SectorTimeSeries <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                       "\t", escape_double = FALSE, trim_ws = TRUE)
        
        SectorTimeSeries$Total <- rowSums(SectorTimeSeries[2:11])
        
        SectorTimeSeries <- SectorTimeSeries[c(1,12)]
        
        names(SectorTimeSeries)[1] <- "Year"
        
        GHGTransport <- merge(GHGTransport,SectorTimeSeries)
        
        GHGTransport <- GHGTransport[which(GHGTransport$Year >= 1998),]
        
        ### Variables
        ChartColours <- c("#39ab2c", "#1c9099")
        sourcecaption = "Source: SG"
        plottitle = "Transport emissions"
        
        
        GHGTransportChart <- GHGTransport %>%
          ggplot(aes(x = Year), family = "Century Gothic") +
          
          geom_line(
            aes(
              y = Renewables,
              
              label = percent(Renewables, 0.1)
            ),
            size = 1.5,
            colour = ChartColours[1],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = Year,
              y = Renewables,
              label = ifelse(Year == min(Year), paste0(format(round(Renewables, digits = 1), big.mark = ",", trim = TRUE), " MtCO2e"), ""),
              hjust = 0.5,
              vjust = 3.2,
              
              fontface = 2
            ),
            colour = ChartColours[1],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = Year,
              y = Renewables,
              label = ifelse(Year == max(Year), paste0(format(round(Renewables, digits = 1), big.mark = ",", trim = TRUE), " MtCO2e"), ""),
              hjust = 0.5,
              vjust = -1,
              fontface = 2
            ),
            colour = ChartColours[1],
            family = "Century Gothic"
          ) +
          geom_point(
            data = tail(GHGTransport, 1),
            aes(
              x = Year,
              y = Renewables,
              
              show_guide = FALSE
            ),
            size = 4,
            colour = ChartColours[1],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = mean(Year),
              y = mean(Renewables),
              label = "Transport\nEmissions",
              hjust = 0.5,
              vjust = -1,
              
              fontface = 2
            ),
            colour = ChartColours[1],
            family = "Century Gothic"
          )+
          
          
          geom_line(
            aes(
              y = Total,
              
              label = percent(Total, 0.1)
            ),
            size = 1.5,
            linetype = "dashed",
            colour = ChartColours[2],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = Year,
              y = Total,
              label = ifelse(Year == min(Year), paste0(format(round(Total, digits = 1), big.mark = ",", trim = TRUE), " MtCO2e"), ""),
              hjust = 0.5,
              vjust = 3.2,
              
              fontface = 2
            ),
            colour = ChartColours[2],
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = Year,
              y = Total,
              label = ifelse(Year == max(Year), paste0(format(round(Total, digits = 1), big.mark = ",", trim = TRUE), " MtCO2e"), ""),
              hjust = 0.5,
              vjust = -1,
              
              fontface = 2
            ),
            colour = ChartColours[2],
            family = "Century Gothic"
          ) +
          geom_point(
            data = tail(GHGTransport, 1),
            aes(
              x = Year,
              y = Total,
              
              show_guide = FALSE
            ),
            colour = ChartColours[2],
            size = 4,
            family = "Century Gothic"
          ) +
          geom_text(
            aes(
              x = mean(Year),
              y = mean(Total),
              label = "Total\nEmissions",
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
        
        
        GHGTransportChart <-
          LinePercentChart(GHGTransportChart,
                           GHGTransport,
                           plottitle,
                           sourcecaption,
                           ChartColours)
        
        GHGTransportChart <- GHGTransportChart +
          xlim(min(GHGTransport$Year) -1 , max(GHGTransport$Year) +1)+
          ylim(-.35,max(GHGTransport$Total)*1.05)+
          labs(subtitle = paste0("Scotland, ",min(GHGTransport$Year)," - ", max(GHGTransport$Year)))
        
        GHGTransportChart
        
        ggsave(
          file,
          plot =  GHGTransportChart,
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
                     paste(readtext("Structure/2 - Renewables/Transport/GHGTransport.txt")[2])
                     
                   )))
  })
  
  output$GHGTable = renderDataTable({
    
    GHGTransport <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1, 2, 3, 6, 7)]
    
    names(GHGTransport) <- c("Year", "Electricity Emissions", "Heat (specifically relating to buildings) Emissions", "Transport Emissions", "Total")
    
    GHGTransport %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGTransport <- as_tibble(GHGTransport)
    
    GHGTransport <- GHGTransport[complete.cases(GHGTransport),]
    
    GHGTransport$Total <- rowSums(GHGTransport[2:5])
    
    GHGTransportBreakdown <- read_csv("Processed Data/Output/Greenhouse Gas/GHGTransportBreakdown.csv")
    
    names(GHGTransportBreakdown)[1] <- "Year"
    
    GHGTransportBreakdown$Year <- as.numeric(GHGTransportBreakdown$Year)
    
    GHGTransport <- merge(GHGTransport[which(GHGTransport$Year >= 1998),],GHGTransportBreakdown[which(GHGTransportBreakdown$Year >= 1998),])
    
    GHGTransport$Proportion <- GHGTransport$`Transport Emissions` / GHGTransport$Total
    
    
    GHGTransport <- GHGTransport[c(1,4,8,7,6)]
    
    datatable(
      GHGTransport,
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
      formatRound(2:5, 1) %>% 
      formatPercentage(3,1) %>% 
      formatStyle(2, fontWeight = "bold") %>% 
      formatStyle(4:5, fontStyle = "italic")

  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("GHGTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  

}