require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

GHGHeatOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
               column(8,
                      h3("Greenhouse gas emissions associated with heating of buildings", style = "color: #39ab2c;  font-weight:bold"),
                      h4(textOutput(ns('GHGHeatSubtitle')), style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('GHGHeat.png'), 'Download Graph', style="float:right")
               )),

             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenEnTgtPlot")),
             plotlyOutput(ns("GHGHeatPlot"))%>% withSpinner(color="#39ab2c"),
             HTML("<blockquote><p>*heat refers to business and industrial processes, public sector buildings and residential categories in the greenhouse gas inventory</p></blockquote>"),
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
GHGHeat <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("GHGHeat.R")


#####################################################################

  output$GHGHeatPlot <- renderPlotly  ({
    
    GHGHeat <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1,3)]
    
    names(GHGHeat) <- c("Year", "Renewables")
    
    GHGHeat %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGHeat <- as_tibble(GHGHeat)
    
    GHGHeat <- GHGHeat[complete.cases(GHGHeat),]
    
    GHGHeat <- GHGHeat[which(GHGHeat$Year >= 1998),]
    
    SectorTimeSeries <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
    
    SectorTimeSeries$Total <- rowSums(SectorTimeSeries[2:11])
    
    SectorTimeSeries <- SectorTimeSeries[c(1,12)]
    
    names(SectorTimeSeries)[1] <- "Year"
    
    GHGHeat <- merge(GHGHeat,SectorTimeSeries)
    
    ### Variables
    ChartColours <- c("#39ab2c", "#1c9099")
    sourcecaption = "Source: BEIS, SG"
    plottitle = "Energy productivity target progress"
    
    #GHGHeat$OilPercentage <- PercentLabel(GHGHeat$Oil)
    
    GHGHeat$Year <-
      paste0("01/01/", GHGHeat$Year)
    
    GHGHeat$Year <- dmy(GHGHeat$Year)
    
    p <-  plot_ly(GHGHeat, x = ~ Year) %>% 
      add_trace(
        y = ~ Renewables,
        name = "Heat Emissions",
        type = 'scatter',
        mode = 'lines',
        text = paste0(
          "Greenhouse gas emissions associated with heating of buildings: ",
          format(round(GHGHeat$Renewables, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(GHGHeat$Year, "%Y")
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>%
      add_trace(
        data = tail(GHGHeat[which(GHGHeat$Renewables > 0 | GHGHeat$Renewables < 0),],1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Heat Emissions",
        text = paste0(
          "Greenhouse gas emissions associated with heating of buildings: ",
          format(round(tail(GHGHeat[which(GHGHeat$Renewables > 0 | GHGHeat$Renewables < 0),],1)$Renewables, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(tail(GHGHeat[which(GHGHeat$Renewables > 0 | GHGHeat$Renewables < 0),],1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>%
      add_trace(
        data = GHGHeat,
        y = ~ Total,
        name = "Total",
        type = 'scatter',
        mode = 'lines',
        text = paste0(
          "Total Emissions: ",
          format(round(GHGHeat$Total, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(GHGHeat$Year, "%Y")
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[2], dash = "dash")
      ) %>%
      add_trace(
        data = tail(GHGHeat[which(GHGHeat$Total > 0 | GHGHeat$Total < 0),],1),
        x = ~ Year,
        y = ~ `Total`,
        name = "Total Emissions",
        text = paste0(
          "Total Emissions: ",
          format(round(tail(GHGHeat[which(GHGHeat$Total > 0 | GHGHeat$Total < 0),],1)$Total, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(tail(GHGHeat[which(GHGHeat$Total > 0 | GHGHeat$Total < 0),],1)$Year, "%Y")
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
                     range = c(min(GHGHeat$Year)-100, max(GHGHeat$Year)+100)),
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
    
    output$GHGHeatSubtitle <- renderText({
      
      GHGHeat <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1,3)]
      
      names(GHGHeat) <- c("Year", "Renewables")
      
      GHGHeat$Year <- as.numeric(GHGHeat$Year)
      
      GHGHeat <- GHGHeat[complete.cases(GHGHeat),]
      
      GHGHeat <- GHGHeat[which(GHGHeat$Year >= 1998),]
      
      paste("Scotland,", min(GHGHeat$Year),"-", max(GHGHeat$Year))
      
      
      
    })
    
    
    
    output$GHGHeat.png <- downloadHandler(
      filename = "GHGHeat.png",
      content = function(file) {
        
        GHGHeat <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1,3)]
        
        names(GHGHeat) <- c("Year", "Renewables")
        
        GHGHeat %<>% lapply(function(x) as.numeric(as.character(x)))
        
        GHGHeat <- as_tibble(GHGHeat)
        
        GHGHeat <- GHGHeat[complete.cases(GHGHeat),]
        
        SectorTimeSeries <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                       "\t", escape_double = FALSE, trim_ws = TRUE)
        
        SectorTimeSeries$Total <- rowSums(SectorTimeSeries[2:11])
        
        SectorTimeSeries <- SectorTimeSeries[c(1,12)]
        
        names(SectorTimeSeries)[1] <- "Year"
        
        GHGHeat <- merge(GHGHeat,SectorTimeSeries)
        
        GHGHeat <- GHGHeat[which(GHGHeat$Year >= 1998),]
        
        ### Variables
        ChartColours <- c("#39ab2c", "#1c9099")
        sourcecaption = "Source: SG"
        plottitle = "Greenhouse gas emissions associated with heating of buildings"
        
        
        GHGHeatChart <- GHGHeat %>%
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
            data = tail(GHGHeat, 1),
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
              label = "Heat\nEmissions",
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
            data = tail(GHGHeat, 1),
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
        
        
        GHGHeatChart <-
          LinePercentChart(GHGHeatChart,
                           GHGHeat,
                           plottitle,
                           sourcecaption,
                           ChartColours)
        
        GHGHeatChart <- GHGHeatChart +
          xlim(min(GHGHeat$Year) -1 , max(GHGHeat$Year) +1)+
          ylim(-.35,max(GHGHeat$Total)*1.05)+
          labs(subtitle = paste0("Scotland, ",min(GHGHeat$Year)," - ", max(GHGHeat$Year)))
        
        GHGHeatChart
        
        ggsave(
          file,
          plot =  GHGHeatChart,
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
                     paste(readtext("Structure/2 - Renewables/Heat/GHGHeat.txt")[2])
                     
                   )))
  })
  
  output$GHGTable = renderDataTable({
    
    GHGHeat <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1, 2, 3, 6, 7)]
    
    names(GHGHeat) <- c("Year", "Heat Emissions", "Heat (specifically relating to buildings) Emissions", "Transport Emissions", "Total")
    
    GHGHeat %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGHeat <- as_tibble(GHGHeat)
    
    GHGHeat <- GHGHeat[complete.cases(GHGHeat),]
    
    GHGHeat$Total <- rowSums(GHGHeat[2:5])
    
    GHGHeatBreakdown <- read_csv("Processed Data/Output/Greenhouse Gas/GHGHeatBreakdown.csv")
    
    names(GHGHeatBreakdown)[1] <- "Year"
    
    GHGHeatBreakdown$Year <- as.numeric(GHGHeatBreakdown$Year)
    
    GHGHeat <- merge(GHGHeat[which(GHGHeat$Year >= 1998),],GHGHeatBreakdown[which(GHGHeatBreakdown$Year >= 1998),])
    
    GHGHeat$`Proportion of total emissions` <- GHGHeat$`Heat (specifically relating to buildings) Emissions` / GHGHeat$Total
    
    
    GHGHeat <- GHGHeat[c(1,3,9,8,6,7)]
    
    datatable(
      GHGHeat,
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
      formatRound(2:6, 1) %>% 
      formatPercentage(3,1) %>% 
      formatStyle(2, fontWeight = "bold") %>% 
      formatStyle(4:6, fontStyle = "italic")
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("GHGTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  

}