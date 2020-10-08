require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

GHGElecHeatTransportOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Electricity",
    fluidRow(
                     column(8,
                      h3("Electricity emissions", style = "color: #39ab2c;  font-weight:bold"),
                      h4(textOutput(ns('GHGElectricitySubtitle')), style = "color: #39ab2c;")
                     ),
                      column(
                           4, style = 'padding:15px;',
                           downloadButton(ns('GHGElectricity.png'), 'Download Graph', style="float:right")
                         )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenEnTgtPlot")),
             plotlyOutput(ns("GHGElectricityPlot"))%>% withSpinner(color="#39ab2c"),
             HTML("<blockquote><p>*electricity emissions refer to the power stations, autogenerators, public sector combustion and miscellaneous industrial/commercial combustion categories in the greenhouse gas inventory</p></blockquote>"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Heat",
             fluidRow(
               column(8,
                      h3("Heat (specifically relating to buildings) emissions", style = "color: #39ab2c;  font-weight:bold"),
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
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Transport",
             fluidRow(
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
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
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
    
    GHGElectricity <- GHGElectricity[which(GHGElectricity$Year >= 1998),]
    
    ### Variables
    ChartColours <- c("#39ab2c", "#FF8500")
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
        format(round(GHGElectricity$Renewables, 1), big.mark = ","),
        " MtCO2e\nYear: ",
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
          format(round(tail(GHGElectricity[which(GHGElectricity$Renewables > 0 | GHGElectricity$Renewables < 0),],1)$Renewables, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(tail(GHGElectricity[which(GHGElectricity$Renewables > 0 | GHGElectricity$Renewables < 0),],1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>%
      layout(
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(GHGElectricity$Year)-100, max(GHGElectricity$Year)+100)),
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
  
  output$GHGElectricitySubtitle <- renderText({
    
    GHGElectricity <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,2)]
    
    names(GHGElectricity) <- c("Year", "Renewables")
    
    GHGElectricity$Year <- as.numeric(GHGElectricity$Year)
    
    GHGElectricity <- GHGElectricity[complete.cases(GHGElectricity),]
    
    GHGElectricity <- GHGElectricity[which(GHGElectricity$Year >= 1998),]
    
    paste("Scotland,", min(GHGElectricity$Year),"-", max(GHGElectricity$Year))
    
    
    
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
      
      GHGElectricity <- GHGElectricity[which(GHGElectricity$Year >= 1998),]
      
      ### Variables
      ChartColours <- c("#39ab2c", "#FF8500")
      sourcecaption = "Source: SG"
      plottitle = "Electricity emissions"
      
      
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
            label = ifelse(Year == min(Year), paste0(format(round(Renewables, digits = 1), big.mark = ",", trim = TRUE), " MtCO2e"), ""),
            hjust = 0.5,
            vjust = 3.2,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == max(Year), paste0(format(round(Renewables, digits = 1), big.mark = ",", trim = TRUE), " MtCO2e"), ""),
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
    
    
    
    output$GHGHeatPlot <- renderPlotly  ({
      
      GHGHeat <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,3)]
      
      names(GHGHeat) <- c("Year", "Renewables")
      
      GHGHeat %<>% lapply(function(x) as.numeric(as.character(x)))
      
      GHGHeat <- as_tibble(GHGHeat)
      
      GHGHeat <- GHGHeat[complete.cases(GHGHeat),]
      
      GHGHeat <- GHGHeat[which(GHGHeat$Year >= 1998),]
      
      ### Variables
      ChartColours <- c("#39ab2c", "#FF8500")
      sourcecaption = "Source: BEIS, SG"
      plottitle = "Energy productivity target progress"
      
      #GHGHeat$OilPercentage <- PercentLabel(GHGHeat$Oil)
      
      GHGHeat$Year <-
        paste0("01/01/", GHGHeat$Year)
      
      GHGHeat$Year <- dmy(GHGHeat$Year)
      
      p <-  plot_ly(GHGHeat, x = ~ Year) %>% 
        add_trace(
          y = ~ Renewables,
          name = "Renewables",
          type = 'scatter',
          mode = 'lines',
          text = paste0(
            "Progress: ",
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
          name = "Renewables",
          text = paste0(
            "Progress: ",
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
      
      GHGHeat <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,3)]
      
      names(GHGHeat) <- c("Year", "Renewables")
      
      GHGHeat$Year <- as.numeric(GHGHeat$Year)
      
      GHGHeat <- GHGHeat[complete.cases(GHGHeat),]
      
      GHGHeat <- GHGHeat[which(GHGHeat$Year >= 1998),]
      
      paste("Scotland,", min(GHGHeat$Year),"-", max(GHGHeat$Year))
      
      
      
    })
    
    
    
    output$GHGHeat.png <- downloadHandler(
      filename = "GHGHeat.png",
      content = function(file) {
        
        GHGHeat <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                     "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,3)]
        
        names(GHGHeat) <- c("Year", "Renewables")
        
        GHGHeat %<>% lapply(function(x) as.numeric(as.character(x)))
        
        GHGHeat <- as_tibble(GHGHeat)
        
        GHGHeat <- GHGHeat[complete.cases(GHGHeat),]
        
        GHGHeat <- GHGHeat[which(GHGHeat$Year >= 1998),]
        
        ### Variables
        ChartColours <- c("#39ab2c", "#FF8500")
        sourcecaption = "Source: SG"
        plottitle = "Heat (specifically relating to buildings) emissions"
        
        
        GHGHeatChart <- GHGHeat %>%
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
              label = ifelse(Year == min(Year), paste0(format(round(Renewables, digits = 1), big.mark = ",", trim = TRUE), " MtCO2e"), ""),
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
              label = ifelse(Year == max(Year), paste0(format(round(Renewables, digits = 1), big.mark = ",", trim = TRUE), " MtCO2e"), ""),
              hjust = 0.5,
              vjust = -1,
              colour = ChartColours[2],
              fontface = 2
            ),
            family = "Century Gothic"
          ) +
          geom_point(
            data = tail(GHGHeat, 1),
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
        
        
        GHGHeatChart <-
          LinePercentChart(GHGHeatChart,
                           GHGHeat,
                           plottitle,
                           sourcecaption,
                           ChartColours)
        
        GHGHeatChart <- GHGHeatChart +
          xlim(min(GHGHeat$Year) -1 , max(GHGHeat$Year) +1)+
          ylim(-.30,max(GHGHeat$Renewables)*1.05)+
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
    
    
    output$GHGTransportPlot <- renderPlotly  ({
      
      GHGTransport <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,5)]
      
      names(GHGTransport) <- c("Year", "Renewables")
      
      GHGTransport %<>% lapply(function(x) as.numeric(as.character(x)))
      
      GHGTransport <- as_tibble(GHGTransport)
      
      GHGTransport <- GHGTransport[complete.cases(GHGTransport),]
      
      GHGTransport <- GHGTransport[which(GHGTransport$Year >= 1998),]
      
      ### Variables
      ChartColours <- c("#39ab2c", "#FF8500")
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
            "Progress: ",
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
            "Progress: ",
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
      
      GHGTransport <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,5)]
      
      names(GHGTransport) <- c("Year", "Renewables")
      
      GHGTransport$Year <- as.numeric(GHGTransport$Year)
      
      GHGTransport <- GHGTransport[complete.cases(GHGTransport),]
      
      GHGTransport <- GHGTransport[which(GHGTransport$Year >= 1998),]
      
      paste("Scotland,", min(GHGTransport$Year),"-", max(GHGTransport$Year))
      
      
      
    })
    
    
    
    output$GHGTransport.png <- downloadHandler(
      filename = "GHGTransport.png",
      content = function(file) {
        
        GHGTransport <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                     "\t", escape_double = FALSE, trim_ws = TRUE)[c(1,5)]
        
        names(GHGTransport) <- c("Year", "Renewables")
        
        GHGTransport %<>% lapply(function(x) as.numeric(as.character(x)))
        
        GHGTransport <- as_tibble(GHGTransport)
        
        GHGTransport <- GHGTransport[complete.cases(GHGTransport),]
        
        GHGTransport <- GHGTransport[which(GHGTransport$Year >= 1998),]
        
        ### Variables
        ChartColours <- c("#39ab2c", "#FF8500")
        sourcecaption = "Source: SG"
        plottitle = "Transport emissions"
        
        
        GHGTransportChart <- GHGTransport %>%
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
              label = ifelse(Year == min(Year), paste0(format(round(Renewables, digits = 1), big.mark = ",", trim = TRUE), " MtCO2e"), ""),
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
              label = ifelse(Year == max(Year), paste0(format(round(Renewables, digits = 1), big.mark = ",", trim = TRUE), " MtCO2e"), ""),
              hjust = 0.5,
              vjust = -1,
              colour = ChartColours[2],
              fontface = 2
            ),
            family = "Century Gothic"
          ) +
          geom_point(
            data = tail(GHGTransport, 1),
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
        
        
        GHGTransportChart <-
          LinePercentChart(GHGTransportChart,
                           GHGTransport,
                           plottitle,
                           sourcecaption,
                           ChartColours)
        
        GHGTransportChart <- GHGTransportChart +
          xlim(min(GHGTransport$Year) -1 , max(GHGTransport$Year) +1)+
          ylim(-.30,max(GHGTransport$Renewables)*1.05)+
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
                     paste(readtext("Structure/8 - Greenhouse Gases/GHGElecHeatTransport.txt")[2])
                     
                   )))
  })
  
  output$GHGTable = renderDataTable({
    
    GHGElectricity <- read_delim("Processed Data/Output/Greenhouse Gas/GHGElecHeatTransport.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)[c(1, 2, 3, 5, 4)]
    
    names(GHGElectricity) <- c("Year", "Electricity Emissions", "Heat (specifically relating to buildings) Emissions", "Transport Emissions", "Total")
    
    GHGElectricity %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGElectricity <- as_tibble(GHGElectricity)
    
    GHGElectricity <- GHGElectricity[complete.cases(GHGElectricity),]
    
    GHGElectricity$Total <- rowSums(GHGElectricity[2:5])
    
    
    datatable(
      GHGElectricity,
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
      formatRound(2:5, 1) 
  })
  
  
  observeEvent(input$ToggleTable, {
    toggle("GHGTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  

}