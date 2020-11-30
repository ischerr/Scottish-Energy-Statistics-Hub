require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



GridEmissionsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Grid Emissions",
    fluidRow(column(8,
                    h3("Average greenhouse gas emissions per kilowatt hour of electricity", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('GridEmissionsSubtitle')), style = "color: #39ab2c;")
    ),
    column(
      4, style = 'padding:15px;',
      downloadButton(ns('GridEmissions.png'), 'Download Graph', style="float:right")
    )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("GridEmissionsPlot")),
    plotlyOutput(ns("GridEmissionsPlot"))%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Electricity Emissions",
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
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    
    
    fluidRow(
      column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Grid Emissions",
    fluidRow(
      column(10, h3("Data - Grid Emissions", style = "color: #39ab2c;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("GridEmissionsTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Electricity Emissions",
    fluidRow(
      column(10, h3("Data - Electricity emissions (MtCO2e)", style = "color: #39ab2c;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("GHGTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("BEISRenElec", "SGEmissionsPublic"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISElecGen"),
        SourceLookup("SGEmissionsPublic")
        
      )
    )
  )
}




###### Server ######
GridEmissions <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("GridEmissions.R")
  
  output$GridEmissionsSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Grid emissions", skip = 15)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    Data <- setDT(Data, keep.rownames = TRUE)[]
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    names(Data) <- c("Year", "Renewables")
    
    GridEmissions <- Data
    
    paste("Scotland,", min(GridEmissions$Year),"-", max(GridEmissions$Year))
  })
  
  output$GridEmissionsPlot <- renderPlotly  ({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Grid emissions", skip = 15)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    Data <- setDT(Data, keep.rownames = TRUE)[]
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data <- Data[c(1,2,6)]
    
    names(Data) <- c("Year", "Renewables", "UKEmissions")
    
    GridEmissions <- Data
    
    plottitle <- "Average greenhouse gas emissions per kilowatt hour of electricity"
    sourcecaption <- "Source: BEIS, SG"
    ChartColours <- c("#39ab2c", "#FF8500", "#2b8cbe")
    
    GridEmissions$Year <- paste0("01/01/", GridEmissions$Year)
    
    GridEmissions$Year <- dmy(GridEmissions$Year)
    
    
    p <-  plot_ly(GridEmissions,x = ~ Year ) %>% 
      add_trace(y = ~ Renewables,
                name = "Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0("Scotland: ",
                  round(GridEmissions$Renewables, digits = 1),
                  " gCO2e/kWh\nYear: ",
                  format(GridEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>%      
      add_trace(y = ~ UKEmissions,
                name = "UK",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0("UK: ",
                  round(GridEmissions$UKEmissions, digits = 1),
                  " gCO2e/kWh\nYear: ",
                  format(GridEmissions$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(GridEmissions[which(GridEmissions$Renewables > 0 | GridEmissions$Renewables < 0),], 1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Scotland",
        legendgroup = "1",
        text = paste0("Scotland: ",
          round(GridEmissions[which(GridEmissions$Renewables > 0 | GridEmissions$Renewables < 0),][-1,]$Renewables, digits = 1),
          " gCO2e/kWh\nYear: ",
          format(GridEmissions[which(GridEmissions$Renewables > 0 | GridEmissions$Renewables < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18,
                      color = ChartColours[1])
      ) %>%

      add_trace(
        data = tail(GridEmissions[which(GridEmissions$UKEmissions > 0 | GridEmissions$UKEmissions < 0),], 1),
        x = ~ Year,
        y = ~ `UKEmissions`,
        name = "UK",
        legendgroup = "2",
        text = paste0("UK: ",
          round(GridEmissions[which(GridEmissions$UKEmissions > 0 | GridEmissions$UKEmissions < 0),][-1,]$UKEmissions, digits = 1),
          " gCO2e/kWh\nYear: ",
          format(GridEmissions[which(GridEmissions$UKEmissions > 0 | GridEmissions$UKEmissions < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      add_annotations(
        x = dmy("01/01/2002"),
        y = 50,
        text = "<b>Ambition:\n50 gCO2e/kWh</b>",
        font = list(color = ChartColours[2],
                    family = "Century Gothic"),
        textposistion = "bottom right",
        showarrow = FALSE
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        shapes = list(list(
          type = "line",
          x0 = 0,
          x1 = 1, 
          xref = "paper",
          y0 = 50, 
          y1 = 50, 
          line = list(color = ChartColours[2],
                      dash = "dash")
        )
        ),
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(GridEmissions$Year)-100, max(GridEmissions$Year)+100)),
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
  
  
  output$GridEmissionsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Grid emissions", skip = 15)
    
    Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
    Data <- setDT(Data, keep.rownames = TRUE)[]
    colnames(Data) <- as.character(unlist(Data[1,]))
    Data = Data[-1, ]
    
    Data <- as_tibble(sapply( Data, as.numeric ))
    
    Data[3] <- NULL
    
    names(Data) <- c("Year", "Scottish Grid emissions (gCO2e/kWh)", "UK Energy supply emissions (MtCO2e)", "UK Electricity generated (GWh)", "UK Grid emissions (gCO2e/kWh)" )
    
    GridEmissions <- Data
    
    EnSupplyEmissions <- read_excel("Structure/CurrentWorking.xlsx", 
                                    sheet = "R - ElecProductionEmissions", col_names = FALSE)
    
    EnSupplyEmissions <- as_tibble(t(EnSupplyEmissions))
    
    names(EnSupplyEmissions) <- c("Year", "Emissions")
    
    EnSupplyEmissions$Year <- as.numeric(EnSupplyEmissions$Year)
    
    EnSupplyEmissions$Emissions <- as.numeric(EnSupplyEmissions$Emissions)
    
    ElecGeneration <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Elec generation", skip = 12)[1:2]
    
    GridEmissions <- merge(GridEmissions, ElecGeneration)
    
    GridEmissions <- merge(GridEmissions, EnSupplyEmissions)
    
    names(GridEmissions) <- c("Year", "Scottish Grid emissions (gCO2e/kWh)", "UK Energy supply emissions (MtCO2e)", "UK Electricity generated (GWh)", "UK Grid emissions (gCO2e/kWh)", 
                              "Scottish Electricity generated (GWh)", "Scottish Energy supply emissions (MtCO2e)")
    
    datatable(
      GridEmissions[c(1,2,6,7,5,4,3)],
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
        title = "Grid Emissions (Average Greenhouse Gas Emissions per kilowatt hour of electricity)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Grid Emissions (Average Greenhouse Gas Emissions per kilowatt hour of electricity)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Grid Emissions (Average Greenhouse Gas Emissions per kilowatt hour of electricity)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:7, 1) %>% 
      formatRound(c(3,6),0)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/GridEmissions.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("GridEmissionsTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$GridEmissions.png <- downloadHandler(
    filename = "GridEmissions.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Grid emissions", skip = 15)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      Data <- setDT(Data, keep.rownames = TRUE)[]
      colnames(Data) <- as.character(unlist(Data[1,]))
      Data = Data[-1, ]
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      Data <- Data[c(1,2,6)]
      
      names(Data) <- c("Year", "Renewables", "UKRenewables")
      
      GridEmissions <- Data
      
      GridEmissions$Renewables <-
        round(GridEmissions$Renewables, digits = 1)
      
      GridEmissions$UKRenewables <-
        round(GridEmissions$UKRenewables, digits = 1)
      
      plottitle <- "Average greenhouse gas emissions per kilowatt hour of electricity"
      sourcecaption <- "Source: BEIS, SG"
      ChartColours <- c("#39ab2c", "#FF8500", "#2b8cbe")
      
      GridEmissionsChart <-
        GridEmissions %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Renewables,
              
              label = Renewables),colour = ChartColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-1,
            y = Renewables,
            label = ifelse(Year == min(Year), paste(Renewables,"\ngCO2e/kWh"), ""),
            
            fontface = 2
          ),colour = ChartColours[1],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year+1,
            y = Renewables,
            label = ifelse(Year == max(Year), paste(sprintf("%.1f", Renewables),"\ngCO2e/kWh"), ""),
            
            fontface = 2
          ),colour = ChartColours[1],
          family = "Century Gothic",
          vjust = -.5
        ) +
        geom_point(
          data = tail(GridEmissions, 1),
          aes(
            x = Year,
            y = Renewables,
            
            label = Renewables,
            show_guide = FALSE
          ),colour = ChartColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(Renewables),
            label = "Scotland",
            
            fontface = 2
          ),colour = ChartColours[1],
          vjust = 1,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = UKRenewables,
              
              label = Renewables),colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year-1,
            y = UKRenewables,
            label = ifelse(Year == 2004, paste(UKRenewables,"\ngCO2e/kWh"), ""),
            
            fontface = 2
          ),colour = ChartColours[3],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year+1,
            y = UKRenewables,
            label = ifelse(Year == max(Year), paste(sprintf("%.1f", UKRenewables),"\ngCO2e/kWh"), ""),
            
            fontface = 2
          ),colour = ChartColours[3],
          family = "Century Gothic",
          vjust = -.5
        ) +
        geom_point(
          data = tail(GridEmissions, 1),
          aes(
            x = Year,
            y = UKRenewables,
            
            label = UKRenewables,
            show_guide = FALSE
          ),colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = mean(Year),
            y = mean(UKRenewables, na.rm = TRUE),
            label = "UK",
            fontface = 2
          ),colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year) | Year == 2004 |
                             Year == min(Year), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
           
            fontface = 2
          ), colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      
      GridEmissionsChart <-
        LinePercentChart(GridEmissionsChart,
                         GridEmissions,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      GridEmissionsChart <- GridEmissionsChart +
        xlim(min(GridEmissions$Year)-1.5,max(GridEmissions$Year)+1.5)+
        ylim(-17,max(GridEmissions$UKRenewables)+22) +
        geom_hline(yintercept = 50,
                   color = ChartColours[2],
                   linetype = 5)+
        annotate(
          "text",
          x = 2001,
          y = 50,
          label = "Ambition:\n50 gCO2e/kWh",
          fontface = 2,
          color = ChartColours[2],
          family = "Century Gothic"
        )
      
      GridEmissionsChart
      
      
      ggsave(
        file,
        plot = GridEmissionsChart,
        width = 26,
        height = 10,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$GHGElectricityPlot <- renderPlotly  ({
    
    GHGElectricity <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1,2)]
    
    names(GHGElectricity) <- c("Year", "Renewables")
    
    GHGElectricity %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGElectricity <- as_tibble(GHGElectricity)
    
    GHGElectricity <- GHGElectricity[complete.cases(GHGElectricity),]
    
    GHGElectricity <- GHGElectricity[which(GHGElectricity$Year >= 1998),]
    
    SectorTimeSeries <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
    
    SectorTimeSeries$Total <- rowSums(SectorTimeSeries[2:11])
    
    SectorTimeSeries <- SectorTimeSeries[c(1,12)]
    
    names(SectorTimeSeries)[1] <- "Year"
    
    GHGElectricity <- merge(GHGElectricity,SectorTimeSeries)
    
    ### Variables
    ChartColours <- c("#39ab2c", "#1c9099")
    sourcecaption = "Source: BEIS, SG"
    plottitle = "Energy productivity target progress"
    
    #GHGElectricity$OilPercentage <- PercentLabel(GHGElectricity$Oil)
    
    GHGElectricity$Year <-
      paste0("01/01/", GHGElectricity$Year)
    
    GHGElectricity$Year <- dmy(GHGElectricity$Year)
    
    p <-  plot_ly(GHGElectricity, x = ~ Year) %>% 
      add_trace(
        y = ~ Renewables,
        name = "Electricity Emissions",
        type = 'scatter',
        mode = 'lines',
        text = paste0(
          "Electricity Emissions: ",
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
        name = "Electricity Emissions",
        text = paste0(
          "Electricity Emissions: ",
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
      add_trace(
        data = GHGElectricity,
        y = ~ Total,
        name = "Total",
        type = 'scatter',
        mode = 'lines',
        text = paste0(
          "Total Emissions: ",
          format(round(GHGElectricity$Total, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(GHGElectricity$Year, "%Y")
        ),
        hoverinfo = 'text',
        line = list(width = 6, color = ChartColours[2], dash = "dash")
      ) %>%
      add_trace(
        data = tail(GHGElectricity[which(GHGElectricity$Total > 0 | GHGElectricity$Total < 0),],1),
        x = ~ Year,
        y = ~ `Total`,
        name = "Total Emissions",
        text = paste0(
          "Total Emissions: ",
          format(round(tail(GHGElectricity[which(GHGElectricity$Total > 0 | GHGElectricity$Total < 0),],1)$Total, 1), big.mark = ","),
          " MtCO2e\nYear: ",
          format(tail(GHGElectricity[which(GHGElectricity$Total > 0 | GHGElectricity$Total < 0),],1)$Year, "%Y")
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
    
    GHGElectricity <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1,2)]
    
    names(GHGElectricity) <- c("Year", "Renewables")
    
    GHGElectricity$Year <- as.numeric(GHGElectricity$Year)
    
    GHGElectricity <- GHGElectricity[complete.cases(GHGElectricity),]
    
    GHGElectricity <- GHGElectricity[which(GHGElectricity$Year >= 1998),]
    
    paste("Scotland,", min(GHGElectricity$Year),"-", max(GHGElectricity$Year))
    
    
    
  })
  
  output$GHGElectricity.png <- downloadHandler(
    filename = "GHGElectricity.png",
    content = function(file) {
      
      GHGElectricity <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1,2)]
      
      names(GHGElectricity) <- c("Year", "Renewables")
      
      GHGElectricity %<>% lapply(function(x) as.numeric(as.character(x)))
      
      GHGElectricity <- as_tibble(GHGElectricity)
      
      GHGElectricity <- GHGElectricity[complete.cases(GHGElectricity),]
      
      SectorTimeSeries <- read_delim("Processed Data/Output/Greenhouse Gas/SectorTimeSeries.csv", 
                                     "\t", escape_double = FALSE, trim_ws = TRUE)
      
      SectorTimeSeries$Total <- rowSums(SectorTimeSeries[2:11])
      
      SectorTimeSeries <- SectorTimeSeries[c(1,12)]
      
      names(SectorTimeSeries)[1] <- "Year"
      
      GHGElectricity <- merge(GHGElectricity,SectorTimeSeries)
      
      GHGElectricity <- GHGElectricity[which(GHGElectricity$Year >= 1998),]
      
      ### Variables
      ChartColours <- c("#39ab2c", "#1c9099")
      sourcecaption = "Source: SG"
      plottitle = "Electricity emissions"
      
      
      GHGElectricityChart <- GHGElectricity %>%
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
          data = tail(GHGElectricity, 1),
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
            label = "Electricity\nEmissions",
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
          data = tail(GHGElectricity, 1),
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
      
      
      GHGElectricityChart <-
        LinePercentChart(GHGElectricityChart,
                         GHGElectricity,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      GHGElectricityChart <- GHGElectricityChart +
        xlim(min(GHGElectricity$Year) -1 , max(GHGElectricity$Year) +1)+
        ylim(-.35,max(GHGElectricity$Total)*1.05)+
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
  
  output$GHGTable = renderDataTable({
    
    GHGElectricity <- read_csv("Processed Data/Output/Greenhouse Gas/GHGSector.csv")[c(1, 2, 3, 6, 7)]
    
    names(GHGElectricity) <- c("Year", "Electricity Emissions", "Heat (specifically relating to buildings) Emissions", "Transport Emissions", "Total")
    
    GHGElectricity %<>% lapply(function(x) as.numeric(as.character(x)))
    
    GHGElectricity <- as_tibble(GHGElectricity)
    
    GHGElectricity <- GHGElectricity[complete.cases(GHGElectricity),]
    
    GHGElectricity$Total <- rowSums(GHGElectricity[2:5])
    
    GHGElecBreakdown <- read_csv("Processed Data/Output/Greenhouse Gas/GHGElectricityBreakdown.csv")
    
    names(GHGElecBreakdown)[1] <- "Year"
    
    GHGElecBreakdown$Year <- as.numeric(GHGElecBreakdown$Year)
    
    GHGElectricity <- merge(GHGElectricity[which(GHGElectricity$Year >= 1998),],GHGElecBreakdown[which(GHGElecBreakdown$Year >= 1998),])
    
    GHGElectricity$`Proportion of total emissions` <- GHGElectricity$`Electricity Emissions` / GHGElectricity$Total
    
    
    GHGElectricity <- GHGElectricity[c(1,2,10,8,7,9,6)]
    
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
        title = "Electricity emissions (MtCO2e)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Electricity emissions (MtCO2e)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Electricity emissions (MtCO2e)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:7, 2) %>% 
      formatPercentage(3,1) %>% 
      formatStyle(2, fontWeight = "bold") %>% 
      formatStyle(4:7, fontStyle = "italic")
  })
  
  
}
