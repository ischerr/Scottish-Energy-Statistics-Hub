require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenElecOverviewOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Onshore Wind",
               fluidRow(column(8,
                               h3("Quarterly Electricity generated from renewable sources", style = "color: #39ab2c;  font-weight:bold"),
                               h4("Scotland, 2018", style = "color: #39ab2c;")
               )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               fluidRow(column(4,
                 plotlyOutput(ns("OnshoreWindGenPiePlot"))%>% withSpinner(color="#39ab2c")),
                 column(4,
                 plotlyOutput(ns("OnshoreWindCapPiePlot"))%>% withSpinner(color="#39ab2c")),
                 column(4,
                 plotlyOutput(ns("OnshoreWindPipePiePlot"))%>% withSpinner(color="#39ab2c"))
               ),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Quarterly Electrical Generation",
               fluidRow(
                 column(10, h3("Data - Quarterly electrical generation", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable5"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("RenElecQuarterTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Electricity Generated",
             fluidRow(
               column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("RenElecFuelGenTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Data - Scottish Proportion",
             fluidRow(
               column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ScotRenGenTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Wind",
             fluidRow(
               column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EUWindTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Hydro",
             fluidRow(
               column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EUHydroTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),
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
        SourceLookup("BEISRenElec"),
        SourceLookup("EURORenEn"),
        SourceLookup("BEISSubNatEnergy")
        
      )
    )
  )
}




###### Server ######
RenElecOverview <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecFuel.R")

######
  
    RenElecGenFuel <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Renewable elec by fuel",
                               col_names = TRUE,
                               skip = 12
  )
  
  RenElecGenFuel<- RenElecGenFuel[complete.cases(RenElecGenFuel),]
  
  RenElecGenFuel<-RenElecGenFuel[dim(RenElecGenFuel)[1]:1,]
  
  RenElecGenFuel <- distinct(RenElecGenFuel, Year, .keep_all = TRUE)
  
  RenElecGenFuel <- head(RenElecGenFuel, -2)
  
  RenElecGenFuel<-RenElecGenFuel[dim(RenElecGenFuel)[1]:1,]
  
  RenElecGenFuel %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  RenElecGenFuel <- as_tibble(RenElecGenFuel)
  
  RenElecGenFuel[is.na(RenElecGenFuel)] <- 0
  
  RenElecGenFuel <- RenElecGenFuel[c(1, (ncol(RenElecGenFuel) - 1):2)]
  
  RenElecGenFuel <- arrange(RenElecGenFuel,-row_number())
  
  RenElecGenFuel$Total <- RenElecGenFuel$`Other bioenergy` + RenElecGenFuel$`Sewage gas` + RenElecGenFuel$`Wave and tidal` + RenElecGenFuel$`Landfill gas` + RenElecGenFuel$`Solar PV` + RenElecGenFuel$Hydro + RenElecGenFuel$`Offshore Wind` + RenElecGenFuel$`Onshore Wind`
  
  RenElecGenFuel$Bioenergy <- RenElecGenFuel$`Other bioenergy` + RenElecGenFuel$`Sewage gas` + RenElecGenFuel$`Landfill gas`
  
  Data <- read_excel("Structure/CurrentWorking.xlsx",
                     sheet = "Renewable elec by fuel",
                     col_names = TRUE,
                     skip = 12
  )
  
  Data<- Data[complete.cases(Data),]
  
  Data <- distinct(Data, Year, .keep_all = TRUE)
  
  Data <- head(Data, -1)
  
  Data %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  RenElecCapFuel <- as_tibble(Data)
  
  RenElecCapFuel <- RenElecCapFuel[c(1, (ncol(RenElecCapFuel) - 1):2)]
  
  RenElecCapFuel <- arrange(RenElecCapFuel,-row_number())
  
  RenElecCapFuel$Total <- RenElecCapFuel$`Other bioenergy` + RenElecCapFuel$`Sewage gas` + RenElecCapFuel$`Wave and tidal` + RenElecCapFuel$`Landfill gas` + RenElecCapFuel$`Solar PV` + RenElecCapFuel$Hydro + RenElecCapFuel$`Offshore Wind` + RenElecCapFuel$`Onshore Wind`
  
  RenElecCapFuel$Bioenergy <- RenElecCapFuel$`Other bioenergy` + RenElecCapFuel$`Sewage gas` + RenElecCapFuel$`Landfill gas`
  
  RenElecPipeline <- read_excel("Structure/CurrentWorking.xlsx",
                                sheet = "Renewable elec pipeline", col_names = FALSE, 
                                skip = 13,
                                n_max = 12)[c(1,5)]
  
  RenElecPipeline <- as_tibble(t(RenElecPipeline))
  
  names(RenElecPipeline) <- unlist(RenElecPipeline[1,])
  
  names(RenElecPipeline)[1] <- c("Type")
  
  RenElecPipeline <- RenElecPipeline[-1,]
  
  RenElecPipeline %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  RenElecPipeline <- as_tibble(RenElecPipeline)
  
  RenElecPipeline$Hydro <- RenElecPipeline$`Large Hydro` + RenElecPipeline$`Small Hydro`
  
  RenElecPipeline$Bioenergy <- RenElecPipeline$`Biomass (co-firing)` + RenElecPipeline$`Energy from waste` + RenElecPipeline$`Anaerobic Digestion` + RenElecPipeline$`Landfill Gas`
  
  ######

  output$RenElecFuelGenSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -1)
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecOverviewFuel <- as_tibble(Data)
    
    paste(min(RenElecOverviewFuel$Year),"-", max(RenElecOverviewFuel$Year))
  })

  output$RenElecFuelGenTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Renewable elec by fuel",
                       col_names = TRUE,
                       skip = 12
    )
    
    Data<- Data[complete.cases(Data),]
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data <- distinct(Data, Year, .keep_all = TRUE)
    
    Data <- head(Data, -2)
    
    Data<-Data[dim(Data)[1]:1,]
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    RenElecOverviewFuel <- as_tibble(Data)
    
    RenElecOverviewFuel <- RenElecOverviewFuel[c(1, (ncol(RenElecOverviewFuel) - 1):2)]
    
    RenElecOverviewFuel <- arrange(RenElecOverviewFuel,-row_number())
    
    RenElecOverviewFuel$Total <- RenElecOverviewFuel$`Other bioenergy` + RenElecOverviewFuel$`Sewage gas` + RenElecOverviewFuel$`Wave and tidal` + RenElecOverviewFuel$`Landfill gas` + RenElecOverviewFuel$`Solar PV` + RenElecOverviewFuel$Hydro + RenElecOverviewFuel$`Offshore Wind` + RenElecOverviewFuel$`Onshore Wind`
    
    datatable(
      RenElecOverviewFuel[c(1,9:2,10)],
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
        title = "Renewable Electricity - Electricity Generated (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Electricity - Electricity Generated (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Electricity - Electricity Generated (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(RenElecOverviewFuel), 0)
  })
  
 output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecOverview.txt")[2])
                    
                  )))
 })

  
  observeEvent(input$ToggleTable, {
    toggle("RenElecFuelGenTable")
  })

  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$RenElecFuelGen.png <- downloadHandler(
    filename = "RenElecFuelGen.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                                   sheet = "Renewable elec by fuel",
                                   col_names = TRUE,
                                   skip = 12
      )
      
      Data<- Data[complete.cases(Data),]
      
      Data<-Data[dim(Data)[1]:1,]
      
      Data <- distinct(Data, Year, .keep_all = TRUE)
      
      Data <- head(Data, -1)
      
      Data<-Data[dim(Data)[1]:1,]
      
      Data %<>% lapply(function(x)
        as.numeric(as.character(x)))
      
      RenElecOverviewFuel <- as_tibble(Data)
      
      RenElecOverviewFuel <- RenElecOverviewFuel[c(1, (ncol(RenElecOverviewFuel) - 1):2)]
      
      RenElecOverviewFuel <- arrange(RenElecOverviewFuel,-row_number())
      
      RenElecOverviewFuel[is.na(RenElecOverviewFuel)] <- 0
      
      RenElecOverviewFuel <- melt(RenElecOverviewFuel, id.vars = "Year")
      
      
      RenElecOverviewFuel$variable <-
        factor(RenElecOverviewFuel$variable,
               levels = unique(RenElecOverviewFuel$variable),
               ordered = TRUE)
      
      RenElecOverviewFuel <- RenElecOverviewFuel %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Electricity generated from renewable sources"
      sourcecaption <- "Source: BEIS"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c(
          "#004529",
          "#006837",
          "#238443",
          "#41ab5d",
          "#78c679",
          "#7bccc4",
          "#4eb3d3",
          "#2b8cbe"
        )
      
      
      
      RenElecOverviewFuelChart <- RenElecOverviewFuel %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Onshore Wind" = BarColours[1],
            "Offshore Wind" = BarColours[2],
            "Hydro" = BarColours[3],
            "Solar PV" = BarColours[4],
            "Landfill gas" = BarColours[5],
            "Wave and tidal" = BarColours[6],
            "Sewage gas" = BarColours[7],
            "Other bioenergy" = BarColours[8],
            "Total" = "White"
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = RenElecOverviewFuel$Year,
          y = -20,
          label = ifelse(RenElecOverviewFuel$Year == "z", "", str_wrap(RenElecOverviewFuel$Year, width = 20)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1],
          size = 3,
          hjust = 1.05
        ) +
        geom_text(
          aes(
           x = 1999,
            y = 31000 * (.5 / 8),
            label = "Onshore\nWind"
          ),
          fontface = 2,
          colour =  BarColours[1],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1999,
            y = 31000 * (1.5 / 8),
            label = "Offshore\nWind"
          ),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1999,
            y = 31000 * (2.5 / 8),
            label = "Hydro"
          ),
          fontface = 2,
          colour =  BarColours[3],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1999,
            y = 31000 * (3.5 / 8),
            label = "Solar PV"
          ),
          fontface = 2,
          colour =  BarColours[4],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1999,
            y = 31000 * (4.5 / 8),
            label = "Landfill\ngas"
          ),
          fontface = 2,
          colour =  BarColours[5],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1999,
            y = 31000 * (5.5 / 8),
            label = "Wave\nand Tidal"
          ),
          fontface = 2,
          colour =  BarColours[6],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1999,
            y = 31000 * (6.5 / 8),
            label = "Sewage\nGas"
          ),
          fontface = 2,
          colour =  BarColours[7],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
           x = 1999,
            y = 31000 * (7.5 / 8),
            label = "Other\nBioenergy"
          ),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 2010.5,
              y = 31000 * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = 31000 * (8 / 8),
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(x = 0.1,
              y = -350,
              label = " "),
          fontface = 2,
          colour =  BarColours[8],
          family = "Century Gothic",
          hjust = 0.5,
          size = 3
        ) +
        geom_text(
          aes(
            x = RenElecOverviewFuel$Year ,
            y = RenElecOverviewFuel$top,
            label = paste(format(
              round(RenElecOverviewFuel$top, digits = 0), big.mark = ","
            ), "GWh")
          ),
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic",
          hjust = -0.1,
          size = 3
        ) 
      RenElecOverviewFuelChart
      
      
      RenElecOverviewFuelChart <-
        StackedBars(RenElecOverviewFuelChart,
                    RenElecOverviewFuel,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      RenElecOverviewFuelChart <-
        RenElecOverviewFuelChart +
        labs(subtitle = paste("Scotland,", min(RenElecOverviewFuel$Year), "-", max(RenElecOverviewFuel$Year))) +
        coord_flip() +
        xlim(max(RenElecOverviewFuel$Year+.5),min(RenElecOverviewFuel$Year-1))
      
      RenElecOverviewFuelChart
      
      ggsave(
        file,
        plot = RenElecOverviewFuelChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$ScotRenGenSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish renewables generation", col_names = TRUE, 
                       skip = 13)
    
    Data <- Data[c(1,4,7,10)]
    
    names(Data) <- c("Year", "Renewables", "Wind", "Hydro")
    
    ScotRenGen <- Data
    ### variables
    
    paste("Scotland,", min(ScotRenGen$Year),"-", max(ScotRenGen$Year))
  })
  
  output$ScotRenGenTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Scottish renewables generation", col_names = TRUE, 
                       skip = 13)
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    ScotRenGen <- as_tibble(Data)
    
    names(ScotRenGen) <- c("Year", "Renewables - Scotland (GWh)", "Renewables - UK (GWh)", "Renewables - Scottish proportion of UK Total", 
                           "Wind - Scotland (GWh)", "Wind - UK (GWh)", "Wind - Scottish proportion of UK Total",
                           "Hydro - Scotland (GWh)", "Hydro - UK (GWh)", "Hydro - Scottish proportion of UK Total")
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 2, 'Year'),
          th(colspan = 3, 'Total Renewables'),
          th(colspan = 3, 'Wind'),
          th(colspan = 3, 'Hydro')
        ),
        tr(
          lapply(rep(c('Scotland', 'UK', "% Scotland of UK Total"), 3), th)
        )
      )
    ))
    
    print(sketch)
    datatable(
      ScotRenGen,
      extensions = 'Buttons',
      # container = sketch,
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(0, 'desc')),
        title = "Scottish Renewable Generation",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Scottish Renewable Generation',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Scottish Renewable Generation')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = -1
      )
    ) %>%
      formatPercentage(c(4,7,10), 1) %>% 
      formatRound(c(2:3, 5:6,8:9), 0)
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ScotRenGenTable")
  })
  
    output$ScotRenGen.png <- downloadHandler(
    filename = "ScotRenGen.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Scottish renewables generation", col_names = TRUE, 
                         skip = 13)
      
      Data <- Data[c(1,4,7,10)]
      
      names(Data) <- c("Year", "Renewables", "Wind", "Hydro")
      
      ScotRenGen <- Data
      ### variables
      ChartColours <- c("#39ab2c",  "#fdb462", "#34d1a3", "#66c2a5","#8da0cb")
      sourcecaption = "Source: BEIS"
      plottitle = "Renewable generation as proportion of U.K."
      
      #ScotRenGen$`Renewables`Percentage <- PercentLabel(ScotRenGen$`Renewables`)
      
      
      ScotRenGenChart <- ScotRenGen %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = `Renewables`,
              label = percent(`Renewables`, 0.1)),
          colour = ChartColours[1],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1.4,
            y = `Renewables`,
            label = ifelse(Year == min(Year), percent(`Renewables`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.65,
            y = `Renewables`,
            label = ifelse(Year == max(Year), percent(`Renewables`, accuracy = .1), ""),
            hjust = 0.5,
            
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotRenGen, 1),
          aes(x = Year,
              y = `Renewables`,
              show_guide = FALSE),
          colour = ChartColours[1],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotRenGen$Year),
          y = mean(ScotRenGen$`Renewables`),
          label = "All renewables",
          hjust = 0.5,
          vjust = 2.5,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Wind`,
              label = paste0(`Wind` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1.4,
            y = `Wind`,
            label = ifelse(Year == min(Year), percent(`Wind`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = -0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.65,
            y = `Wind`,
            label = ifelse(Year == max(Year), percent(`Wind`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotRenGen, 1),
          aes(x = Year,
              y = `Wind`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotRenGen$Year),
          y = mean(ScotRenGen$`Wind`),
          label = "Wind",
          hjust = 0.5,
          vjust = -3,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `Hydro`,
              label = percent(`Hydro`, 0.1)),
          colour = ChartColours[5],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - 1.4,
            y = `Hydro`,
            label = ifelse(Year == min(Year), percent(`Hydro`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[5],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + 1.65,
            y = `Hydro`,
            label = ifelse(Year == max(Year), percent(`Hydro`, accuracy = .1), ""),
            hjust = 0.5,
            vjust = 0,
            fontface = 2
          ),
          colour = ChartColours[5],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ScotRenGen, 1),
          aes(x = Year,
              y = `Hydro`,
              show_guide = FALSE),
          colour = ChartColours[5],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(ScotRenGen$Year),
          y = mean(ScotRenGen$`Hydro`),
          label = "Hydro",
          hjust = 0.5,
          vjust = -1,
          colour = ChartColours[5],
          fontface = 2,
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              Year,
              ""
            ),
            hjust = 0.5,
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )
      
      ScotRenGenChart
      
      ScotRenGenChart <-
        StackedArea(ScotRenGenChart,
                    ScotRenGen,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ScotRenGenChart <- ScotRenGenChart
      
      ScotRenGenChart
      
      ggsave(
        file,
        plot =  ScotRenGenChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EUWindSubtitle <- renderText({
    
    EURenElec <- read_excel("Structure/CurrentWorking.xlsx",
                            sheet = "Wind and hydro gen EU", col_names = TRUE, 
                            skip = 15, n_max = 30)
    
    EURenElec <- EURenElec[,c(1:ncol(EURenElec))]
    
    
    
    names(EURenElec)[1] <- c("Countries")
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenElec[2:ncol(EURenElec)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EURenElec)), na.rm = TRUE))
  })
  
  output$EUWindTable = renderDataTable({
    
    EUWind <- read_excel("Structure/CurrentWorking.xlsx",
                         sheet = "Wind and hydro gen EU", col_names = TRUE, 
                         skip = 15, n_max = 30)
    
    EUWind <- EUWind[,c(1:ncol(EUWind))]
    
    
    
    names(EUWind)[1] <- c("Countries")
    
    EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EUWind[2:ncol(EUWind)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      EUWind,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(EUWind)-2, 'desc')),
        title = "EU Wind Generation (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'EU Wind Generation (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'EU Wind Generation (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(EUWind), 0) %>% 
      formatStyle(1,
                  target = 'row',
                  backgroundColor = styleEqual(c('Scotland'), c('#bdbdbd')))
  })
  
  output$EUWind.png <- downloadHandler(
    filename = "EUWind.png",
    content = function(file) {
      
      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      EUWind <- read_excel("Structure/CurrentWorking.xlsx",
                           sheet = "Wind and hydro gen EU", col_names = FALSE, 
                           skip = 17, n_max = 26)
      
      EUWind <- EUWind[,c(1,ncol(EUWind))]
      
      names(EUWind) <- c("Countries", "Renewables")
      
      EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "Rest of UK", "U.K."))
      
      EUWind <- merge(EUWind, EUFlagLookup)
      
      EUWind$Group <- ifelse(EUWind$Renewables > 0 & EUWind$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "B",
                             ifelse(EUWind$Renewables <= 0 & EUWind$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                    ifelse(EUWind$Renewables > 0 & EUWind$Renewables %in% c(min(EUWind$Renewables), max(EUWind$Renewables)), "C",
                                           ifelse(EUWind$Renewables <= 0 & EUWind$Renewables %in% c(min(EUWind$Renewables), max(EUWind$Renewables)), "E",      
                                                  ifelse(EUWind$Renewables <= 0 , "D",  
                                                         "A")))))
      
      EUWind <- EUWind[order(-EUWind$Renewables),]
      
      EUWind$Renewables <- EUWind$Renewables /100000
      
      ### variables
      ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Wind generation in EU countries"
      
      
      EUWind <- EUWind[order(EUWind$Renewables),]
      EUWind$Countries <-
        factor(EUWind$Countries, levels = EUWind$Countries)
      
      EUWindChart <-
        EUWind %>%  ggplot(aes(x = Countries, y = Renewables, fill = Group)) +
        geom_flag(aes(
          y = -.025,
          size = 10,
          country = Flag
        )) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        ylim(-.3, 1.10) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual("Group",
                          values = c("A" = ChartColours[2], "B" = ChartColours[1], "C" = ChartColours[2])) +
        geom_text(
          label = ifelse(
            EUWind$Group == "B" |
              EUWind$Group == "C" ,
            paste(format(round(EUWind$Renewables*100000, digits = 0), big.mark = ","), "GWh") ,
            ""
          ),
          fontface = 2,
          family = "Century Gothic",
          hjust = ifelse(EUWind$Renewables > .3, 1.1, 0),
          vjust = .5,
          color = ifelse(EUWind$Renewables > .3, "white", ChartColours[2])
        ) +
        geom_text(
          y = -0.055,
          label = EUWind$Countries,
          fontface = 2,
          family = "Century Gothic",
          hjust = 1,
          vjust = .5,
          color = "Black"
        ) +
        theme(
          text = element_text(family = "Century Gothic")
          ,
          panel.background = element_rect(fill = "transparent") # bg of the panel
          ,
          plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
          ,
          legend.background = element_rect(fill = "transparent") # get rid of legend bg
          ,
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
          ,
          legend.title = ggplot2::element_blank()
          ,
          axis.text.x = element_blank()
          ,
          axis.text.y = element_blank()
          ,
          axis.title = ggplot2::element_blank()
          ,
          legend.text = element_text(colour = "black", family = "Century Gothic")
          ,
          axis.ticks = ggplot2::element_blank()
          ,
          panel.grid.major = ggplot2::element_blank()
          ,
          legend.position = "none"
          ,
          title = element_text(colour = ChartColours[1], size = 14)
          ,
          plot.title = ggplot2::element_text(face = "bold")
        ) + ### Label Plot
        labs(y = "Percentage", caption = sourcecaption) +
        labs(title = plottitle,
             face = "bold",
             subtitle = 2017) +
        ### 0 Axis
        
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
        
        
        ### Plot Borders
        annotate(
          geom = 'segment',
          x = Inf,
          xend = Inf,
          color = ChartColours[1],
          y = -Inf,
          yend = Inf,
          size = 1.5
        ) +
        annotate(
          geom = 'segment',
          x = -Inf,
          xend = -Inf,
          color = ChartColours[1],
          y = -Inf,
          yend = Inf,
          size = 1
        ) +
        annotation_custom(
          ScotFlag,
          xmin = match("SCOTLAND", EUWind$Countries) - .45,
          xmax = match("SCOTLAND", EUWind$Countries) + .45,
          ymax = .312
        ) +
        annotation_custom(
          LatviaFlag,
          xmin = match("Latvia", EUWind$Countries) - .45,
          xmax = match("Latvia", EUWind$Countries) + .45,
          ymax = .312
        )
      
      
      EUWindChart
      
      
      ggsave(
        file,
        plot =  EUWindChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EUHydroSubtitle <- renderText({
    
    EUHydro <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Wind and hydro gen EU", col_names = TRUE, 
                          skip = 48, n_max = 30)
    
    EUHydro <- EUHydro[,c(1:ncol(EUHydro))]
    
    
    
    names(EUHydro)[1] <- c("Countries")
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EUHydro[2:ncol(EUHydro)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EUHydro)), na.rm = TRUE))
  })
  
  output$EUHydroTable = renderDataTable({
    
    EUHydro <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Wind and hydro gen EU", col_names = TRUE, 
                          skip = 48, n_max = 30)
    
    EUHydro <- EUHydro[,c(1:ncol(EUHydro))]
    
    
    
    names(EUHydro)[1] <- c("Countries")
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EUHydro[2:ncol(EUHydro)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      EUHydro,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(EUHydro)-2, 'desc')),
        title = "EU Hydro Generation (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'EU Hydro Generation (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'EU Hydro Generation (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(EUHydro), 0) %>% 
      formatStyle(1,
                  target = 'row',
                  backgroundColor = styleEqual(c('Scotland'), c('#bdbdbd')))
  })
  
  output$EUHydro.png <- downloadHandler(
    filename = "EUHydro.png",
    content = function(file) {
      
      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      EUHydro <- read_excel("Structure/CurrentWorking.xlsx",
                            sheet = "Wind and hydro gen EU", col_names = FALSE, 
                            skip = 50, n_max = 23)
      
      EUHydro <- EUHydro[,c(1,ncol(EUHydro))]
      
      names(EUHydro) <- c("Countries", "Renewables")
      
      EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "Rest of UK", "U.K."))
      
      EUHydro <- merge(EUHydro, EUFlagLookup)
      
      EUHydro$Group <- ifelse(EUHydro$Renewables > 0 & EUHydro$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "B",
                              ifelse(EUHydro$Renewables <= 0 & EUHydro$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                     ifelse(EUHydro$Renewables > 0 & EUHydro$Renewables %in% c(min(EUHydro$Renewables), max(EUHydro$Renewables)), "C",
                                            ifelse(EUHydro$Renewables <= 0 & EUHydro$Renewables %in% c(min(EUHydro$Renewables), max(EUHydro$Renewables)), "E",      
                                                   ifelse(EUHydro$Renewables <= 0 , "D",  
                                                          "A")))))
      
      EUHydro <- EUHydro[order(-EUHydro$Renewables),]
      
      
      EUHydro$Renewables <- EUHydro$Renewables /100000
      ### variables
      ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Hydro generation in EU countries"
      
      
      EUHydro <- EUHydro[order(EUHydro$Renewables),]
      EUHydro$Countries <-
        factor(EUHydro$Countries, levels = EUHydro$Countries)
      
      EUHydroChart <-
        EUHydro %>%  ggplot(aes(x = Countries, y = Renewables, fill = Group)) +
        geom_flag(aes(
          y = -.017,
          size = 10,
          country = Flag
        )) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        ylim(-.15, .66) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual("Group",
                          values = c("A" = ChartColours[2], "B" = ChartColours[1], "C" = ChartColours[2])) +
        geom_text(
          label = ifelse(
            EUHydro$Group == "B" |
              EUHydro$Group == "C" ,
            paste(format(round(EUHydro$Renewables*100000, digits = 0), big.mark = ","), "GWh") ,
            ""
          ),
          fontface = 2,
          family = "Century Gothic",
          hjust = ifelse(EUHydro$Renewables > .3, 1.1, 0),
          vjust = .5,
          color = ifelse(EUHydro$Renewables > .3, "white", ChartColours[2])
        ) +
        geom_text(
          y = -0.04,
          label = EUHydro$Countries,
          fontface = 2,
          family = "Century Gothic",
          hjust = 1,
          vjust = .5,
          color = "Black"
        ) +
        theme(
          text = element_text(family = "Century Gothic")
          ,
          panel.background = element_rect(fill = "transparent") # bg of the panel
          ,
          plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
          ,
          legend.background = element_rect(fill = "transparent") # get rid of legend bg
          ,
          legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
          ,
          legend.title = ggplot2::element_blank()
          ,
          axis.text.x = element_blank()
          ,
          axis.text.y = element_blank()
          ,
          axis.title = ggplot2::element_blank()
          ,
          legend.text = element_text(colour = "black", family = "Century Gothic")
          ,
          axis.ticks = ggplot2::element_blank()
          ,
          panel.grid.major = ggplot2::element_blank()
          ,
          legend.position = "none"
          ,
          title = element_text(colour = ChartColours[1], size = 14)
          ,
          plot.title = ggplot2::element_text(face = "bold")
        ) + ### Label Plot
        labs(y = "Percentage", caption = sourcecaption) +
        labs(title = plottitle,
             face = "bold",
             subtitle = 2017) +
        ### 0 Axis
        
        geom_hline(
          yintercept = 0,
          color = "grey",
          alpha = 0.7,
          linetype = 2
        ) +
        #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
        
        
        ### Plot Borders
        annotate(
          geom = 'segment',
          x = Inf,
          xend = Inf,
          color = ChartColours[1],
          y = -Inf,
          yend = Inf,
          size = 1.5
        ) +
        annotate(
          geom = 'segment',
          x = -Inf,
          xend = -Inf,
          color = ChartColours[1],
          y = -Inf,
          yend = Inf,
          size = 1
        ) +
        annotation_custom(
          ScotFlag,
          xmin = match("SCOTLAND", EUHydro$Countries) - .4,
          xmax = match("SCOTLAND", EUHydro$Countries) + .4,
          ymax = .153
        ) +
        annotation_custom(
          LatviaFlag,
          xmin = match("Latvia", EUHydro$Countries) - .4,
          xmax = match("Latvia", EUHydro$Countries) + .4,
          ymax = .153
        )
      
      
      EUHydroChart
      
      
      ggsave(
        file,
        plot =  EUHydroChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )})
  
  observeEvent(input$ToggleTable3, {
    toggle("EUWindTable")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("EUHydroTable")
  })
  
  Data <- read_excel("Structure/CurrentWorking.xlsx",
                     sheet = "R - QTRScotGen",
                     col_names = FALSE
  )
  
  Data <- as_tibble(t(Data))
  
  names(Data) <- unlist(Data[1,])
  
  names(Data)[1] <- "Year"
  
  Data <- Data[-1,]
  
  Data[2:10] %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  Data <- as_tibble(Data)
  
  Data$Quarter <- paste0("Q",substr(Data$Year,8,8))
  
  Data$Year <- as.numeric(substr(Data$Year,1,4))
  
  names(Data) <- c("Year", "Onshore Wind", "Offshore Wind", "Shoreline wave / tidal", "Solar PV", "Hydro", "Landfill gas", "Sewage sludge digestion", "Other biomass (inc. co-firing)", "Total" , "Quarter")
  
  Data <- melt(Data, id.vars = c("Year", "Quarter"))
  
  Subset <- Data[which(Data$variable == "Total"),]
  
  Subset <- dcast(Subset, Year ~ Quarter)
  
  Subset <- Subset[which(Subset$Year >= min(Subset[complete.cases(Subset),]$Year)),]
  
  Subset[is.na(Subset)] <- 0
  
  Subset$Total <- Subset$Q1 + Subset$Q2 + Subset$Q3 + Subset$Q4
  
  Subset <- Subset[order(-Subset$Year),]
  
  row.names(Subset) <- NULL
  
  output$RenElecQuarterGenerationSubtitle <- renderText({
    
    paste("Scotland,", min(Subset$Year), " - " ,max(Subset$Year))
  })
  
  output$RenElecQuarterGenerationPlot = renderPlotly({
    ChartColours <- c("#39ab2c", "#FF8500")
    BarColours <-
      c(
        "#004529",
        "#006837",
        "#238443",
        "#41ab5d",
        "#78c679",
        "#7bccc4",
        "#4eb3d3",
        "#2b8cbe"
      )
    
    Subset$Year <- paste0("<b>", Subset$Year, "</b>")
    
    p <- plot_ly(
      data = Subset,
      y = ~Year,
      x = ~`Q1`,
      legendgroup = 1,
      text = paste0(
        "Q1: ",
        format(round(Subset$`Q1`, digits = 0),big.mark = ","),
        " GWh\nYear: ",
        Subset$Year
      ),
      name = "Q1",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>% 
      add_trace(
        data = Subset,
        y = ~Year,
        x = ~`Q2`,
        legendgroup = 2,
        text = paste0(
          "Q2: ",
          format(round(Subset$`Q2`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          Subset$Year
        ),
        name = "Q2",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[2])
      )  %>% 
      add_trace(
        data = Subset,
        y = ~Year,
        x = ~`Q3`,
        legendgroup = 3,
        text = paste0(
          "Q3: ",
          format(round(Subset$`Q3`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          Subset$Year
        ),
        name = "Q3",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[3])
      )  %>% 
      add_trace(
        data = Subset,
        y = ~Year,
        x = ~`Q4`,
        legendgroup = 4,
        text = paste0(
          "Q4: ",
          format(round(Subset$`Q4`, digits = 0),big.mark = ","),
          " GWh\nYear: ",
          Subset$Year
        ),
        name = "Q4",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[4])
      )  %>% 
      add_trace(
        data = Subset,
        y = ~Year,
        x = ~Total + 100,
        showlegend = FALSE,
        type = 'scatter',
        mode = 'text',
        text = paste("<b>",format(round(Subset$Total, digits = 0), big.mark = ","),"GWh</b>"),
        textposition = 'middle right',
        textfont = list(color = ChartColours[1]),
        hoverinfo = 'skip',
        marker = list(
          size = 0.00001
        )
      ) %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     autorange = 'reversed',
                     dtick = 1),
        xaxis = list(
          title = "",
          tickformat = "",
          showgrid = TRUE,
          zeroline = TRUE,
          range = c(0, max(Subset$Total)*1.15),
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
  })
  
  output$RenElecQuarterGeneration.png <- downloadHandler(
    filename = "RenElecQuarterGeneration.png",
    content = function(file) {
      
      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      Subset$Total <- NULL
      
      Subset <- melt(Subset, id.vars = "Year")
      
      Subset$variable <-
        factor(Subset$variable,
               levels = rev(unique(Subset$variable)),
               ordered = TRUE)
      
      Subset <- Subset %>%
        group_by(Year) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      plottitle <-
        "Quarterly Renewable Generation - Total"
      sourceGention <- "Source: BEIS"
      
      ChartColours <- c("#39ab2c", "#FF8500")
      BarColours <-
        c(
          "#004529",
          "#006837",
          "#238443",
          "#41ab5d",
          "#78c679",
          "#7bccc4",
          "#4eb3d3",
          "#2b8cbe"
        )
      
      
      SubsetChart <- Subset %>%
        ggplot(aes(x = Year, y = value, fill = variable), family = "Century Gothic") +
        scale_fill_manual(
          "variable",
          values = c(
            "Q1" = BarColours[1],
            "Q2" = BarColours[2],
            "Q3" = BarColours[3],
            "Q4" = BarColours[4]
          )
        ) +
        geom_bar(stat = "identity", width = .8) +
        annotate(
          "text",
          x = Subset$Year,
          y = -.01,
          label = ifelse(Subset$Year == "z", "", str_wrap(Subset$Year, width = 20)),
          family = "Century Gothic",
          fontface = 2,
          colour =  ChartColours[1],
          hjust = 1.05
        ) +
        geom_text(
          aes(
            x = 2010,
            y = max(Subset$top) * (0.1/ 3),
            label = "Q1"
          ),
          fontface = 2,
          colour =  BarColours[1],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(
            x = 2010,
            y = max(Subset$top) * (1 / 3),
            label = "Q2"
          ),
          fontface = 2,
          colour =  BarColours[2],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(
            x = 2010,
            y = max(Subset$top) * (2 / 3),
            label = "Q3"
          ),
          fontface = 2,
          colour =  BarColours[3],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(
            x = 2010,
            y = max(Subset$top) * (3 / 3),
            label = "Q4"
          ),
          fontface = 2,
          colour =  BarColours[4],
          family = "Century Gothic",
          hjust = 0.5
        ) +
        geom_text(
          aes(
            x = Subset$Year ,
            y = Subset$top,
            label = paste(format(
              round(Subset$top, digits = 0), big.mark = ","
            ), "GWh")
          ),
          fontface = 2,
          colour =  ChartColours[1],
          family = "Century Gothic",
          hjust = -0.1
          
        ) 
      SubsetChart
      
      
      SubsetChart <-
        StackedBars(SubsetChart,
                    SubsetFuel,
                    plottitle,
                    sourceGention,
                    ChartColours)
      
      SubsetChart <-
        SubsetChart +
        labs(subtitle = paste("Scotland,", min(Subset$Year), "-", max(Subset$Year))) +
        coord_flip() +
        xlim(max(Subset$Year+.5),min(Subset$Year-1)) +
        ylim(-max(Subset$top*0.05),max(Subset$top)*1.2)
      
      SubsetChart
      
      ggsave(
        file,
        plot = SubsetChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenElecQuarterTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx",
                       sheet = "R - QTRScotGen",
                       col_names = FALSE
    )
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year & Quarter"
    
    Data <- Data[-1,]
    
    Data[2:10] %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    names(Data) <- c("Quarter", "Onshore Wind", "Offshore Wind", "Shoreline wave / tidal", "Solar PV", "Hydro", "Landfill gas", "Sewage sludge digestion", "Other biomass (inc. co-firing)", "Total")
    
    Data$Quarter <- paste0(substr(Data$Quarter,1,4), " Q", substr(Data$Quarter,8,8))
    
    datatable(
      Data,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Installed Generation by Quarter (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Installed Generation by Quarter (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Installed Generation by Quarter (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(Data), 0)
  })
  
  observeEvent(input$ToggleTable5, {
    toggle("RenElecQuarterTable")
  })
  
  
  output$OnshoreWindGenPiePlot <- renderPlotly  ({
    OnshoreWindGenPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Onshore Wind`, (RenElecGenFuel$Total - RenElecGenFuel$`Onshore Wind`)))
    
    names(OnshoreWindGenPie) <- c("Year", "Onshore Wind", "Other")
    
    OnshoreWindGenPie <- melt(OnshoreWindGenPie, id = "Year")
    
    OnshoreWindGenPie <- OnshoreWindGenPie[which(OnshoreWindGenPie$Year == max(OnshoreWindGenPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = OnshoreWindGenPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(OnshoreWindGenPie$variable,": ", format(round(OnshoreWindGenPie$value, digits = 0), big.mark = ","), " GWh\n", percent((OnshoreWindGenPie$value)/ sum(OnshoreWindGenPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Onshore Wind Generation</b>:",format(round(OnshoreWindGenPie[which(OnshoreWindGenPie$variable == "Onshore Wind"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
    
  })
  output$OnshoreWindCapPiePlot <- renderPlotly  ({
    OnshoreWindCapPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Onshore Wind`, (RenElecGenFuel$Total - RenElecGenFuel$`Onshore Wind`)))
    
    names(OnshoreWindCapPie) <- c("Year", "Onshore Wind", "Other")
    
    OnshoreWindCapPie <- melt(OnshoreWindCapPie, id = "Year")
    
    OnshoreWindCapPie <- OnshoreWindCapPie[which(OnshoreWindCapPie$Year == max(OnshoreWindCapPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = OnshoreWindCapPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(OnshoreWindCapPie$variable,": ", format(round(OnshoreWindCapPie$value, digits = 0), big.mark = ","), " GWh\n", percent((OnshoreWindCapPie$value)/ sum(OnshoreWindCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Onshore Wind Capacity</b>:",format(round(OnshoreWindCapPie[which(OnshoreWindCapPie$variable == "Onshore Wind"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$OnshoreWindPipePiePlot <- renderPlotly  ({
    OnshoreWindPipePie <- as_tibble(cbind(RenElecPipeline$`Wind Onshore`, (RenElecPipeline$Total - RenElecPipeline$`Wind Onshore`)))
    
    names(OnshoreWindPipePie) <- c("Onshore Wind", "Other")
    
    OnshoreWindPipePie <- melt(OnshoreWindPipePie)
    
    p <- plot_ly() %>% 
      add_pie(data = OnshoreWindPipePie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(OnshoreWindPipePie$variable,": ", format(round(OnshoreWindPipePie$value, digits = 0), big.mark = ","), " MW\n", percent((OnshoreWindPipePie$value)/ sum(OnshoreWindPipePie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Onshore Wind Pipeline Capacity</b>:",format(round(OnshoreWindPipePie[which(OnshoreWindPipePie$variable == "Onshore Wind"),]$value, digits = 0), big.mark = ","), "MW"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  
  output$OffshoreWindGenPiePlot <- renderPlotly  ({
    OffshoreWindGenPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Offshore Wind`, (RenElecGenFuel$Total - RenElecGenFuel$`Offshore Wind`)))
    
    names(OffshoreWindGenPie) <- c("Year", "Offshore Wind", "Other")
    
    OffshoreWindGenPie <- melt(OffshoreWindGenPie, id = "Year")
    
    OffshoreWindGenPie <- OffshoreWindGenPie[which(OffshoreWindGenPie$Year == max(OffshoreWindGenPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = OffshoreWindGenPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(OffshoreWindGenPie$variable,": ", format(round(OffshoreWindGenPie$value, digits = 0), big.mark = ","), " GWh\n", percent((OffshoreWindGenPie$value)/ sum(OffshoreWindGenPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Offshore Wind Generation</b>:",format(round(OffshoreWindGenPie[which(OffshoreWindGenPie$variable == "Offshore Wind"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$OffshoreWindCapPiePlot <- renderPlotly  ({
    OffshoreWindCapPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Offshore Wind`, (RenElecGenFuel$Total - RenElecGenFuel$`Offshore Wind`)))
    
    names(OffshoreWindCapPie) <- c("Year", "Offshore Wind", "Other")
    
    OffshoreWindCapPie <- melt(OffshoreWindCapPie, id = "Year")
    
    OffshoreWindCapPie <- OffshoreWindCapPie[which(OffshoreWindCapPie$Year == max(OffshoreWindCapPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = OffshoreWindCapPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(OffshoreWindCapPie$variable,": ", format(round(OffshoreWindCapPie$value, digits = 0), big.mark = ","), " GWh\n", percent((OffshoreWindCapPie$value)/ sum(OffshoreWindCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Offshore Wind Capacity</b>:",format(round(OffshoreWindCapPie[which(OffshoreWindCapPie$variable == "Offshore Wind"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$OffshoreWindPipePiePlot <- renderPlotly  ({
    OffshoreWindPipePie <- as_tibble(cbind(RenElecPipeline$`Wind Offshore`, (RenElecPipeline$Total - RenElecPipeline$`Wind Offshore`)))
    
    names(OffshoreWindPipePie) <- c("Offshore Wind", "Other")
    
    OffshoreWindPipePie <- melt(OffshoreWindPipePie)
    
    p <- plot_ly() %>% 
      add_pie(data = OffshoreWindPipePie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(OffshoreWindPipePie$variable,": ", format(round(OffshoreWindPipePie$value, digits = 0), big.mark = ","), " MW\n", percent((OffshoreWindPipePie$value)/ sum(OffshoreWindPipePie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Offshore Wind Pipeline Capacity</b>:",format(round(OffshoreWindPipePie[which(OffshoreWindPipePie$variable == "Offshore Wind"),]$value, digits = 0), big.mark = ","), "MW"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  
  output$HydroGenPiePlot <- renderPlotly  ({
    HydroGenPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Hydro`, (RenElecGenFuel$Total - RenElecGenFuel$`Hydro`)))
    
    names(HydroGenPie) <- c("Year", "Hydro", "Other")
    
    HydroGenPie <- melt(HydroGenPie, id = "Year")
    
    HydroGenPie <- HydroGenPie[which(HydroGenPie$Year == max(HydroGenPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = HydroGenPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(HydroGenPie$variable,": ", format(round(HydroGenPie$value, digits = 0), big.mark = ","), " GWh\n", percent((HydroGenPie$value)/ sum(HydroGenPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Hydro Generation</b>:",format(round(HydroGenPie[which(HydroGenPie$variable == "Hydro"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$HydroCapPiePlot <- renderPlotly  ({
    HydroCapPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Hydro`, (RenElecGenFuel$Total - RenElecGenFuel$`Hydro`)))
    
    names(HydroCapPie) <- c("Year", "Hydro", "Other")
    
    HydroCapPie <- melt(HydroCapPie, id = "Year")
    
    HydroCapPie <- HydroCapPie[which(HydroCapPie$Year == max(HydroCapPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = HydroCapPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(HydroCapPie$variable,": ", format(round(HydroCapPie$value, digits = 0), big.mark = ","), " GWh\n", percent((HydroCapPie$value)/ sum(HydroCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Hydro Capacity</b>:",format(round(HydroCapPie[which(HydroCapPie$variable == "Hydro"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$HydroPipePiePlot <- renderPlotly  ({
    HydroPipePie <- as_tibble(cbind(RenElecPipeline$`Hydro`, (RenElecPipeline$Total - RenElecPipeline$`Hydro`)))
    
    names(HydroPipePie) <- c("Hydro", "Other")
    
    HydroPipePie <- melt(HydroPipePie)
    
    p <- plot_ly() %>% 
      add_pie(data = HydroPipePie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(HydroPipePie$variable,": ", format(round(HydroPipePie$value, digits = 0), big.mark = ","), " MW\n", percent((HydroPipePie$value)/ sum(HydroPipePie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Hydro Pipeline Capacity</b>:",format(round(HydroPipePie[which(HydroPipePie$variable == "Hydro"),]$value, digits = 0), big.mark = ","), "MW"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  
  output$SolarPVGenPiePlot <- renderPlotly  ({
    SolarPVGenPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Solar PV`, (RenElecGenFuel$Total - RenElecGenFuel$`Solar PV`)))
    
    names(SolarPVGenPie) <- c("Year", "Solar PV", "Other")
    
    SolarPVGenPie <- melt(SolarPVGenPie, id = "Year")
    
    SolarPVGenPie <- SolarPVGenPie[which(SolarPVGenPie$Year == max(SolarPVGenPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = SolarPVGenPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(SolarPVGenPie$variable,": ", format(round(SolarPVGenPie$value, digits = 0), big.mark = ","), " GWh\n", percent((SolarPVGenPie$value)/ sum(SolarPVGenPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Solar PV Generation</b>:",format(round(SolarPVGenPie[which(SolarPVGenPie$variable == "Solar PV"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
    
  })
  output$SolarPVCapPiePlot <- renderPlotly  ({
    SolarPVCapPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Solar PV`, (RenElecGenFuel$Total - RenElecGenFuel$`Solar PV`)))
    
    names(SolarPVCapPie) <- c("Year", "Solar PV", "Other")
    
    SolarPVCapPie <- melt(SolarPVCapPie, id = "Year")
    
    SolarPVCapPie <- SolarPVCapPie[which(SolarPVCapPie$Year == max(SolarPVCapPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = SolarPVCapPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(SolarPVCapPie$variable,": ", format(round(SolarPVCapPie$value, digits = 0), big.mark = ","), " GWh\n", percent((SolarPVCapPie$value)/ sum(SolarPVCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Solar PV Capacity</b>:",format(round(SolarPVCapPie[which(SolarPVCapPie$variable == "Solar PV"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$SolarPipePiePlot <- renderPlotly  ({
    SolarPipePie <- as_tibble(cbind(RenElecPipeline$`Solar Photovoltaics`, (RenElecPipeline$Total - RenElecPipeline$`Solar Photovoltaics`)))
    
    names(SolarPipePie) <- c("Solar PV", "Other")
    
    SolarPipePie <- melt(SolarPipePie)
    
    p <- plot_ly() %>% 
      add_pie(data = SolarPipePie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(SolarPipePie$variable,": ", format(round(SolarPipePie$value, digits = 0), big.mark = ","), " MW\n", percent((SolarPipePie$value)/ sum(SolarPipePie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Solar PV Pipeline Capacity</b>:",format(round(SolarPipePie[which(SolarPipePie$variable == "Solar PV"),]$value, digits = 0), big.mark = ","), "MW"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  
  output$BioenergyGenPiePlot <- renderPlotly  ({
    BioenergyGenPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Bioenergy`, (RenElecGenFuel$Total - RenElecGenFuel$`Bioenergy`)))
    
    names(BioenergyGenPie) <- c("Year", "Bioenergy", "Other")
    
    BioenergyGenPie <- melt(BioenergyGenPie, id = "Year")
    
    BioenergyGenPie <- BioenergyGenPie[which(BioenergyGenPie$Year == max(BioenergyGenPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = BioenergyGenPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(BioenergyGenPie$variable,": ", format(round(BioenergyGenPie$value, digits = 0), big.mark = ","), " GWh\n", percent((BioenergyGenPie$value)/ sum(BioenergyGenPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Bioenergy Generation</b>:",format(round(BioenergyGenPie[which(BioenergyGenPie$variable == "Bioenergy"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$BioenergyCapPiePlot <- renderPlotly  ({
    BioenergyCapPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Bioenergy`, (RenElecGenFuel$Total - RenElecGenFuel$`Bioenergy`)))
    
    names(BioenergyCapPie) <- c("Year", "Bioenergy", "Other")
    
    BioenergyCapPie <- melt(BioenergyCapPie, id = "Year")
    
    BioenergyCapPie <- BioenergyCapPie[which(BioenergyCapPie$Year == max(BioenergyCapPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = BioenergyCapPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(BioenergyCapPie$variable,": ", format(round(BioenergyCapPie$value, digits = 0), big.mark = ","), " GWh\n", percent((BioenergyCapPie$value)/ sum(BioenergyCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Bioenergy Capacity</b>:",format(round(BioenergyCapPie[which(BioenergyCapPie$variable == "Bioenergy"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$BioenergyPipePiePlot <- renderPlotly  ({
    BioenergyPipePie <- as_tibble(cbind(RenElecPipeline$`Bioenergy`, (RenElecPipeline$Total - RenElecPipeline$`Bioenergy`)))
    
    names(BioenergyPipePie) <- c("Bioenergy", "Other")
    
    BioenergyPipePie <- melt(BioenergyPipePie)
    
    p <- plot_ly() %>% 
      add_pie(data = BioenergyPipePie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(BioenergyPipePie$variable,": ", format(round(BioenergyPipePie$value, digits = 0), big.mark = ","), " MW\n", percent((BioenergyPipePie$value)/ sum(BioenergyPipePie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Bioenergy Pipeline Capacity</b>:",format(round(BioenergyPipePie[which(BioenergyPipePie$variable == "Bioenergy"),]$value, digits = 0), big.mark = ","), "MW"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  
  output$WaveTidalGenPiePlot <- renderPlotly  ({
    WaveTidalGenPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Wave and tidal`, (RenElecGenFuel$Total - RenElecGenFuel$`Wave and tidal`)))
    
    names(WaveTidalGenPie) <- c("Year", "Wave and tidal", "Other")
    
    WaveTidalGenPie <- melt(WaveTidalGenPie, id = "Year")
    
    WaveTidalGenPie <- WaveTidalGenPie[which(WaveTidalGenPie$Year == max(WaveTidalGenPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = WaveTidalGenPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(WaveTidalGenPie$variable,": ", format(round(WaveTidalGenPie$value, digits = 0), big.mark = ","), " GWh\n", percent((WaveTidalGenPie$value)/ sum(WaveTidalGenPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Wave and tidal Generation</b>:",format(round(WaveTidalGenPie[which(WaveTidalGenPie$variable == "Wave and tidal"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$WaveTidalCapPiePlot <- renderPlotly  ({
    WaveTidalCapPie <- as_tibble(cbind(RenElecGenFuel$Year,RenElecGenFuel$`Wave and tidal`, (RenElecGenFuel$Total - RenElecGenFuel$`Wave and tidal`)))
    
    names(WaveTidalCapPie) <- c("Year", "Wave and tidal", "Other")
    
    WaveTidalCapPie <- melt(WaveTidalCapPie, id = "Year")
    
    WaveTidalCapPie <- WaveTidalCapPie[which(WaveTidalCapPie$Year == max(WaveTidalCapPie$Year)),]
    
    p <- plot_ly() %>% 
      add_pie(data = WaveTidalCapPie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(WaveTidalCapPie$variable,": ", format(round(WaveTidalCapPie$value, digits = 0), big.mark = ","), " GWh\n", percent((WaveTidalCapPie$value)/ sum(WaveTidalCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Wave and tidal Capacity</b>:",format(round(WaveTidalCapPie[which(WaveTidalCapPie$variable == "Wave and tidal"),]$value, digits = 0), big.mark = ","), "GWh"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$WaveTidalPipePiePlot <- renderPlotly  ({
    WaveTidalPipePie <- as_tibble(cbind(RenElecPipeline$`Shoreline wave / tidal`, (RenElecPipeline$Total - RenElecPipeline$`Shoreline wave / tidal`)))
    
    names(WaveTidalPipePie) <- c("Wave and tidal", "Other")
    
    WaveTidalPipePie <- melt(WaveTidalPipePie)
    
    p <- plot_ly() %>% 
      add_pie(data = WaveTidalPipePie,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(WaveTidalPipePie$variable,": ", format(round(WaveTidalPipePie$value, digits = 0), big.mark = ","), " MW\n", percent((WaveTidalPipePie$value)/ sum(WaveTidalPipePie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Wave and tidal Pipeline Capacity</b>:",format(round(WaveTidalPipePie[which(WaveTidalPipePie$variable == "Wave and tidal"),]$value, digits = 0), big.mark = ","), "MW"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  
}
