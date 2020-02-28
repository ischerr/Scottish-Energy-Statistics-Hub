require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenElecSourcesOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Onshore Wind",
               fluidRow(column(8,
                               h3("Onshore Wind", style = "color: #39ab2c;  font-weight:bold"),
                               h4("Scotland, Latest Figures", style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('OnshoreWindGen.png'), 'Download Graph', style="float:right")
               )),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
                 plotlyOutput(ns("OnshoreWindGenPlot"))%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
      fluidRow(
        column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
        column(2,style = "padding:15px",actionButton(ns("ToggleText1"), "Show/Hide Text", style = "float:right; "))),
      
      fluidRow(
        uiOutput(ns("TextOnshore"))
      ),
      tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Offshore Wind",
               fluidRow(column(8,
                               h3("Offshore Wind", style = "color: #39ab2c;  font-weight:bold"),
                               h4("Scotland, Latest Figures", style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('OffshoreWindGen.png'), 'Download Graph', style="float:right")
               )),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               plotlyOutput(ns("OffshoreWindGenPlot"))%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               fluidRow(
                 column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
                 column(2,style = "padding:15px",actionButton(ns("ToggleText2"), "Show/Hide Text", style = "float:right; "))),
               
               fluidRow(
                 uiOutput(ns("TextOffshore"))
               ),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Hydro",
               fluidRow(column(8,
                               h3("Hydro", style = "color: #39ab2c;  font-weight:bold"),
                               h4("Scotland, Latest Figures", style = "color: #39ab2c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('HydroGen.png'), 'Download Graph', style="float:right")
               )),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               plotlyOutput(ns("HydroGenPlot"))%>% withSpinner(color="#39ab2c"),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               fluidRow(
                 column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
                 column(2,style = "padding:15px",actionButton(ns("ToggleText3"), "Show/Hide Text", style = "float:right; "))),
               
               fluidRow(
                 uiOutput(ns("TextHydro"))
               ),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Solar",
             fluidRow(column(8,
                             h3("Solar", style = "color: #39ab2c;  font-weight:bold"),
                             h4("Scotland, Latest Figures", style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('SolarGen.png'), 'Download Graph', style="float:right")
             )),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             plotlyOutput(ns("SolarGenPlot"))%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             fluidRow(
               column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
               column(2,style = "padding:15px",actionButton(ns("ToggleText4"), "Show/Hide Text", style = "float:right; "))),
             
             fluidRow(
               uiOutput(ns("TextSolar"))
             ),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
  tabPanel("Bioenergy",
           fluidRow(column(8,
                           h3("Bioenergy", style = "color: #39ab2c;  font-weight:bold"),
                           h4("Scotland, Latest Figures", style = "color: #39ab2c;")
           ),
           column(
             4, style = 'padding:15px;',
             downloadButton(ns('BioenergyGen.png'), 'Download Graph', style="float:right")
           )),
           tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
           plotlyOutput(ns("BioenergyGenPlot"))%>% withSpinner(color="#39ab2c"),
           tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
           fluidRow(
             column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
             column(2,style = "padding:15px",actionButton(ns("ToggleText4"), "Show/Hide Text", style = "float:right; "))),
           
           fluidRow(
             uiOutput(ns("TextBioenergy"))
           ),
           tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
  tabPanel("Wave and Tidal",
           fluidRow(column(8,
                           h3("Wave and Tidal", style = "color: #39ab2c;  font-weight:bold"),
                           h4("Scotland, Latest Figures", style = "color: #39ab2c;")
           ),
           column(
             4, style = 'padding:15px;',
             downloadButton(ns('WaveTidalGen.png'), 'Download Graph', style="float:right")
           )),
           tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
           plotlyOutput(ns("WaveTidalGenPlot"))%>% withSpinner(color="#39ab2c"),
           tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
           fluidRow(
             column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
             column(2,style = "padding:15px",actionButton(ns("ToggleText4"), "Show/Hide Text", style = "float:right; "))),
           
           fluidRow(
             uiOutput(ns("TextWaveTidal"))
           ),
           tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))
    ),

               fluidRow(
                 column(10, h3("Data - Latest Figures", style = "color: #39ab2c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("RenSourcesTable"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
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
RenElecSources <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecFuel.R")

  ###### Data Processing ######
  RenElecGenFuel <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Renewable elec by fuel",
                               col_names = TRUE,
                               skip = 12
  )
  
  RenElecGenFuel<- RenElecGenFuel[complete.cases(RenElecGenFuel),]
  
  RenElecGenFuel<-RenElecGenFuel[dim(RenElecGenFuel)[1]:1,]
  
  RenElecGenFuel <- distinct(RenElecGenFuel, Year, .keep_all = TRUE)
  
  RenElecGenFuel <- head(RenElecGenFuel, -1)
  
  RenElecGenFuel<-RenElecGenFuel[dim(RenElecGenFuel)[1]:1,]
  
  RenElecGenFuel %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  RenElecGenFuel <- as_tibble(RenElecGenFuel)
  
  RenElecGenFuel[is.na(RenElecGenFuel)] <- 0
  
  RenElecGenFuel <- RenElecGenFuel[c(1, (ncol(RenElecGenFuel) - 1):2)]
  
  RenElecGenFuel <- arrange(RenElecGenFuel,-row_number())
  
  RenElecGenFuel$Total <- RenElecGenFuel$`Other bioenergy` + RenElecGenFuel$`Sewage gas` + RenElecGenFuel$`Wave and tidal` + RenElecGenFuel$`Landfill gas` + RenElecGenFuel$`Solar PV` + RenElecGenFuel$Hydro + RenElecGenFuel$`Offshore Wind` + RenElecGenFuel$`Onshore Wind`
  
  RenElecGenFuel$Bioenergy <- RenElecGenFuel$`Other bioenergy` + RenElecGenFuel$`Sewage gas` + RenElecGenFuel$`Landfill gas`
  
  
  ###
  Data <- read_excel("Structure/CurrentWorking.xlsx", 
                     sheet = "R - QTRCapacity", col_names = FALSE)
  
  Data <- as_tibble(t(Data))
  
  names(Data) <- c("Date", "Onshore Wind", "Offshore Wind", "Wave and tidal", "Solar PV", "Small Hydro", "Large Hydro", "Landfill Gas", "Sewage", "Waste", "Animal Biomass", "Anaerobic Digestion", "Plant", "Total")
  
  Data <- Data[-1,]
  
  Data$Date <- paste0(substr(Data$Date,1,4), " Q", substr(Data$Date, 8,8))
  
  Data[2:14]%<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  Data$Hydro <- Data$`Small Hydro` + Data$`Large Hydro`
  
  Data$Bioenergy <- Data$`Landfill Gas`+ Data$Sewage + Data$Waste + Data$`Animal Biomass` + Data$`Anaerobic Digestion` + Data$Plant
  
  names(Data)[1] <- "Year"
  
  Data<- Data[complete.cases(Data),]
  
  RenElecCapFuel <- as_tibble(Data)
  
  ####
  
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
  
  
  RenEmployees <- read_excel("Structure/2 - Renewables/Economy/EconomyTables.xlsx", 
                             sheet = "Employees", col_names = TRUE)
  
  RenEconomyYear <- max(names(RenEmployees))
  
  
  for (i in 2:6){
    RenEmployees[2,i] <- percent(as.numeric(RenEmployees[2,i]),0.1)
  }
  
  names(RenEmployees)[1] <- " "
  
  RenEmployees <- RenEmployees[c(1,ncol(RenEmployees))]
  
  RenEmployees[1,1] <- "Other"
  
  RenEmployees <- RenEmployees[c(9,10,12,11,1),]
  
  names(RenEmployees) <- c("variable", "value")
  
  RenEmployees$value <- as.numeric(RenEmployees$value)
  
  RenEmployees$variable[RenEmployees$variable == "Hydropower"] <- "Hydro"
  
  RenEmployees$variable[RenEmployees$variable == "Solar photovoltaic"] <- "Solar PV"
  
  RenTurnover <- read_excel("Structure/2 - Renewables/Economy/EconomyTables.xlsx", 
                            sheet = "Turnover", col_names = TRUE)
  
  
  for (i in 2:6){
    RenTurnover[2,i] <- percent(as.numeric(RenTurnover[2,i]),0.1)
  }
  
  names(RenTurnover)[1] <- " "
  
  RenTurnover <- RenTurnover[c(1,ncol(RenTurnover))]
  
  RenTurnover[5,1] <- "Other"
  
  RenTurnover <- RenTurnover[c(9,10,12,11,5),]
  
  names(RenTurnover) <- c("variable", "value")
  
  RenTurnover$value <- as.numeric(RenTurnover$value)/1000000
  
  RenTurnover$variable[RenTurnover$variable == "Hydropower"] <- "Hydro"
  
  RenTurnover$variable[RenTurnover$variable == "Solar photovoltaic"] <- "Solar PV"
  
  OnshoreWindTable <- as_tibble(cbind("Onshore Wind",
                                      RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Onshore Wind`,
                                      RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Onshore Wind`,
                                      RenElecPipeline$`Wind Onshore`,
                                      RenEmployees[which(RenEmployees$variable == "Onshore wind"),]$value,
                                      RenTurnover[which(RenTurnover$variable == "Onshore wind"),]$value
  ))
  names(OnshoreWindTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)", "Employees (FTE)", "Turnover (\u00A3bn)")
  
  
  OffshoreWindTable <- as_tibble(cbind("Offshore Wind",
                                       RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Offshore Wind`,
                                       RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Offshore Wind`,
                                       RenElecPipeline$`Wind Offshore`,
                                       RenEmployees[which(RenEmployees$variable == "Offshore wind"),]$value,
                                       RenTurnover[which(RenTurnover$variable == "Offshore wind"),]$value
  ))
  names(OffshoreWindTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)", "Employees (FTE)", "Turnover (\u00A3bn)")
  
  HydroTable <- as_tibble(cbind("Hydro",
                                RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Hydro`,
                                RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Hydro`,
                                RenElecPipeline$`Hydro`,
                                RenEmployees[which(RenEmployees$variable == "Hydro"),]$value,
                                RenTurnover[which(RenTurnover$variable == "Hydro"),]$value
  ))
  names(HydroTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)", "Employees (FTE)", "Turnover (\u00A3bn)")
  
  SolarPVTable <- as_tibble(cbind("SolarPV",
                                  RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Solar PV`,
                                  RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Solar PV`,
                                  RenElecPipeline$`Solar Photovoltaics`,
                                  RenEmployees[which(RenEmployees$variable == "Solar PV"),]$value,
                                  RenTurnover[which(RenTurnover$variable == "Solar PV"),]$value
  ))
  names(SolarPVTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)", "Employees (FTE)", "Turnover (\u00A3bn)")
  
  BioenergyTable <- as_tibble(cbind("Bioenergy",
                                    RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Bioenergy`,
                                    RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Bioenergy`,
                                    RenElecPipeline$`Bioenergy`
  ))
  names(BioenergyTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)")
  
  
  WaveTidalTable <- as_tibble(cbind("Wave and Tidal",
                                    RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Wave and tidal`,
                                    RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Wave and tidal`,
                                    RenElecPipeline$`Shoreline wave / tidal`
  ))
  names(WaveTidalTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)")
  
  TotalTable <- as_tibble(cbind("Total",
                                RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Total`,
                                RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Total`,
                                RenElecPipeline$`Total`,
                                RenEmployees[which(RenEmployees$variable == "Other"),]$value,
                                RenTurnover[which(RenTurnover$variable == "Other"),]$value
  ))
  names(TotalTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)", "Employees (FTE)", "Turnover (\u00A3bn)")
  
  TechTable <- rbind.fill(OnshoreWindTable, OffshoreWindTable, HydroTable, SolarPVTable, BioenergyTable, WaveTidalTable, TotalTable)
  
  Stacked <- as_tibble(t(TechTable), rownames = "rowname")
  
  Stacked <- Stacked[-1,]
  
  names(Stacked) <- c("Tech", "Onshore Wind", "Offshore Wind", "Hydro", "Solar PV", "Bioenergy", "Wave and Tidal", "Total")
  
  Stacked[2:8] %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  Stacked <- as_tibble(Stacked)
  Stacked$Unit <- c("GWh", "MW", "MW", "FTE", "bn")
  Stacked$Prefix <- c("","","","","\u00A3")
  
  Stacked2 <- Stacked
  
  Stacked[1,1] <- paste0("<b>", Stacked[1,1], "</b>" , "\n", max(RenElecGenFuel$Year))
  Stacked[2,1] <- paste0("<b>", Stacked[2,1], "</b>" , "\n", max(RenElecCapFuel$Year))
  Stacked[3,1] <- paste0("<b>", Stacked[3,1], "</b>" , "\n", max(RenElecCapFuel$Year))
  Stacked[4,1] <- paste0("<b>", Stacked[4,1], "</b>" , "\n", RenEconomyYear)
  Stacked[5,1] <- paste0("<b>", Stacked[5,1], "</b>" , "\n", RenEconomyYear)
  
  Stacked2$Time <- c(max(RenElecGenFuel$Year), max(RenElecCapFuel$Year),max(RenElecCapFuel$Year),RenEconomyYear,RenEconomyYear)

  
  #Stacked$Tech <- factor(Stacked$Tech, levels = unique(Stacked$Tech)[order(row.names(Stacked), decreasing = FALSE)])
  ChartColours <- c("#39ab2c")
  BarColours <- c("#39ab2c", "#d9d9d9")
  
######
# Outputs
  output$OnshoreWindGenPlot <- renderPlotly  ({
    p <- plot_ly(
      data = Stacked,
      y = ~Tech,
      x = ~(`Onshore Wind`/ Total),
      legendgroup = 1,
      text = paste0(
        "<b>Onshore Wind</b>\n",
        Stacked$Prefix,
        ifelse(Stacked$`Onshore Wind` > 100,
          format(round(Stacked$`Onshore Wind`, digits = 0),big.mark = ",", trim = TRUE),
          format(round(Stacked$`Onshore Wind`, digits = 2),big.mark = ",", trim = TRUE)),
        " ",
        Stacked$Unit,
        "\n",
        percent(Stacked$`Onshore Wind` / Stacked$Total, 0.1)
      ),
      name = "Onshore Wind",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>%
      add_trace(
        data = Stacked,
        y = ~Tech,
        x = ~((Total -`Onshore Wind`)/ Total),
        legendgroup = 2,
        text = paste0(
          "<b>Other Renewables</b>\n ",
          Stacked$Prefix,
          ifelse(Stacked$`Onshore Wind` > 100,
                 format(round(Stacked$Total - Stacked$`Onshore Wind`, digits = 0),big.mark = ",", trim = TRUE),
                 format(round(Stacked$Total - Stacked$`Onshore Wind`, digits = 2),big.mark = ",", trim = TRUE)),
          " ",
          Stacked$Unit,
          "\n",
           percent(( Stacked$Total - Stacked$`Onshore Wind`) / Stacked$Total, 0.1)
        ),
        name = "Other Renewables",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[2])
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#a3d65c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "%",
                     showticklabels = FALSE,
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$Country),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
  })

  output$OnshoreWindGen.png <- downloadHandler(
    filename = "OnshoreWindGen.png",
    content = function(file) {
      
      OnshoreWindSources <- Stacked2[c(1,11,10,9,2,8)]
      
      OnshoreWindSources[6] <- OnshoreWindSources[6] - OnshoreWindSources[5]
      
      names(OnshoreWindSources)[6] <- "Other Renewables"
      
      OnshoreWindSources <- melt(OnshoreWindSources)
      
      OnshoreWindSources <- OnshoreWindSources[order(-as.numeric(rownames(OnshoreWindSources))),]
      
      OnshoreWindSources <- OnshoreWindSources %>%
        group_by(Tech) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      OnshoreWindSources$variable <-
        factor(OnshoreWindSources$variable, 
               levels = unique(OnshoreWindSources$variable),
               ordered = TRUE)
      
      OnshoreWindSources$Tech <-
        factor(OnshoreWindSources$Tech, 
               levels = unique(OnshoreWindSources$Tech),
               ordered = TRUE)

      
      plottitle <-
        "Onshore Wind"
      sourcecaption <- "Source: BEIS"
      
      OnshoreWindSourcesChart <- OnshoreWindSources %>%
        ggplot(aes(x = Tech, y = (value/top), fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Onshore Wind" = BarColours[1],
                                     "Other Renewables" = BarColours[2])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = (OnshoreWindSources$value/OnshoreWindSources$top)+0.01,
          label = ifelse(
            OnshoreWindSources$variable == "Other Renewables",
            "",
            paste0(
              OnshoreWindSources$Prefix,
              ifelse(OnshoreWindSources$value > 100,
                     format(round(OnshoreWindSources$`value`, digits = 0),big.mark = ",", trim = TRUE),
                     format(round(OnshoreWindSources$`value`, digits = 2),big.mark = ",", trim = TRUE)),
              " ",
              OnshoreWindSources$Unit
            )
          ),
          fontface = 2,
          hjust = 0,
          colour = ChartColours[1]
        ) +
        annotate(
          "text",
          x = OnshoreWindSources$Tech,
          y = -0.01,
          label = ifelse(OnshoreWindSources$variable == "Other Renewables", "", as.character(OnshoreWindSources$Tech)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= -0.3,
          fontface = 2
        ) +
        annotate(
          "text",
          x = OnshoreWindSources$Tech,
          y = -0.01,
          label = ifelse(OnshoreWindSources$variable == "Other Renewables", "", as.character(OnshoreWindSources$Time)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= 1.3,
        ) +
        annotate(
          "text",
          x = 5.7,
          y = 0,
          label = "Onshore Wind",
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 0,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 5.7,
          y = 1,
          label = "Other Renewables",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 5.95,
          y = 1,
          label = " ",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) 
      
      
      
      OnshoreWindSourcesChart
      
      
      OnshoreWindSourcesChart <-
        StackedBars(OnshoreWindSourcesChart,
                    OnshoreWindSources,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      OnshoreWindSourcesChart <-
        OnshoreWindSourcesChart +
        coord_flip() +
        labs(subtitle = "Scotland, Latest Figures") +
        ylim(-0.3,1)
      
      OnshoreWindSourcesChart
      
      ggsave(
        file,
        plot = OnshoreWindSourcesChart,
        width = 23.5,
        height = 12.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$TextOnshore <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/RenElecSourcesOnshore.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText1, {
    toggle("TextOnshore")
  })
  
  
  
  
  output$OffshoreWindGenPlot <- renderPlotly  ({
    p <- plot_ly(
      data = Stacked,
      y = ~Tech,
      x = ~(`Offshore Wind`/ Total),
      legendgroup = 1,
      text = paste0(
        "<b>Offshore Wind</b>\n",
        Stacked$Prefix,
        ifelse(Stacked$`Offshore Wind` > 100,
               format(round(Stacked$`Offshore Wind`, digits = 0),big.mark = ",", trim = TRUE),
               format(round(Stacked$`Offshore Wind`, digits = 2),big.mark = ",", trim = TRUE)),
        " ",
        Stacked$Unit,
        "\n",
        percent(Stacked$`Offshore Wind` / Stacked$Total, 0.1)
      ),
      name = "Offshore Wind",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>%
      add_trace(
        data = Stacked,
        y = ~Tech,
        x = ~((Total -`Offshore Wind`)/ Total),
        legendgroup = 2,
        text = paste0(
          "<b>Other Renewables</b>\n ",
          Stacked$Prefix,
          ifelse(Stacked$`Offshore Wind` > 100,
                 format(round(Stacked$Total - Stacked$`Offshore Wind`, digits = 0),big.mark = ",", trim = TRUE),
                 format(round(Stacked$Total - Stacked$`Offshore Wind`, digits = 2),big.mark = ",", trim = TRUE)),
          " ",
          Stacked$Unit,
          "\n",
          percent(( Stacked$Total - Stacked$`Offshore Wind`) / Stacked$Total, 0.1)
        ),
        name = "Other Renewables",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[2])
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#a3d65c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "%",
                     showticklabels = FALSE,
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$Country),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
  })
  
  output$OffshoreWindGen.png <- downloadHandler(
    filename = "OffshoreWindGen.png",
    content = function(file) {
      
      OffshoreWindSources <- Stacked2[c(1,11,10,9,3,8)]
      
      OffshoreWindSources[6] <- OffshoreWindSources[6] - OffshoreWindSources[5]
      
      names(OffshoreWindSources)[6] <- "Other Renewables"
      
      OffshoreWindSources <- melt(OffshoreWindSources)
      
      OffshoreWindSources <- OffshoreWindSources[order(-as.numeric(rownames(OffshoreWindSources))),]
      
      OffshoreWindSources <- OffshoreWindSources %>%
        group_by(Tech) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      OffshoreWindSources$variable <-
        factor(OffshoreWindSources$variable, 
               levels = unique(OffshoreWindSources$variable),
               ordered = TRUE)
      
      OffshoreWindSources$Tech <-
        factor(OffshoreWindSources$Tech, 
               levels = unique(OffshoreWindSources$Tech),
               ordered = TRUE)
      
      plottitle <-
        "Offshore Wind"
      sourcecaption <- "Source: BEIS"
      
      OffshoreWindSourcesChart <- OffshoreWindSources %>%
        ggplot(aes(x = Tech, y = (value/top), fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Offshore Wind" = BarColours[1],
                                     "Other Renewables" = BarColours[2])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = (OffshoreWindSources$value/OffshoreWindSources$top)+0.01,
          label = ifelse(
            OffshoreWindSources$variable == "Other Renewables",
            "",
            paste0(
              OffshoreWindSources$Prefix,
              ifelse(OffshoreWindSources$value > 100,
                     format(round(OffshoreWindSources$`value`, digits = 0),big.mark = ",", trim = TRUE),
                     format(round(OffshoreWindSources$`value`, digits = 2),big.mark = ",", trim = TRUE)),
              " ",
              OffshoreWindSources$Unit
            )
          ),
          fontface = 2,
          hjust = 0,
          colour = ChartColours[1]
        ) +
        annotate(
          "text",
          x = OffshoreWindSources$Tech,
          y = -0.01,
          label = ifelse(OffshoreWindSources$variable == "Other Renewables", "", as.character(OffshoreWindSources$Tech)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= -0.3,
          fontface = 2
        ) +
        annotate(
          "text",
          x = OffshoreWindSources$Tech,
          y = -0.01,
          label = ifelse(OffshoreWindSources$variable == "Other Renewables", "", as.character(OffshoreWindSources$Time)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= 1.3,
        ) +
        annotate(
          "text",
          x = 5.7,
          y = 0,
          label = "Offshore Wind",
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 0,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 5.7,
          y = 1,
          label = "Other Renewables",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 5.95,
          y = 1,
          label = " ",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) 
      
      
      
      OffshoreWindSourcesChart
      
      
      OffshoreWindSourcesChart <-
        StackedBars(OffshoreWindSourcesChart,
                    OffshoreWindSources,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      OffshoreWindSourcesChart <-
        OffshoreWindSourcesChart +
        coord_flip() +
        labs(subtitle = "Scotland, Latest Figures") +
        ylim(-0.3,1)
      
      OffshoreWindSourcesChart
      
      ggsave(
        file,
        plot = OffshoreWindSourcesChart,
        width = 23.5,
        height = 12.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$TextOffshore <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/RenElecSourcesOffshore.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText2, {
    toggle("TextOffshore")
  })
  
  
  
  output$HydroGenPlot <- renderPlotly  ({
    p <- plot_ly(
      data = Stacked,
      y = ~Tech,
      x = ~(`Hydro`/ Total),
      legendgroup = 1,
      text = paste0(
        "<b>Hydro</b>\n",
        Stacked$Prefix,
        ifelse(Stacked$`Hydro` > 100,
               format(round(Stacked$`Hydro`, digits = 0),big.mark = ",", trim = TRUE),
               format(round(Stacked$`Hydro`, digits = 2),big.mark = ",", trim = TRUE)),
        " ",
        Stacked$Unit,
        "\n",
        percent(Stacked$`Hydro` / Stacked$Total, 0.1)
      ),
      name = "Hydro",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>%
      add_trace(
        data = Stacked,
        y = ~Tech,
        x = ~((Total -`Hydro`)/ Total),
        legendgroup = 2,
        text = paste0(
          "<b>Other Renewables</b>\n ",
          Stacked$Prefix,
          ifelse(Stacked$`Hydro` > 100,
                 format(round(Stacked$Total - Stacked$`Hydro`, digits = 0),big.mark = ",", trim = TRUE),
                 format(round(Stacked$Total - Stacked$`Hydro`, digits = 2),big.mark = ",", trim = TRUE)),
          " ",
          Stacked$Unit,
          "\n",
          percent(( Stacked$Total - Stacked$`Hydro`) / Stacked$Total, 0.1)
        ),
        name = "Other Renewables",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[2])
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#a3d65c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "%",
                     showticklabels = FALSE,
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$Country),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
  })
  
  output$HydroGen.png <- downloadHandler(
    filename = "HydroGen.png",
    content = function(file) {
      
      HydroSources <- Stacked2[c(1,11,10,9,4,8)]
      
      HydroSources[6] <- HydroSources[6] - HydroSources[5]
      
      names(HydroSources)[6] <- "Other Renewables"
      
      HydroSources <- melt(HydroSources)
      
      HydroSources <- HydroSources[order(-as.numeric(rownames(HydroSources))),]
      
      HydroSources <- HydroSources %>%
        group_by(Tech) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      HydroSources$variable <-
        factor(HydroSources$variable, 
               levels = unique(HydroSources$variable),
               ordered = TRUE)
      
      HydroSources$Tech <-
        factor(HydroSources$Tech, 
               levels = unique(HydroSources$Tech),
               ordered = TRUE)
      
      plottitle <-
        "Hydro"
      sourcecaption <- "Source: BEIS"
      
      HydroSourcesChart <- HydroSources %>%
        ggplot(aes(x = Tech, y = (value/top), fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Hydro" = BarColours[1],
                                     "Other Renewables" = BarColours[2])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = (HydroSources$value/HydroSources$top)+0.01,
          label = ifelse(
            HydroSources$variable == "Other Renewables",
            "",
            paste0(
              HydroSources$Prefix,
              ifelse(HydroSources$value > 100,
                     format(round(HydroSources$`value`, digits = 0),big.mark = ",", trim = TRUE),
                     format(round(HydroSources$`value`, digits = 2),big.mark = ",", trim = TRUE)),
              " ",
              HydroSources$Unit
            )
          ),
          fontface = 2,
          hjust = 0,
          colour = ChartColours[1]
        ) +
        annotate(
          "text",
          x = HydroSources$Tech,
          y = -0.01,
          label = ifelse(HydroSources$variable == "Other Renewables", "", as.character(HydroSources$Tech)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= -0.3,
          fontface = 2
        ) +
        annotate(
          "text",
          x = HydroSources$Tech,
          y = -0.01,
          label = ifelse(HydroSources$variable == "Other Renewables", "", as.character(HydroSources$Time)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= 1.3,
        ) +
        annotate(
          "text",
          x = 5.7,
          y = 0,
          label = "Hydro",
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 0,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 5.7,
          y = 1,
          label = "Other Renewables",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 5.95,
          y = 1,
          label = " ",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) 
      
      
      
      HydroSourcesChart
      
      
      HydroSourcesChart <-
        StackedBars(HydroSourcesChart,
                    HydroSources,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      HydroSourcesChart <-
        HydroSourcesChart +
        coord_flip() +
        labs(subtitle = "Scotland, Latest Figures") +
        ylim(-0.3,1)
      
      HydroSourcesChart
      
      ggsave(
        file,
        plot = HydroSourcesChart,
        width = 23.5,
        height = 12.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$TextHydro <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/RenElecSourcesHydro.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText3, {
    toggle("TextHydro")
  })
  
  output$SolarGenPlot <- renderPlotly  ({
    p <- plot_ly(
      data = Stacked,
      y = ~Tech,
      x = ~(`Solar PV`/ Total),
      legendgroup = 1,
      text = paste0(
        "<b>Solar PV</b>\n",
        Stacked$Prefix,
        ifelse(Stacked$`Solar PV` > 100,
               format(round(Stacked$`Solar PV`, digits = 0),big.mark = ",", trim = TRUE),
               format(round(Stacked$`Solar PV`, digits = 2),big.mark = ",", trim = TRUE)),
        " ",
        Stacked$Unit,
        "\n",
        percent(Stacked$`Solar PV` / Stacked$Total, 0.1)
      ),
      name = "Solar PV",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>%
      add_trace(
        data = Stacked,
        y = ~Tech,
        x = ~((Total -`Solar PV`)/ Total),
        legendgroup = 2,
        text = paste0(
          "<b>Other Renewables</b>\n ",
          Stacked$Prefix,
          ifelse(Stacked$`Solar PV` > 100,
                 format(round(Stacked$Total - Stacked$`Solar PV`, digits = 0),big.mark = ",", trim = TRUE),
                 format(round(Stacked$Total - Stacked$`Solar PV`, digits = 2),big.mark = ",", trim = TRUE)),
          " ",
          Stacked$Unit,
          "\n",
          percent(( Stacked$Total - Stacked$`Solar PV`) / Stacked$Total, 0.1)
        ),
        name = "Other Renewables",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[2])
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#a3d65c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "%",
                     showticklabels = FALSE,
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$Country),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
  })
  
  output$SolarGen.png <- downloadHandler(
    filename = "SolarGen.png",
    content = function(file) {
      
      SolarSources <- Stacked2[c(1,11,10,9,5,8)]
      
      SolarSources[6] <- SolarSources[6] - SolarSources[5]
      
      names(SolarSources)[6] <- "Other Renewables"
      
      SolarSources <- melt(SolarSources)
      
      SolarSources <- SolarSources[order(-as.numeric(rownames(SolarSources))),]
      
      SolarSources <- SolarSources %>%
        group_by(Tech) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      SolarSources$variable <-
        factor(SolarSources$variable, 
               levels = unique(SolarSources$variable),
               ordered = TRUE)
      
      SolarSources$Tech <-
        factor(SolarSources$Tech, 
               levels = unique(SolarSources$Tech),
               ordered = TRUE)
      
      plottitle <-
        "Solar"
      sourcecaption <- "Source: BEIS"
      
      SolarSourcesChart <- SolarSources %>%
        ggplot(aes(x = Tech, y = (value/top), fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Solar PV" = BarColours[1],
                                     "Other Renewables" = BarColours[2])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = (SolarSources$value/SolarSources$top)+0.01,
          label = ifelse(
            SolarSources$variable == "Other Renewables",
            "",
            paste0(
              SolarSources$Prefix,
              ifelse(SolarSources$value > 100,
                     format(round(SolarSources$`value`, digits = 0),big.mark = ",", trim = TRUE),
                     format(round(SolarSources$`value`, digits = 2),big.mark = ",", trim = TRUE)),
              " ",
              SolarSources$Unit
            )
          ),
          fontface = 2,
          hjust = 0,
          colour = ChartColours[1]
        ) +
        annotate(
          "text",
          x = SolarSources$Tech,
          y = -0.01,
          label = ifelse(SolarSources$variable == "Other Renewables", "", as.character(SolarSources$Tech)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= -0.3,
          fontface = 2
        ) +
        annotate(
          "text",
          x = SolarSources$Tech,
          y = -0.01,
          label = ifelse(SolarSources$variable == "Other Renewables", "", as.character(SolarSources$Time)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= 1.3,
        ) +
        annotate(
          "text",
          x = 5.7,
          y = 0,
          label = "Solar",
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 0,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 5.7,
          y = 1,
          label = "Other Renewables",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 5.95,
          y = 1,
          label = " ",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) 
      
      
      
      SolarSourcesChart
      
      
      SolarSourcesChart <-
        StackedBars(SolarSourcesChart,
                    SolarSources,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      SolarSourcesChart <-
        SolarSourcesChart +
        coord_flip() +
        labs(subtitle = "Scotland, Latest Figures") +
        ylim(-0.3,1)
      
      SolarSourcesChart
      
      ggsave(
        file,
        plot = SolarSourcesChart,
        width = 23.5,
        height = 12.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$TextSolar <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/RenElecSourcesSolar.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText4, {
    toggle("TextSolar")
  })
  
  
  
  output$BioenergyGenPlot <- renderPlotly  ({
    p <- plot_ly(
      data = Stacked,
      y = ~Tech,
      x = ~(`Bioenergy`/ Total),
      legendgroup = 1,
      text = paste0(
        "<b>Bioenergy</b>\n",
        Stacked$Prefix,
        ifelse(Stacked$`Bioenergy` > 100,
               format(round(Stacked$`Bioenergy`, digits = 0),big.mark = ",", trim = TRUE),
               format(round(Stacked$`Bioenergy`, digits = 2),big.mark = ",", trim = TRUE)),
        " ",
        Stacked$Unit,
        "\n",
        percent(Stacked$`Bioenergy` / Stacked$Total, 0.1)
      ),
      name = "Bioenergy",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>%
      add_trace(
        data = Stacked,
        y = ~Tech,
        x = ~((Total -`Bioenergy`)/ Total),
        legendgroup = 2,
        text = paste0(
          "<b>Other Renewables</b>\n ",
          Stacked$Prefix,
          ifelse(Stacked$`Bioenergy` > 100,
                 format(round(Stacked$Total - Stacked$`Bioenergy`, digits = 0),big.mark = ",", trim = TRUE),
                 format(round(Stacked$Total - Stacked$`Bioenergy`, digits = 2),big.mark = ",", trim = TRUE)),
          " ",
          Stacked$Unit,
          "\n",
          percent(( Stacked$Total - Stacked$`Bioenergy`) / Stacked$Total, 0.1)
        ),
        name = "Other Renewables",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[2])
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#a3d65c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "%",
                     showticklabels = FALSE,
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$Country),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
  })
  
  output$BioenergyGen.png <- downloadHandler(
    filename = "BioenergyGen.png",
    content = function(file) {
      
      BioenergySources <- Stacked2[c(1,11,10,9,6,8)]
      
      
      BioenergySources[6] <- BioenergySources[6] - BioenergySources[5]
      
      names(BioenergySources)[6] <- "Other Renewables"
      
      BioenergySources <- melt(BioenergySources)
      
      BioenergySources <- BioenergySources[order(-as.numeric(rownames(BioenergySources))),]
      
      BioenergySources <- BioenergySources %>%
        group_by(Tech) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      BioenergySources$variable <-
        factor(BioenergySources$variable, 
               levels = unique(BioenergySources$variable),
               ordered = TRUE)
      
      BioenergySources$Tech <-
        factor(BioenergySources$Tech, 
               levels = unique(BioenergySources$Tech),
               ordered = TRUE)
      
      BioenergySources <- BioenergySources[complete.cases(BioenergySources),]
      
      plottitle <-
        "Bioenergy"
      sourcecaption <- "Source: BEIS"
      
      BioenergySourcesChart <- BioenergySources %>%
        ggplot(aes(x = Tech, y = (value/top), fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Bioenergy" = BarColours[1],
                                     "Other Renewables" = BarColours[2])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = (BioenergySources$value/BioenergySources$top)+0.01,
          label = ifelse(
            BioenergySources$variable == "Other Renewables",
            "",
            paste0(
              BioenergySources$Prefix,
              ifelse(BioenergySources$value > 100,
                     format(round(BioenergySources$`value`, digits = 0),big.mark = ",", trim = TRUE),
                     format(round(BioenergySources$`value`, digits = 2),big.mark = ",", trim = TRUE)),
              " ",
              BioenergySources$Unit
            )
          ),
          fontface = 2,
          hjust = 0,
          colour = ChartColours[1]
        ) +
        annotate(
          "text",
          x = BioenergySources$Tech,
          y = -0.01,
          label = ifelse(BioenergySources$variable == "Other Renewables", "", as.character(BioenergySources$Tech)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= -0.3,
          fontface = 2
        ) +
        annotate(
          "text",
          x = BioenergySources$Tech,
          y = -0.01,
          label = ifelse(BioenergySources$variable == "Other Renewables", "", as.character(BioenergySources$Time)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= 1.3,
        ) +
        annotate(
          "text",
          x = 3.7,
          y = 0,
          label = "Bioenergy",
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 0,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 3.7,
          y = 1,
          label = "Other Renewables",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 3.95,
          y = 1,
          label = " ",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) 
      
      
      
      BioenergySourcesChart
      
      
      BioenergySourcesChart <-
        StackedBars(BioenergySourcesChart,
                    BioenergySources,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      BioenergySourcesChart <-
        BioenergySourcesChart +
        coord_flip() +
        labs(subtitle = "Scotland, Latest Figures") +
        ylim(-0.3,1)
      
      BioenergySourcesChart
      
      ggsave(
        file,
        plot = BioenergySourcesChart,
        width = 23.5,
        height = 10.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$TextBioenergy <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/RenElecSourcesBioenergy.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText5, {
    toggle("TextBioenergy")
  })

  
  output$WaveTidalGenPlot <- renderPlotly  ({
    p <- plot_ly(
      data = Stacked,
      y = ~Tech,
      x = ~(`Wave and Tidal`/ Total),
      legendgroup = 1,
      text = paste0(
        "<b>Wave and Tidal</b>\n",
        Stacked$Prefix,
        ifelse(Stacked$`Wave and Tidal` > 100,
               format(round(Stacked$`Wave and Tidal`, digits = 0),big.mark = ",", trim = TRUE),
               format(round(Stacked$`Wave and Tidal`, digits = 2),big.mark = ",", trim = TRUE)),
        " ",
        Stacked$Unit,
        "\n",
        percent(Stacked$`Wave and Tidal` / Stacked$Total, 0.1)
      ),
      name = "Wave and Tidal",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  BarColours[1])
    ) %>%
      add_trace(
        data = Stacked,
        y = ~Tech,
        x = ~((Total -`Wave and Tidal`)/ Total),
        legendgroup = 2,
        text = paste0(
          "<b>Other Renewables</b>\n ",
          Stacked$Prefix,
          ifelse(Stacked$`Wave and Tidal` > 100,
                 format(round(Stacked$Total - Stacked$`Wave and Tidal`, digits = 0),big.mark = ",", trim = TRUE),
                 format(round(Stacked$Total - Stacked$`Wave and Tidal`, digits = 2),big.mark = ",", trim = TRUE)),
          " ",
          Stacked$Unit,
          "\n",
          percent(( Stacked$Total - Stacked$`Wave and Tidal`) / Stacked$Total, 0.1)
        ),
        name = "Other Renewables",
        type = "bar",
        hoverinfo = "text",
        orientation = 'h',
        marker = list(color =  BarColours[2])
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#a3d65c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "%",
                     showticklabels = FALSE,
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(title = "",
                     showgrid = FALSE,
                     type = "category",
                     autorange = "reversed",
                     ticktext = as.list(Data$Country),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
  })
  
  output$WaveTidalGen.png <- downloadHandler(
    filename = "WaveTidalGen.png",
    content = function(file) {
      
      WaveTidalSources <- Stacked2[c(1,11,10,9,7,8)]
      
      
      WaveTidalSources[6] <- WaveTidalSources[6] - WaveTidalSources[5]
      
      names(WaveTidalSources)[6] <- "Other Renewables"
      
      WaveTidalSources <- melt(WaveTidalSources)
      
      WaveTidalSources <- WaveTidalSources[order(-as.numeric(rownames(WaveTidalSources))),]
      
      WaveTidalSources <- WaveTidalSources %>%
        group_by(Tech) %>%
        mutate(pos = cumsum(value) - value / 2) %>%
        mutate(top = sum(value))
      
      WaveTidalSources$variable <-
        factor(WaveTidalSources$variable, 
               levels = unique(WaveTidalSources$variable),
               ordered = TRUE)
      
      WaveTidalSources$Tech <-
        factor(WaveTidalSources$Tech, 
               levels = unique(WaveTidalSources$Tech),
               ordered = TRUE)
      
      WaveTidalSources <- WaveTidalSources[complete.cases(WaveTidalSources),]
      
      plottitle <-
        "WaveTidal"
      sourcecaption <- "Source: BEIS"
      
      WaveTidalSourcesChart <- WaveTidalSources %>%
        ggplot(aes(x = Tech, y = (value/top), fill = variable), family = "Century Gothic") +
        scale_fill_manual("variable",
                          values = c("Wave and Tidal" = BarColours[1],
                                     "Other Renewables" = BarColours[2])) +
        geom_bar(stat = "identity", width = .8) +
        geom_text(
          y = (WaveTidalSources$value/WaveTidalSources$top)+0.01,
          label = ifelse(
            WaveTidalSources$variable == "Other Renewables",
            "",
            paste0(
              WaveTidalSources$Prefix,
              ifelse(WaveTidalSources$value > 100,
                     format(round(WaveTidalSources$`value`, digits = 0),big.mark = ",", trim = TRUE),
                     format(round(WaveTidalSources$`value`, digits = 2),big.mark = ",", trim = TRUE)),
              " ",
              WaveTidalSources$Unit
            )
          ),
          fontface = 2,
          hjust = 0,
          colour = ChartColours[1]
        ) +
        annotate(
          "text",
          x = WaveTidalSources$Tech,
          y = -0.01,
          label = ifelse(WaveTidalSources$variable == "Other Renewables", "", as.character(WaveTidalSources$Tech)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= -0.3,
          fontface = 2
        ) +
        annotate(
          "text",
          x = WaveTidalSources$Tech,
          y = -0.01,
          label = ifelse(WaveTidalSources$variable == "Other Renewables", "", as.character(WaveTidalSources$Time)),
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 1,
          vjust= 1.3,
        ) +
        annotate(
          "text",
          x = 3.7,
          y = 0,
          label = "WaveTidal",
          family = "Century Gothic",
          colour =  ChartColours[1],
          hjust = 0,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 3.7,
          y = 1,
          label = "Other Renewables",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) +
        annotate(
          "text",
          x = 3.95,
          y = 1,
          label = " ",
          family = "Century Gothic",
          colour =  BarColours[2],
          hjust = 1,
          fontface = 2
        ) 
      
      
      
      WaveTidalSourcesChart
      
      
      WaveTidalSourcesChart <-
        StackedBars(WaveTidalSourcesChart,
                    WaveTidalSources,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      WaveTidalSourcesChart <-
        WaveTidalSourcesChart +
        coord_flip() +
        labs(subtitle = "Scotland, Latest Figures") +
        ylim(-0.3,1)
      
      WaveTidalSourcesChart
      
      ggsave(
        file,
        plot = WaveTidalSourcesChart,
        width = 23.5,
        height = 10.5,
        units = "cm",
        dpi = 300
      )
      
      
    }
  )
  
  output$TextWaveTidal <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Electricity/RenElecSourcesWaveTidal.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText5, {
    toggle("TextWaveTidal")
  })
  
  
  output$RenSourcesTable = renderDataTable({
    
    
    
    datatable(
      TechTable,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Onshore Wind",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Onshore Wind',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Onshore Wind')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:(ncol(TechTable)-1), 0) %>% 
      formatRound(ncol(TechTable), 2)
  })
}
