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
               )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               
                 plotlyOutput(ns("OnshoreWindGenPiePlot"))%>% withSpinner(color="#39ab2c"),
                 
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
      fluidRow(
        column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
        column(2,style = "padding:15px",actionButton(ns("ToggleText1"), "Show/Hide Text", style = "float:right; "))),
      
      fluidRow(
        uiOutput(ns("TextOnshore"))
      ),
      tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),

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

######

 output$TextOnshore <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecSourcesOnshore.txt")[2])
                    
                  )))
 })

  
  observeEvent(input$ToggleText1, {
    toggle("TextOnshore")
  })
  
  
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
  
  
  RenEmployees <- read_excel("Structure/2 - Renewables/Economy/EconomyTables.xlsx", 
                             sheet = "Employees", col_names = TRUE)
  
  
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
  
  RenTurnover[1,1] <- "Other"
  
  RenTurnover <- RenTurnover[c(9,10,12,11,1),]
  
  names(RenTurnover) <- c("variable", "value")
  
  RenTurnover$value <- as.numeric(RenTurnover$value)
  
  RenTurnover$variable[RenTurnover$variable == "Hydropower"] <- "Hydro"
  
  RenTurnover$variable[RenTurnover$variable == "Solar photovoltaic"] <- "Solar PV"
  
  OnshoreWindTable <- as_tibble(cbind("Onshore Wind",
                                      RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Onshore Wind`,
                                      RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Onshore Wind`,
                                      RenElecPipeline$`Wind Onshore`,
                                      RenEmployees[which(RenEmployees$variable == "Onshore wind"),]$value,
                                      RenTurnover[which(RenTurnover$variable == "Onshore wind"),]$value
  ))
  names(OnshoreWindTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)", "Employees (FTE)", "Turnover (\u00A3000s)")
  
  
  OffshoreWindTable <- as_tibble(cbind("Offshore Wind",
                                       RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Offshore Wind`,
                                       RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Offshore Wind`,
                                       RenElecPipeline$`Wind Offshore`,
                                       RenEmployees[which(RenEmployees$variable == "Offshore wind"),]$value,
                                       RenTurnover[which(RenTurnover$variable == "Offshore wind"),]$value
  ))
  names(OffshoreWindTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)", "Employees (FTE)", "Turnover (\u00A3000s)")
  
  HydroTable <- as_tibble(cbind("Hydro",
                                RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Hydro`,
                                RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Hydro`,
                                RenElecPipeline$`Hydro`,
                                RenEmployees[which(RenEmployees$variable == "Hydro"),]$value,
                                RenTurnover[which(RenTurnover$variable == "Hydro"),]$value
  ))
  names(HydroTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)", "Employees (FTE)", "Turnover (\u00A3000s)")
  
  SolarPVTable <- as_tibble(cbind("SolarPV",
                                  RenElecGenFuel[which(RenElecGenFuel$Year == max(RenElecGenFuel$Year)),]$`Solar PV`,
                                  RenElecCapFuel[which(RenElecCapFuel$Year == max(RenElecCapFuel$Year)),]$`Solar PV`,
                                  RenElecPipeline$`Solar Photovoltaics`,
                                  RenEmployees[which(RenEmployees$variable == "Solar PV"),]$value,
                                  RenTurnover[which(RenTurnover$variable == "Solar PV"),]$value
  ))
  names(SolarPVTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)", "Employees (FTE)", "Turnover (\u00A3000s)")
  
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
  names(TotalTable) <- c("Tech","Generation (GWh)", "Operational Capacity (MW)", "Pipeline Capacity (MW)", "Employees (FTE)", "Turnover (\u00A3000s)")
  
  TechTable <- rbind.fill(OnshoreWindTable, OffshoreWindTable, HydroTable, SolarPVTable, BioenergyTable, WaveTidalTable, TotalTable)
  
  Stacked <- as_tibble(t(TechTable), rownames = "rowname")
  
  Stacked <- Stacked[-1,]
  
  names(Stacked) <- c("Tech", "Onshore Wind", "Offshore Wind", "Hydro", "Solar PV", "Bioenergy", "Wave and Tidal", "Total")
  
  Stacked[2:8] %<>% lapply(function(x)
    as.numeric(as.character(x)))
  
  Stacked <- as_tibble(Stacked)
  
  Stacked[1,1] <- paste0("<b>", Stacked[1,1], "</b>" , "\n2000")
  Stacked[2,1] <- paste0("<b>", Stacked[2,1], "</b>" , "\n2000 Q1")
  Stacked[3,1] <- paste0("<b>", Stacked[3,1], "</b>" , "\n2000 Q1")
  Stacked[4,1] <- paste0("<b>", Stacked[4,1], "</b>" , "\n2000")
  Stacked[5,1] <- paste0("<b>", Stacked[5,1], "</b>" , "\n2000")
  
  Stacked$Unit <- c("GWh", "MW", "MW", "FTE", "(£000s)")
  
  #Stacked$Tech <- factor(Stacked$Tech, levels = unique(Stacked$Tech)[order(row.names(Stacked), decreasing = FALSE)])
  
  BarColours <- c("#39ab2c", "#d9d9d9")
  
  ##### Outputs
  output$OnshoreWindGenPiePlot <- renderPlotly  ({
    p <- plot_ly(
      data = Stacked,
      y = ~Tech,
      x = ~(`Onshore Wind`/ Total),
      legendgroup = 1,
      text = paste0(
        "<b>Onshore Wind</b>\n",
        format(round(Stacked$`Onshore Wind`, digits = 0),big.mark = ",", trim = TRUE),
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
          format(round(Stacked$Total - Stacked$`Onshore Wind`, digits = 0),big.mark = ",", trim = TRUE),
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
  output$OnshoreWindCapPiePlot <- renderPlotly  ({
    OnshoreWindCapPie <- as_tibble(cbind(RenElecCapFuel$Year,RenElecCapFuel$`Onshore Wind`, (RenElecCapFuel$Total - RenElecCapFuel$`Onshore Wind`)))
    
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
              text = paste0(OnshoreWindCapPie$variable,": ", format(round(OnshoreWindCapPie$value, digits = 0), big.mark = ","), " MW\n", percent((OnshoreWindCapPie$value)/ sum(OnshoreWindCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Onshore Wind Capacity</b>:",format(round(OnshoreWindCapPie[which(OnshoreWindCapPie$variable == "Onshore Wind"),]$value, digits = 0), big.mark = ","), "MW"),
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
  output$OnshoreWindEmployeesPiePlot <- renderPlotly  ({
    OnshoreWindEmployees <- RenEmployees[which(RenEmployees$variable == "Other" | RenEmployees$variable == "Onshore wind"),]
    
    OnshoreWindEmployees[2,2] <- OnshoreWindEmployees[2,2] - OnshoreWindEmployees[1,2]
    
    OnshoreWindEmployees
    
    
    p <- plot_ly() %>% 
      add_pie(data = OnshoreWindEmployees,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(OnshoreWindEmployees$variable,": ", format(round(OnshoreWindEmployees$value, digits = 0), big.mark = ","), " (FTE)\n", percent((OnshoreWindEmployees$value)/ sum(OnshoreWindEmployees$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Onshore Wind Employees</b>:",format(round(OnshoreWindEmployees[which(OnshoreWindEmployees$variable == "Onshore wind"),]$value, digits = 0), big.mark = ","), "(FTE)"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
    
  })
  output$OnshoreWindTurnoverPiePlot <- renderPlotly  ({
    OnshoreWindTurnover <- RenTurnover[which(RenTurnover$variable == "Other" | RenTurnover$variable == "Onshore wind"),]
    
    OnshoreWindTurnover[2,2] <- OnshoreWindTurnover[2,2] - OnshoreWindTurnover[1,2]
    
    OnshoreWindTurnover
    
    
    p <- plot_ly() %>% 
      add_pie(data = OnshoreWindTurnover,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(OnshoreWindTurnover$variable,": ", format(round(OnshoreWindTurnover$value, digits = 0), big.mark = ","), " (\u00A3000s)\n", percent((OnshoreWindTurnover$value)/ sum(OnshoreWindTurnover$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Onshore Wind Turnover</b>:",format(round(OnshoreWindTurnover[which(OnshoreWindTurnover$variable == "Onshore wind"),]$value, digits = 0), big.mark = ","), "(\u00A3000s)"),
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
    OffshoreWindCapPie <- as_tibble(cbind(RenElecCapFuel$Year,RenElecCapFuel$`Offshore Wind`, (RenElecCapFuel$Total - RenElecCapFuel$`Offshore Wind`)))
    
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
              text = paste0(OffshoreWindCapPie$variable,": ", format(round(OffshoreWindCapPie$value, digits = 0), big.mark = ","), " MW\n", percent((OffshoreWindCapPie$value)/ sum(OffshoreWindCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Offshore Wind Capacity</b>:",format(round(OffshoreWindCapPie[which(OffshoreWindCapPie$variable == "Offshore Wind"),]$value, digits = 0), big.mark = ","), "MW"),
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
  output$OffshoreWindEmployeesPiePlot <- renderPlotly  ({
    OffshoreWindEmployees <- RenEmployees[which(RenEmployees$variable == "Other" | RenEmployees$variable == "Offshore wind"),]
    
    OffshoreWindEmployees[2,2] <- OffshoreWindEmployees[2,2] - OffshoreWindEmployees[1,2]
    
    OffshoreWindEmployees
    
    
    p <- plot_ly() %>% 
      add_pie(data = OffshoreWindEmployees,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(OffshoreWindEmployees$variable,": ", format(round(OffshoreWindEmployees$value, digits = 0), big.mark = ","), " (FTE)\n", percent((OffshoreWindEmployees$value)/ sum(OffshoreWindEmployees$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Offshore Wind Employees</b>:",format(round(OffshoreWindEmployees[which(OffshoreWindEmployees$variable == "Offshore wind"),]$value, digits = 0), big.mark = ","), "(FTE)"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$OffshoreWindTurnoverPiePlot <- renderPlotly  ({
    OffshoreWindTurnover <- RenTurnover[which(RenTurnover$variable == "Other" | RenTurnover$variable == "Offshore wind"),]
    
    OffshoreWindTurnover[2,2] <- OffshoreWindTurnover[2,2] - OffshoreWindTurnover[1,2]
    
    OffshoreWindTurnover
    
    
    p <- plot_ly() %>% 
      add_pie(data = OffshoreWindTurnover,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(OffshoreWindTurnover$variable,": ", format(round(OffshoreWindTurnover$value, digits = 0), big.mark = ","), " (\u00A3000s)\n", percent((OffshoreWindTurnover$value)/ sum(OffshoreWindTurnover$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Offshore Wind Turnover</b>:",format(round(OffshoreWindTurnover[which(OffshoreWindTurnover$variable == "Offshore wind"),]$value, digits = 0), big.mark = ","), "(\u00A3000s)"),
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
    HydroCapPie <- as_tibble(cbind(RenElecCapFuel$Year,RenElecCapFuel$`Hydro`, (RenElecCapFuel$Total - RenElecCapFuel$`Hydro`)))
    
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
              text = paste0(HydroCapPie$variable,": ", format(round(HydroCapPie$value, digits = 0), big.mark = ","), " MW\n", percent((HydroCapPie$value)/ sum(HydroCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Hydro Capacity</b>:",format(round(HydroCapPie[which(HydroCapPie$variable == "Hydro"),]$value, digits = 0), big.mark = ","), "MW"),
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
  output$HydroEmployeesPiePlot <- renderPlotly  ({
    HydroEmployees <- RenEmployees[which(RenEmployees$variable == "Other" | RenEmployees$variable == "Hydro"),]
    
    HydroEmployees[2,2] <- HydroEmployees[2,2] - HydroEmployees[1,2]
    
    HydroEmployees
    
    
    p <- plot_ly() %>% 
      add_pie(data = HydroEmployees,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(HydroEmployees$variable,": ", format(round(HydroEmployees$value, digits = 0), big.mark = ","), " (FTE)\n", percent((HydroEmployees$value)/ sum(HydroEmployees$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Hydro Employees</b>:",format(round(HydroEmployees[which(HydroEmployees$variable == "Hydro"),]$value, digits = 0), big.mark = ","), "(FTE)"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$HydroTurnoverPiePlot <- renderPlotly  ({
    HydroTurnover <- RenTurnover[which(RenTurnover$variable == "Other" | RenTurnover$variable == "Hydro"),]
    
    HydroTurnover[2,2] <- HydroTurnover[2,2] - HydroTurnover[1,2]
    
    HydroTurnover
    
    
    p <- plot_ly() %>% 
      add_pie(data = HydroTurnover,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(HydroTurnover$variable,": ", format(round(HydroTurnover$value, digits = 0), big.mark = ","), " (\u00A3000s)\n", percent((HydroTurnover$value)/ sum(HydroTurnover$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Hydro Turnover</b>:",format(round(HydroTurnover[which(HydroTurnover$variable == "Hydro"),]$value, digits = 0), big.mark = ","), "(\u00A3000s)"),
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
    SolarPVCapPie <- as_tibble(cbind(RenElecCapFuel$Year,RenElecCapFuel$`Solar PV`, (RenElecCapFuel$Total - RenElecCapFuel$`Solar PV`)))
    
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
              text = paste0(SolarPVCapPie$variable,": ", format(round(SolarPVCapPie$value, digits = 0), big.mark = ","), " MW\n", percent((SolarPVCapPie$value)/ sum(SolarPVCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Solar PV Capacity</b>:",format(round(SolarPVCapPie[which(SolarPVCapPie$variable == "Solar PV"),]$value, digits = 0), big.mark = ","), "MW"),
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
  output$SolarEmployeesPiePlot <- renderPlotly  ({
    SolarPVEmployees <- RenEmployees[which(RenEmployees$variable == "Other" | RenEmployees$variable == "Solar PV"),]
    
    SolarPVEmployees[2,2] <- SolarPVEmployees[2,2] - SolarPVEmployees[1,2]
    
    SolarPVEmployees
    
    
    p <- plot_ly() %>% 
      add_pie(data = SolarPVEmployees,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(SolarPVEmployees$variable,": ", format(round(SolarPVEmployees$value, digits = 0), big.mark = ","), " (FTE)\n", percent((SolarPVEmployees$value)/ sum(SolarPVEmployees$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Solar PV Employees</b>:",format(round(SolarPVEmployees[which(SolarPVEmployees$variable == "Solar PV"),]$value, digits = 0), big.mark = ","), "(FTE)"),
          font = list(
            color = "#262626"
          )
        ),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h')
      )
    p
  })
  output$SolarTurnoverPiePlot <- renderPlotly  ({
    
    
    SolarPVTurnover <- RenTurnover[which(RenTurnover$variable == "Other" | RenTurnover$variable == "Solar PV"),]
    
    SolarPVTurnover[2,2] <- SolarPVTurnover[2,2] - SolarPVTurnover[1,2]
    
    SolarPVTurnover
    
    
    p <- plot_ly() %>% 
      add_pie(data = SolarPVTurnover,
              labels = ~variable,
              values = ~value,
              sort = FALSE,
              hole = 0.5,
              textposition = "inside",
              textinfo = 'none',
              hoverinfo = 'text',
              marker = list(colors = c("#1a5d38",  "#d9d9d9", "#31859c","#77933c", "#4f6228", "#184d0f"),
                            line = list(color = '#FFFFFF', width = 2)),
              text = paste0(SolarPVTurnover$variable,": ", format(round(SolarPVTurnover$value, digits = 0), big.mark = ","), " (\u00A3000s)\n", percent((SolarPVTurnover$value)/ sum(SolarPVTurnover$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Solar PV Turnover</b>:",format(round(SolarPVTurnover[which(SolarPVTurnover$variable == "Solar PV"),]$value, digits = 0), big.mark = ","), "(\u00A3000s)"),
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
    BioenergyCapPie <- as_tibble(cbind(RenElecCapFuel$Year,RenElecCapFuel$`Bioenergy`, (RenElecCapFuel$Total - RenElecCapFuel$`Bioenergy`)))
    
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
              text = paste0(BioenergyCapPie$variable,": ", format(round(BioenergyCapPie$value, digits = 0), big.mark = ","), " MW\n", percent((BioenergyCapPie$value)/ sum(BioenergyCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Bioenergy Capacity</b>:",format(round(BioenergyCapPie[which(BioenergyCapPie$variable == "Bioenergy"),]$value, digits = 0), big.mark = ","), "MW"),
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
    WaveTidalCapPie <- as_tibble(cbind(RenElecCapFuel$Year,RenElecCapFuel$`Wave and tidal`, (RenElecCapFuel$Total - RenElecCapFuel$`Wave and tidal`)))
    
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
              text = paste0(WaveTidalCapPie$variable,": ", format(round(WaveTidalCapPie$value, digits = 0), big.mark = ","), " MW\n", percent((WaveTidalCapPie$value)/ sum(WaveTidalCapPie$value))),
              sort = T) %>% 
      layout(
        title = list(
          text = paste("<b>Wave and tidal Capacity</b>:",format(round(WaveTidalCapPie[which(WaveTidalCapPie$variable == "Wave and tidal"),]$value, digits = 0), big.mark = ","), "MW"),
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
      formatRound(2:ncol(RenElecGenFuel), 0)
  })
  
}
