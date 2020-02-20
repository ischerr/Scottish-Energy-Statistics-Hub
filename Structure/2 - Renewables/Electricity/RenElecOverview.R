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
                               h3("Onshore Wind", style = "color: #39ab2c;  font-weight:bold"),
                               h4("Scotland, 2018", style = "color: #39ab2c;")
               )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               fluidRow(column(6,
                 plotlyOutput(ns("OnshoreWindGenPiePlot"))%>% withSpinner(color="#39ab2c")),
                 column(6,
                 plotlyOutput(ns("OnshoreWindCapPiePlot"))%>% withSpinner(color="#39ab2c"))),
               fluidRow(column(3),column(6,
                 plotlyOutput(ns("OnshoreWindPipePiePlot"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Offshore Wind",
               fluidRow(column(8,
                               h3("Offshore Wind", style = "color: #39ab2c;  font-weight:bold"),
                               h4("Scotland, 2018", style = "color: #39ab2c;")
               )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               fluidRow(column(6,
                               plotlyOutput(ns("OffshoreWindGenPiePlot"))%>% withSpinner(color="#39ab2c")),
                        column(6,
                               plotlyOutput(ns("OffshoreWindCapPiePlot"))%>% withSpinner(color="#39ab2c"))),
               fluidRow(column(3),column(6,
                                         plotlyOutput(ns("OffshoreWindPipePiePlot"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Hydro",
               fluidRow(column(8,
                               h3("Hydro", style = "color: #39ab2c;  font-weight:bold"),
                               h4("Scotland, 2018", style = "color: #39ab2c;")
               )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               fluidRow(column(6,
                               plotlyOutput(ns("HydroGenPiePlot"))%>% withSpinner(color="#39ab2c")),
                        column(6,
                               plotlyOutput(ns("HydroCapPiePlot"))%>% withSpinner(color="#39ab2c"))),
               fluidRow(column(3),column(6,
                                         plotlyOutput(ns("HydroPipePiePlot"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Solar PV",
               fluidRow(column(8,
                               h3("Solar PV", style = "color: #39ab2c;  font-weight:bold"),
                               h4("Scotland, 2018", style = "color: #39ab2c;")
               )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               fluidRow(column(6,
                               plotlyOutput(ns("SolarPVGenPiePlot"))%>% withSpinner(color="#39ab2c")),
                        column(6,
                               plotlyOutput(ns("SolarPVCapPiePlot"))%>% withSpinner(color="#39ab2c"))),
               fluidRow(column(3),column(6,
                                         plotlyOutput(ns("SolarPVPipePiePlot"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Bioenergy",
               fluidRow(column(8,
                               h3("Bioenergy", style = "color: #39ab2c;  font-weight:bold"),
                               h4("Scotland, 2018", style = "color: #39ab2c;")
               )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               fluidRow(column(6,
                               plotlyOutput(ns("BioenergyGenPiePlot"))%>% withSpinner(color="#39ab2c")),
                        column(6,
                               plotlyOutput(ns("BioenergyCapPiePlot"))%>% withSpinner(color="#39ab2c"))),
               fluidRow(column(3),column(6,
                                         plotlyOutput(ns("BioenergyPipePiePlot"))%>% withSpinner(color="#39ab2c"))),
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
      tabPanel("Wave and Tidal",
               fluidRow(column(8,
                               h3("Wave and Tidal", style = "color: #39ab2c;  font-weight:bold"),
                               h4("Scotland, 2018", style = "color: #39ab2c;")
               )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
               #dygraphOutput(ns("RenElecFuelPlot")),
               fluidRow(column(6,
                               plotlyOutput(ns("WaveTidalGenPiePlot"))%>% withSpinner(color="#39ab2c")),
                        column(6,
                               plotlyOutput(ns("WaveTidalCapPiePlot"))%>% withSpinner(color="#39ab2c"))),
               fluidRow(column(3),column(6,
                                         plotlyOutput(ns("WaveTidalPipePiePlot"))%>% withSpinner(color="#39ab2c"))),
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

 output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecOverview.txt")[2])
                    
                  )))
 })

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
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
    OnshoreWindCapPie <- as_tibble(cbind(RenElecCapFuel$Year,RenElecCapFuel$`Onshore Wind`, (RenElecGenFuel$Total - RenElecCapFuel$`Onshore Wind`)))
    
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
