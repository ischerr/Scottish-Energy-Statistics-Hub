require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

PrimaryOilGasOutput <- function(id) {
  ns <- NS(id)
  tagList(

    fluidRow(column(8,
                    h3("Distribution of primary energy (indigenous production and imports)", style = "color: #5d8be1;  font-weight:bold"),
                    h4(textOutput(ns('PrimaryOilGasSubtitle')), style = "color: #5d8be1;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('PrimaryOilGas.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    #dygraphOutput(ns("PrimaryOilGasPlot")),
    plotlyOutput(ns("PrimaryOilGasPlot"), height = "600px")%>% withSpinner(color="#5d8be1"),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #5d8be1;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:;background-color:#5d8be1;"),
    fluidRow(
    column(10, h3("Data", style = "color: #5d8be1;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("PrimaryOilGasTable"))%>% withSpinner(color="#5d8be1"))),
    tags$hr(style = "height:3px;border:none;color:#5d8be1;background-color:#5d8be1;"),
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
        SourceLookup("BEISFinalConsump"),
        SourceLookup("ETElecGen"),
        SourceLookup("ESTRenHeat")
        
      )
    )
  )
}




###### Server ######
PrimaryOilGas <- function(input, output, session) {

  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("PrimaryOilGas.R")

  output$PrimaryOilGasSubtitle <- renderText({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Primary energy oil and gas",
                       skip = 13)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    PrimaryOilGas <- Data[complete.cases(Data),]
    
    paste("Scotland,", min(PrimaryOilGas$Year),"-", max(PrimaryOilGas$Year))
  })
  
  output$PrimaryOilGasPlot <- renderPlotly  ({
    
    if (exists("PackageHeader") == 0){
      source("Structure/PackageHeader.R")
    }
        ###### Daily Demand  #####
    
    # PrimaryOilGas <-
    #   read_csv(
    #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/PrimaryOilGas.csv"
    #   )
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Primary energy oil and gas",
                       skip = 13)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    PrimaryOilGas <- Data[complete.cases(Data),]
    
    
    
    ### variables
    ChartColours <- c("#126992", "#238b45", "#a1d99b")
    BarColours <- c("#d7301f","#ef6548", "#fc8d59", "#fdbb84", "#a8ddb5", "#4eb3d3")
    sourcecaption = "Source: University of Sheffield, National Grid, BEIS"
    plottitle = "Energy use in Scotland per day"
    
    #PrimaryOilGas$GasPercentage <- PercentLabel(PrimaryOilGas$Gas)
    
    
    p <- plot_ly(PrimaryOilGas, x = ~Year) %>% 

      add_trace(y = ~`Primary oils`, 
                name = "Primary oils",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                legendgroup = 1,
                text = paste0(
                  "Primary oils: ",
                  round(PrimaryOilGas$`Primary oils`, digits = 1),
                  " TWh\nDate: ",
                  PrimaryOilGas$Year
                ),
                hoverinfo = 'text',
                fillcolor = BarColours[1]) %>% 
      add_trace(y = ~`Petroleum products`, 
                name = "Petroleum products",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                legendgroup = 2,
                text = paste0(
                  "Petroleum products: ",
                  round(PrimaryOilGas$`Petroleum products`, digits = 1),
                  " TWh\nDate: ",
                  PrimaryOilGas$Year
                ),
                hoverinfo = 'text',
                fillcolor = BarColours[2]) %>% 
      add_trace(y = ~`Natural gas`, 
                name = "Natural gas",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                legendgroup = 3,
                text = paste0(
                  "Natural gas: ",
                  round(PrimaryOilGas$`Natural gas`, digits = 1),
                  " TWh\nDate: ",
                  PrimaryOilGas$Year
                ),
                hoverinfo = 'text',
                fillcolor = BarColours[3]) %>% 
      add_trace(y = ~Coal, 
                name = "Coal",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                legendgroup = 4,
                text = paste0(
                  "Coal: ",
                  round(PrimaryOilGas$Coal, digits = 1),
                  " TWh\nDate: ",
                  PrimaryOilGas$Year
                ),
                hoverinfo = 'text',
                fillcolor = BarColours[4]) %>% 
      add_trace(y = ~`Bioenergy & wastes`, 
                name = "Bioenergy & wastes",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                legendgroup = 5,
                text = paste0(
                  "Bioenergy & wastes: ",
                  round(PrimaryOilGas$`Bioenergy & wastes`, digits = 1),
                  " TWh\nDate: ",
                  PrimaryOilGas$Year
                ),
                hoverinfo = 'text',
                fillcolor = BarColours[5]) %>% 
      add_trace(y = ~`Electricity`, 
                name = "Electricity",
                type = 'scatter',
                mode = 'none',
                stackgroup = 'one',
                legendgroup = 6,
                text = paste0(
                  "Electricity: ",
                  round(PrimaryOilGas$`Electricity`, digits = 1),
                  " TWh\nDate: ",
                  PrimaryOilGas$Year
                ),
                hoverinfo = 'text',
                fillcolor = BarColours[6]) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#5d8be1"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE
                     ),
        yaxis = list(
          title = "TWh",
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

  output$PrimaryOilGasTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Primary energy oil and gas",
                       skip = 13)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    PrimaryOilGas <- Data[complete.cases(Data),]
    
    datatable(
      PrimaryOilGas,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = 10,
        searching = TRUE,
        fixedColumns = FALSE,
        columnDefs = list(list(visible=FALSE, targets=c(4))),
        autoWidth = TRUE,
        title = "Daily Demand - 12 month rolling average",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Daily Demand - 12 month rolling average',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Daily Demand - 12 month rolling average')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:9, 1) 
  })
  
 output$Text <- renderUI({
   tagList(column(12,
                  tags$p(
                    HTML(
                      "<p>For energy supplies to remain secure, the system must be able to meet&nbsp;<strong>demand&nbsp;</strong>at&nbsp;<strong>peak times</strong>.
                      &nbsp;&nbsp;Hence&nbsp;<strong>seasonality&nbsp;</strong>is key as demand is&nbsp;<strong>higher&nbsp;</strong>in the&nbsp;
                      <strong>winter&nbsp;</strong>than the&nbsp;<strong>summer</strong>.&nbsp;&nbsp;The chart below illustrates this, showing Scotland&rsquo;s 
                      energy use on a daily basis from 2013.&nbsp;&nbsp;It shows that&nbsp;<strong>seasonal variations&nbsp;</strong>in the demand for&nbsp;
                      <strong>electricity&nbsp;</strong>are much&nbsp;<strong>smaller&nbsp;</strong>than they are for&nbsp;<strong>gas</strong>.</p>
                      <p>During the winter of 2018/19&nbsp;<strong>the peak daily demand</strong>&nbsp;for gas was&nbsp;<strong>344 TWh&nbsp;</strong>
                      (on 31st January 2019). This was almost&nbsp;<strong>six times greater&nbsp;</strong>than the minimum gas demand in summer 2018 
                      (<strong>58 TWh&nbsp;</strong>on 28th July 2018).</p><p>In 2018/19,&nbsp;<strong>peak electricity daily demand&nbsp;</strong>
                      (<strong>100 TWh&nbsp;</strong>on the 1st February 2019) was&nbsp;<strong>double&nbsp;</strong>that of&nbsp;<strong>minimum&nbsp;</strong>
                      demand (<strong>51 TWh</strong>&nbsp;on the 20th May 2018).&nbsp;&nbsp;</p>
"
                    )
                  )))
 })
 
 
  observeEvent(input$ToggleTable, {
    toggle("PrimaryOilGasTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$PrimaryOilGas.png <- downloadHandler(
    filename = "PrimaryOilGas.png",
    content = function(file) {
      
      print("Energy daily demand")
      ###### Daily Demand  #####
      
      # PrimaryOilGas <-
      #   read_csv(
      #     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/PrimaryOilGas.csv"
      #   )
      
     Data <- read_excel("Structure/CurrentWorking.xlsx", 
                                    sheet = "Primary energy oil and gas", skip = 13, col_names = TRUE)
      
      Data <- Data[complete.cases(Data),]
      
      Data <- Data[1:7]
      
      Data %<>% lapply(function(x) as.numeric(as.character(x)))
      
      PrimaryEnergy <- as_tibble(Data)
      
      PrimaryEnergyMin <- head(PrimaryEnergy, 1)
      PrimaryEnergyMax <- tail(PrimaryEnergy, 1)
      
      
      PrimaryEnergy <- melt(PrimaryEnergy, id.vars = "Year")
      
      PrimaryEnergy <- PrimaryEnergy %>% mutate(variable = factor(variable),
                                                variable = factor(variable, levels = rev(c("Primary oils", "Petroleum products", "Natural gas", "Coal", "Bioenergy & wastes", "Electricity"))))
      
      ### variables
      ChartColours <- c("#126992", "#238b45", "#a1d99b")
      BarColours <- c("#d7301f","#ef6548", "#fc8d59", "#fdbb84", "#a8ddb5", "#4eb3d3")
      sourcecaption = "Source: BEIS, SG, HMRC"
      plottitle = "Distribution of primary energy (indigenous production and imports)"
      
      #PrimaryEnergy$CavityPercentage <- PercentLabel(PrimaryEnergy$Cavity)
      
      
      PrimaryEnergyChart <- PrimaryEnergy %>%
        ggplot(aes(
          x = Year,
          y = value,
          group = variable,
          fill = variable
        ))+
        scale_fill_manual(
          "variable",
          values = c(
            "Primary oils" = BarColours[1],
            "Petroleum products" = BarColours[2],
            "Natural gas" = BarColours[3],
            "Coal" = BarColours[4],
            "Bioenergy & wastes" = BarColours[5],
            "Electricity" = BarColours[6]
          ))+
        geom_area(posistion = "fill") +
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
            hjust = ifelse(Year == min(Year),0, 1),
            vjust = 1.5,
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic"
        )+
        geom_text(
          aes(
            x = min(Year)-1.6,
            y = 44000*0.01163,
            label = paste0("Primary Oils\n", format(round(PrimaryEnergyMin$`Primary oils`, digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = BarColours[1],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = min(Year)-1.6,
            y = 90000*0.01163,
            label = paste0("Petroleum products\n",format(round(PrimaryEnergyMin$`Petroleum products`, digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = min(Year)-1.6,
            y = 120000*0.01163,
            label = paste0("Natural gas\n",format(round(PrimaryEnergyMin$`Natural gas`, digits = 0), big.mark=",")," TWh"),
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = min(Year)-1.6,
            y = 140000*0.01163,
            label = paste0("Coal\n",format(round(PrimaryEnergyMin$Coal, digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = min(Year)-1.6,
            y = 155000*0.01163,
            label = paste0("Bioenergy & wastes\n", format(round(PrimaryEnergyMin$`Bioenergy & wastes`, digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = BarColours[5],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = min(Year)-1.6,
            y = 170000*0.01163,
            label = paste0("Electricity\n", format(round(PrimaryEnergyMin$Electricity, digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = BarColours[6],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = max(Year)+0.9,
            y = 26000*0.01163,
            label = paste0(format(round(PrimaryEnergyMax$`Primary oils`, digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = BarColours[1],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = max(Year)+0.9,
            y = 55000*0.01163,
            label = paste0(format(round(PrimaryEnergyMax$`Petroleum products`, digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = BarColours[2],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = max(Year)+0.9,
            y = 78000*0.01163,
            label = paste0(format(round(PrimaryEnergyMax$`Natural gas`, digits = 0), big.mark=",")," TWh"),
            fontface = 2
          ),
          colour = BarColours[3],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = max(Year)+0.9,
            y = 92200*0.01163,
            label = paste0(format(round(PrimaryEnergyMax$Coal, digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = BarColours[4],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = max(Year)+0.9,
            y = 97000*0.01163,
            label = paste0( format(round(PrimaryEnergyMax$`Bioenergy & wastes`, digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = BarColours[5],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = max(Year)+0.9,
            y = 102000*0.01163,
            label = paste0(format(round(PrimaryEnergyMax$Electricity, digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = BarColours[6],
          family = "Century Gothic",
          size = 3
        )+
        geom_text(
          aes(
            x = min(Year)-1.6,
            y = 185000*0.01163,
            label = paste0("Total\n",format(round(sum(PrimaryEnergy[which(PrimaryEnergy$Year == min(PrimaryEnergy$Year)),]$value), digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3)+
        geom_text(
          aes(
            x = max(Year)+0.9,
            y = 108000*0.01163,
            label = paste0(format(round(sum(PrimaryEnergy[which(PrimaryEnergy$Year == max(PrimaryEnergy$Year)),]$value), digits = 0), big.mark=","), " TWh"),
            fontface = 2
          ),
          colour = ChartColours[1],
          family = "Century Gothic",
          size = 3)
      
      PrimaryEnergyChart
      
      
      PrimaryEnergyChart <-
        DailyChart(PrimaryEnergyChart,
                   PrimaryEnergy,
                   plottitle,
                   sourcecaption,
                   ChartColours)
      
      
      PrimaryEnergyChart <- PrimaryEnergyChart+
        coord_cartesian(xlim = c(min(PrimaryEnergy$Year)-2.2, max(PrimaryEnergy$Year)+1.2)) 
      
      PrimaryEnergyChart
      ggsave(
        file,
        plot =  PrimaryEnergyChart,
        width = 20,
        height = 14,
        units = "cm",
        dpi = 300
      )
    }
)

}
