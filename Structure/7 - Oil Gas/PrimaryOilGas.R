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
                    h3("Distribution of primary energy (indigenous production and imports)", style = "color: #126992;  font-weight:bold"),
                    h4(textOutput(ns('PrimaryOilGasSubtitle')), style = "color: #126992;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('PrimaryOilGas.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    #dygraphOutput(ns("PrimaryOilGasPlot")),
    plotlyOutput(ns("PrimaryOilGasPlot"), height = "600px")%>% withSpinner(color="#126992"),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #126992;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:;background-color:#126992;"),
    tabsetPanel(
      tabPanel("Primary Energy",
    fluidRow(
    column(10, h3("Data - Distribution of primary energy - indigenous production and imports (TWh)", style = "color: #126992;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("PrimaryOilGasTable"))%>% withSpinner(color="#126992"))),
    tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;")),
    tabPanel("Proportion",
             fluidRow(
               column(10, h3("Data - Oil and gas by indigenous production and imports", style = "color: #126992;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("PrimaryOilGasImportsTable"))%>% withSpinner(color="#126992"))),
             tags$hr(style = "height:3px;border:none;color:#126992;background-color:#126992;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("SESHEnergyBalance"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("SESHEnergyBalance")
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
        legend = list(font = list(color = "#126992"),
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
    
    Data<- Data[seq(dim(Data)[1],1),]
    
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
        autoWidth = TRUE,
        title = "Distribution of primary energy - indigenous production and imports (TWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Distribution of primary energy - indigenous production and imports (TWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Distribution of primary energy - indigenous production and imports (TWh)')
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
                   
                   HTML(
                     paste(readtext("Structure/7 - Oil Gas/PrimaryOilGas.txt")[2])
                     
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
      plottitle = "Distribution of primary energy (indigenous production\nand imports)"
      
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
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
)
  
  
  output$PrimaryOilGasImportsTable = renderDataTable({
    
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Primary energy oil and gas",
                       skip = 13)[1:7]
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    Data <- arrange(Data, -row_number())
    
    Data <- Data[!duplicated(Data$Year), ]
    
    names(Data) <- c("Year",  "Oil/petroleum - Indigenous production", "Oil/petroleum - Imports",
                              "Gas - Indigenous production", "Gas - Imports",
                              "Total - Indigenous production", "Total - Imports")
    
    
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
        autoWidth = TRUE,
        title = "Oil and gas by indigenous production and imports",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Oil and gas by indigenous production and imports',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Oil and gas by indigenous production and imports')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:9, 1) 
  })
  
  observeEvent(input$ToggleTable, {
    toggle("PrimaryOilGasImportsTable")
  })
  
  output$ProportionPlot <- renderPlotly  ({
    Data <- read_excel("Structure/CurrentWorking.xlsx", 
                       sheet = "Primary energy oil and gas",
                       skip = 13)
    
    Data %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    Data<- Data[seq(dim(Data)[1],1),]
    
    PrimaryOilGas <- Data[complete.cases(Data),]
    
    PrimaryOilGas$Prop <- (PrimaryOilGas$`Primary oils` + PrimaryOilGas$`Natural gas` + PrimaryOilGas$`Petroleum products`) / PrimaryOilGas$Total
    
    ChartColours <- c("#126992", "#66c2a5", "#fc8d62", "#8da0cb")
    BarColours <-
      c(    "#0868ac","#43a2ca","#7bccc4"
      )
    
    PrimaryOilGas$Year2 <- paste0("01/01/", PrimaryOilGas$Year)
    
    PrimaryOilGas$Year2 <- dmy(PrimaryOilGas$Year2)
    
    PrimaryOilGas <- PrimaryOilGas[order(PrimaryOilGas$Year),]
    
    p <-  plot_ly(PrimaryOilGas,x = ~ Year2 ) %>% 
      add_trace(data = PrimaryOilGas,
                x = ~ Year2,
                y = ~ Prop,
                name = "Prop",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Proportion of primary energy from oil and gas: ",
                  percent(PrimaryOilGas$Prop, .1),
                  "\nYear: ",
                  paste(PrimaryOilGas$Year)
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      )  %>% 
      add_trace(
        data = tail(PrimaryOilGas[which(PrimaryOilGas$Prop > 0 | PrimaryOilGas$Prop < 0),], 1),
        x = ~ Year2,
        y = ~ Prop,
        legendgroup = "1",
        name = "Total",
        text = paste0(
          "Proportion of primary energy from oil and gas: ",
          percent(PrimaryOilGas[which(PrimaryOilGas$Prop > 0 | PrimaryOilGas$Prop < 0),][-1,]$Prop,  .1),
          " \nYear: ",
          paste(PrimaryOilGas[which(PrimaryOilGas$Prop > 0 | PrimaryOilGas$Prop < 0),][-1,]$Year)
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      )  %>%  
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "",
                     showgrid = FALSE),
        yaxis = list(
          title = "",
          tickformat = "%",
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

}
