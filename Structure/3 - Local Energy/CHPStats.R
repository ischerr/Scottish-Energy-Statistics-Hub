require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

CHPStatsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("CHP schemes",
               fluidRow(column(8,
                               h3("Number of CHP schemes", style = "color: #a3d65c;  font-weight:bold"),
                               h4(textOutput(ns('CHPSubtitle')), style = "color: #a3d65c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('CHP.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               #dygraphOutput(ns("CHPPlot")),
               plotlyOutput(ns("CHPPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      tabPanel("Energy generated",
               fluidRow(column(8,
                               h3("Electricity and heat generated via CHP", style = "color: #a3d65c;  font-weight:bold"),
                               h4(textOutput(ns('ElecHeatGenSubtitle')), style = "color: #a3d65c;")
               ),
               column(
                 4, style = 'padding:15px;',
                 downloadButton(ns('ElecHeatGen.png'), 'Download Graph', style="float:right")
               )),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               #dygraphOutput(ns("CHPPlot")),
               plotlyOutput(ns("ElecHeatGenPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"))
    ),
    fluidRow(
      column(10,h3("Commentary", style = "color: #a3d65c;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               fluidRow(
                 column(10, h3("Data - CHP statistics by installation size", style = "color: #a3d65c;  font-weight:bold")),
                 column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
               ),
               fluidRow(
                 column(12, dataTableOutput(ns("CHPTable"))%>% withSpinner(color="#a3d65c"))),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
      column(2, p("Update expected:")),
    column(2,
           DateLookup(c("BEISCHP"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("BEISCHP")
        
      )
    )
  }




###### Server ######
CHPStats <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("CHP.R")
  
  
  output$CHPSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "CHP", col_names = FALSE,
        skip = 12)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year" 
        
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(Data$Year), "-", max(Data$Year))
  
      })
  
  output$CHPPlot <- renderPlotly  ({
    
    ChartColours <- c("#a3d65c", "#34d1a3", "#fc9272")
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "CHP", col_names = FALSE,
        skip = 12)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year" 
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    ElecHeatGen <- as_tibble(Data)
    
    p <-  plot_ly(ElecHeatGen, x = ~ Year ) %>%  
      add_trace(y = ~ `No. of schemes`,
                name = "No. of schemes",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Proportion: ",
                  ElecHeatGen$`No. of schemes`,
                  "\nYear: ",
                  ElecHeatGen$Year
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[1], dash = "none")
      ) %>% 
      add_trace(
        data = tail(ElecHeatGen[which(ElecHeatGen$`No. of schemes` != 0),], 1),
        x = ~ Year,
        y = ~ `No. of schemes`,
        name = "No. of schemes",
        legendgroup = "1",
        text = paste0(
          "Proportion: ",
          tail(ElecHeatGen[which(ElecHeatGen$`No. of schemes` != 0),], 1)$`No. of schemes`,
          "\nYear: ",
          tail(ElecHeatGen[which(ElecHeatGen$`No. of schemes` != 0),], 1)$Year
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>%
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#a3d65c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE),
        yaxis = list(
          title = "",
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
  
  output$ElecHeatGenSubtitle <- renderText({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "CHP", col_names = FALSE,
        skip = 12)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year" 
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(Data$Year), "-", max(Data$Year))
  })
  
  output$ElecHeatGenPlot <- renderPlotly  ({

    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "CHP", col_names = FALSE,
        skip = 12)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year" 
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    Data <- as_tibble(Data)
    
    ChartColours <- c("#a3d65c", "#66c2a5", "#fc8d62", "#8da0cb")
    
    p <-  plot_ly(Data, 
                  x = ~Year, 
                  y = ~ `Electricity generated (GWh)`, 
                  name = 'Electricity generated (GWh)',
                  type = 'scatter',
                  mode = 'none',
                  stackgroup = 'one',
                  fillcolor = ChartColours[2],
                  hoverinfo = "text",
                  text = paste0("Electricity generated: ", format(round(Data$`Electricity generated (GWh)`), big.mark = ","), " GWh\nYear: ", Data$Year)
    )%>%
      add_trace(
        y = ~ `Heat generated (GWh)`, 
        name = 'Heat generated (GWh)',
        fillcolor = ChartColours[3],
        hoverinfo = "text",
        text = paste0("Heat generated: ", format(round(Data$`Heat generated (GWh)`), big.mark = ","), " GWh\nYear: ", Data$Year)
      ) %>% 
      layout(
        barmode = 'group',
        bargap = 0.25,
        legend = list(font = list(color = "#a3d65c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'skip',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     showgrid = FALSE,
                     range = c(min(Data$Year)-.25, max(Data$Year)+.25)
                     
        ),
        yaxis = list(
          title = "GWh",
          showgrid = TRUE,
          zeroline = FALSE,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    p
    
    
  })
  
  output$CHPTable = renderDataTable({
    
    Data <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "CHP", col_names = FALSE,
        skip = 12)
    
    Data <- as_tibble(t(Data))
    
    names(Data) <- unlist(Data[1,])
    
    names(Data)[1] <- "Year" 
    
    Data <- Data[-1,]
    
    Data %<>% lapply(function(x) as.numeric(as.character(x)))
    
    CHP <- as_tibble(Data)
    
    datatable(
      CHP,
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
        title = "CHP statistics by installation size, Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "CHP statistics by installation size, Scotland",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "CHP statistics by installation size, Scotland")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(c(2:7), 0) %>% 
      formatPercentage(7,2)
  })

  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/3 - Local Energy/CHPStats.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable1, {
    toggle("CHPTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$CHP.png <- downloadHandler(
    filename = "CHP.png",
    content = function(file) {
      
      Data <-
        read_excel(
          "Structure/CurrentWorking.xlsx",
      sheet = "CHP", skip = 12, col_names = FALSE)
  
  Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
  
  Data <- tail(Data[c(1,2)], -1)
  
  names(Data) <- c("Year", "Schemes")
  
  Data <- as_tibble(sapply( Data, as.numeric ))
  
  CHPSchemes <- Data
  
  ### variables
  ChartColours <- c("#a3d65c", "#66c2a5", "#fc8d62", "#8da0cb")
  sourcecaption = "Source: BEIS"
  plottitle = "Number of CHP schemes"
  
  #CHPSchemes$SchemesPercentage <- PercentLabel(CHPSchemes$Schemes)
  
  CHPSchemesChart <- CHPSchemes %>%
    ggplot(aes(x = Year), family = "Century Gothic") +
    
    geom_line(
      aes(
        y = Schemes,
        colour = ChartColours[2],
        label = percent(Schemes)
      ),
      size = 1.5,
      family = "Century Gothic"
    ) +
    geom_text(
      aes(
        x = Year-.2,
        y = Schemes,
        label = ifelse(Year == min(Year), Schemes, ""),
        hjust = 1,
        colour = ChartColours[2],
        fontface = 2
      ),
      family = "Century Gothic"
    ) +
    geom_text(
      aes(
        x = Year+.2,
        y = Schemes,
        label = ifelse(Year == max(Year),Schemes, ""),
        hjust = 0,
        colour = ChartColours[2],
        fontface = 2
      ),
      family = "Century Gothic"
    ) +
    geom_point(
      data = tail(CHPSchemes, 1),
      aes(
        x = Year,
        y = Schemes,
        colour = ChartColours[2],
        show_guide = FALSE
      ),
      size = 4,
      family = "Century Gothic"
    ) +
    geom_text(
      aes(
        x = mean(Year),
        y = mean(Schemes),
        label = "CHP\nschemes",
        hjust = 0.5,
        vjust = -2,
        colour = ChartColours[2],
        fontface = 2
      ),
      family = "Century Gothic"
    ) +
    geom_text(
      aes(
        x = Year,
        y = 0,
        label = ifelse(Year == max(Year) |
                         Year == min(Year), Year, ""),
        hjust = 0.5,
        vjust = 1.5,
        fontface = 2
      ),
      colour = ChartColours[1],
      family = "Century Gothic"
    )
  
  
  CHPSchemesChart <-
    LinePercentChart(CHPSchemesChart,
                     CHPSchemes,
                     plottitle,
                     sourcecaption,
                     ChartColours)
  
  CHPSchemesChart <- CHPSchemesChart +
    xlim(min(CHPSchemes$Year)-.4, max(CHPSchemes$Year)+.4)
  
  CHPSchemesChart
  
  ggsave(
    file,
    plot =  CHPSchemesChart,
    width = 14,
    height = 16,
    units = "cm",
    dpi = 300
  )
    }
  )
  
  output$ElecHeatGen.png <- downloadHandler(
    filename = "ElecHeatGen.png",
    content = function(file) {
      
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "CHP", skip = 12, col_names = FALSE)
      
      Data <- as.data.frame(t(Data), stringsAsFactors = FALSE)
      
      Data <- tail(Data[c(1,4,5)], -1)
      
      names(Data) <- c("Year", "ElecGen", "HeatGen")
      
      Data <- as_tibble(sapply( Data, as.numeric ))
      
      CHPGen <- Data
      
      CHPGenMin <- head(CHPGen, 1)
      CHPGenMax <- tail(CHPGen, 1)
      
      CHPGen <- melt(CHPGen, id.vars = "Year")
      
      CHPGen <- CHPGen %>% mutate(variable = factor(variable),
                                  variable = factor(variable, levels = rev(levels(variable))))
      
      
      ### variables
      ChartColours <- c("#a3d65c", "#34d1a3", "#fc9272")
      sourcecaption = "Source: BEIS"
      plottitle = "Electricity and heat generated via CHP"
      
      #CHPGen$CavityPercentage <- PercentLabel(CHPGen$Cavity)
      
      
      CHPGenChart <- CHPGen %>%
        ggplot(aes(
          x = Year,
          y = value,
          group = variable,
          fill = variable
        )) +
        scale_fill_manual(
          "variable",
          values = c(
            "ElecGen" = ChartColours[2],
            "HeatGen" = ChartColours[3]
          )
        ) +
        geom_area(posistion = "fill") +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(
              Year == max(Year) |
                Year == min(Year),
              format(Year, format = "%Y Q%q"),
              ""
            ),
            hjust = ifelse(Year == min(Year), 0, 1),
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2,
            family = "Century Gothic"
          )
        ) +
        annotate(
          "text",
          x = CHPGenMin$Year+.1,
          y = CHPGenMin$`ElecGen` * 0.5,
          label = paste(format(round(CHPGenMin$`ElecGen`, digits = 0),big.mark = ","), "GWh"),
          hjust = 0,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = CHPGenMin$Year+.1,
          y = (CHPGenMin$`HeatGen` * 0.5) + CHPGenMin$`ElecGen`,
          label = paste(format(round(CHPGenMin$`HeatGen`, digits = 0),big.mark = ","), "GWh"),
          hjust = 0,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = CHPGenMax$Year-.1,
          y = CHPGenMax$`ElecGen` * 0.5,
          label = paste(format(round(CHPGenMax$`ElecGen`, digits = 0), big.mark = ","), "GWh"),
          hjust = 1,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = CHPGenMax$Year-.1,
          y = (CHPGenMax$`HeatGen` * 0.5) + CHPGenMax$`ElecGen`,
          label = paste(format(round(CHPGenMax$`HeatGen`,digits = 0), big.mark = ","), "GWh"),
          hjust = 1,
          vjust = 0,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(CHPGen$Year),
          y = (
            CHPGenMax$`ElecGen` + CHPGenMin$`ElecGen`
          ) * .25,
          label = "Electricity\ngenerated",
          hjust = .5,
          vjust = 1,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(CHPGen$Year),
          y = ((CHPGenMax$`HeatGen` + CHPGenMin$`HeatGen`) *
                 .25
          ) + ((
            CHPGenMax$`ElecGen` + CHPGenMin$`ElecGen`
          ) * .5
          ),
          label = "Heat\ngenerated",
          hjust = .5,
          vjust = 1.5,
          colour = "white",
          fontface = 2,
          family = "Century Gothic"
        )
      
      CHPGenChart
      
      
      CHPGenChart <-
        StackedArea(CHPGenChart,
                    CHPGen,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      
      CHPGenChart <- CHPGenChart

      
      ggsave(
        file,
        plot =  CHPGenChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
      
    }
  )
}

