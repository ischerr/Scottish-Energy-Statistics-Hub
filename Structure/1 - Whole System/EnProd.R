require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnProdOutput <- function(id) {
  ns <- NS(id)
  tagList(
      tabsetPanel(
      tabPanel("Energy productivity target progress", 
             fluidRow(
                     column(8,
                      h3("Energy productivity target progress", style = "color: #1A5D38;  font-weight:bold"),
                      h4(textOutput(ns('EnProdSubtitle')), style = "color: #1A5D38;")
                     ),
                      column(
                           4, style = 'padding:15px;',
                           downloadButton(ns('EnProd.png'), 'Download Graph', style="float:right")
                         )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("RenEnTgtPlot")),
             plotlyOutput(ns("EnProdPlot"))%>% withSpinner(color="#1A5D38"),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
      tabPanel("Change in energy productivity", 
             fluidRow(column(8,
                                h3("Estimated change in energy productivity", style = "color: #1A5D38;  font-weight:bold"),
                             h4(textOutput(ns('EnProdHistSubtitle')), style = "color: #1A5D38;")
             ),
                         column(
                           4, style = 'padding:15px;',
                           downloadButton(ns('EnProdHist.png'), 'Download Graph', style="float:right")
                         )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("RenEnTgtPlot")),
             plotlyOutput(ns("EnProdHistPlot"))%>% withSpinner(color="#1A5D38"),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"))
    ),
    fluidRow(
      column(10,h3("Commentary", style = "color: #1A5D38;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    fluidRow(
      column(10, h3("Data", style = "color: #1A5D38;  font-weight:bold")),
      column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnProdTable"))%>% withSpinner(color="#1A5D38"))),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
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
EnProd <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("EnProd.R")


#####################################################################
  output$EnProdPlot <- renderPlotly  ({
    
    EnProd <- read_excel(
      "Structure/CurrentWorking.xlsx", 
      sheet = "Energy productivity", col_names = FALSE, 
      skip = 26, n_max = 4)
    
    EnProd <- as.data.frame(t(EnProd))
    
    EnProd <- EnProd[c(1,3)]
    
    EnProd <- EnProd[complete.cases(EnProd),]
    
    names(EnProd) <- c("Year", "Renewables")
    
    EnProd %<>% lapply(function(x) as.numeric(as.character(x)))
    
    EnProd <- merge(EnProd, data.frame(Year = 2030, Renewables = NA, Tgt = 0.3), all = T)
    
    ### Variables
    ChartColours <- c("#1a5d38", "#FF8500")
    sourcecaption = "Source: BEIS, SG"
    plottitle = "Energy productivity target progress"
    
    #EnProd$OilPercentage <- PercentLabel(EnProd$Oil)
    
    EnProd$Year <-
      paste0("01/01/", EnProd$Year)
    
    EnProd$Year <- dmy(EnProd$Year)
    
    p <-  plot_ly(EnProd, x = ~ Year) %>% 
      add_trace(
      y = ~ Renewables,
      name = "Renewables",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "Progress: ",
        percent(EnProd$Renewables, accuracy = 0.1),
        "\nYear: ",
        format(EnProd$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = tail(EnProd[which(EnProd$Renewables > 0 | EnProd$Renewables < 0),],1),
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewables",
        text = paste0(
          "Progress: ",
          percent(tail(EnProd[which(EnProd$Renewables > 0 | EnProd$Renewables < 0),],1)$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(tail(EnProd[which(EnProd$Renewables > 0 | EnProd$Renewables < 0),],1)$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[1])
      ) %>%
      add_trace(
        data = EnProd[nrow(EnProd), ],
        x = ~ Year,
        y = ~ Tgt,
        name = "Target",
        text = paste0(
          "Target: ",
          percent(EnProd$Tgt, accuracy = 0.1),
          "\nYear: ",
          format(EnProd$Year, "%Y")
        ),
        hoverinfo = 'text',
        mode = 'markers',
        marker = list(size = 25,
                      symbol = "diamond",
                      color = ChartColours[2])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(EnProd$Year)-100, max(EnProd$Year)+100)),
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
  
  output$EnProdSubtitle <- renderText({
    
    EnProd <- read_excel(
      "Structure/CurrentWorking.xlsx", 
      sheet = "Energy productivity", col_names = FALSE, 
      skip = 26, n_max = 4)
    
    EnProd <- as.data.frame(t(EnProd))
    
    EnProd <- EnProd[c(1,3)]
    
    EnProd <- EnProd[complete.cases(EnProd),]
    
    names(EnProd) <- c("Year", "Renewables")
    
    EnProd %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(EnProd$Year),"-", max(EnProd$Year))
    
    
    
  })

  output$EnProdHistPlot <- renderPlotly  ({
    
    EnProdHist <- read_excel(
      "Structure/CurrentWorking.xlsx", 
      sheet = "Energy productivity", col_names = FALSE, 
      skip = 26, n_max = 4)
    
    
    EnProdHist <- as.data.frame(t(EnProdHist))
    
    EnProdHist <- EnProdHist[c(1,4)]
    
    EnProdHist <- EnProdHist[complete.cases(EnProdHist),]
    
    names(EnProdHist) <- c("Year", "Renewables")
    
    EnProdHist %<>% lapply(function(x) as.numeric(as.character(x)))
    
    EnProdHist <- as.data.frame(EnProdHist)
    
    plottitle <- "Estimated change in energy productivity"
    sourcecaption <- "Source: BEIS, SG"
    ChartColours <- c("#1a5d38", "#FF8500")
    
    #EnProdHist$OilPercentage <- PercentLabel(EnProdHist$Oil)
    
    EnProdHist$Year <-
      paste0("01/01/", EnProdHist$Year)
    
    EnProdHist$Year <- dmy(EnProdHist$Year)
    
    p <-  plot_ly(
      EnProdHist,
      x = ~ Year,
      y = ~ Renewables,
      name = "Renewables",
      type = 'scatter',
      mode = 'lines',
      text = paste0(
        "Progress: ",
        percent(EnProdHist$Renewables, accuracy = 0.1),
        "\nYear: ",
        format(EnProdHist$Year, "%Y")
      ),
      hoverinfo = 'text',
      line = list(width = 6, color = ChartColours[1], dash = "none")
    ) %>%
      add_trace(
        data = EnProdHist[nrow(EnProdHist), ],
        x = ~ Year,
        y = ~ `Renewables`,
        name = "Renewables",
        text = paste0(
          "Progress: ",
          percent(EnProdHist$Renewables, accuracy = 0.1),
          "\nYear: ",
          format(EnProdHist$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        mode = 'markers',
        marker = list(size = 18, color = ChartColours[1])
      ) %>%
      layout(
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(EnProdHist$Year)-100, max(EnProdHist$Year)+100)),
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
  
  output$EnProdHistSubtitle <- renderText({
    
    EnProdHist <- read_excel(
      "Structure/CurrentWorking.xlsx", 
      sheet = "Energy productivity", col_names = FALSE, 
      skip = 26, n_max = 4)
    
    
    EnProdHist <- as.data.frame(t(EnProdHist))
    
    EnProdHist <- EnProdHist[c(1,4)]
    
    EnProdHist <- EnProdHist[complete.cases(EnProdHist),]
    
    names(EnProdHist) <- c("Year", "Renewables")
    
    EnProdHist %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste("Scotland,", min(EnProdHist$Year),"-", max(EnProdHist$Year))
    
    })
  
  output$EnProdTable = renderDataTable({
    
    EnProdData <- read_excel(
      "Structure/CurrentWorking.xlsx", 
      sheet = "Energy productivity", col_names = FALSE, 
      skip = 26, n_max = 10)
    
    
    EnProdData <- as.data.frame(t(EnProdData))
    
    EnProdData <- EnProdData[c(1:4,9:10)]
    
    names(EnProdData) <- as.character(unlist(EnProdData[1,]))
    
    EnProdData <- EnProdData[-1,]
    
    names(EnProdData)[1] <- "Year"
    
    EnProdData <- as_tibble(EnProdData)
    
    
    datatable(
      EnProdData,
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
        title = "Energy Productivity",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Energy Productivity',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Energy Productivity')
        ),
        
        # customize the length menu
      lengthMenu = list( c(10, 20, -1) # declare values
                             , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(3:4, 1) %>% 
      formatRound(5:7, 0) %>% 
      formatRound(2,3)
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/1 - Whole System/EnProd.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleTable, {
    toggle("EnProdTable")
  })
  
  
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnProd.png <- downloadHandler(
    filename = "EnProd.png",
    content = function(file) {

      EnProd <- read_excel(
        "Structure/CurrentWorking.xlsx", 
        sheet = "Energy productivity", col_names = FALSE, 
        skip = 26, n_max = 4)
      
      EnProd <- as.data.frame(t(EnProd))
      
      EnProd <- EnProd[c(1,3)]
      
      EnProd <- EnProd[complete.cases(EnProd),]
      
      names(EnProd) <- c("Year", "Renewables")
      
      EnProd %<>% lapply(function(x) as.numeric(as.character(x)))
      
      EnProd <- merge(EnProd, data.frame(Year = 2030, Renewables = NA, Tgt = 0.3), all = T)
      
      ### Variables
      ChartColours <- c("#1a5d38", "#FF8500")
      sourcecaption = "Source: BEIS, SG"
      plottitle = "Energy productivity target progress"
      
      EnProdTargetChart <-
        function(data,
                 plottitle,
                 sourcecaption,
                 ChartColours) {
          ######
          data[2] <- round(data[2], digits = 3) # Round Numberss
          data$Percentage <-
            percent(data$Renewables, accuracy = 0.1) # Create Percentage Output for Labels
          data$Target <-
            percent(data$Tgt, 0.1) # Create Percentage Output for Labels
          
          
          ### Get Max Year with Actual Data ###
          dataMax <- data
          dataMax$Tgt <- NULL # Remove Target Values
          dataMax$Target <- NULL
          dataMax <-
            dataMax[complete.cases(dataMax), ] #Keep Only Years with Actual Data
          dataMax <- tail(dataMax, 1) # Keep Most Recent Year
          
          
          YearLow <- min(data$Year) # Find Lowest Year
          YearHigh <-
            max(data$Year + 1.1) #Find Hest Year, plus space for plotting
          
          
          
          
          chart <-
            data %>%  ggplot(aes(x = Year), family = "Century Gothic") +
            
            
            ### Line of Values
            geom_line(
              aes(
                y = Renewables,
                colour = "Renewables",
                label = Percentage
              ),
              size = 1.5,
              family = "Century Gothic"
            ) +
            geom_text(
              aes(
                y = Renewables,
                label = ifelse(Year == min(Year), Percentage, ''),
                colour = "Renewables",
                hjust = ifelse(max(data$Tgt, na.rm = TRUE) > 0, 0.4, -.6),
                vjust = ifelse(max(data$Tgt, na.rm = TRUE) > 0, -1, 1.3)
              ),
              family = "Century Gothic"
            ) +
            
            ### X Axis Labels
            
            geom_text(
              aes(
                y = ifelse(dataMax$Renewables > 0, -.005, -.02),
                x = min(Year),
                label = min(Year),
                colour = "Renewables",
                hjust = 0.5,
                vjust = ifelse(max(data$Tgt, na.rm = TRUE) > 0, 1.5, -4.5),
                fontface = 2
              ),
              family = "Century Gothic"
            ) +
            
            geom_text(
              aes(
                y = ifelse(dataMax$Renewables > 0, -.005, -.015),
                x = dataMax$Year,
                label = dataMax$Year,
                colour = "Renewables",
                hjust = 0.5,
                vjust = ifelse(max(data$Tgt, na.rm = TRUE) > 0, 1.5, -4.5),
                fontface = 2
              ),
              
              family = "Century Gothic"
            ) +
            geom_text(
              aes(
                y = ifelse(dataMax$Renewables > 0, -.005, -.015),
                x = Year,
                label = ifelse(Tgt > 0 | Tgt < 0, Year, ""),
                # Labels only years with a Target Value
                colour = "Target",
                hjust = 0.5,
                vjust = ifelse(Tgt > 0, 1.5, -4.5),
                fontface = 2
              ),
              
              ### Point Labels
              
              family = "Century Gothic"
            ) +
            geom_point(aes(
              y = Tgt,
              colour = "Target",
              label = Target
            ),
            size = 6,
            shape = 18) +
            geom_text(aes(
              y = Tgt,
              label = paste0("Target:\n",Target),
              colour = "Target",
              vjust = 1.4
            ),
            family = "Century Gothic") +
            geom_point(
              data = dataMax,
              aes(
                x = dataMax$Year,
                y = Renewables,
                colour = "Renewables",
                label = Percentage,
                show_guide = FALSE
              ),
              size = 4,
              family = "Century Gothic"
            ) +
            geom_text(
              data = dataMax,
              aes(
                x = dataMax$Year,
                y = Renewables,
                label = Percentage,
                hjust = 0.4,
                vjust = -1,
                colour = "Renewables",
                fontface = 2
              ),
              family = "Century Gothic"
            ) +
            ### Axis Settingss
            scale_x_continuous(breaks = seq(YearLow, YearHigh, 2)) +
            scale_y_continuous(labels = scales::percent) +
            scale_color_manual(values = ChartColours) +
            
            ### Label Plot
            labs(y = "Percentage", caption = sourcecaption) +
            labs(
              title = plottitle,
              face = 2,
              subtitle = paste("Scotland,", min(data$Year), "-", dataMax$Year)
            ) +
            ### Theme Options ###
            
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
              #, axis.text.x = element_text(colour = "black", face="bold")
              ,
              axis.text.x = ggplot2::element_blank()
              ,
              axis.text.y =  ggplot2::element_blank()
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
              plot.title = ggplot2::element_text(face = 2)
            ) +
            
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
              y = Inf,
              yend = Inf,
              color = ChartColours[1],
              x = -Inf,
              xend = Inf,
              size = 1.5
            ) +
            annotate(
              geom = 'segment',
              y = -Inf,
              yend = -Inf,
              color = ChartColours[1],
              x = -Inf,
              xend = Inf,
              size = 1
            )+
            xlim(min(data$Year)-1,max(data$Year)+1)
          #geom_hline(yintercept=-0.03, color = ChartColours[2], alpha = 0.7)
        }
      
      EnProdChart <-
        EnProdTargetChart(EnProd, plottitle, sourcecaption, ChartColours)
      
      EnProdChart <- EnProdChart +
        xlim(min(EnProd$Year -1), max(EnProd$Year +1))
      
      ggsave(
        file,
        plot = EnProdChart,
        width = 14,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EnProdHist.png <- downloadHandler(
    filename = "EnProdHist.png",
    content = function(file) {
      
      EnProdHist <- read_excel(
        "Structure/CurrentWorking.xlsx", 
        sheet = "Energy productivity", col_names = FALSE, 
        skip = 26, n_max = 4)
      
      
      EnProdHist <- as.data.frame(t(EnProdHist))
      
      EnProdHist <- EnProdHist[c(1,4)]
      
      EnProdHist <- EnProdHist[complete.cases(EnProdHist),]
      
      names(EnProdHist) <- c("Year", "Renewables")
      
      EnProdHist %<>% lapply(function(x) as.numeric(as.character(x)))
      
      EnProdHist <- as.data.frame(EnProdHist)
      
      plottitle <- "Estimated change in energy productivity"
      sourcecaption <- "Source: BEIS, SG"
      ChartColours <- c("#1a5d38", "#FF8500")
      
      
      EnProdHistChart <-
        EnProdHist %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        
        ### Line of Values
        geom_line(
          aes(y = Renewables,
              colour = ChartColours[1]),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == min(Year), percent(Renewables, accuracy = 0.1), ""),
            hjust = 0.9,
            vjust = -.8,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == max(Year), percent(Renewables, accuracy = 0.1), ""),
            hjust = 0.5,
            vjust = - 0.8,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EnProdHist, 1),
          aes(
            x = Year,
            y = Renewables,
            colour = ChartColours[1],
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
                             Year == min(Year), Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == 2015, Year, ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          aes(
            x = 2015,
            y = Renewables[which(Year == 2015)],
            colour = ChartColours[1],
            show_guide = FALSE
          ),
          size = 3,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == 2015, percent(Renewables, accuracy = 0.1), ""), ### NEEDS AUTOMATED
            hjust = 0.5,
            vjust = -0.8,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      EnProdHistChart <-
        LinePercentChart(EnProdHistChart,
                         EnProdHist,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      EnProdHistChart <- EnProdHistChart +
        xlim(min(EnProdHist$Year)-0.5,max(EnProdHist$Year)+0.5)
      
      EnProdHistChart
      
      ggsave(
        file,
        plot = EnProdHistChart,
        width = 14,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  )
}
