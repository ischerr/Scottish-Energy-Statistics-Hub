require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



ComplaintsOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Complaints volume",
    fluidRow(column(8,
                    h3("Volume of energy complaints to Ombudsman Services in Scotland and elsewhere", style = "color: #68c3ea;  font-weight:bold"),
                    h4(textOutput(ns('ComplaintsPropSubtitle')), style = "color: #68c3ea;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ComplaintsProp.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    #dygraphOutput(ns("ComplaintsPropPlot")),
    plotlyOutput(ns("ComplaintsPropPlot"), height = "500px")%>% withSpinner(color="#68c3ea"),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Complaints by type",
             fluidRow(column(8,
                             h3("Proportion of energy complaints by type", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('ComplaintsTypeSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ComplaintsType.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("ComplaintsPropPlot")),
             plotlyOutput(ns("ComplaintsTypePlot"), height = "800px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Complaints by outcome",
             fluidRow(column(8,
                             h3("Proportion of energy complaints by outcome", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('ComplaintsOutcomeSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ComplaintsOutcome.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("ComplaintsPropPlot")),
             plotlyOutput(ns("ComplaintsOutcomePlot"), height = "500px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Complaints by region",
             fluidRow(column(8,
                             h3("Energy complaint volume by District of Scotland", style = "color: #68c3ea;  font-weight:bold"),
                             h4(textOutput(ns('ComplaintsAreaSubtitle')), style = "color: #68c3ea;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('ComplaintsArea.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
             #dygraphOutput(ns("ComplaintsPropPlot")),
             plotlyOutput(ns("ComplaintsAreaPlot"), height = "800px")%>% withSpinner(color="#68c3ea"),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #68c3ea;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"),
    tabsetPanel(
      tabPanel("Complaints volume",
    fluidRow(
    column(10, h3("Data - Volume of energy complaints to Ombudsman Services in Scotland and elsewhere", style = "color: #68c3ea;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable1"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("ComplaintsPropTable"))%>% withSpinner(color="#68c3ea"))),
    tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Complaints by type",
             fluidRow(
               column(10, h3("Data - Proportion of energy complaints by type", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ComplaintsTypeTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Complaints by outcome",
             fluidRow(
               column(10, h3("Data - Proportion of energy complaints by outcome", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ComplaintsOutcomeTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;")),
    tabPanel("Complaints by region",
             fluidRow(
               column(10, h3("Data - Energy complaint volume by District of Scotland", style = "color: #68c3ea;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("ComplaintsAreaTable"))%>% withSpinner(color="#68c3ea"))),
             tags$hr(style = "height:3px;border:none;color:#68c3ea;background-color:#68c3ea;"))),
    fluidRow(
      column(2, p("Update expected:")),
      column(2,
             DateLookup(c("Ombudsman"))),
      column(1, align = "right",
             p("Sources:")),
      column(7, align = "right",
        SourceLookup("Ombudsman")
        
      )
    )
  )
}




###### Server ######
Complaints <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("ComplaintsProp.R")
  ###### Renewable Energy ###### ######


  ComplaintsProp <- read_csv("Processed Data/Output/Consumers/MarketShare.csv")
  
  ChartColours <- c("#4292c6", "#7bccc4", "#08519c", "#ef3b2c")
  sourcecaption = "Source: Xoserve, Ofgem"
  plottitle = "Market Shares, combined electricity and gas"
  
  ### From ESD ###
  
  output$ComplaintsPropSubtitle <- renderText({
    
    paste("Scotland, 2019")
  })
  
  output$ComplaintsPropPlot <- renderPlotly({
    
    ComplaintsProp <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsScotProp.csv", 
                  "\t", escape_double = FALSE, trim_ws = TRUE)[1:2]
    
    names(ComplaintsProp) <- c("variable", "value")
    
    
    ChartColours <- c("#2b8cbe", "#fc9272", "#34d1a3", "#02818a")

    ComplaintsProp$variable <- paste0("<b>", ComplaintsProp$variable, "</b>")
    
    ComplaintsProp <- ComplaintsProp[seq(dim(ComplaintsProp)[1],1),]
    
    p <- plot_ly(
      data = ComplaintsProp,
      labels = ~variable,
      type = 'pie',
      sort = FALSE,
      values = ~value,
      text = paste0(
        ComplaintsProp$variable,
        ": ", format(round(ComplaintsProp$value, 0), big.mark = ","), " Complaints" 
      ),
      textposition = 'outside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      marker = list(colors = ChartColours,
                    line = list(color = '#FFFFFF', width = 1))
    )  %>% 
      layout(
        barmode = 'stack',
        sort = 'false',
        margin = list(l = 1,
                      r = 1,
                      b = 20,
                      t = 40,
                      pad = 1),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE),
        xaxis = list(
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
    
    #orca(p, "StaticCharts/ComplaintsPropSectorPie.svg")
    
  })
  
  
  output$ComplaintsPropTable = renderDataTable({
    
    ComplaintsProp <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsScotProp.csv", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    ComplaintsProp <- ComplaintsProp[seq(dim(ComplaintsProp)[1],1),]

    datatable(
      ComplaintsProp,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Volume of energy complaints to Ombudsman Services in Scotland and elsewhere",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Volume of energy complaints to Ombudsman Services in Scotland and elsewhere',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Volume of energy complaints to Ombudsman Services in Scotland and elsewhere')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(c(3), 1)
  })
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/5 - Consumers/Complaints.txt")[2])
                     
                   )))
  })
  
  
  observeEvent(input$ToggleTable1, {
    toggle("ComplaintsPropTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("ComplaintsAreaTable")
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("ComplaintsTypeTable")
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("ComplaintsOutcomeTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$ComplaintsProp.png <- downloadHandler(
    filename = "ComplaintsProp.png",
    content = function(file) {
      
      writePNG(readPNG("Structure/5 - Consumers/ComplaintsVolume.png"), file)
      
    }
  )
  
  output$ComplaintsAreaSubtitle <- renderText({
    
    paste("Scotland, 2019")
  })
  
  output$ComplaintsAreaPlot <- renderPlotly({
    ChartColours <- c("#2b8cbe", "#fc9272", "#34d1a3", "#02818a")
    BarColours <- c("#2b8cbe", "#fc9272", "#34d1a3", "#02818a")
    
    ComplaintsArea <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsDistrict.csv", 
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(ComplaintsArea) <- c("Region", "Renewables", "Proportion")
    
    ComplaintsArea <- ComplaintsArea[order(ComplaintsArea$Renewables),]
    
    ComplaintsArea$RegionFormat <- paste0("<b>",ComplaintsArea$Region, "</b>")
    
    p <-  plot_ly(ComplaintsArea, y = ~ RegionFormat ) %>%  
      add_trace(x = ~ `Renewables`, 
                orientation = 'h',
                name = "Renewables",
                type = 'bar',
                legendgroup = "1",
                text = paste0(
                  format(ComplaintsArea$`Renewables`, big.mark = ",", trim = TRUE)," Complaints\n",
                  percent(ComplaintsArea$Proportion,0.1), "\n",
                  ComplaintsArea$Region, "\n"),
                hoverinfo = 'text',
                marker = list(color = BarColours[1])
      ) %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "",
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(
          title = "",
          tickformat = "",
          ticktext = as.list(ComplaintsArea$`Region`),
          tickmode = "array",
          tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
          showgrid = FALSE,
          zeroline = FALSE,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
  })
  
  output$ComplaintsAreaTable = renderDataTable({
    
    ComplaintsArea <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsDistrict.csv", 
                                      "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(ComplaintsArea) <- c("Region", "Number of Complaints", "Proportion of Complaints")
    
    datatable(
      ComplaintsArea,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Energy complaint volume by District of Scotland",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Energy complaint volume by District of Scotland",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Energy complaint volume by District of Scotland")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2,0) %>% 
      formatPercentage(3, 1)
  })
  
  output$ComplaintsArea.png <- downloadHandler(
    filename = "ComplaintsArea.png",
    content = function(file) {
      
      ComplaintsArea  <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsDistrict.csv", 
                                      "\t", escape_double = FALSE, trim_ws = TRUE)
      
      names(ComplaintsArea) <- c("Region", "Renewables")
      
      ComplaintsArea <- ComplaintsArea[order(ComplaintsArea$Renewables),]
      ### variables
      ChartColours <- c("#2b8cbe", "#fc9272", "#34d1a3", "#02818a")
      sourcecaption = "Source: Ombudsman Services"
      plottitle = "Energy complaint volume by District of Scotland"
      
      length <- max(ComplaintsArea$Renewables)
      
      
      ComplaintsArea$Region <-
        factor(ComplaintsArea$Region, levels = ComplaintsArea$Region)
      
      ComplaintsAreaChart <-
        ComplaintsArea %>%  ggplot(aes(x = Region, y = Renewables)) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        geom_bar(stat = "identity", fill = ChartColours[1]) +
        coord_flip() +
        geom_text(
          y = -(length*0.01),
          label = ComplaintsArea$Region,
          fontface = 2,
          family = "Century Gothic",
          hjust = 1,
          vjust = .5,
          color = ChartColours[1]
        ) +
        geom_text(
          y = ComplaintsArea$Renewables + (length*0.01),
          label = format(ComplaintsArea$Renewables, big.mark = ",", trim = TRUE),
          fontface = 2,
          family = "Century Gothic",
          hjust = 0,
          vjust = .5,
          color = ChartColours[1]
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
             subtitle = "Scotland, 2019") +
        ### 0 Axis
        
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
        )  +
        ylim(-(length*0.6), (length*1.1))
      
      ComplaintsAreaChart
      
      ggsave(
        file,
        plot =  ComplaintsAreaChart,
        width = 16,
        height = 16,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
  
  output$ComplaintsTypeSubtitle <- renderText({
    
    paste("Scotland, 2019")
  })
  
  output$ComplaintsTypePlot <- renderPlotly({
    ChartColours <- c("#2b8cbe", "#fc9272", "#34d1a3", "#02818a")
    BarColours <- c("#2b8cbe", "#fc9272", "#34d1a3", "#02818a")
    
    ComplaintsType <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsType.csv", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(ComplaintsType) <- c("Region", "Renewables")
    
    ComplaintsType <- ComplaintsType[order(ComplaintsType$Renewables),]
    
    ComplaintsType$RegionFormat <- paste0("<b>",ComplaintsType$Region, "</b>")
    
    p <-  plot_ly(ComplaintsType, y = ~ RegionFormat ) %>%  
      add_trace(x = ~ `Renewables`, 
                orientation = 'h',
                name = "Renewables",
                type = 'bar',
                legendgroup = "1",
                text = paste0(
                  ComplaintsType$Region,"\n",
                  percent(ComplaintsType$`Renewables`, 0.1)),
                hoverinfo = 'text',
                marker = list(color = BarColours[1])
      ) %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(title = "",
                     zeroline = FALSE,
                     tickformat = "%",
                     showgrid = TRUE,
                     x = 0.5
                     
        ),
        yaxis = list(
          title = "",
          tickformat = "",
          ticktext = as.list(ComplaintsType$`Region`),
          tickmode = "array",
          tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
          showgrid = FALSE,
          zeroline = FALSE,
          rangemode = "tozero"
        )
      ) %>% 
      config(displayModeBar = F)
    
    p
  })
  
  output$ComplaintsTypeTable = renderDataTable({
    
    ComplaintsType <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsType.csv", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(ComplaintsType) <- c("Complaint Type", "Proportion of Cases")
    
    datatable(
      ComplaintsType,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Proportion of energy complaints by type",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Proportion of energy complaints by type",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Proportion of energy complaints by type")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2, 1)
  })
  
  output$ComplaintsType.png <- downloadHandler(
    filename = "ComplaintsType.png",
    content = function(file) {
      
      ComplaintsType  <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsType.csv", 
                                    "\t", escape_double = FALSE, trim_ws = TRUE)
      
      names(ComplaintsType) <- c("Region", "Renewables")
      
      ComplaintsType <- ComplaintsType[order(ComplaintsType$Renewables),]
      ### variables
      ChartColours <- c("#2b8cbe", "#fc9272", "#34d1a3", "#02818a")
      sourcecaption = "Source: Ombudsman Services"
      plottitle = "Proportion of energy complaints by type"
      
      length <- max(ComplaintsType$Renewables)
      
      
      ComplaintsType$Region <-
        factor(ComplaintsType$Region, levels = ComplaintsType$Region)
      
      ComplaintsTypeChart <-
        ComplaintsType %>%  ggplot(aes(x = Region, y = Renewables)) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        geom_bar(stat = "identity", fill = ChartColours[1]) +
        coord_flip() +
        geom_text(
          y = -(length*0.01),
          label = ComplaintsType$Region,
          fontface = 2,
          family = "Century Gothic",
          hjust = 1,
          vjust = .5,
          color = ChartColours[1]
        ) +
        geom_text(
          y = ComplaintsType$Renewables + (length*0.01),
          label = percent(ComplaintsType$Renewables, 0.1),
          fontface = 2,
          family = "Century Gothic",
          hjust = 0,
          vjust = .5,
          color = ChartColours[1]
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
             subtitle = "Scotland, 2019") +
        ### 0 Axis
        
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
        )  +
        ylim(-(length*0.4), (length*1.1))
      
      ComplaintsTypeChart
      
      ggsave(
        file,
        plot =  ComplaintsTypeChart,
        width = 16,
        height = 16,
        units = "cm",
        dpi = 300
      )
      
    }
  )
  
  
  
  
  output$ComplaintsOutcomeSubtitle <- renderText({
    
    paste("Scotland, 2019")
  })
  
  output$ComplaintsOutcomePlot <- renderPlotly({
    ChartColours <- c("#2b8cbe", "#fc9272", "#34d1a3", "#02818a")
    BarColours <- c("#081d58", "#225ea8", "#41b6c4", "#feb24c")
    
    ComplaintsOutcome <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsOutcomes.csv", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(ComplaintsOutcome) <- c("variable", "value")
    
    
    ComplaintsOutcome$variable <- paste0("<b>", ComplaintsOutcome$variable, "</b>")
    
    p <- plot_ly(
      data = ComplaintsOutcome,
      labels = ~variable,
      type = 'pie',
      values = ~value,
      text = paste0(
        ComplaintsOutcome$variable,
        ": ", percent(ComplaintsOutcome$value, 0.1), "" 
      ),
      textposition = 'outside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      marker = list(colors = BarColours,
                    line = list(color = '#FFFFFF', width = 1))
    )  %>% 
      layout(
        barmode = 'stack',
        sort = 'false',
        margin = list(l = 1,
                      r = 1,
                      b = 20,
                      t = 40,
                      pad = 1),
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE),
        xaxis = list(
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
    
    #orca(p, "StaticCharts/ComplaintsOutcomeSectorPie.svg")
  })
  
  output$ComplaintsOutcomeTable = renderDataTable({
    
    ComplaintsOutcome <- read_delim("Processed Data/Output/Consumers/EnergyComplaintsOutcomes.csv", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
    
    names(ComplaintsOutcome) <- c("Outcome", "Proportion of Cases")
    
    datatable(
      ComplaintsOutcome,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        title = "Proportion of energy complaints by outcome",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = "Proportion of energy complaints by outcome",
            header = TRUE
          ),
          list(extend = 'csv',
               title = "Proportion of energy complaints by outcome")
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2, 1)
  })
  
  output$ComplaintsOutcome.png <- downloadHandler(
    filename = "ComplaintsOutcome.png",
    content = function(file) {
      
      writePNG(readPNG("Structure/5 - Consumers/ComplaintsOutcome.png"), file)
      
    }
  )
  
}
