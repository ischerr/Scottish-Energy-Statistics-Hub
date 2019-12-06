require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

EnConsumptionEUOutput <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(8,
                    h3("Renewable electricity as a percentage of gross consumption across the EU", style = "color: #34d1a3;  font-weight:bold"),
                    h4(textOutput(ns('EnConsumptionEUSubtitle')), style = "color: #34d1a3;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EnConsumptionEU.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    #dygraphOutput(ns("EnConsumptionEUPlot")),
    plotlyOutput(ns("EnConsumptionEUPlot"), height = "700px")%>% withSpinner(color="#34d1a3"),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10,h3("Commentary", style = "color: #34d1a3;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
    fluidRow(
    column(10, h3("Data", style = "color: #34d1a3;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EnConsumptionEUTable"))%>% withSpinner(color="#34d1a3"))),
    tags$hr(style = "height:3px;border:none;color:#34d1a3;background-color:#34d1a3;"),
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
EnConsumptionEU <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("EnConsumptionEU.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$EnConsumptionEUSubtitle <- renderText({
    
    EnConsumptionEU <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Energy consump EU", col_names = TRUE, 
                          skip = 18, n_max = 30)
    
    EnConsumptionEU <- EnConsumptionEU[,c(1:ncol(EnConsumptionEU)-1)]
    
    
    
    names(EnConsumptionEU)[1] <- c("Countries")
    
    EnConsumptionEU <- EnConsumptionEU %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EnConsumptionEU <- EnConsumptionEU %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EnConsumptionEU[2:ncol(EnConsumptionEU)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EnConsumptionEU)), na.rm = TRUE))
  })
  
 
  output$EnConsumptionEUPlot <- renderPlotly  ({
    
    ChartColours <- c("#34d1a3", "#fc9272", "#99d8c9")
    
    EnConsumptionEU <- read_excel("Structure/CurrentWorking.xlsx",
                                      sheet = "Energy consump EU", col_names = TRUE, 
                                      skip = 18, n_max = 30)
    
    EnConsumptionEU <- EnConsumptionEU[,c(1,ncol(EnConsumptionEU)-1)]
    
    names(EnConsumptionEU) <- c("Countries", "Renewables")
    
    EnConsumptionEU <- EnConsumptionEU %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EnConsumptionEU <- merge(EnConsumptionEU, EUFlagLookup)
    
    EnConsumptionEU$Group <- ifelse(EnConsumptionEU$Renewables > 0 & EnConsumptionEU$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), ChartColours[2],
                            ifelse(EnConsumptionEU$Renewables <= 0 & EnConsumptionEU$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), ChartColours[1],
                                   ifelse(EnConsumptionEU$Renewables > 0 & EnConsumptionEU$Renewables %in% c(min(EnConsumptionEU$Renewables), max(EnConsumptionEU$Renewables)), ChartColours[2],
                                          ifelse(EnConsumptionEU$Renewables <= 0 & EnConsumptionEU$Renewables %in% c(min(EnConsumptionEU$Renewables), max(EnConsumptionEU$Renewables)), ChartColours[1],      
                                                 ifelse(EnConsumptionEU$Renewables <= 0 , ChartColours[3],  
                                                        ChartColours[2])))))
    
    EnConsumptionEU$Countries <- paste0("<b>", EnConsumptionEU$Countries, "</b>")
    
    #EnConsumptionEU <- EnConsumptionEU[order(-EnConsumptionEU$Renewables),]
    
    p <- plot_ly(
      data = EnConsumptionEU,
      y = ~Countries,
      x = ~Renewables,
      text = paste0(
        "Share of Renewable Energy: ",
        percent(EnConsumptionEU$Renewables, accuracy = 0.1),
        "\nCountry: ",
        EnConsumptionEU$Countries
      ),
      name = "EU Renewable Energy",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  as.list(EnConsumptionEU$Group))
    )  %>% 
      add_trace(
        x = 0.15,
        y = "<b>Ireland</b>",
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Consumption decreased\nsince 2005", "</b>"),
        textposistion = 'center',
        textfont = list(color = ChartColours[1]),
        marker = list(
          size = 0,
          opacity = 0
        )
      ) %>% 
      add_trace(
        x = -0.125,
        y = "<b>Poland</b>",
        type = 'scatter',
        showlegend = FALSE ,
        mode = 'text',
        hoverinfo = 'skip',
        text = paste0("<b>", "Consumption increased\nsince 2005", "</b>"),
        textposistion = 'center',
        textfont = list(color = ChartColours[2]),
        marker = list(
          size = 0,
          opacity = 0
        )
      ) %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#34d1a3"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     showgrid = FALSE,
                     categoryorder = "array",
                     categoryarray = EnConsumptionEU[order(-EnConsumptionEU$Renewables),]$Countries),
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
    
    
    
    
  })
  
  
  output$EnConsumptionEUTable = renderDataTable({
    
    EnConsumptionEU <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Energy consump EU", col_names = TRUE, 
                          skip = 18, n_max = 30)
    
    EnConsumptionEU <- EnConsumptionEU[,c(1:ncol(EnConsumptionEU))]
    
    
    
    names(EnConsumptionEU)[1] <- c("Countries")
    
    EnConsumptionEU <- EnConsumptionEU %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EnConsumptionEU <- EnConsumptionEU %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EnConsumptionEU[2:ncol(EnConsumptionEU)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      EnConsumptionEU,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(EnConsumptionEU)-2, 'desc')),
        title = "Change in final energy consumption in EU countries",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Change in final energy consumption in EU countries',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Change in final energy consumption in EU countries')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:ncol(EnConsumptionEU), 1)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/4 - Energy Efficiency/EnConsumptionEU.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("EnConsumptionEUTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$EnConsumptionEU.png <- downloadHandler(
    filename = "EnConsumptionEU.png",
    content = function(file) {

      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      EUConsumption <- read_excel("Structure/CurrentWorking.xlsx",
                           sheet = "Energy consump EU", col_names = FALSE, 
                           skip = 19, n_max = 30)
      
      EUConsumption <- EUConsumption[,c(1,ncol(EUConsumption)-1)]
      
      names(EUConsumption) <- c("Countries", "Renewables")
      
      EUConsumption <- EUConsumption %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
      
      EUConsumption <- merge(EUConsumption, EUFlagLookup)
      
      EUConsumption$Group <- ifelse(EUConsumption$Renewables > 0 & EUConsumption$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                    ifelse(EUConsumption$Renewables <= 0 & EUConsumption$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "B",
                                           ifelse(EUConsumption$Renewables > 0 & EUConsumption$Renewables %in% c(min(EUConsumption$Renewables), max(EUConsumption$Renewables)), "E",
                                                  ifelse(EUConsumption$Renewables <= 0 & EUConsumption$Renewables %in% c(min(EUConsumption$Renewables), max(EUConsumption$Renewables)), "B",      
                                                         ifelse(EUConsumption$Renewables <= 0 , "A",  
                                                                "D")))))
      
      EUConsumption <- EUConsumption[order(EUConsumption$Renewables),]
      
      ### variables
      ChartColours <- c("#34d1a3", "#99d8c9", "#a3d65c")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Change in final energy consumption across\nEU countries against 2005-07 baseline"
      
      
      EUConsumption <- EUConsumption[order(-EUConsumption$Renewables),]
      EUConsumption$Countries <-
        factor(EUConsumption$Countries, levels = EUConsumption$Countries)
      
      EUConsumptionChart <-
        EUConsumption %>%  ggplot(aes(x = Countries, y = Renewables, fill = Group)) +
        geom_flag(aes(
          y = -ifelse(EUConsumption$Renewables > 0, .01, -.01) ,
          size = 10,
          country = Flag
        )) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        ylim(-.23, .32) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(
          "Group",
          values = c(
            "A" = ChartColours[2],
            "B" = ChartColours[1],
            "C" = ChartColours[2],
            "D" = "#fc9272",
            "E" = "#fc9272"
          )
        ) +
        geom_text(
          label = ifelse(
            EUConsumption$Group == "B" |
              EUConsumption$Group == "C" |
              EUConsumption$Group == "E" ,
            scales::percent(EUConsumption$Renewables) ,
            ""
          ),
          fontface = 2,
          family = "Century Gothic",
          hjust = ifelse(EUConsumption$Renewables > 0, 1.1,-.1),
          vjust = .5,
          color = "white"
        ) +
        geom_text(
          y = ifelse(EUConsumption$Renewables > 0, -0.015, 0.02),
          label = EUConsumption$Countries,
          fontface = 2,
          family = "Century Gothic",
          hjust = ifelse(EUConsumption$Renewables > 0, 1.1, -0),
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
          xmin = match("SCOTLAND", EUConsumption$Countries) - .55,
          xmax = match("SCOTLAND", EUConsumption$Countries) + .55,
          ymax = .277
        ) +
        annotation_custom(
          LatviaFlag,
          xmin = match("Latvia", EUConsumption$Countries) - .55,
          xmax = match("Latvia", EUConsumption$Countries) + .55,
          ymax = .277
        ) +
        annotate(
          "text",
          x = 2.2,
          y = .25,
          label = "Consumption\nincreased\nsince 2005",
          hjust = 0.5,
          vjust = 0,
          colour = "#fc9272",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = 20,
          y = -.16,
          label = "Consumption\ndecreased\nsince 2005",
          hjust = 0.5,
          vjust = 0,
          colour = ChartColours[1],
          fontface = 2,
          family = "Century Gothic"
        )
      
      
      EUConsumptionChart
      
      ggsave(
        file,
        plot =  EUConsumptionChart,
        bg = "white",
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
}
