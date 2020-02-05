require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenEnEUOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("EU Renewable Energy",
    fluidRow(column(8,
                    h3("Share of renewable energy in gross final energy consumption across the EU", style = "color: #1A5D38;  font-weight:bold"),
                    h4(textOutput(ns('RenEnEUSubtitle')), style = "color: #1A5D38;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenEnEU.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
    #dygraphOutput(ns("RenEnEUPlot")),
    plotlyOutput(ns("RenEnEUPlot"), height = "600px")%>% withSpinner(color="#1A5D38"),
    tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;")),
    tabPanel("Trend",
             fluidRow(column(8,
                             h3("Trend of renewable energy share in gross final energy consumption", style = "color: #1A5D38;  font-weight:bold"),
                             h4(textOutput(ns('RenEnEUTrendSubtitle')), style = "color: #1A5D38;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenEnEUTrend.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"),
             #dygraphOutput(ns("RenEnEUPlot")),
             plotlyOutput(ns("RenEnEUTrendPlot"))%>% withSpinner(color="#1A5D38"),
             tags$hr(style = "height:3px;border:none;color:#1A5D38;background-color:#1A5D38;"))),
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
      column(12, dataTableOutput(ns("RenEnEUTable"))%>% withSpinner(color="#1A5D38"))),
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
        SourceLookup("BEISFinalConsump"),
        SourceLookup("ETElecGen"),
        SourceLookup("ESTRenHeat")
        
      )
    )
  )
}




###### Server ######
RenEnEU <- function(input, output, session) {

  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenEnEU.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$RenEnEUSubtitle <- renderText({
    
    EURenEn <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable energy EU", col_names = TRUE, 
                          skip = 19, n_max = 30)
    
    EURenEn <- EURenEn[,c(1:ncol(EURenEn))]
    
    
    
    names(EURenEn)[1] <- c("Countries")
    
    EURenEn <- EURenEn %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenEn <- EURenEn %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenEn[2:ncol(EURenEn)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EURenEn)), na.rm = TRUE))
  })
  
  output$RenEnEUTrendSubtitle <- renderText({
    
    EURenEnTrend <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Renewable energy EU", col_names = FALSE, 
                               skip = 19, n_max = 31)
    EURenEnTrend <- as.data.frame(t(EURenEnTrend))
    names(EURenEnTrend) <- as.character(unlist(EURenEnTrend[1,]))
    EURenEnTrend<- tail(EURenEnTrend, -1)
    EURenEnTrend<- head(EURenEnTrend, -1)
    names(EURenEnTrend)[1] <- "Year"
    EURenEnTrend$Year <- as.numeric(as.character(EURenEnTrend$Year))
    EURenEnTrend <- EURenEnTrend[c("Year", "SCOTLAND", "United Kingdom", "EU (28)")]
    names(EURenEnTrend) <- c("Year", "Scotland", "United Kingdom", "European Union")
    EURenEnTrend <- EURenEnTrend[which(EURenEnTrend$Year >= 2009),]
    EURenEnTrend[,c(2,3,4)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(min(EURenEnTrend$Year),"-", max(EURenEnTrend$Year))
  })
  
  output$RenEnEUPlot <- renderPlotly  ({
    
    ChartColours <- c("#1a5d38", "#41ab5d", "#1a5d38")
    
    EURenEn <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable energy EU", col_names = FALSE, 
                          skip = 20, n_max = 30)
    
    EURenEn <- EURenEn[,c(1,ncol(EURenEn)-1)]
    
    names(EURenEn) <- c("Countries", "Renewables")
    
    EURenEn <- EURenEn %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenEn <- merge(EURenEn, EUFlagLookup)
    
    EURenEn$Group <- ifelse(EURenEn$Renewables > 0 & EURenEn$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), ChartColours[1],
                            ifelse(EURenEn$Renewables <= 0 & EURenEn$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                   ifelse(EURenEn$Renewables > 0 & EURenEn$Renewables %in% c(min(EURenEn$Renewables), max(EURenEn$Renewables)), ChartColours[2],
                                          ifelse(EURenEn$Renewables <= 0 & EURenEn$Renewables %in% c(min(EURenEn$Renewables), max(EURenEn$Renewables)), "E",      
                                                 ifelse(EURenEn$Renewables <= 0 , "D",  
                                                        ChartColours[2])))))
    
    EURenEn <- EURenEn[order(-EURenEn$Renewables),]
    
    EURenEn <- EURenEn %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenEn$Countries <- factor(EURenEn$Countries, levels = unique(EURenEn$Countries)[order(EURenEn$Renewables, decreasing = FALSE)])
    
    p <- plot_ly(
      data = EURenEn,
      y = ~Countries,
      x = ~Renewables,
      text = paste0(
        "Share of Renewable Energy: ",
        percent(EURenEn$Renewables, accuracy = 0.1),
        "\nCountry: ",
        EURenEn$Countries
      ),
      name = "EU Renewable Energy",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  as.list(EURenEn$Group))
    )  %>% 
      layout(
        barmode = 'stack',
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
    
    
    
    
  })
  
  output$RenEnEUTable = renderDataTable({
    
    EURenEn <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable energy EU", col_names = TRUE, 
                          skip = 19, n_max = 30)
    
    EURenEn <- EURenEn[,c(1:ncol(EURenEn))]
    
    
    
    names(EURenEn)[1] <- c("Countries")
    
    EURenEn <- EURenEn %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenEn <- EURenEn %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenEn[2:ncol(EURenEn)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      EURenEn,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(EURenEn)-2, 'desc')),
        title = "EU Renewable Energy",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'EU Renewable Energy',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'EU Renewable Energy')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:ncol(EURenEn), 1)
  })
  
  output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/1 - Whole System/RenEnEU.txt")[2])
                    
                  )))
 })
 
  observeEvent(input$ToggleTable, {
    toggle("RenEnEUTable")
  })

  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  output$RenEnEU.png <- downloadHandler(
    filename = "RenEnEU.png",
    content = function(file) {

      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      EURenEn <- read_excel("Structure/CurrentWorking.xlsx",
                            sheet = "Renewable energy EU", col_names = FALSE, 
                            skip = 20, n_max = 30)
      
      EURenEn <- EURenEn[,c(1,ncol(EURenEn)-1)]
    
      names(EURenEn) <- c("Countries", "Renewables")
      
      EURenEn <- EURenEn %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
      
      EURenEn <- merge(EURenEn, EUFlagLookup)
      
      EURenEn$Group <- ifelse(EURenEn$Renewables > 0 & EURenEn$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "B",
                              ifelse(EURenEn$Renewables <= 0 & EURenEn$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                     ifelse(EURenEn$Renewables > 0 & EURenEn$Renewables %in% c(min(EURenEn$Renewables), max(EURenEn$Renewables)), "C",
                                            ifelse(EURenEn$Renewables <= 0 & EURenEn$Renewables %in% c(min(EURenEn$Renewables), max(EURenEn$Renewables)), "E",      
                                                   ifelse(EURenEn$Renewables <= 0 , "D",  
                                                          "A")))))
      
      EURenEn <- EURenEn[order(EURenEn$Renewables),]
      ### variables
      ChartColours <- c("#1a5d38", "#41ab5d", "#1a5d38")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Share of renewable energy in gross final\nenergy consumption across the EU"
      
      
      
      
      EURenEn$Countries <-
        factor(EURenEn$Countries, levels = EURenEn$Countries)
      
      EURenEnChart <-
        EURenEn %>%  ggplot(aes(x = Countries, y = Renewables, fill = Group)) +
        geom_flag(aes(
          y = -.014,
          size = 10,
          country = Flag
        )) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        ylim(-.15, .55) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual("Group",
                          values = c("A" = ChartColours[2], "B" = ChartColours[1], "C" = ChartColours[2])) +
        geom_text(
          label = ifelse(
            EURenEn$Group == "B" |
              EURenEn$Group == "C" ,
            scales::percent(EURenEn$Renewables, 0.1) ,
            ""
          ),
          fontface = 2,
          family = "Century Gothic",
          hjust = 1.1,
          vjust = .5,
          color = "white"
        ) +
        geom_text(
          y = -0.02,
          label = EURenEn$Countries,
          fontface = 2,
          family = "Century Gothic",
          hjust = 1.1,
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
          xmin = match("SCOTLAND", EURenEn$Countries) - .55,
          xmax = match("SCOTLAND", EURenEn$Countries) + .55,
          ymax = .156
        ) +
        annotation_custom(
          LatviaFlag,
          xmin = match("Latvia", EURenEn$Countries) - .55,
          xmax = match("Latvia", EURenEn$Countries) + .55,
          ymax = .156
        )
      
      
      EURenEnChart
      
      ggsave(
        file,
        plot =  EURenEnChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenEnEUTrendPlot <- renderPlotly  ({
    
    EURenEnTrend <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Renewable energy EU", col_names = FALSE, 
                               skip = 19, n_max = 31)
    EURenEnTrend <- as.data.frame(t(EURenEnTrend))
    names(EURenEnTrend) <- as.character(unlist(EURenEnTrend[1,]))
    EURenEnTrend<- tail(EURenEnTrend, -1)
    EURenEnTrend<- head(EURenEnTrend, -1)
    names(EURenEnTrend)[1] <- "Year"
    EURenEnTrend$Year <- as.numeric(as.character(EURenEnTrend$Year))
    EURenEnTrend <- EURenEnTrend[c("Year", "SCOTLAND", "United Kingdom", "EU (28)")]
    names(EURenEnTrend) <- c("Year", "Scotland", "United Kingdom", "European Union")
    EURenEnTrend <- EURenEnTrend[which(EURenEnTrend$Year >= 2009),]
    EURenEnTrend[,c(2,3,4)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    ### variables
    ChartColours <- c("#1a5d38", "#2c7fb8", "#feb24c" ,"#de2d26")
    sourcecaption = "Source: Eurostat, BEIS"
    plottitle = "Trend of renewable energy share\nin gross final energy consumption"
    
    EURenEnTrend$Year <- paste0("01/01/", EURenEnTrend$Year)
    
    EURenEnTrend$Year <- dmy(EURenEnTrend$Year)
    
    p <-  plot_ly(EURenEnTrend,x = ~ Year ) %>% 
      add_trace(data = EURenEnTrend,
                x = ~ Year,
                y = ~ Scotland,
                name = "Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Scotland: ",
                  percent(EURenEnTrend$Scotland, accuracy = 0.1),
                  "\nYear: ",
                  format(EURenEnTrend$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EURenEnTrend[which(EURenEnTrend$Scotland > 0 | EURenEnTrend$Scotland < 0),], 1),
        x = ~ Year,
        y = ~ Scotland,
        legendgroup = "1",
        name = "Scotland",
        text = paste0(
          "Scotland: ",
          percent(EURenEnTrend[which(EURenEnTrend$Scotland > 0 | EURenEnTrend$Scotland < 0),][-1,]$Scotland, accuracy = 0.1),
          "\nYear: ",
          format(EURenEnTrend[which(EURenEnTrend$Scotland > 0 | EURenEnTrend$Scotland < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = EURenEnTrend,
                x = ~ Year,
                y = ~ `United Kingdom`,
                name = "United Kingdom",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "United Kingdom: ",
                  percent(EURenEnTrend$`United Kingdom`, accuracy = 0.1),
                  "\nYear: ",
                  format(EURenEnTrend$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[4], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EURenEnTrend[which(EURenEnTrend$`United Kingdom` > 0 | EURenEnTrend$`United Kingdom` < 0),], 1),
        x = ~ Year,
        y = ~ `United Kingdom`,
        legendgroup = "2",
        name = "United Kingdom",
        text = paste0(
          "United Kingdom: ",
          percent(EURenEnTrend[which(EURenEnTrend$`United Kingdom` > 0 | EURenEnTrend$`United Kingdom` < 0),][-1,]$`United Kingdom`, accuracy = 0.1),
          "\nYear: ",
          format(EURenEnTrend[which(EURenEnTrend$`United Kingdom` > 0 | EURenEnTrend$`United Kingdom` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[4])
      ) %>% 
      add_trace(data = EURenEnTrend,
                x = ~ Year,
                y = ~ `European Union`,
                name = "European Union",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "European Union: ",
                  percent(EURenEnTrend$`European Union`, accuracy = 0.1),
                  "\nYear: ",
                  format(EURenEnTrend$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EURenEnTrend[which(EURenEnTrend$`European Union` > 0 | EURenEnTrend$`European Union` < 0),], 1),
        x = ~ Year,
        y = ~ `European Union`,
        legendgroup = "3",
        name = "European Union",
        text = paste0(
          "European Union: ",
          percent(EURenEnTrend[which(EURenEnTrend$`European Union` > 0 | EURenEnTrend$`European Union` < 0),][-1,]$`European Union`, accuracy = 0.1),
          "\nYear: ",
          format(EURenEnTrend[which(EURenEnTrend$`European Union` > 0 | EURenEnTrend$`European Union` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[3])
      ) %>% 
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#126992"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        
        xaxis = list(title = "",
                     showgrid = FALSE,
                     range = c(min(EURenEnTrend$Year)-100, max(EURenEnTrend$Year)+100)),
        yaxis = list(
          title = "GWh",
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
  
  output$RenEnEUTrend.png <- downloadHandler(
    filename = "RenEnEUTrend.png",
    content = function(file) {
      
      EURenEnTrend <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Renewable energy EU", col_names = FALSE, 
                                 skip = 19, n_max = 31)
      EURenEnTrend <- as.data.frame(t(EURenEnTrend))
      names(EURenEnTrend) <- as.character(unlist(EURenEnTrend[1,]))
      EURenEnTrend<- tail(EURenEnTrend, -1)
      EURenEnTrend<- head(EURenEnTrend, -1)
      names(EURenEnTrend)[1] <- "Year"
      EURenEnTrend$Year <- as.numeric(as.character(EURenEnTrend$Year))
      EURenEnTrend <- EURenEnTrend[c("Year", "SCOTLAND", "United Kingdom", "EU (28)")]
      names(EURenEnTrend) <- c("Year", "Scotland", "United Kingdom", "European Union")
      EURenEnTrend <- EURenEnTrend[which(EURenEnTrend$Year >= 2009),]
      EURenEnTrend[,c(2,3,4)] %<>% lapply(function(x) as.numeric(as.character(x)))
      
      ### variables
      ChartColours <- c("#1a5d38", "#2c7fb8", "#feb24c" ,"#de2d26")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Trend of renewable energy share\nin gross final energy consumption"
      
      
      ### Load Packages and Functions
      
      EURenEnTrendChart <- EURenEnTrend %>%
        ggplot(aes(x = Year), family = "Century Gothic") +
        
        geom_line(
          aes(y = `Scotland`,
              label = percent(`Scotland`)),
          colour = ChartColours[2],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .4,
            y = `Scotland`,
            label = ifelse(Year == min(Year), percent(`Scotland`, accuracy = .1), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .6,
            y = `Scotland`,
            label = ifelse(Year == max(Year), percent(`Scotland`, accuracy = .1), ""),
            hjust = 0.5,
            
            fontface = 2
          ),
          colour = ChartColours[2],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EURenEnTrend, 1),
          aes(x = Year,
              y = `Scotland`,
              show_guide = FALSE),
          colour = ChartColours[2],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(EURenEnTrend$Year),
          y = mean(EURenEnTrend$`Scotland`),
          label = "Scotland",
          hjust = 0.5,
          vjust = 3.5,
          colour = ChartColours[2],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `European Union`,
              label = paste0(`European Union` * 100, "%")),
          colour = ChartColours[3],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .4,
            y = `European Union`,
            label = ifelse(Year == min(Year), percent(
              `European Union`, accuracy  = .1
            ), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year + .6,
            y = `European Union`,
            label = ifelse(Year == max(Year), percent(
              `European Union`, accuracy  = .1
            ), ""),
            hjust = 0.5,
            vjust = 1.2,
            fontface = 2
          ),
          colour = ChartColours[3],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EURenEnTrend, 1),
          aes(x = Year,
              y = `European Union`,
              show_guide = FALSE),
          colour = ChartColours[3],
          size = 4,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(EURenEnTrend$Year),
          y = mean(EURenEnTrend$`European Union`),
          label = "European Union",
          hjust = 0.5,
          vjust = -2.5,
          colour = ChartColours[3],
          fontface = 2,
          family = "Century Gothic"
        ) +
        geom_line(
          aes(y = `United Kingdom`,
              label = paste0(`United Kingdom` * 100, "%")),
          colour = ChartColours[4],
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year - .4,
            y = `United Kingdom`,
            label = ifelse(Year == min(Year), percent(
              `United Kingdom`, accuracy  = .1
            ), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year +.6,
            y = `United Kingdom`,
            label = ifelse(Year == max(Year), percent(
              `United Kingdom`, accuracy  = .1
            ), ""),
            hjust = 0.5,
            fontface = 2
          ),
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(EURenEnTrend, 1),
          aes(x = Year,
              y = `United Kingdom`,
              
              show_guide = FALSE),
          size = 4,
          colour = ChartColours[4],
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = mean(EURenEnTrend$Year),
          y = mean(EURenEnTrend$`United Kingdom`),
          label = "United Kingdom",
          hjust = 0.5,
          vjust = 5,
          colour = ChartColours[4],
          fontface = 2,
          family = "Century Gothic"
        ) +
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
      
      EURenEnTrendChart
      
      EURenEnTrendChart <-
        StackedArea(EURenEnTrendChart,
                    EURenEnTrend,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      EURenEnTrendChart <- EURenEnTrendChart +
        labs(subtitle = paste(min(EURenEnTrend$Year), "-", max(EURenEnTrend$Year))
        )
      
      EURenEnTrendChart
      ggsave(
        file,
        plot =  EURenEnTrendChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
}
