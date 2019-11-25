require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenHeatEUOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("EU Renewable Heat",
    fluidRow(column(8,
                    h3("Renewable Heat as a percentage of gross consumption across the EU", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenHeatEUSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenHeatEU.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenHeatEUPlot")),
    plotlyOutput(ns("RenHeatEUPlot"), height = "600px")%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("EU Renewable Energy Trend",
             fluidRow(column(8,
                             h3("Trend of renewable Heat share in gross final energy consumption", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenHeatEUTrendSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenHeatEUTrend.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenHeatEUPlot")),
             plotlyOutput(ns("RenHeatEUTrendPlot"))%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    fluidRow(
    column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("RenHeatEUTable"))%>% withSpinner(color="#39ab2c"))),
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
        SourceLookup("BEISFinalConsump"),
        SourceLookup("ETHeatGen"),
        SourceLookup("ESTRenHeat")
        
      )
    )
  )
}




###### Server ######
RenHeatEU <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenHeatEU.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$RenHeatEUSubtitle <- renderText({
    
    EURenHeat <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable heat EU", col_names = TRUE, 
                          skip = 18, n_max = 30)
    
    EURenHeat <- EURenHeat[,c(1:ncol(EURenHeat))]
    
    
    
    names(EURenHeat)[1] <- c("Countries")
    
    EURenHeat <- EURenHeat %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenHeat <- EURenHeat %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenHeat[2:ncol(EURenHeat)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EURenHeat)), na.rm = TRUE))
  })
  
  output$RenHeatEUTrendSubtitle <- renderText({
    
    EURenHeatTrend <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Renewable heat EU", col_names = FALSE, 
                                 skip = 18, n_max = 31)
    EURenHeatTrend <- as.data.frame(t(EURenHeatTrend))
    names(EURenHeatTrend) <- as.character(unlist(EURenHeatTrend[1,]))
    EURenHeatTrend<- tail(EURenHeatTrend, -1)
    names(EURenHeatTrend)[1] <- "Year"
    EURenHeatTrend$Year <- as.numeric(as.character(EURenHeatTrend$Year))
    EURenHeatTrend <- EURenHeatTrend[c("Year", "SCOTLAND", "United Kingdom", "EU (28)")]
    names(EURenHeatTrend) <- c("Year", "Scotland", "United Kingdom", "European Union")
    EURenHeatTrend[,c(2,3,4)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(min(EURenHeatTrend$Year),"-", max(EURenHeatTrend$Year))
  })
  
  output$RenHeatEUPlot <- renderPlotly  ({
    
    ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
    
    EURenHeat <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable heat EU", col_names = FALSE, 
                          skip = 19, n_max = 30)
    
    EURenHeat <- EURenHeat[,c(1,ncol(EURenHeat))]
    
    names(EURenHeat) <- c("Countries", "Renewables")
    
    EURenHeat <- EURenHeat %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenHeat <- merge(EURenHeat, EUFlagLookup)
    
    EURenHeat$Group <- ifelse(EURenHeat$Renewables > 0 & EURenHeat$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), ChartColours[1],
                            ifelse(EURenHeat$Renewables <= 0 & EURenHeat$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                   ifelse(EURenHeat$Renewables > 0 & EURenHeat$Renewables %in% c(min(EURenHeat$Renewables), max(EURenHeat$Renewables)), ChartColours[2],
                                          ifelse(EURenHeat$Renewables <= 0 & EURenHeat$Renewables %in% c(min(EURenHeat$Renewables), max(EURenHeat$Renewables)), "E",      
                                                 ifelse(EURenHeat$Renewables <= 0 , "D",  
                                                        ChartColours[2])))))
    
    EURenHeat <- EURenHeat[order(-EURenHeat$Renewables),]
    
    EURenHeat$Countries <- factor(EURenHeat$Countries, levels = unique(EURenHeat$Countries)[order(EURenHeat$Renewables, decreasing = FALSE)])
    
    p <- plot_ly(
      data = EURenHeat,
      y = ~Countries,
      x = ~Renewables,
      text = paste0(
        "Share of Renewable Energy: ",
        percent(EURenHeat$Renewables, accuracy = 0.1),
        "\nCountry: ",
        EURenHeat$Countries
      ),
      name = "EU Renewable Energy",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  as.list(EURenHeat$Group))
    )  %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
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
  
  
  output$RenHeatEUTable = renderDataTable({
    
    EURenHeat <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable heat EU", col_names = TRUE, 
                          skip = 18, n_max = 30)
    
    EURenHeat <- EURenHeat[,c(1:ncol(EURenHeat))]
    
    
    
    names(EURenHeat)[1] <- c("Countries")
    
    EURenHeat <- EURenHeat %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenHeat <- EURenHeat %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenHeat[2:ncol(EURenHeat)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      EURenHeat,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(EURenHeat)-2, 'desc')),
        title = "Renewable Heat EU",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Heat EU',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Heat EU')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:ncol(EURenHeat), 1)
  })
  
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   HTML(
                     paste(readtext("Structure/2 - Renewables/Heat/RenHeatEU.txt")[2])
                     
                   )))
  })
 
 
  observeEvent(input$ToggleTable, {
    toggle("RenHeatEUTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenHeatEU.png <- downloadHandler(
    filename = "RenHeatEU.png",
    content = function(file) {

      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      EUHeat <- read_excel("Structure/CurrentWorking.xlsx",
                              sheet = "Renewable heat EU", col_names = FALSE, 
                              skip = 19, n_max = 30)
      
      EUHeat <- EUHeat[,c(1,ncol(EUHeat))]

      names(EUHeat) <- c("Countries", "Renewables")
      
      EUHeat <- EUHeat %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
      
      EUHeat <- merge(EUHeat, EUFlagLookup)
      
      EUHeat$Group <- ifelse(EUHeat$Renewables > 0 & EUHeat$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "B",
                             ifelse(EUHeat$Renewables <= 0 & EUHeat$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                    ifelse(EUHeat$Renewables > 0 & EUHeat$Renewables %in% c(min(EUHeat$Renewables), max(EUHeat$Renewables)), "C",
                                           ifelse(EUHeat$Renewables <= 0 & EUHeat$Renewables %in% c(min(EUHeat$Renewables), max(EUHeat$Renewables)), "E",      
                                                  ifelse(EUHeat$Renewables <= 0 , "D",  
                                                         "A")))))
      
      EUHeat <- EUHeat[order(EUHeat$Renewables),]
      
      ### variables
      ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Renewable Heat as a percentage of \ngross consumption for EU countries"
      
      
      EUHeat <- EUHeat[order(EUHeat$Renewables),]
      EUHeat$Countries <-
        factor(EUHeat$Countries, levels = EUHeat$Countries)
      
      EUHeatChart <-
        EUHeat %>%  ggplot(aes(x = Countries, y = Renewables, fill = Group)) +
        geom_flag(aes(
          y = -.0164,
          size = 10,
          country = Flag
        )) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        ylim(-.2, .727) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual("Group",
                          values = c("A" = ChartColours[2], "B" = ChartColours[1], "C" = ChartColours[2])) +
        geom_text(
          label = ifelse(
            EUHeat$Group == "B" |
              EUHeat$Group == "C" ,
            scales::percent(EUHeat$Renewables) ,
            ""
          ),
          fontface = 2,
          family = "Century Gothic",
          hjust = ifelse(EUHeat$Renewables > .1, 1.1,-.1),
          vjust = .5,
          color = ifelse(EUHeat$Renewables > .1, "white", ChartColours[2])
        ) +
        geom_text(
          y = -0.04,
          label = EUHeat$Countries,
          fontface = 2,
          family = "Century Gothic",
          hjust = 1,
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
          xmin = match("SCOTLAND", EUHeat$Countries) - .55,
          xmax = match("SCOTLAND", EUHeat$Countries) + .55,
          ymax = .213
        ) +
        annotation_custom(
          LatviaFlag,
          xmin = match("Latvia", EUHeat$Countries) - .55,
          xmax = match("Latvia", EUHeat$Countries) + .55,
          ymax = .213
        )
      
      
      EUHeatChart
      
      
      ggsave(
        file,
        plot =  EUHeatChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenHeatEUTrendPlot <- renderPlotly  ({
    
    EURenHeatTrend <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Renewable heat EU", col_names = FALSE, 
                               skip = 18, n_max = 31)
    EURenHeatTrend <- as.data.frame(t(EURenHeatTrend))
    names(EURenHeatTrend) <- as.character(unlist(EURenHeatTrend[1,]))
    EURenHeatTrend<- tail(EURenHeatTrend, -1)
    names(EURenHeatTrend)[1] <- "Year"
    EURenHeatTrend$Year <- as.numeric(as.character(EURenHeatTrend$Year))
    EURenHeatTrend <- EURenHeatTrend[c("Year", "SCOTLAND", "United Kingdom", "EU (28)")]
    names(EURenHeatTrend) <- c("Year", "Scotland", "United Kingdom", "European Union")
    EURenHeatTrend <- EURenHeatTrend[which(EURenHeatTrend$Year >= 2009),]
    EURenHeatTrend[,c(2,3,4)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    ### variables
    ChartColours <- c("#39ab2c", "#2c7fb8", "#feb24c" ,"#de2d26")
    sourcecaption = "Source: Eurostat, BEIS"
    plottitle = "Trend of renewable energy share\nin gross final energy consumption"
    
    EURenHeatTrend$Year <- paste0("01/01/", EURenHeatTrend$Year)
    
    EURenHeatTrend$Year <- dmy(EURenHeatTrend$Year)
    
    p <-  plot_ly(EURenHeatTrend,x = ~ Year ) %>% 
      add_trace(data = EURenHeatTrend,
                x = ~ Year,
                y = ~ Scotland,
                name = "Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Scotland: ",
                  percent(EURenHeatTrend$Scotland, accuracy = 0.1),
                  "\nYear: ",
                  format(EURenHeatTrend$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EURenHeatTrend[which(EURenHeatTrend$Scotland > 0 | EURenHeatTrend$Scotland < 0),], 1),
        x = ~ Year,
        y = ~ Scotland,
        legendgroup = "1",
        name = "Scotland",
        text = paste0(
          "Scotland: ",
          percent(EURenHeatTrend[which(EURenHeatTrend$Scotland > 0 | EURenHeatTrend$Scotland < 0),][-1,]$Scotland, accuracy = 0.1),
          "\nYear: ",
          format(EURenHeatTrend[which(EURenHeatTrend$Scotland > 0 | EURenHeatTrend$Scotland < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = EURenHeatTrend,
                x = ~ Year,
                y = ~ `United Kingdom`,
                name = "United Kingdom",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "United Kingdom: ",
                  percent(EURenHeatTrend$`United Kingdom`, accuracy = 0.1),
                  "\nYear: ",
                  format(EURenHeatTrend$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[4], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EURenHeatTrend[which(EURenHeatTrend$`United Kingdom` > 0 | EURenHeatTrend$`United Kingdom` < 0),], 1),
        x = ~ Year,
        y = ~ `United Kingdom`,
        legendgroup = "2",
        name = "United Kingdom",
        text = paste0(
          "United Kingdom: ",
          percent(EURenHeatTrend[which(EURenHeatTrend$`United Kingdom` > 0 | EURenHeatTrend$`United Kingdom` < 0),][-1,]$`United Kingdom`, accuracy = 0.1),
          "\nYear: ",
          format(EURenHeatTrend[which(EURenHeatTrend$`United Kingdom` > 0 | EURenHeatTrend$`United Kingdom` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[4])
      ) %>% 
      add_trace(data = EURenHeatTrend,
                x = ~ Year,
                y = ~ `European Union`,
                name = "European Union",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "European Union: ",
                  percent(EURenHeatTrend$`European Union`, accuracy = 0.1),
                  "\nYear: ",
                  format(EURenHeatTrend$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EURenHeatTrend[which(EURenHeatTrend$`European Union` > 0 | EURenHeatTrend$`European Union` < 0),], 1),
        x = ~ Year,
        y = ~ `European Union`,
        legendgroup = "3",
        name = "European Union",
        text = paste0(
          "European Union: ",
          percent(EURenHeatTrend[which(EURenHeatTrend$`European Union` > 0 | EURenHeatTrend$`European Union` < 0),][-1,]$`European Union`, accuracy = 0.1),
          "\nYear: ",
          format(EURenHeatTrend[which(EURenHeatTrend$`European Union` > 0 | EURenHeatTrend$`European Union` < 0),][-1,]$Year, "%Y")
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
                     range = c(min(EURenHeatTrend$Year)-100, max(EURenHeatTrend$Year)+100)),
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
  
  output$RenHeatEUTrend.png <- downloadHandler(
    filename = "RenHeatEUTrend.png",
    content = function(file) {
      
      EURenHeatTrend <- read_excel("Structure/CurrentWorking.xlsx", 
                                   sheet = "Renewable heat EU", col_names = FALSE, 
                                                                skip = 18, n_max = 31)
                                   EURenHeatTrend <- as.data.frame(t(EURenHeatTrend))
                                   names(EURenHeatTrend) <- as.character(unlist(EURenHeatTrend[1,]))
                                   EURenHeatTrend<- tail(EURenHeatTrend, -1)
                                   names(EURenHeatTrend)[1] <- "Year"
                                   EURenHeatTrend$Year <- as.numeric(as.character(EURenHeatTrend$Year))
                                   EURenHeatTrend <- EURenHeatTrend[c("Year", "SCOTLAND", "United Kingdom", "EU (28)")]
                                   names(EURenHeatTrend) <- c("Year", "Scotland", "United Kingdom", "European Union")
                                   EURenHeatTrend[,c(2,3,4)] %<>% lapply(function(x) as.numeric(as.character(x)))
                                   EURenHeatTrend <- EURenHeatTrend[complete.cases(EURenHeatTrend),]
                                   
                                   ### variables
                                   ChartColours <- c("#39ab2c", "#2c7fb8", "#feb24c" ,"#de2d26")
                                   sourcecaption = "Source: Eurostat, BEIS"
                                   plottitle = "Trend of renewable Heat share\nin gross final energy consumption"
                                   
                                   #EURenHeatTrend$`Scotland`Percentage <- PercentLabel(EURenHeatTrend$`Scotland`)
                                   
                                   
                
                                   
                                   
                                   EURenHeatTrendChart <- EURenHeatTrend %>%
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
                                         vjust = 0.5,
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
                                       data = tail(EURenHeatTrend, 1),
                                       aes(x = Year,
                                           y = `Scotland`,
                                           show_guide = FALSE),
                                       colour = ChartColours[2],
                                       size = 4,
                                       family = "Century Gothic"
                                     ) +
                                     annotate(
                                       "text",
                                       x = mean(EURenHeatTrend$Year),
                                       y = mean(EURenHeatTrend$`Scotland`),
                                       label = "Scotland",
                                       hjust = 0.5,
                                       vjust = 3,
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
                                         vjust = 0,
                                         fontface = 2
                                       ),
                                       colour = ChartColours[3],
                                       family = "Century Gothic"
                                     ) +
                                     geom_point(
                                       data = tail(EURenHeatTrend, 1),
                                       aes(x = Year,
                                           y = `European Union`,
                                           show_guide = FALSE),
                                       colour = ChartColours[3],
                                       size = 4,
                                       family = "Century Gothic"
                                     ) +
                                     annotate(
                                       "text",
                                       x = mean(EURenHeatTrend$Year),
                                       y = mean(EURenHeatTrend$`European Union`),
                                       label = "European Union",
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
                                         vjust = 0,
                                         fontface = 2
                                       ),
                                       colour = ChartColours[4],
                                       family = "Century Gothic"
                                     ) +
                                     geom_text(
                                       aes(
                                         x = Year + .6,
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
                                       data = tail(EURenHeatTrend, 1),
                                       aes(x = Year,
                                           y = `United Kingdom`,
                                           
                                           show_guide = FALSE),
                                       size = 4,
                                       colour = ChartColours[4],
                                       family = "Century Gothic"
                                     ) +
                                     annotate(
                                       "text",
                                       x = mean(EURenHeatTrend$Year),
                                       y = mean(EURenHeatTrend$`United Kingdom`),
                                       label = "United Kingdom",
                                       hjust = 0.5,
                                       vjust = -1,
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
                                   
                                   EURenHeatTrendChart
                                   
                                   EURenHeatTrendChart <-
                                     StackedArea(EURenHeatTrendChart,
                                                 EURenHeatTrend,
                                                 plottitle,
                                                 sourcecaption,
                                                 ChartColours)
                                   
                                   EURenHeatTrendChart <- EURenHeatTrendChart +
                                     labs(subtitle = paste(min(EURenHeatTrend$Year), "-", max(EURenHeatTrend$Year))
                                     )+
                                     ylim(-.0,.21)
                                   
                                   EURenHeatTrendChart
                                   
                                   ggsave(
                                     file,
                                     plot =  EURenHeatTrendChart,
                                     width = 14,
                                     height = 16,
                                     units = "cm",
                                     dpi = 300
                                   )
    }
  )
}
