require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

RenElecEUOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("EU Renewable Electricity",
    fluidRow(column(8,
                    h3("Renewable electricity as a percentage of gross consumption across the EU", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('RenElecEUSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecEU.png'), 'Download Graph (2017)', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("RenElecEUPlot")),
    plotlyOutput(ns("RenElecEUPlot"), height = "600px")%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("EU Renewable Energy Trend",
             fluidRow(column(8,
                             h3("Trend of renewable electricity share in gross final energy consumption", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('RenElecEUTrendSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('RenElecEUTrend.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("RenElecEUPlot")),
             plotlyOutput(ns("RenElecEUTrendPlot"))%>% withSpinner(color="#39ab2c"),
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
      column(12, dataTableOutput(ns("RenElecEUTable"))%>% withSpinner(color="#39ab2c"))),
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
        SourceLookup("ETElecGen"),
        SourceLookup("ESTRenHeat")
        
      )
    )
  )
}




###### Server ######
RenElecEU <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("RenElecEU")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$RenElecEUSubtitle <- renderText({
    
    EURenElec <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable elec EU", col_names = TRUE, 
                          skip = 18, n_max = 30)
    
    EURenElec <- EURenElec[,c(1:ncol(EURenElec))]
    
    
    
    names(EURenElec)[1] <- c("Countries")
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenElec[2:ncol(EURenElec)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EURenElec)), na.rm = TRUE))
  })
  
  output$RenElecEUTrendSubtitle <- renderText({
    
    EURenElecTrend <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Renewable elec EU", col_names = FALSE, 
                                 skip = 18, n_max = 31)
    EURenElecTrend <- as.data.frame(t(EURenElecTrend))
    names(EURenElecTrend) <- as.character(unlist(EURenElecTrend[1,]))
    EURenElecTrend<- tail(EURenElecTrend, -1)
    EURenElecTrend<- head(EURenElecTrend, -1)
    names(EURenElecTrend)[1] <- "Year"
    EURenElecTrend$Year <- as.numeric(as.character(EURenElecTrend$Year))
    EURenElecTrend <- EURenElecTrend[c("Year", "SCOTLAND", "United Kingdom", "EU (28)")]
    names(EURenElecTrend) <- c("Year", "Scotland", "United Kingdom", "European Union")
    EURenElecTrend[,c(2,3,4)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(min(EURenElecTrend$Year),"-", max(EURenElecTrend$Year))
  })
  
  output$RenElecEUPlot <- renderPlotly  ({
    
    ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
    
    EURenElec <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable elec EU", col_names = FALSE, 
                          skip = 19, n_max = 30)
    
    EURenElec <- EURenElec[,c(1,ncol(EURenElec)-1)]
    
    names(EURenElec) <- c("Countries", "Renewables")
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenElec <- merge(EURenElec, EUFlagLookup)
    
    EURenElec$Group <- ifelse(EURenElec$Renewables > 0 & EURenElec$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), ChartColours[1],
                            ifelse(EURenElec$Renewables <= 0 & EURenElec$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                   ifelse(EURenElec$Renewables > 0 & EURenElec$Renewables %in% c(min(EURenElec$Renewables), max(EURenElec$Renewables)), ChartColours[2],
                                          ifelse(EURenElec$Renewables <= 0 & EURenElec$Renewables %in% c(min(EURenElec$Renewables), max(EURenElec$Renewables)), "E",      
                                                 ifelse(EURenElec$Renewables <= 0 , "D",  
                                                        ChartColours[2])))))
    
    EURenElec <- EURenElec[order(-EURenElec$Renewables),]
    
    EURenElec$Countries <- factor(EURenElec$Countries, levels = unique(EURenElec$Countries)[order(EURenElec$Renewables, decreasing = FALSE)])
    
    p <- plot_ly(
      data = EURenElec,
      y = ~Countries,
      x = ~Renewables,
      text = paste0(
        "Share of Renewable Energy: ",
        percent(EURenElec$Renewables, accuracy = 0.1),
        "\nCountry: ",
        EURenElec$Countries
      ),
      name = "EU Renewable Energy",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  as.list(EURenElec$Group))
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
  
  
  output$RenElecEUTable = renderDataTable({
    
    EURenElec <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Renewable elec EU", col_names = TRUE, 
                          skip = 18, n_max = 30)
    
    EURenElec <- EURenElec[,c(1:ncol(EURenElec))]
    
    
    
    names(EURenElec)[1] <- c("Countries")
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenElec[2:ncol(EURenElec)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      EURenElec,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(EURenElec)-2, 'desc')),
        title = "Renewable Electricity EU",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Renewable Electricity EU',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Renewable Electricity EU')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatPercentage(2:ncol(EURenElec), 1)
  })
  
  
  
 output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/RenElecEU.txt")[2])
                    
                  )))
 })
 
 
  observeEvent(input$ToggleTable, {
    toggle("RenElecEUTable")
  })
  

  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$RenElecEU.png <- downloadHandler(
    filename = "RenElecEU.png",
    content = function(file) {

      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      EUElec <- read_excel("Structure/CurrentWorking.xlsx",
                              sheet = "Renewable elec EU", col_names = FALSE, 
                              skip = 19, n_max = 30)
      
      EUElec <- EUElec[,c(1,ncol(EUElec)-1)]

      names(EUElec) <- c("Countries", "Renewables")
      
      EUElec <- EUElec %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
      
      EUElec <- merge(EUElec, EUFlagLookup)
      
      EUElec$Group <- ifelse(EUElec$Renewables > 0 & EUElec$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "B",
                             ifelse(EUElec$Renewables <= 0 & EUElec$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                    ifelse(EUElec$Renewables > 0 & EUElec$Renewables %in% c(min(EUElec$Renewables), max(EUElec$Renewables)), "C",
                                           ifelse(EUElec$Renewables <= 0 & EUElec$Renewables %in% c(min(EUElec$Renewables), max(EUElec$Renewables)), "E",      
                                                  ifelse(EUElec$Renewables <= 0 , "D",  
                                                         "A")))))
      
      EUElec <- EUElec[order(EUElec$Renewables),]
      
      ### variables
      ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Renewable electricity as a percentage of \ngross consumption for EU countries"
      
      
      EUElec <- EUElec[order(EUElec$Renewables),]
      EUElec$Countries <-
        factor(EUElec$Countries, levels = EUElec$Countries)
      
      EUElecChart <-
        EUElec %>%  ggplot(aes(x = Countries, y = Renewables, fill = Group)) +
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
            EUElec$Group == "B" |
              EUElec$Group == "C" ,
            scales::percent(EUElec$Renewables, 0.1) ,
            ""
          ),
          fontface = 2,
          family = "Century Gothic",
          hjust = ifelse(EUElec$Renewables > .1, 1.1,-.1),
          vjust = .5,
          color = ifelse(EUElec$Renewables > .1, "white", ChartColours[2])
        ) +
        geom_text(
          y = -0.04,
          label = EUElec$Countries,
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
          xmin = match("SCOTLAND", EUElec$Countries) - .55,
          xmax = match("SCOTLAND", EUElec$Countries) + .55,
          ymax = .213
        ) +
        annotation_custom(
          LatviaFlag,
          xmin = match("Latvia", EUElec$Countries) - .55,
          xmax = match("Latvia", EUElec$Countries) + .55,
          ymax = .213
        )
      
      
      EUElecChart
      
      
      ggsave(
        file,
        plot =  EUElecChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$RenElecEUTrendPlot <- renderPlotly  ({
    
    EURenElecTrend <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Renewable elec EU", col_names = FALSE, 
                               skip = 18, n_max = 31)
    EURenElecTrend <- as.data.frame(t(EURenElecTrend))
    names(EURenElecTrend) <- as.character(unlist(EURenElecTrend[1,]))
    EURenElecTrend<- tail(EURenElecTrend, -1)
    EURenElecTrend<- head(EURenElecTrend, -1)
    names(EURenElecTrend)[1] <- "Year"
    EURenElecTrend$Year <- as.numeric(as.character(EURenElecTrend$Year))
    EURenElecTrend <- EURenElecTrend[c("Year", "SCOTLAND", "United Kingdom", "EU (28)")]
    names(EURenElecTrend) <- c("Year", "Scotland", "United Kingdom", "European Union")
    EURenElecTrend <- EURenElecTrend[which(EURenElecTrend$Year >= 2009),]
    EURenElecTrend[,c(2,3,4)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    ### variables
    ChartColours <- c("#39ab2c", "#2c7fb8", "#feb24c" ,"#de2d26")
    sourcecaption = "Source: Eurostat, BEIS"
    plottitle = "Trend of renewable energy share\nin gross final energy consumption"
    
    EURenElecTrend$Year <- paste0("01/01/", EURenElecTrend$Year)
    
    EURenElecTrend$Year <- dmy(EURenElecTrend$Year)
    
    p <-  plot_ly(EURenElecTrend,x = ~ Year ) %>% 
      add_trace(data = EURenElecTrend,
                x = ~ Year,
                y = ~ Scotland,
                name = "Scotland",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "1",
                text = paste0(
                  "Scotland: ",
                  percent(EURenElecTrend$Scotland, accuracy = 0.1),
                  "\nYear: ",
                  format(EURenElecTrend$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[2], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EURenElecTrend[which(EURenElecTrend$Scotland > 0 | EURenElecTrend$Scotland < 0),], 1),
        x = ~ Year,
        y = ~ Scotland,
        legendgroup = "1",
        name = "Scotland",
        text = paste0(
          "Scotland: ",
          percent(EURenElecTrend[which(EURenElecTrend$Scotland > 0 | EURenElecTrend$Scotland < 0),][-1,]$Scotland, accuracy = 0.1),
          "\nYear: ",
          format(EURenElecTrend[which(EURenElecTrend$Scotland > 0 | EURenElecTrend$Scotland < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[2])
      ) %>% 
      add_trace(data = EURenElecTrend,
                x = ~ Year,
                y = ~ `United Kingdom`,
                name = "United Kingdom",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "2",
                text = paste0(
                  "United Kingdom: ",
                  percent(EURenElecTrend$`United Kingdom`, accuracy = 0.1),
                  "\nYear: ",
                  format(EURenElecTrend$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[4], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EURenElecTrend[which(EURenElecTrend$`United Kingdom` > 0 | EURenElecTrend$`United Kingdom` < 0),], 1),
        x = ~ Year,
        y = ~ `United Kingdom`,
        legendgroup = "2",
        name = "United Kingdom",
        text = paste0(
          "United Kingdom: ",
          percent(EURenElecTrend[which(EURenElecTrend$`United Kingdom` > 0 | EURenElecTrend$`United Kingdom` < 0),][-1,]$`United Kingdom`, accuracy = 0.1),
          "\nYear: ",
          format(EURenElecTrend[which(EURenElecTrend$`United Kingdom` > 0 | EURenElecTrend$`United Kingdom` < 0),][-1,]$Year, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18, 
                      color = ChartColours[4])
      ) %>% 
      add_trace(data = EURenElecTrend,
                x = ~ Year,
                y = ~ `European Union`,
                name = "European Union",
                type = 'scatter',
                mode = 'lines',
                legendgroup = "3",
                text = paste0(
                  "European Union: ",
                  percent(EURenElecTrend$`European Union`, accuracy = 0.1),
                  "\nYear: ",
                  format(EURenElecTrend$Year, "%Y")
                ),
                hoverinfo = 'text',
                line = list(width = 6, color = ChartColours[3], dash = "none")
      ) %>% 
      add_trace(
        data = tail(EURenElecTrend[which(EURenElecTrend$`European Union` > 0 | EURenElecTrend$`European Union` < 0),], 1),
        x = ~ Year,
        y = ~ `European Union`,
        legendgroup = "3",
        name = "European Union",
        text = paste0(
          "European Union: ",
          percent(EURenElecTrend[which(EURenElecTrend$`European Union` > 0 | EURenElecTrend$`European Union` < 0),][-1,]$`European Union`, accuracy = 0.1),
          "\nYear: ",
          format(EURenElecTrend[which(EURenElecTrend$`European Union` > 0 | EURenElecTrend$`European Union` < 0),][-1,]$Year, "%Y")
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
                     range = c(min(EURenElecTrend$Year)-100, max(EURenElecTrend$Year)+100)),
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
  
  output$RenElecEUTrend.png <- downloadHandler(
    filename = "RenElecEUTrend.png",
    content = function(file) {
      
      EURenElecTrend <- read_excel("Structure/CurrentWorking.xlsx", 
                                   sheet = "Renewable elec EU", col_names = FALSE, 
                                                                skip = 18, n_max = 31)
                                   EURenElecTrend <- as.data.frame(t(EURenElecTrend))
                                   names(EURenElecTrend) <- as.character(unlist(EURenElecTrend[1,]))
                                   EURenElecTrend<- tail(EURenElecTrend, -1)
                                   EURenElecTrend<- head(EURenElecTrend, -1)
                                   names(EURenElecTrend)[1] <- "Year"
                                   EURenElecTrend$Year <- as.numeric(as.character(EURenElecTrend$Year))
                                   EURenElecTrend <- EURenElecTrend[c("Year", "SCOTLAND", "United Kingdom", "EU (28)")]
                                   names(EURenElecTrend) <- c("Year", "Scotland", "United Kingdom", "European Union")
                                   EURenElecTrend[,c(2,3,4)] %<>% lapply(function(x) as.numeric(as.character(x)))
                                   
                                   ### variables
                                   ChartColours <- c("#39ab2c", "#2c7fb8", "#feb24c" ,"#de2d26")
                                   sourcecaption = "Source: Eurostat, BEIS"
                                   plottitle = "Trend of renewable electricity share\nin gross final energy consumption"
                                   
                                   #EURenElecTrend$`Scotland`Percentage <- PercentLabel(EURenElecTrend$`Scotland`)
                                   
                                   
                
                                   
                                   
                                   EURenElecTrendChart <- EURenElecTrend %>%
                                     ggplot(aes(x = Year), family = "Century Gothic") +
                                     
                                     geom_line(
                                       aes(y = `Scotland`,
                                           label = percent(`Scotland`, 0.1)),
                                       colour = ChartColours[2],
                                       size = 1.5,
                                       family = "Century Gothic"
                                     ) +
                                     geom_text(
                                       aes(
                                         x = Year - .6,
                                         y = `Scotland`,
                                         label = ifelse(Year == min(Year), percent(`Scotland`, accuracy = .1), ""),
                                         hjust = 0.5,
                                         vjust = -.5,
                                         fontface = 2
                                       ),
                                       colour = ChartColours[2],
                                       family = "Century Gothic"
                                     ) +
                                     geom_text(
                                       aes(
                                         x = Year + .8,
                                         y = `Scotland`,
                                         label = ifelse(Year == max(Year), percent(`Scotland`, accuracy = .1), ""),
                                         hjust = 0.5,
                                         
                                         fontface = 2
                                       ),
                                       colour = ChartColours[2],
                                       family = "Century Gothic"
                                     ) +
                                     geom_point(
                                       data = tail(EURenElecTrend, 1),
                                       aes(x = Year,
                                           y = `Scotland`,
                                           show_guide = FALSE),
                                       colour = ChartColours[2],
                                       size = 4,
                                       family = "Century Gothic"
                                     ) +
                                     annotate(
                                       "text",
                                       x = mean(EURenElecTrend$Year),
                                       y = mean(EURenElecTrend$`Scotland`),
                                       label = "Scotland",
                                       hjust = 0.5,
                                       vjust = -2.,
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
                                         x = Year - .6,
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
                                         x = Year + .8,
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
                                       data = tail(EURenElecTrend, 1),
                                       aes(x = Year,
                                           y = `European Union`,
                                           show_guide = FALSE),
                                       colour = ChartColours[3],
                                       size = 4,
                                       family = "Century Gothic"
                                     ) +
                                     annotate(
                                       "text",
                                       x = mean(EURenElecTrend$Year),
                                       y = mean(EURenElecTrend$`European Union`),
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
                                         x = Year - .6,
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
                                         x = Year + .88,
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
                                       data = tail(EURenElecTrend, 1),
                                       aes(x = Year,
                                           y = `United Kingdom`,
                                           
                                           show_guide = FALSE),
                                       size = 4,
                                       colour = ChartColours[4],
                                       family = "Century Gothic"
                                     ) +
                                     annotate(
                                       "text",
                                       x = mean(EURenElecTrend$Year),
                                       y = mean(EURenElecTrend$`United Kingdom`),
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
                                   
                                   EURenElecTrendChart
                                   
                                   EURenElecTrendChart <-
                                     StackedArea(EURenElecTrendChart,
                                                 EURenElecTrend,
                                                 plottitle,
                                                 sourcecaption,
                                                 ChartColours)
                                   
                                   EURenElecTrendChart <- EURenElecTrendChart +
                                     labs(subtitle = paste(min(EURenElecTrend$Year), "-", max(EURenElecTrend$Year))
                                     )+
                                     ylim(-.005,.705)
                                   
                                   EURenElecTrendChart
                                   
                                   ggsave(
                                     file,
                                     plot =  EURenElecTrendChart,
                                     width = 14,
                                     height = 16,
                                     units = "cm",
                                     dpi = 300
                                   )
    }
  )
}
