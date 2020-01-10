require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

WindHydroEUOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Wind",
    fluidRow(column(8,
                    h3("Wind generation in EU countries", style = "color: #39ab2c;  font-weight:bold"),
                    h4(textOutput(ns('EUWindSubtitle')), style = "color: #39ab2c;")
    ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EUWind.png'), 'Download Graph', style="float:right")
             )),
    
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    #dygraphOutput(ns("EUWindPlot")),
    plotlyOutput(ns("EUWindPlot"), height = "600px")%>% withSpinner(color="#39ab2c"),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Hydro",
             fluidRow(column(8,
                             h3("Hydro generation in EU countries", style = "color: #39ab2c;  font-weight:bold"),
                             h4(textOutput(ns('EUHydroSubtitle')), style = "color: #39ab2c;")
             ),
             column(
               4, style = 'padding:15px;',
               downloadButton(ns('EUHydro.png'), 'Download Graph', style="float:right")
             )),
             
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
             #dygraphOutput(ns("EUHydroPlot")),
             plotlyOutput(ns("EUHydroPlot"), height = "600px")%>% withSpinner(color="#39ab2c"),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
    fluidRow(
    column(10,h3("Commentary", style = "color: #39ab2c;  font-weight:bold")),
    column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
    uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"),
    tabsetPanel(
      tabPanel("Wind",
    fluidRow(
    column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
    column(2, style = "padding:15px",  actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; "))
    ),
    fluidRow(
      column(12, dataTableOutput(ns("EUWindTable"))%>% withSpinner(color="#39ab2c"))),
    tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;")),
    tabPanel("Hydro",
             fluidRow(
               column(10, h3("Data", style = "color: #39ab2c;  font-weight:bold")),
               column(2, style = "padding:15px",  actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; "))
             ),
             fluidRow(
               column(12, dataTableOutput(ns("EUHydroTable"))%>% withSpinner(color="#39ab2c"))),
             tags$hr(style = "height:3px;border:none;color:#39ab2c;background-color:#39ab2c;"))),
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
WindHydroEU <- function(input, output, session) {

  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  print("WindHydroEU.R")
  ###### Renewable Energy ###### ######
  
  ### From ESD ###
  
  output$EUWindSubtitle <- renderText({
    
    EURenElec <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Wind and hydro gen EU", col_names = TRUE, 
                          skip = 18, n_max = 30)
    
    EURenElec <- EURenElec[,c(1:ncol(EURenElec))]
    
    
    
    names(EURenElec)[1] <- c("Countries")
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EURenElec <- EURenElec %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EURenElec[2:ncol(EURenElec)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EURenElec)), na.rm = TRUE))
  })
  
  output$EUWindPlot <- renderPlotly  ({
    
    ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
    
    EUWind <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Wind and hydro gen EU", col_names = FALSE, 
                          skip = 19, n_max = 30)
    
    EUWind <- EUWind[,c(1,ncol(EUWind))]
    
    names(EUWind) <- c("Countries", "Renewables")
    
    EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "Rest of UK", "U.K."))
    
    EUWind <- merge(EUWind, EUFlagLookup)
    
    EUWind$Group <- ifelse(EUWind$Renewables > 0 & EUWind$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), ChartColours[1],
                            ifelse(EUWind$Renewables <= 0 & EUWind$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                   ifelse(EUWind$Renewables > 0 & EUWind$Renewables %in% c(min(EUWind$Renewables), max(EUWind$Renewables)), ChartColours[2],
                                          ifelse(EUWind$Renewables <= 0 & EUWind$Renewables %in% c(min(EUWind$Renewables), max(EUWind$Renewables)), "E",      
                                                 ifelse(EUWind$Renewables <= 0 , "D",  
                                                        ChartColours[2])))))
    
    EUWind <- EUWind[order(-EUWind$Renewables),]
    
    EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "U.K.", "Rest of the UK"))
    
    EUWind <- EUWind[-1,]
    
    EUWind$Countries <- factor(EUWind$Countries, levels = unique(EUWind$Countries)[order(EUWind$Renewables, decreasing = FALSE)])
    
    p <- plot_ly(
      data = EUWind,
      y = ~Countries,
      x = ~Renewables,
      text = paste0(
        "Wind Generation: ",
        format(round(EUWind$Renewables, digits = 0), big.mark = ","),
        " GWh\nCountry: ",
        EUWind$Countries
      ),
      name = "EU Renewable Energy",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  as.list(EUWind$Group))
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
  
  output$EUWindTable = renderDataTable({
    
    EUWind <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Wind and hydro gen EU", col_names = TRUE, 
                          skip = 18, n_max = 30)
    
    EUWind <- EUWind[,c(1:ncol(EUWind))]
    
    
    
    names(EUWind)[1] <- c("Countries")
    
    EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EUWind[2:ncol(EUWind)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      EUWind,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(EUWind)-2, 'desc')),
        title = "EU Wind Generation (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'EU Wind Generation (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'EU Wind Generation (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(EUWind), 0) %>% 
      formatStyle(1,
                  target = 'row',
                  backgroundColor = styleEqual(c('Scotland'), c('#bdbdbd')))
  })
  
  output$EUWind.png <- downloadHandler(
    filename = "EUWind.png",
    content = function(file) {
      
      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      EUWind <- read_excel("Structure/CurrentWorking.xlsx",
                           sheet = "Wind and hydro gen EU", col_names = FALSE, 
                           skip = 20, n_max = 26)
      
      EUWind <- EUWind[,c(1,ncol(EUWind))]
      
      names(EUWind) <- c("Countries", "Renewables")
      
      EUWind <- EUWind %>% mutate(Countries = replace(Countries, Countries == "Rest of UK", "U.K."))
      
      EUWind <- merge(EUWind, EUFlagLookup)
      
      EUWind$Group <- ifelse(EUWind$Renewables > 0 & EUWind$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "B",
                             ifelse(EUWind$Renewables <= 0 & EUWind$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                    ifelse(EUWind$Renewables > 0 & EUWind$Renewables %in% c(min(EUWind$Renewables), max(EUWind$Renewables)), "C",
                                           ifelse(EUWind$Renewables <= 0 & EUWind$Renewables %in% c(min(EUWind$Renewables), max(EUWind$Renewables)), "E",      
                                                  ifelse(EUWind$Renewables <= 0 , "D",  
                                                         "A")))))
      
      EUWind <- EUWind[order(-EUWind$Renewables),]
      
      EUWind$Renewables <- EUWind$Renewables /100000
      
      ### variables
      ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Wind generation in EU countries"
      
      
      EUWind <- EUWind[order(EUWind$Renewables),]
      EUWind$Countries <-
        factor(EUWind$Countries, levels = EUWind$Countries)
      
      EUWindChart <-
        EUWind %>%  ggplot(aes(x = Countries, y = Renewables, fill = Group)) +
        geom_flag(aes(
          y = -.025,
          size = 10,
          country = Flag
        )) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        ylim(-.3, 1.08) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual("Group",
                          values = c("A" = ChartColours[2], "B" = ChartColours[1], "C" = ChartColours[2])) +
        geom_text(
          label = ifelse(
            EUWind$Group == "B" |
              EUWind$Group == "C" ,
            paste(format(round(EUWind$Renewables*100000, digits = 0), big.mark = ","), "GWh") ,
            ""
          ),
          fontface = 2,
          family = "Century Gothic",
          hjust = ifelse(EUWind$Renewables > .3, 1.1, 0),
          vjust = .5,
          color = ifelse(EUWind$Renewables > .3, "white", ChartColours[2])
        ) +
        geom_text(
          y = -0.055,
          label = EUWind$Countries,
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
          xmin = match("SCOTLAND", EUWind$Countries) - .45,
          xmax = match("SCOTLAND", EUWind$Countries) + .45,
          ymax = .312
        ) +
        annotation_custom(
          LatviaFlag,
          xmin = match("Latvia", EUWind$Countries) - .45,
          xmax = match("Latvia", EUWind$Countries) + .45,
          ymax = .312
        )
      
      
      EUWindChart
      
      
      ggsave(
        file,
        plot =  EUWindChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EUHydroSubtitle <- renderText({
    
    EUHydro <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Wind and hydro gen EU", col_names = TRUE, 
                          skip = 51, n_max = 30)
    
    EUHydro <- EUHydro[,c(1:ncol(EUHydro))]
    
    
    
    names(EUHydro)[1] <- c("Countries")
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EUHydro[2:ncol(EUHydro)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    paste(max(as.numeric(names(EUHydro)), na.rm = TRUE))
  })
  
  output$EUHydroPlot <- renderPlotly  ({
    
    ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
    
    EUHydro <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Wind and hydro gen EU", col_names = FALSE, 
                          skip = 52, n_max = 28)
    
    EUHydro <- EUHydro[,c(1,ncol(EUHydro))]
    
    names(EUHydro) <- c("Countries", "Renewables")
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "Rest of UK", "U.K."))
    
    EUHydro <- merge(EUHydro, EUFlagLookup)
    
    EUHydro$Group <- ifelse(EUHydro$Renewables > 0 & EUHydro$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), ChartColours[1],
                            ifelse(EUHydro$Renewables <= 0 & EUHydro$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                   ifelse(EUHydro$Renewables > 0 & EUHydro$Renewables %in% c(min(EUHydro$Renewables), max(EUHydro$Renewables)), ChartColours[2],
                                          ifelse(EUHydro$Renewables <= 0 & EUHydro$Renewables %in% c(min(EUHydro$Renewables), max(EUHydro$Renewables)), "E",      
                                                 ifelse(EUHydro$Renewables <= 0 , "D",  
                                                        ChartColours[2])))))
    
    EUHydro <- EUHydro[order(-EUHydro$Renewables),]
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "U.K.", "Rest of the UK"))
    
    EUHydro <- EUHydro[-1,]
    
    EUHydro$Countries <- factor(EUHydro$Countries, levels = unique(EUHydro$Countries)[order(EUHydro$Renewables, decreasing = FALSE)])
    
    p <- plot_ly(
      data = EUHydro,
      y = ~Countries,
      x = ~Renewables,
      text = paste0(
        "Wind Generation: ",
        format(round(EUHydro$Renewables, digits = 0), big.mark = ","),
        " GWh\nCountry: ",
        EUHydro$Countries
      ),
      name = "EU Renewable Energy",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  as.list(EUHydro$Group))
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
  
  output$EUHydroTable = renderDataTable({
    
    EUHydro <- read_excel("Structure/CurrentWorking.xlsx",
                          sheet = "Wind and hydro gen EU", col_names = TRUE, 
                          skip = 51, n_max = 30)
    
    EUHydro <- EUHydro[,c(1:ncol(EUHydro))]
    
    
    
    names(EUHydro)[1] <- c("Countries")
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "United Kingdom", "U.K."))
    
    EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "SCOTLAND", "Scotland"))
    
    EUHydro[2:ncol(EUHydro)] %<>% lapply(function(x) as.numeric(as.character(x)))
    
    datatable(
      EUHydro,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        order = list(list(ncol(EUHydro)-2, 'desc')),
        title = "EU Hydro Generation (GWh)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'EU Hydro Generation (GWh)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'EU Hydro Generation (GWh)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) %>%
      formatRound(2:ncol(EUHydro), 0) %>% 
      formatStyle(1,
                  target = 'row',
                  backgroundColor = styleEqual(c('Scotland'), c('#bdbdbd')))
  })
  
  output$EUHydro.png <- downloadHandler(
    filename = "EUHydro.png",
    content = function(file) {
      
      ### Load Packages and Functions
      
      if (exists("PackageHeader") == 0){
        source("Structure/PackageHeader.R")
      }
      
      EUHydro <- read_excel("Structure/CurrentWorking.xlsx",
                            sheet = "Wind and hydro gen EU", col_names = FALSE, 
                            skip = 53, n_max = 23)
      
      EUHydro <- EUHydro[,c(1,ncol(EUHydro))]
      
      names(EUHydro) <- c("Countries", "Renewables")
      
      EUHydro <- EUHydro %>% mutate(Countries = replace(Countries, Countries == "Rest of UK", "U.K."))
      
      EUHydro <- merge(EUHydro, EUFlagLookup)
      
      EUHydro$Group <- ifelse(EUHydro$Renewables > 0 & EUHydro$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "B",
                              ifelse(EUHydro$Renewables <= 0 & EUHydro$Countries %in% c("SCOTLAND", "U.K.", "EU (28)"), "D",
                                     ifelse(EUHydro$Renewables > 0 & EUHydro$Renewables %in% c(min(EUHydro$Renewables), max(EUHydro$Renewables)), "C",
                                            ifelse(EUHydro$Renewables <= 0 & EUHydro$Renewables %in% c(min(EUHydro$Renewables), max(EUHydro$Renewables)), "E",      
                                                   ifelse(EUHydro$Renewables <= 0 , "D",  
                                                          "A")))))
      
      EUHydro <- EUHydro[order(-EUHydro$Renewables),]
      
      
      EUHydro$Renewables <- EUHydro$Renewables /100000
      ### variables
      ChartColours <- c("#39ab2c", "#78c679", "#a3d65c")
      sourcecaption = "Source: Eurostat, BEIS"
      plottitle = "Hydro generation in EU countries"
      
      
      EUHydro <- EUHydro[order(EUHydro$Renewables),]
      EUHydro$Countries <-
        factor(EUHydro$Countries, levels = EUHydro$Countries)
      
      EUHydroChart <-
        EUHydro %>%  ggplot(aes(x = Countries, y = Renewables, fill = Group)) +
        geom_flag(aes(
          y = -.017,
          size = 10,
          country = Flag
        )) +
        #scale_country()+
        #scale_size(range = c(15,30), guide = FALSE)+
        ylim(-.15, .66) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual("Group",
                          values = c("A" = ChartColours[2], "B" = ChartColours[1], "C" = ChartColours[2])) +
        geom_text(
          label = ifelse(
            EUHydro$Group == "B" |
              EUHydro$Group == "C" ,
            paste(format(round(EUHydro$Renewables*100000, digits = 0), big.mark = ","), "GWh") ,
            ""
          ),
          fontface = 2,
          family = "Century Gothic",
          hjust = ifelse(EUHydro$Renewables > .3, 1.1, 0),
          vjust = .5,
          color = ifelse(EUHydro$Renewables > .3, "white", ChartColours[2])
        ) +
        geom_text(
          y = -0.04,
          label = EUHydro$Countries,
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
          xmin = match("SCOTLAND", EUHydro$Countries) - .4,
          xmax = match("SCOTLAND", EUHydro$Countries) + .4,
          ymax = .153
        ) +
        annotation_custom(
          LatviaFlag,
          xmin = match("Latvia", EUHydro$Countries) - .4,
          xmax = match("Latvia", EUHydro$Countries) + .4,
          ymax = .153
        )
      
      
      EUHydroChart
      
      
      ggsave(
        file,
        plot =  EUHydroChart,
        width = 14,
        height = 16,
        units = "cm",
        dpi = 300
      )})
    
    
  
  output$Text <- renderUI({
   tagList(column(12,
                  HTML(
                    paste(readtext("Structure/2 - Renewables/Electricity/WindHydroEU.txt")[2])
                    
                  )))
 })
 
  observeEvent(input$ToggleTable, {
    toggle("EUWindTable")
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("EUHydroTable")
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  

  
}
