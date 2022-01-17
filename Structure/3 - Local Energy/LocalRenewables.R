require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######



LocalRenewablesOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      
      tabPanel("Operating community renewable capacity",
               fluidRow(
                 column(
                   8,
                   h3("Capacity of operating locally owned renewable energy initiatives", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('CommunityOperatingCapacitySubtitle')), style = "color: #a3d65c;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('CommunityOperatingCapacity.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               plotlyOutput(ns("CommunityOperatingCapacityPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      
      tabPanel("Capacity by stage of development",
               fluidRow(
                 column(
                   8,
                   h3("Community and locally owned renewable energy capacity in different stages of development", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('CommunityCapacitySubtitle')), style = "color: #a3d65c;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('CommunityCapacity.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               plotlyOutput(ns("CommunityCapacityPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      
      tabPanel("Operating capacity by technology",
               fluidRow(
                 column(
                   8,
                   h3("Capacity of operational community and locally owned renewable installations by technology", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('CommunityOperatingTechSubtitle')), style = "color: #a3d65c;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('CommunityOperatingTech.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               plotlyOutput(ns("CommunityOperatingTechPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      
      tabPanel("Operating capacity by output",
               fluidRow(
                 column(
                   8,
                   h3("Capacity of operational community and locally owned renewable installations by type of output", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('CommunityOperatingOutputTypeSubtitle')), style = "color: #a3d65c;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('CommunityOperatingOutputType.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               plotlyOutput(ns("CommunityOperatingOutputTypePlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      
      tabPanel("Estimated generation by output",
               fluidRow(
                 column(
                   8,
                   h3("Estimated generation of operational community and locally owned renewable installations by type of output (GWh)", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('CommunityOperatingOutputTypeGenSubtitle')), style = "color: #a3d65c;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('CommunityOperatingOutputTypeGen.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               plotlyOutput(ns("CommunityOperatingOutputTypeGenPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      
      tabPanel("Operating capacity by ownership",
               fluidRow(
                 column(
                   8,
                   h3("Capacity of operational community and locally owned renewable installations by ownership category", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('EnSectorOwnershipSubtitle')), style = "color: #a3d65c;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('EnSectorOwnership.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),

               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               plotlyOutput(ns("EnSectorOwnershipPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"))
    ),
    
    fluidRow(
      column(10,h3("Commentary", style = "color: #a3d65c;  font-weight:bold")),
      column(2,style = "padding:15px",actionButton(ns("ToggleText"), "Show/Hide Text", style = "float:right; "))),
    
    fluidRow(
      uiOutput(ns("Text"))
    ),
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
    tabsetPanel(
      tabPanel("Time series",
               fluidRow(
                 column(10, h3("Data - Capacity of operating locally owned renewable energy initiatives time series", style = "color: #a3d65c;  font-weight:bold")),
                 column(
                   2,
                   style = "padding:15px",
                   actionButton(ns("ToggleTable2"), "Show/Hide Table", style = "float:right; ")
                 )
               ),
               fluidRow(column(12, dataTableOutput(
                 ns("CommunityOperatingCapacityTable")
               )%>% withSpinner(color="#a3d65c"))),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      
      tabPanel("Pipeline",
    fluidRow(
      column(10, h3("Data - Community and locally owned renewable energy capacity by stage of development", style = "color: #a3d65c;  font-weight:bold")),
      column(
        2,
        style = "padding:15px",
        actionButton(ns("ToggleTable"), "Show/Hide Table", style = "float:right; ")
      )
    ),
    fluidRow(column(12, dataTableOutput(
      ns("CommunityCapacityTable")
    )%>% withSpinner(color="#a3d65c"))),
    tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
    
    tabPanel("Technology",
             fluidRow(
               column(10, h3("Data - Capacity of operational community and locally owned renewable installations by technology", style = "color: #a3d65c;  font-weight:bold")),
               column(
                 2,
                 style = "padding:15px",
                 actionButton(ns("ToggleTable3"), "Show/Hide Table", style = "float:right; ")
               )
             ),
             fluidRow(column(12, dataTableOutput(
               ns("CommunityOperatingTechTable")
             )%>% withSpinner(color="#a3d65c"))),
             tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
    
    tabPanel("Output",
             fluidRow(
               column(10, h3("Data - Estimated capacity of operational community and locally owned renewable installations by type of output", style = "color: #a3d65c;  font-weight:bold")),
               column(
                 2,
                 style = "padding:15px",
                 actionButton(ns("ToggleTable4"), "Show/Hide Table", style = "float:right; ")
               )
             ),
             fluidRow(column(12, dataTableOutput(
               ns("CommunityOperatingOutputTypeTable")
             )%>% withSpinner(color="#a3d65c"))),
             tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
    tabPanel("Output, estimated generation",
             fluidRow(
               column(10, h3("Data - Estimated generation of operational community and locally owned renewable installations by type of output", style = "color: #a3d65c;  font-weight:bold")),
               column(
                 2,
                 style = "padding:15px",
                 actionButton(ns("ToggleTable6"), "Show/Hide Table", style = "float:right; ")
               )
             ),
             fluidRow(column(12, dataTableOutput(
               ns("CommunityOperatingOutputTypeGenTable")
             )%>% withSpinner(color="#a3d65c"))),
             tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
    
    tabPanel("Ownership category",
             fluidRow(
               column(10, h3("Data - Estimated capacity of operational community and locally owned renewable installations by ownership category", style = "color: #a3d65c;  font-weight:bold")),
               column(
                 2,
                 style = "padding:15px",
                 actionButton(ns("ToggleTable5"), "Show/Hide Table", style = "float:right; ")
               )
             ),
             fluidRow(column(12, dataTableOutput(
               ns("EnSectorOwnershipTable")
             )%>% withSpinner(color="#a3d65c"))),
             tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"))
    ),
    fluidRow(
      column(2, HTML("<p><strong>Last Updated:</strong></p>")),
      column(2,
             UpdatedLookup(c("SGGrowth"))),
      column(1, align = "right",
             HTML("<p><strong>Reason:</strong></p>")),
      column(7, align = "right", 
             p("Regular updates")
      )),
    fluidRow(p(" ")),
    fluidRow(
      column(2, HTML("<p><strong>Update Expected:</strong></p>")),
      column(2,
             DateLookup(c("SGGrowth"))),
      column(1, align = "right",
             HTML("<p><strong>Sources:</strong></p>")),
      column(7, align = "right",
        SourceLookup("ESTComm")
        
      )
    )
  )
  
}




###### Server ######
LocalRenewables <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("EnEconomy.R")
  
  QuarterSubtitle <- "Scotland, June 2020"
  ###### Energy Sector Emplyment ######
  
  output$CommunityCapacitySubtitle <- renderText({
      
      paste("Scotland, 2020")
  })
  
  output$CommunityCapacityPlot <- renderPlotly  ({
    
    CommunityCapacity <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Comm & locally owned ren",
        col_names = FALSE,
        skip = 13,
        n_max = 6
      )
    
    CommunityCapacity <- CommunityCapacity[2:3]
    
    CommunityCapacity <- as_tibble(t(CommunityCapacity))
    
    names(CommunityCapacity) <- unlist(CommunityCapacity[1,])
    
    CommunityCapacity <- CommunityCapacity[-1,]
    
    names(CommunityCapacity)[1] <- c("Operating")
    
    CommunityCapacity$Type <- 1
    
    CommunityCapacity %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    CommunityCapacity <- as_tibble(CommunityCapacity)
    
    ChartColours <- c("#a3d65c", "#FF8500")
    BarColours <- c("#005a32", "#238b45", "#41ab5d", "#74c476", "#a1d99b", "#d9d9d9", "#737373" )
    
    p <-
      plot_ly(data = CommunityCapacity, y = ~Type) %>%
      add_trace(
        data = CommunityCapacity,
        x = ~ Operating,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Operating",
        text = paste0("Operating: ", CommunityCapacity$Operating, " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = CommunityCapacity,
        x = ~ `Under construction`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Under Construction",
        text = paste0("Under Construction: ", CommunityCapacity$`Under construction`, " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = CommunityCapacity,
        x = ~ `Consented not built`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Consented not built",
        text = paste0("Consented not built: ", CommunityCapacity$`Consented not built`, " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = CommunityCapacity,
        x = ~ `In planning`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "In planning",
        text = paste0("In planning: ", CommunityCapacity$`In planning`, " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = CommunityCapacity,
        x = ~ `In scoping`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "In scoping",
        text = paste0("In scoping: ", CommunityCapacity$`In scoping`, " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[5]),
        legendgroup = 5
      ) %>%
      add_trace(
        data = CommunityCapacity,
        x = ~ `Shared ownership under discussion`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Shared ownership under discussion",
        text = paste0("Shared ownership under discussion: ", CommunityCapacity$`Shared ownership under discussion`, " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[6]),
        legendgroup = 6
      ) %>%
      add_annotations(
        ax = 1000,
        x = 1000,
        ay = 0.7,
        y = 1.2,
        xref = "x", yref = "y",
        axref = "x", ayref = "y",
        showlegend = FALSE,
        arrowhead = 0,
        arrowsize = 1,
        arrowdash = 1,
        arrowcolor = "#ff8500",
        hoverinfo = 'name',
        legendgroup = 10,
        text = "<b>2020\ntarget</b>",
        name = "Carbon sinks absorb more carbon than they generate",
        font = list(
          color = "#ff8500",
          size = 18
        )
      ) %>%
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#1A5D38"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(
          title = "",
          showgrid = FALSE,
          ticktext = list(""),
          tickvals = list(1),
          tickmode = "array"
        ),
        xaxis = list(
          title = "",
          tickformat = "%",
          showgrid = FALSE,
          showticklabels = FALSE,
          zeroline = FALSE,
          zerolinecolor = ChartColours[1],
          zerolinewidth = 2,
          rangemode = "tozero"
        )
      ) %>%
      config(displayModeBar = F) %>% 
      onRender("function(el,x){el.on('plotly_legendclick', function(){ return false; })}")
    
    p
    
    
    
  })
  
  output$CommunityCapacity.png <- downloadHandler(
    filename = "CommunityCapacity.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Comm & locally owned ren", skip = 12, n_max = 6)[1:3]
      
      names(Data) <- c("Year", "Phase", "Capacity")
      
      Data[6,2] <- "Shared ownership"
      
      Data <- Data[order(-as.numeric(row.names(Data))),]
      
      CommunityCapacity <- Data
      
      CommunityCapacity$Phase <-
        factor(CommunityCapacity$Phase, levels = CommunityCapacity$Phase)
      
      CommunityCapacity <- CommunityCapacity %>%
        group_by(Year) %>%
        mutate(pos = cumsum(Capacity) - Capacity / 2)
      
      plottitle <-
        "Community and locally owned renewable energy\ncapacity in different stages of development"
      sourcecaption <- "Source: EST"
      
      ChartColours <- c("#a3d65c", "#FF8500")
      BarColours <- c("#737373", "#d9d9d9", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32" )
      
      
      
      CommunityCapacityChart <-
        CommunityCapacity %>%  ggplot(aes(x = Year, y = Capacity, fill = Phase), family = "Century Gothic") +
        scale_fill_manual(
          "Phase",
          values = c(
            "Operating*" = BarColours[7],
            "Under construction" = BarColours[6],
            "Consented not built" = BarColours[5],
            "In planning" = BarColours[4],
            "In scoping" = BarColours[3],
            "Shared ownership" = BarColours[2]
          )
        ) +
        geom_bar(stat = "identity", width = 1) +
        annotate(
          geom = 'segment',
          y = 1000,
          yend = 1000,
          color = "#fc9272",
          x = CommunityCapacity$Year - 0.5,
          xend = CommunityCapacity$Year + 2.2,
          size = 1,
          linetype = 3
        ) +
        annotate(
          "text",
          x = CommunityCapacity$Year + 3,
          y = 1000,
          label = "2020\ntarget",
          hjust = 0.5,
          vjust = .8,
          colour = "#fc9272",
          fontface = 2,
          family = "Century Gothic"
        ) +
        annotate(
          "text",
          x = ifelse(row_number(CommunityCapacity$Phase) %% 2 == 0, mean(CommunityCapacity$Year) - 1.5, mean(CommunityCapacity$Year) + 1.5),
          y = sum(CommunityCapacity$Capacity) - CommunityCapacity$pos,
          label = ifelse(
            row_number(CommunityCapacity$Phase) %% 2 == 0,
            paste0(CommunityCapacity$Capacity, " MW\n", str_wrap(CommunityCapacity$Phase,11)),
            paste0(
              str_wrap(CommunityCapacity$Phase,11),
              "\n",
              CommunityCapacity$Capacity,
              " MW"
            )
          ),
          fontface = 2,
          color = BarColours[row_number(CommunityCapacity$Phase)],
          family = "Century Gothic"
        )
      
      
      CommunityCapacityChart
      
      
      CommunityCapacityChart <-
        HorizontalChart(CommunityCapacityChart,
                        CommunityCapacity,
                        plottitle,
                        sourcecaption,
                        ChartColours)
      
      CommunityCapacityChart <-
        CommunityCapacityChart +
        xlim(mean(CommunityCapacity$Year) - 3,
             mean(CommunityCapacity$Year) + 3) +
        coord_flip() +
        labs(subtitle = paste("Scotland,", min(CommunityCapacity$Year)))
      
      CommunityCapacityChart
      
      ggsave(
        file,
        plot = CommunityCapacityChart,
        width = 20,
        height = 10,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$CommunityCapacityTable = renderDataTable({
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 12, n_max = 6)
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[2:3]
  
    names(CommunityOperatingOutputType) <- c("Stage", "Capacity (MW)")
    
    CommunityOperatingOutputType[1,1] <- "Operating"
    
        datatable(
      CommunityOperatingOutputType,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Community and locally owned renewable energy capacity in different stages of development (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Community and locally owned renewable energy capacity in different stages of development (MW)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Community and locally owned renewable energy capacity in different stages of development (MW)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) 
    
    
  })
  
  observeEvent(input$ToggleTable, {
    toggle("CommunityCapacityTable")
  })
  
  output$CommunityOperatingCapacitySubtitle <- renderText({
    
    CommunityOperatingOutputType <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Comm & locally owned ren",
      col_names = TRUE,
      skip = 12,
      n_max = 15
    )
    
    CommunityOperatingOutputType <-
      CommunityOperatingOutputType[12:14]
    
    names(CommunityOperatingOutputType) <-
      c("Date", "Capacity", "Number of Installations")
    
    CommunityOperatingOutputType$`Number of Installations` <- NULL
    
    CommunityOperatingOutputType$Target <- NA
    

    
    paste("Scotland,", format(min(CommunityOperatingOutputType$Date[which(CommunityOperatingOutputType$Capacity != 0)], na.rm = TRUE), format = "%B %Y"), "-", format(max(CommunityOperatingOutputType$Date[which(CommunityOperatingOutputType$Capacity != 0)], na.rm = TRUE), format = "%B %Y"))
   })
  
  output$CommunityOperatingCapacityPlot <- renderPlotly  ({
    CommunityOperatingOutputType <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Comm & locally owned ren",
      col_names = TRUE,
      skip = 12,
      n_max = 15
    )
    
    CommunityOperatingOutputType <-
      CommunityOperatingOutputType[12:14]
    
    names(CommunityOperatingOutputType) <-
      c("Date", "Capacity", "Number of Installations")
    
    CommunityOperatingOutputType$`Number of Installations` <- NULL
    
    CommunityOperatingOutputType$Target <- NA
    
   RenEn2 <- data.frame(Date = c(ymd("2020-12-01"), ymd("2030-12-01") ), `Capacity` = NA, Target = c(1000,2000))
   
   CommunityOperatingOutputType <- rbind(CommunityOperatingOutputType, RenEn2)
   
   ChartColours <- c("#a3d65c", "#FF8500")
    
    p <-  plot_ly(CommunityOperatingOutputType, x = ~ Date) %>%
      add_trace(
        y = ~ Capacity,
        name = "Capacity",
        type = 'scatter',
        mode = 'lines',
        legendgroup = "1",
        text = paste0(
          "Progress: ",
          CommunityOperatingOutputType$Capacity,
          " MW\nDate: ",
          format(CommunityOperatingOutputType$Date, "%b %Y")
        ),
        hoverinfo = 'text',
        line = list(
          width = 6,
          color = ChartColours[1],
          dash = "none"
        )
      ) %>%
      add_trace(
        data = tail(CommunityOperatingOutputType[which(CommunityOperatingOutputType$Capacity > 0 |
                                           CommunityOperatingOutputType$Capacity < 0), ], 1),
        x = ~ Date,
        y = ~ `Capacity`,
        name = "Renewable Electricity",
        text = paste0(
          "Progress: ",
          CommunityOperatingOutputType[which(CommunityOperatingOutputType$Capacity > 0 |
                                  CommunityOperatingOutputType$Capacity < 0), ][-1, ]$Capacity,
          " MW\nDate: ",
          format(CommunityOperatingOutputType[which(CommunityOperatingOutputType$Capacity > 0 |
                                 CommunityOperatingOutputType$Capacity < 0), ][-1, ]$Date, "%b %Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18,
                      color = ChartColours[1])
      ) %>%
      add_trace(
        data = CommunityOperatingOutputType,
        x = ~ Date,
        y = ~ Target,
        name = "Target",
        legendgroup = "2",
        text = paste0(
          "Target: ",
          CommunityOperatingOutputType$Target,
          " MW\nDate: ",
          format(CommunityOperatingOutputType$Date, "%b %Y")
        ),
        hoverinfo = 'text',
        mode = 'markers',
        marker = list(
          size = 25,
          symbol = "diamond",
          color = ChartColours[2]
        )
      ) %>%
      layout(
        barmode = 'stack',
        bargap = 0.66,
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        xaxis = list(
          title = "",
          tickformat = "%b %Y",
          showgrid = TRUE,
          range = c(min(CommunityOperatingOutputType$Date) - 100, max(CommunityOperatingOutputType$Date) +
                      100)
        ),
        yaxis = list(
          title = "MW",
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
  
  output$CommunityOperatingCapacity.png <- downloadHandler(
    filename = "CommunityOperatingCapacity.png",
    content = function(file) {
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Comm & locally owned ren", skip = 12, n_max = 15)[12:13]
      
      names(Data) <- c("Year", "Renewables")
      
      Data$Date <- Data$Year
      
      Data$Year <- as.numeric(substr(Data$Year,1,4))
      
      Data <- Data[complete.cases(Data$Year),]
      
      Data2 <- data.frame(Year = c(2020, 2030 ), `Renewables` = NA, Target = c(1000,2000), `Date` = c(dmy("01/12/2020"), dmy("01/12/2030")))
      
      Data <- rbind.fill(Data, Data2)
      
      ComCapOperating <- Data
      
      plottitle <- "Capacity of operating locally owned\nrenewable energy initiatives"
      sourcecaption <- "Source: EST"
      ChartColours <- c("#a3d65c", "#FF8500")
      
      ComCapOperatingChart <-
        ComCapOperating %>%  ggplot(aes(x = Year), family = "Century Gothic") +
        
        ### Line of Values
        geom_line(
          aes(y = Renewables,
              colour = ChartColours[1],
              label = Renewables),
          size = 1.5,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == min(Year), paste0(Renewables, " MW"), ""),
            hjust = 0.3,
            vjust = 2,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = Renewables,
            label = ifelse(Year == max(Year[which(Renewables > 0)]), paste0(Renewables, " MW"), ""),
            vjust = 2.5,
            colour = ChartColours[1],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ComCapOperating[which(ComCapOperating$Renewables > 0),], 1),
          aes(
            x = Year,
            y = Renewables,
            colour = ChartColours[1],
            label = Renewables,
            show_guide = FALSE
          ),
          size = 4,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == max(Year[which(Renewables > 0)]), format(Date, "%b %Y"), ""),
            hjust = 1,
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
            label = ifelse(Year == min(Year), format(Date, "%b %Y"), ""),
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
            y = Target,
            label = ifelse(Year == Year[which(Target > 0)], paste0("Target:\n", Target, " MW"), ""),
            vjust = 1.3,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = ComCapOperating[which(ComCapOperating$Target > 0),],
          aes(
            x = Year,
            y = Target,
            colour = ChartColours[2],
            label = Renewables,
            show_guide = FALSE
          ),      
          size = 6,
          shape = 18,
          family = "Century Gothic"
        ) +
        geom_text(
          aes(
            x = Year,
            y = 0,
            label = ifelse(Year == Year[which(Target > 0)], format(Date, "%b %Y"), ""),
            hjust = 0.5,
            vjust = 1.5,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        )
      
      
      ComCapOperatingChart <-
        LinePercentChart(ComCapOperatingChart,
                         ComCapOperating,
                         plottitle,
                         sourcecaption,
                         ChartColours)
      
      
      #ComCapOperatingChart <- ComCapOperatingChart +
      #  ylim(-15,700)
      
      ComCapOperatingChart <- ComCapOperatingChart +
        ylim(0,2001)+
        xlim(min(ComCapOperating$Year),max(ComCapOperating$Year)+1) + 
        labs(subtitle = paste0("Scotland, ", min(ComCapOperating$Year), " - ",  max(ComCapOperating[which(ComCapOperating$Renewables > 0),]$Year)))
      
      ComCapOperatingChart
      
      ggsave(
        file,
        plot = ComCapOperatingChart,
        width = 20,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$CommunityOperatingCapacityTable = renderDataTable({
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 12, n_max = 15)
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[12:14]
    
    names(CommunityOperatingOutputType) <- c("Date", "Capacity (MW)", "Number of Installations")
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[complete.cases(CommunityOperatingOutputType),]
    
    CommunityOperatingOutputType$Date <- format(CommunityOperatingOutputType$Date, "%b %Y")
    
    datatable(
      CommunityOperatingOutputType,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Community and locally owned renewable energy capacity in different stages of development (MW)",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Community and locally owned renewable energy capacity in different stages of development (MW)',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Community and locally owned renewable energy capacity in different stages of development (MW)')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10
      )
    ) 
    
    
  })
  
  observeEvent(input$ToggleTable2, {
    toggle("CommunityOperatingCapacityTable")
  })
  
  output$CommunityOperatingTechSubtitle <- renderText({
    paste(QuarterSubtitle)
  })
  
  output$CommunityOperatingTechPlot <- renderPlotly  ({
    
    CommunityOperatingTech <- read_excel("Structure/CurrentWorking.xlsx", 
                                  sheet = "Comm & locally owned ren", col_names = TRUE, 
                                  skip = 41, n_max = 8)
    
    CommunityOperatingTech <- CommunityOperatingTech[1:2]
    
    names(CommunityOperatingTech) <- c("Tech", "Capacity")
    
    CommunityOperatingTech$Capacity <- as.numeric(CommunityOperatingTech$Capacity)
    
    CommunityOperatingTech[is.na( CommunityOperatingTech)] <- 0.99
    
    CommunityOperatingTech[2,2] <- CommunityOperatingTech[2,2] + CommunityOperatingTech[5,2]
        
    CommunityOperatingTech[2,1] <- "Bioenergy and Waste"
    
    CommunityOperatingTech <- CommunityOperatingTech[-5,]
    
    CommunityOperatingTech$Tech <- paste0("<b>", CommunityOperatingTech$Tech, "</b>")
    
    CommunityOperatingTech$Tech <- factor(CommunityOperatingTech$Tech, levels = c(as.character(CommunityOperatingTech$Tech)))
  
    ChartColours <- c("#a3d65c", "#FF8500")
    
    p <- plot_ly(
      data = CommunityOperatingTech,
      y = ~Tech,
      x = ~Capacity,
      text = paste0(CommunityOperatingTech$Tech,
        "\nCapacity: ",
        ifelse(CommunityOperatingTech$Capacity <1, "<1", CommunityOperatingTech$Capacity), " MW"
      ),
      name = "Capacity",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  ChartColours[1])
    )  %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     autorange = "reversed",
                     showgrid = FALSE),
        xaxis = list(
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
  
  output$CommunityOperatingTech.png <- downloadHandler(
    filename = "CommunityOperatingTech.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Comm & locally owned ren", skip = 41, n_max = 8)[1:2]
      
      names(Data) <- c("Tech", "Capacity")
      
      Data$Capacity <- as.numeric(Data$Capacity)
      
      Data[is.na(Data)] <- 0
      
      ComCapTech <- Data
      
      ComCapTech[2,2] <- ComCapTech[2,2] + ComCapTech[5,2]
      
      ComCapTech[2,1] <- "Bioenergy and Waste"
      
      ComCapTech <- ComCapTech[-5,]
      
      ComCapTech$Tech <-
        factor(ComCapTech$Tech, levels = ComCapTech$Tech, ordered = TRUE)
      
      
      
      
      
      plottitle <-
        "Capacity of operational community and locally\nowned renewable installations by technology"
      sourcecaption <- "Source: EST"
      
      ChartColours <- c("#a3d65c", "#FF8500")
      BarColours <- c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32")
      
      
      ComCapTechChart <-
        ComCapTech %>%  ggplot(aes(x = Tech, y = Capacity, fill = ChartColours[1]), family = "Century Gothic") +
        geom_bar(stat = "identity", width = 0.8, fill = ChartColours[1]) +
        annotate(
          "text",
          x = ComCapTech$Tech,
          y = -5,
          label = ComCapTech$Tech,
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1],
          hjust = 1
        ) +
        annotate(
          "text",
          x = ComCapTech$Tech,
          y = ifelse(ComCapTech$Capacity > 30, 3, ComCapTech$Capacity+3),
          label = ifelse( ComCapTech$Capacity > 0, paste(ComCapTech$Capacity,"MW"), "<1 MW"),
          family = "Century Gothic",
          fontface = 2,
          colour = ifelse(ComCapTech$Capacity > 30, "white", ChartColours[1]),
          hjust = 0
        )
      
      
      ComCapTechChart
      
      
      ComCapTechChart <-
        StackedBars(ComCapTechChart,
                    ComCapTech,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ComCapTechChart <-
        ComCapTechChart +
        coord_flip() +
        ylim(-115, max(ComCapTech$Capacity))+
        scale_x_discrete(limits = rev(levels(ComCapTech$Tech)))+ 
        labs (subtitle = "Scotland, June 2020")
      
      ComCapTechChart
      
      ggsave(
        file,
        plot = ComCapTechChart,
        width = 17,
        height = 8.5,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$CommunityOperatingTechTable = renderDataTable({
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 41, n_max = 8)[1:5]
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[1:5]
    
    CommunityOperatingOutputType[8,1] <- "Total"
    

    
    CommunityOperatingOutputType[2,1] <- "Bioenergy and Waste"
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[-5,]
    
    names(CommunityOperatingOutputType) <- c("Tech", "Capacity (MW)", "%", "Number of Installations", "%")
    
      datatable(
      CommunityOperatingOutputType,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Capacity of operational community and locally owned renewable installations by technology",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Capacity of operational community and locally owned renewable installations by technology',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Capacity of operational community and locally owned renewable installations by technology')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10 
      ) 
    ) %>% 
          formatPercentage(c(3,5), 1)
    
    
  })
  
  observeEvent(input$ToggleTable3, {
    toggle("CommunityOperatingTechTable")
  })
  
  output$CommunityOperatingOutputTypeSubtitle <- renderText({
    paste(QuarterSubtitle)
  })
  
  output$CommunityOperatingOutputTypePlot <- renderPlotly  ({
    
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Comm & locally owned ren", col_names = TRUE, 
                               skip = 46, n_max = 4)
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[11:13]
    
    names(CommunityOperatingOutputType) <- c("Type", "Capacity", "%")
    
    CommunityOperatingOutputType$Capacity <- as.numeric(CommunityOperatingOutputType$Capacity)
    
    CommunityOperatingOutputType[is.na( CommunityOperatingOutputType)] <- 0.99
    
    CommunityOperatingOutputType$Type <- paste0("<b>", str_wrap(CommunityOperatingOutputType$Type, 16), "</b>")
    
    ChartColours <- c("#a3d65c", "#FF8500")
    
    p <- plot_ly(
      data = CommunityOperatingOutputType,
      y = ~Type,
      x = ~Capacity,
      text = paste0(CommunityOperatingOutputType$Type,
                    "\nCapacity: ",
                    ifelse(CommunityOperatingOutputType$Capacity <1, "<1", CommunityOperatingOutputType$Capacity), " MW"
      ),
      name = "Capacity",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  ChartColours[1])
    )  %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     autorange = "reversed",
                     ticktext = as.list(CommunityOperatingOutputType$Type),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                     showgrid = FALSE),
        
        xaxis = list(
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
  
  output$CommunityOperatingOutputType.png <- downloadHandler(
    filename = "CommunityOperatingOutputType.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Comm & locally owned ren", skip = 46, n_max = 4)[11:12]
      
      names(Data) <- c("Tech", "Capacity")
      
      Data[3,1] <- "CHP"
      
      ComCapType <- Data
      
      ComCapType$Tech <-
        factor(ComCapType$Tech, levels = ComCapType$Tech, ordered = TRUE)
      
      
      plottitle <-
        "Capacity of operational community and\nlocally owned renewable installations\nby type of output (MW)"
      sourcecaption <- "Source: EST"
      
      ChartColours <- c("#a3d65c", "#FF8500")
      BarColours <- c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32")
      
      
      ComCapTypeChart <-
        ComCapType %>%  ggplot(aes(x = Tech, y = Capacity, fill = ChartColours[1]), family = "Century Gothic") +
        geom_bar(stat = "identity", width = 0.8, fill = ChartColours[1]) +
        annotate(
          "text",
          x = ComCapType$Tech,
          y = -5,
          label = ComCapType$Tech,
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1],
          hjust = 1
        ) +
        annotate(
          "text",
          x = ComCapType$Tech,
          y = ifelse(ComCapType$Capacity > 30, 3, ComCapType$Capacity+3),
          label = ComCapType$Capacity,
          family = "Century Gothic",
          fontface = 2,
          colour = ifelse(ComCapType$Capacity > 30, "white", ChartColours[1]),
          hjust = 0
        )
      
      
      ComCapTypeChart
      
      
      ComCapTypeChart <-
        StackedBars(ComCapTypeChart,
                    ComCapType,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ComCapTypeChart <-
        ComCapTypeChart +
        coord_flip() +
        ylim(-125, max(ComCapType$Capacity))+
        scale_x_discrete(limits = rev(levels(ComCapType$Tech)))+ 
        labs (subtitle = "Scotland, June 2020")
      
      ComCapTypeChart
      
      ggsave(
        file,
        plot = ComCapTypeChart,
        width = 14.3,
        height = 7.5,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$CommunityOperatingOutputTypeTable = renderDataTable({
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 46, n_max = 5)
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[11:13]
    
    CommunityOperatingOutputType[5,1] <- "Total"
    
    names(CommunityOperatingOutputType) <- c("Output", "Capacity (MW)", "%")
    
    datatable(
      CommunityOperatingOutputType,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Estimated capacity of operational community and locally owned renewable installations by type of output",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Estimated capacity of operational community and locally owned renewable installations by type of output',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Estimated capacity of operational community and locally owned renewable installations by type of output')
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
  
  observeEvent(input$ToggleTable4, {
    toggle("CommunityOperatingOutputTypeTable")
  })
  
  output$EnSectorOwnershipSubtitle <- renderText({

    
    paste(QuarterSubtitle)
  })
  
  output$EnSectorOwnershipPlot <- renderPlotly  ({
    
    EnSectorOwnership <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 76, n_max = 6)
    
 
    
    names(EnSectorOwnership) <- c("Type", "Capacity", "%", "Installations", "%")
    
    EnSectorOwnership$Capacity <- as.numeric(EnSectorOwnership$Capacity)
    
    EnSectorOwnership[is.na( EnSectorOwnership)] <- 0.99
    
    EnSectorOwnership$Type <- paste0("<b>", EnSectorOwnership$Type, "</b>")
    
    EnSectorOwnership$Type <- factor(EnSectorOwnership$Type, levels = c(as.character(EnSectorOwnership$Type)))
    
    ChartColours <- c("#a3d65c", "#FF8500")
    
    p <- plot_ly(
      data = EnSectorOwnership,
      y = ~Type,
      x = ~Capacity,
      text = paste0(EnSectorOwnership$Type,
                    "\nCapacity: ",
                    ifelse(EnSectorOwnership$Capacity <1, "<1", EnSectorOwnership$Capacity), " MW"
      ),
      name = "Capacity",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  ChartColours[1])
    )  %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     autorange = "reversed",
                     showgrid = FALSE),
        xaxis = list(
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
  
  output$EnSectorOwnership.png <- downloadHandler(
    filename = "EnSectorOwnership.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Comm & locally owned ren", skip = 76, n_max = 6)[1:2]
      
      names(Data) <- c("Tech", "Capacity")
      
      ComCapOwnership <- Data
      
      ComCapOwnership$Tech <-
        factor(ComCapOwnership$Tech, levels = ComCapOwnership$Tech, ordered = TRUE)
      
      
      
      plottitle <-
        "Capacity of operational community and locally owned\nrenewable installations by ownership category"
      sourcecaption <- "Source: EST"
      
      ChartColours <- c("#a3d65c", "#FF8500")
      BarColours <- c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32")
      
      
      ComCapOwnershipChart <-
        ComCapOwnership %>%  ggplot(aes(x = Tech, y = Capacity, fill = ChartColours[1]), family = "Century Gothic") +
        geom_bar(stat = "identity", width = 0.8, fill = ChartColours[1]) +
        annotate(
          "text",
          x = ComCapOwnership$Tech,
          y = -5,
          label = ComCapOwnership$Tech,
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1],
          hjust = 1
        ) +
        annotate(
          "text",
          x = ComCapOwnership$Tech,
          y = ifelse(ComCapOwnership$Capacity > 30, 3, ComCapOwnership$Capacity+3),
          label = paste(ComCapOwnership$Capacity, "MW"),
          family = "Century Gothic",
          fontface = 2,
          colour = ifelse(ComCapOwnership$Capacity > 30, "white", ChartColours[1]),
          hjust = 0
        )
      
      
      ComCapOwnershipChart
      
      
      ComCapOwnershipChart <-
        StackedBars(ComCapOwnershipChart,
                    ComCapOwnership,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ComCapOwnershipChart <-
        ComCapOwnershipChart +
        coord_flip() +
        ylim(-92, max(ComCapOwnership$Capacity))+
        scale_x_discrete(limits = rev(levels(ComCapOwnership$Tech)))+ 
        labs (subtitle = "Scotland, June 2020")
      
      ComCapOwnershipChart
      
      ggsave(
        file, 
        plot = ComCapOwnershipChart,
        width = 19,
        height = 8.5,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EnSectorOwnershipTable = renderDataTable({
    EnSectorOwnership <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 76, n_max = 7)
    
    EnSectorOwnership[7,1] <- "Total"
    
    names(EnSectorOwnership) <- c("Owner", "Capacity (MW)", "%", "Number of installations", "%")
    
    datatable(
      EnSectorOwnership,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Estimated capacity of operational community and locally owned renewable installations by ownership",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Estimated capacity of operational community and locally owned renewable installations by ownership',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Estimated capacity of operational community and locally owned renewable installations by ownership')
        ),
        
        # customize the length menu
        lengthMenu = list( c(10, 20, -1) # declare values
                           , c(10, 20, "All") # declare titles
        ), # end of lengthMenu customization
        pageLength = 10 
      ) 
    ) %>% 
      formatPercentage(c(3,5), 1)
    
    
  })
  
  observeEvent(input$ToggleTable5, {
    toggle("EnSectorOwnershipTable")
  })
  
  
  output$Text <- renderUI({
    tagList(column(12,
                   
                   HTML(
                     paste(readtext("Structure/3 - Local Energy/LocalRenewables.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
  output$CommunityOperatingOutputTypeGenSubtitle <- renderText({
    paste(QuarterSubtitle)
  })
  
  output$CommunityOperatingOutputTypeGenPlot <- renderPlotly  ({
    
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                                               sheet = "Comm & locally owned ren", col_names = TRUE, 
                                               skip = 46, n_max = 4)
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[c(11,14,15)]
    
    names(CommunityOperatingOutputType) <- c("Type", "Generation", "%")
    
    CommunityOperatingOutputType$Generation <- as.numeric(CommunityOperatingOutputType$Generation)
    
    CommunityOperatingOutputType[is.na( CommunityOperatingOutputType)] <- 0.99
    
    CommunityOperatingOutputType$Type <- paste0("<b>", str_wrap(CommunityOperatingOutputType$Type, 16), "</b>")
    
    ChartColours <- c("#a3d65c", "#FF8500")
    
    p <- plot_ly(
      data = CommunityOperatingOutputType,
      y = ~Type,
      x = ~Generation,
      text = paste0(CommunityOperatingOutputType$Type,
                    "\nGeneration: ",
                    ifelse(CommunityOperatingOutputType$Generation <1, "<1", CommunityOperatingOutputType$Generation), " GWh"
      ),
      name = "Generation",
      type = "bar",
      hoverinfo = "text",
      orientation = 'h',
      marker = list(color =  ChartColours[1])
    )  %>% 
      layout(
        barmode = 'stack',
        legend = list(font = list(color = "#39ab2c"),
                      orientation = 'h'),
        hoverlabel = list(font = list(color = "white"),
                          hovername = 'text'),
        hovername = 'text',
        yaxis = list(title = "",
                     autorange = "reversed",
                     ticktext = as.list(CommunityOperatingOutputType$Type),
                     tickmode = "array",
                     tickvalues = list(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                     showgrid = FALSE),
        
        xaxis = list(
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
  
  output$CommunityOperatingOutputTypeGen.png <- downloadHandler(
    filename = "CommunityOperatingOutputGenType.png",
    content = function(file) {
      
      Data <- read_excel("Structure/CurrentWorking.xlsx", 
                         sheet = "Comm & locally owned ren", skip = 46, n_max = 4)[c(11,14)]
      
      names(Data) <- c("Tech", "Generation")
      
      Data[3,1] <- "CHP"
      
      ComCapType <- Data
      
      ComCapType$Tech <-
        factor(ComCapType$Tech, levels = ComCapType$Tech, ordered = TRUE)
      
      
      plottitle <-
        "Estimated generation of operational\ncommunity and locally owned renewable\ninstallations by type of output (GWh)"
      sourcecaption <- "Source: EST"
      
      ChartColours <- c("#a3d65c", "#FF8500")
      BarColours <- c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32")
      
      
      ComCapTypeChart <-
        ComCapType %>%  ggplot(aes(x = Tech, y = Generation, fill = ChartColours[1]), family = "Century Gothic") +
        geom_bar(stat = "identity", width = 0.8, fill = ChartColours[1]) +
        annotate(
          "text",
          x = ComCapType$Tech,
          y = -5,
          label = ComCapType$Tech,
          family = "Century Gothic",
          fontface = 2,
          colour = ChartColours[1],
          hjust = 1
        ) +
        annotate(
          "text",
          x = ComCapType$Tech,
          y = ifelse(ComCapType$Generation > 30, 3, ComCapType$Generation+3),
          label = ComCapType$Generation,
          family = "Century Gothic",
          fontface = 2,
          colour = ifelse(ComCapType$Generation > 30, "white", ChartColours[1]),
          hjust = 0
        )
      
      
      ComCapTypeChart
      
      
      ComCapTypeChart <-
        StackedBars(ComCapTypeChart,
                    ComCapType,
                    plottitle,
                    sourcecaption,
                    ChartColours)
      
      ComCapTypeChart <-
        ComCapTypeChart +
        coord_flip() +
        ylim(-250, max(ComCapType$Generation))+
        scale_x_discrete(limits = rev(levels(ComCapType$Tech)))+ 
        labs (subtitle = "Scotland, June 2020")
      
      ComCapTypeChart
      
      ggsave(
        file,
        plot = ComCapTypeChart,
        width = 14.3,
        height = 7.5,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$CommunityOperatingOutputTypeGenTable = renderDataTable({
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                                               sheet = "Comm & locally owned ren", col_names = TRUE, 
                                               skip = 46, n_max = 5)
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[c(11,14,15)]
    
    CommunityOperatingOutputType[5,1] <- "Total"
    
    names(CommunityOperatingOutputType) <- c("Output", "Estimated generation (GWh)", "%")
    
    datatable(
      CommunityOperatingOutputType,
      extensions = 'Buttons',
      
      rownames = FALSE,
      options = list(
        paging = TRUE,
        pageLength = -1,
        searching = TRUE,
        fixedColumns = FALSE,
        autoWidth = TRUE,
        ordering = TRUE,
        title = "Estimated generation of operational community and locally owned renewable installations by type of output",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Estimated generation of operational community and locally owned renewable installations by type of output',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Estimated generation of operational community and locally owned renewable installations by type of output')
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
  
  
}
