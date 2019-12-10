require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

LocalRenewablesOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Community owned renewable capacity",
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
                   h4(textOutput(ns('CommunityOperatingOutputTypeSubtitle')))
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
      tabPanel("Operating capacity by ownership",
               fluidRow(
                 column(
                   8,
                   h3("Capacity of operational community and locally owned renewable installations by ownership category", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('EnSectorOwnershipSubtitle')))
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
      tabPanel("Stages",
    fluidRow(
      column(10, h3("Data - Stages", style = "color: #a3d65c;  font-weight:bold")),
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
    tabPanel("Initiatives",
             fluidRow(
               column(10, h3("Data - Initiatives", style = "color: #a3d65c;  font-weight:bold")),
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
    tabPanel("Tech",
             fluidRow(
               column(10, h3("Data - Tech", style = "color: #a3d65c;  font-weight:bold")),
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
    tabPanel("Outputs",
             fluidRow(
               column(10, h3("Data - Outputs", style = "color: #a3d65c;  font-weight:bold")),
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
    tabPanel("Owner",
             fluidRow(
               column(10, h3("Data - Owner", style = "color: #a3d65c;  font-weight:bold")),
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
LocalRenewables <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("EnEconomy.R")
  ###### Energy Sector Emplyment ######
  
  output$CommunityCapacitySubtitle <- renderText({
      
      paste("Scotland, 2018")
  })
  
  output$CommunityCapacityPlot <- renderPlotly  ({
    
    CommunityCapacity <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Comm & locally owned ren",
        col_names = FALSE,
        skip = 12,
        n_max = 6
      )
    
    CommunityCapacity <- CommunityCapacity[2:3]
    
    CommunityCapacity <- as_tibble(t(CommunityCapacity))
    
    names(CommunityCapacity) <- unlist(CommunityCapacity[1,])
    
    CommunityCapacity <- CommunityCapacity[-1,]
    
    names(CommunityCapacity)[1:2] <- c("Type", "Operating")
    
    CommunityCapacity[1,1] <- 1
    
    CommunityCapacity %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    CommunityCapacity <- as_tibble(CommunityCapacity)
    
    ChartColours <- c("#a3d65c", "#FF8500")
    BarColours <- c("#005a32", "#238b45", "#41ab5d", "#74c476",  "#a1d99b")
    
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
                         sheet = "Comm & locally owned ren", skip = 12, n_max = 5)[1:3]
      
      names(Data) <- c("Year", "Phase", "Capacity")
      
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
      BarColours <- c("#a1d99b", "#74c476", "#41ab5d", "#238b45", "#005a32")
      
      
      CommunityCapacityChart <-
        CommunityCapacity %>%  ggplot(aes(x = Year, y = Capacity, fill = Phase), family = "Century Gothic") +
        scale_fill_manual(
          "Phase",
          values = c(
            "Operating*" = BarColours[5],
            "Under construction" = BarColours[4],
            "Consented not built" = BarColours[3],
            "In planning" = BarColours[2],
            "In scoping" = BarColours[1]
          )
        ) +
        geom_bar(stat = "identity", width = 1) +
        annotate(
          geom = 'segment',
          y = 1000,
          yend = 1000,
          color = "#fc9272",
          x = CommunityCapacity$Year - 1,
          xend = CommunityCapacity$Year + .5,
          size = 1,
          linetype = 3
        ) +
        annotate(
          "text",
          x = CommunityCapacity$Year - 1.5,
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
          x = mean(CommunityCapacity$Year),
          y = sum(CommunityCapacity$Capacity) - CommunityCapacity$pos,
          label = ifelse(
            row_number(CommunityCapacity$Phase) %% 2 == 0,
            paste0(CommunityCapacity$Capacity, " MW\n", CommunityCapacity$Phase),
            paste0(
              CommunityCapacity$Phase,
              "\n",
              CommunityCapacity$Capacity,
              " MW"
            )
          ),
          vjust = ifelse(row_number(CommunityCapacity$Phase) %% 2 == 0, 1.5, -.6),
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
        width = 15,
        height = 7,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$CommunityCapacityTable = renderDataTable({
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 12, n_max = 5)
    
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
    
    CommunityOperatingOutputType$Date <- format(CommunityOperatingOutputType$Date, format = "%Y")
    
    paste("Scotland,", min(CommunityOperatingOutputType$Date[which(CommunityOperatingOutputType$Capacity != 0)], na.rm = TRUE), "-", max(CommunityOperatingOutputType$Date[which(CommunityOperatingOutputType$Capacity != 0)], na.rm = TRUE))
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
    
   RenEn2 <- data.frame(Date = ymd("2020-01-01"), `Capacity` = NA, Target = 1000)
   
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
          format(CommunityOperatingOutputType$Date, "%Y")
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
                                 CommunityOperatingOutputType$Capacity < 0), ][-1, ]$Date, "%Y")
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
          format(CommunityOperatingOutputType$Date, "%Y")
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
          showgrid = FALSE,
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
                                        sheet = "Comm & locally owned ren", skip = 12, n_max = 15)[12:14]
      
      names(Data) <- c("Year", "Renewables", "Target")
      
      Data$Year <- as.numeric(substr(Data$Year,1,4))
      
      Data <- Data[complete.cases(Data$Year),]
      
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
            label = ifelse(Year == max(Year[which(Renewables > 0)]) |
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
            y = Target,
            label = ifelse(Year == max(Year[which(Target > 0)]), paste0("Target:\n", Target, " MW"), ""),
            vjust = 1.3,
            colour = ChartColours[2],
            fontface = 2
          ),
          family = "Century Gothic"
        ) +
        geom_point(
          data = tail(ComCapOperating[which(ComCapOperating$Target > 0),], 1),
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
            label = ifelse(Year == max(Year[which(Target > 0)]), Year, ""),
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
        ylim(0,1000)+
        xlim(min(ComCapOperating$Year),max(ComCapOperating$Year)+0.5)+
        labs(subtitle = paste0("Scotland, ", min(ComCapOperating$Year), " - ", max(ComCapOperating$Year[which(ComCapOperating$Renewables >0)])))
      
      ComCapOperatingChart
      
      ggsave(
        file,
        plot = ComCapOperatingChart,
        width = 11,
        height = 15,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$CommunityOperatingCapacityTable = renderDataTable({
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 12, n_max = 5)
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[12:14]
    
    names(CommunityOperatingOutputType) <- c("Date", "Capacity (MW)", "Number of Installations")
    
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
    CommunityOperatingTech <- read_excel("Structure/CurrentWorking.xlsx", 
                                  sheet = "Energy economy", col_names = FALSE, 
                                  skip = 12, n_max = 9)
    
    CommunityOperatingTech <- as.data.frame(t(CommunityOperatingTech))
    
    CommunityOperatingTech <- tail(CommunityOperatingTech[c(1,6)],-1)
    CommunityOperatingTech %<>% lapply(function(x) as.numeric(as.character(x)))
    CommunityOperatingTech <- as.data.frame(CommunityOperatingTech)
    names(CommunityOperatingTech) <- c("Year", "GVA")
    
    CommunityOperatingTech <- CommunityOperatingTech[which(CommunityOperatingTech$Year >= 2008),]
    
    CommunityOperatingTech$Year <- as.numeric(substr(CommunityOperatingTech$Year, 1,4))
    
    CommunityOperatingTech$GVA <- CommunityOperatingTech$GVA /1000
    
    paste("Scotland,", min(CommunityOperatingTech$Year),"-", max(CommunityOperatingTech$Year))
  })
  
  output$CommunityOperatingTechPlot <- renderPlotly  ({
    
    CommunityOperatingTech <- read_excel("Structure/CurrentWorking.xlsx", 
                                  sheet = "Comm & locally owned ren", col_names = TRUE, 
                                  skip = 40, n_max = 8)
    
    CommunityOperatingTech <- CommunityOperatingTech[1:2]
    
    names(CommunityOperatingTech) <- c("Tech", "Capacity")
    
    CommunityOperatingTech$Capacity <- as.numeric(CommunityOperatingTech$Capacity)
    
    CommunityOperatingTech[is.na( CommunityOperatingTech)] <- 0.99
        
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
                         sheet = "Comm & locally owned ren", skip = 40, n_max = 8)[1:2]
      
      names(Data) <- c("Tech", "Capacity")
      
      Data$Capacity <- as.numeric(Data$Capacity)
      
      Data[is.na(Data)] <- 0
      
      ComCapTech <- Data
      
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
        ylim(-50, max(ComCapTech$Capacity))+
        scale_x_discrete(limits = rev(levels(ComCapTech$Tech)))+ 
        labs (subtitle = "Scotland, June 2018")
      
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
                                 skip = 40, n_max = 9)
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[1:5]
    
    CommunityOperatingOutputType[9,1] <- "Total"
    
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
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Energy economy", col_names = FALSE, 
                               skip = 12, n_max = 9)
    
    CommunityOperatingOutputType <- as.data.frame(t(CommunityOperatingOutputType))
    
    CommunityOperatingOutputType <- tail(CommunityOperatingOutputType[c(1,6)],-1)
    CommunityOperatingOutputType %<>% lapply(function(x) as.numeric(as.character(x)))
    CommunityOperatingOutputType <- as.data.frame(CommunityOperatingOutputType)
    names(CommunityOperatingOutputType) <- c("Year", "GVA")
    
    CommunityOperatingOutputType <- CommunityOperatingOutputType[which(CommunityOperatingOutputType$Year >= 2008),]
    
    CommunityOperatingOutputType$Year <- as.numeric(substr(CommunityOperatingOutputType$Year, 1,4))
    
    CommunityOperatingOutputType$GVA <- CommunityOperatingOutputType$GVA /1000
    
    paste("Scotland,", min(CommunityOperatingOutputType$Year),"-", max(CommunityOperatingOutputType$Year))
  })
  
  output$CommunityOperatingOutputTypePlot <- renderPlotly  ({
    
    CommunityOperatingOutputType <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Comm & locally owned ren", col_names = TRUE, 
                               skip = 47, n_max = 4)
    
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
                         sheet = "Comm & locally owned ren", skip = 47, n_max = 4)[11:12]
      
      names(Data) <- c("Tech", "Capacity")
      
      Data[3,1] <- "CHP"
      
      ComCapType <- Data
      
      ComCapType$Tech <-
        factor(ComCapType$Tech, levels = ComCapType$Tech, ordered = TRUE)
      
      
      plottitle <-
        "Capacity of operational community and locally\nowned renewable installations by type of output"
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
        ylim(-175, max(ComCapType$Capacity))+
        scale_x_discrete(limits = rev(levels(ComCapType$Tech)))+ 
        labs (subtitle = "Scotland, June 2018")
      
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
                                 skip = 47, n_max = 5)
    
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
      formatPercentage(c(3,5), 1)
    
    
  })
  
  observeEvent(input$ToggleTable4, {
    toggle("CommunityOperatingOutputTypeTable")
  })
  
  output$EnSectorOwnershipSubtitle <- renderText({

    
    paste("Scotland,")
  })
  
  output$EnSectorOwnershipPlot <- renderPlotly  ({
    
    EnSectorOwnership <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 75, n_max = 6)
    
 
    
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
                         sheet = "Comm & locally owned ren", skip = 75, n_max = 6)[1:2]
      
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
        ylim(-90, max(ComCapOwnership$Capacity))+
        scale_x_discrete(limits = rev(levels(ComCapOwnership$Tech)))+ 
        labs (subtitle = "Scotland, June 2018")
      
      ComCapOwnershipChart
      
      ggsave(
        file, 
        plot = ComCapOwnershipChart,
        width = 17,
        height = 8.5,
        units = "cm",
        dpi = 300
      )
    }
  )
  
  output$EnSectorOwnershipTable = renderDataTable({
    EnSectorOwnership <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 75, n_max = 7)
    
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
  
  
}
