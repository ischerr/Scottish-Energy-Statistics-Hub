require(readxl)
require(plotly)
require(dygraphs)
require(png)
require("DT")
###### UI Function ######

source("Structure/Global.R")

CommunityRenewablesOutput <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Energy Sector Employment",
               fluidRow(
                 column(
                   8,
                   h3("Employment in the energy sector", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('EnSectorEmploymentSubtitle')), style = "color: #a3d65c;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('EnSectorEmployment.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               plotlyOutput(ns("EnSectorEmploymentPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      tabPanel("Turnover associated with the energy sector",
               fluidRow(
                 column(
                   8,
                   h3("Energy Sector Turnover", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('EnSectorTurnoverSubtitle')), style = "color: #a3d65c;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('EnSectorTurnover.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               plotlyOutput(ns("EnSectorTurnoverPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      tabPanel("GVA associated with the energy sector",
               fluidRow(
                 column(
                   8,
                   h3("Energy Sector GVA", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('EnSectorGVASubtitle')), style = "color: #a3d65c;")
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('EnSectorGVA.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               plotlyOutput(ns("EnSectorGVAPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      tabPanel("Exports associated with the energy sector",
               fluidRow(
                 column(
                   8,
                   h3("Energy Sector Exports", style = "color: #a3d65c;  font-weight:bold"),
                   h4(textOutput(ns('EnSectorExportsSubtitle')))
                 ),
                 column(
                   4,
                   style = 'padding:15px;',
                   downloadButton(ns('EnSectorExports.png'), 'Download Graph', style =
                                    "float:right")
                 )
               ),
               
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;"),
               plotlyOutput(ns("EnSectorExportsPlot"))%>% withSpinner(color="#a3d65c"),
               tags$hr(style = "height:3px;border:none;color:#a3d65c;background-color:#a3d65c;")),
      tabPanel("Ownership",
               fluidRow(
                 column(
                   8,
                   h3("Ownership", style = "color: #a3d65c;  font-weight:bold"),
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
      ns("EnSectorEmploymentTable")
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
               ns("EnSectorTurnoverTable")
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
               ns("EnSectorGVATable")
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
               ns("EnSectorExportsTable")
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
CommunityRenewables <- function(input, output, session) {
  
  
  if (exists("PackageHeader") == 0) {
    source("Structure/PackageHeader.R")
  }
  
  
  print("EnEconomy.R")
  ###### Energy Sector Emplyment ######
  
  output$EnSectorEmploymentSubtitle <- renderText({
      
      paste("Scotland, 2018")
  })
  
  output$EnSectorEmploymentPlot <- renderPlotly  ({
    
    EnSectorEmployment <-
      read_excel(
        "Structure/CurrentWorking.xlsx",
        sheet = "Comm & locally owned ren",
        col_names = FALSE,
        skip = 12,
        n_max = 6
      )
    
    EnSectorEmployment <- EnSectorEmployment[2:3]
    
    EnSectorEmployment <- as_tibble(t(EnSectorEmployment))
    
    names(EnSectorEmployment) <- unlist(EnSectorEmployment[1,])
    
    EnSectorEmployment <- EnSectorEmployment[-1,]
    
    names(EnSectorEmployment)[1:2] <- c("Type", "Operating")
    
    EnSectorEmployment[1,1] <- 1
    
    EnSectorEmployment %<>% lapply(function(x)
      as.numeric(as.character(x)))
    
    EnSectorEmployment <- as_tibble(EnSectorEmployment)
    
    ChartColours <- c("#a3d65c", "#FF8500")
    BarColours <- c("#005a32", "#238b45", "#41ab5d", "#74c476",  "#a1d99b")
    
    p <-
      plot_ly(data = EnSectorEmployment, y = ~Type) %>%
      add_trace(
        data = EnSectorEmployment,
        x = ~ Operating,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Operating",
        text = paste0("Operating: ", EnSectorEmployment$Operating, " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[1]),
        legendgroup = 1
      ) %>%
      add_trace(
        data = EnSectorEmployment,
        x = ~ `Under construction`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Under Construction",
        text = paste0("Under Construction: ", EnSectorEmployment$`Under construction`, " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[2]),
        legendgroup = 2
      ) %>%
      add_trace(
        data = EnSectorEmployment,
        x = ~ `Consented not built`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "Consented not built",
        text = paste0("Consented not built: ", EnSectorEmployment$`Consented not built`, " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[3]),
        legendgroup = 3
      ) %>%
      add_trace(
        data = EnSectorEmployment,
        x = ~ `In planning`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "In planning",
        text = paste0("In planning: ", EnSectorEmployment$`In planning`, " MW"),
        hoverinfo = 'text',
        marker = list(color = BarColours[4]),
        legendgroup = 4
      ) %>%
      add_trace(
        data = EnSectorEmployment,
        x = ~ `In scoping`,
        type = 'bar',
        width = 0.3,
        orientation = 'h',
        name = "In scoping",
        text = paste0("In scoping: ", EnSectorEmployment$`In scoping`, " MW"),
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
  
  output$EnSectorEmployment.png <- downloadHandler(
    filename = "EnSectorEmployment.png",
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
  
  output$EnSectorEmploymentTable = renderDataTable({
    EnSectorExports <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 12, n_max = 5)
    
    EnSectorExports <- EnSectorExports[2:3]
  
    names(EnSectorExports) <- c("Stage", "Capacity (MW)")
    
    EnSectorExports[1,1] <- "Operating"
    
        datatable(
      EnSectorExports,
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
    toggle("EnSectorEmploymentTable")
  })
  
  output$EnSectorTurnoverSubtitle <- renderText({
    
    EnSectorExports <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Comm & locally owned ren",
      col_names = TRUE,
      skip = 12,
      n_max = 15
    )
    
    EnSectorExports <-
      EnSectorExports[12:14]
    
    names(EnSectorExports) <-
      c("Date", "Capacity", "Number of Installations")
    
    EnSectorExports$`Number of Installations` <- NULL
    
    EnSectorExports$Target <- NA
    
    EnSectorExports$Date <- format(EnSectorExports$Date, format = "%Y")
    
    paste("Scotland,", min(EnSectorExports$Date[which(EnSectorExports$Capacity != 0)], na.rm = TRUE), "-", max(EnSectorExports$Date[which(EnSectorExports$Capacity != 0)], na.rm = TRUE))
   })
  
  output$EnSectorTurnoverPlot <- renderPlotly  ({
    EnSectorExports <- read_excel(
      "Structure/CurrentWorking.xlsx",
      sheet = "Comm & locally owned ren",
      col_names = TRUE,
      skip = 12,
      n_max = 15
    )
    
    EnSectorExports <-
      EnSectorExports[12:14]
    
    names(EnSectorExports) <-
      c("Date", "Capacity", "Number of Installations")
    
    EnSectorExports$`Number of Installations` <- NULL
    
    EnSectorExports$Target <- NA
    
   RenEn2 <- data.frame(Date = ymd("2020-01-01"), `Capacity` = NA, Target = 1000)
   
   EnSectorExports <- rbind(EnSectorExports, RenEn2)
   
   ChartColours <- c("#a3d65c", "#FF8500")
    
    p <-  plot_ly(EnSectorExports, x = ~ Date) %>%
      add_trace(
        y = ~ Capacity,
        name = "Capacity",
        type = 'scatter',
        mode = 'lines',
        legendgroup = "1",
        text = paste0(
          "Progress: ",
          EnSectorExports$Capacity,
          " MW\nDate: ",
          format(EnSectorExports$Date, "%Y")
        ),
        hoverinfo = 'text',
        line = list(
          width = 6,
          color = ChartColours[1],
          dash = "none"
        )
      ) %>%
      add_trace(
        data = tail(EnSectorExports[which(EnSectorExports$Capacity > 0 |
                                           EnSectorExports$Capacity < 0), ], 1),
        x = ~ Date,
        y = ~ `Capacity`,
        name = "Renewable Electricity",
        text = paste0(
          "Progress: ",
          EnSectorExports[which(EnSectorExports$Capacity > 0 |
                                  EnSectorExports$Capacity < 0), ][-1, ]$Capacity,
          " MW\nDate: ",
          format(EnSectorExports[which(EnSectorExports$Capacity > 0 |
                                 EnSectorExports$Capacity < 0), ][-1, ]$Date, "%Y")
        ),
        hoverinfo = 'text',
        showlegend = FALSE ,
        type = "scatter",
        mode = 'markers',
        marker = list(size = 18,
                      color = ChartColours[1])
      ) %>%
      add_trace(
        data = EnSectorExports,
        x = ~ Date,
        y = ~ Target,
        name = "Target",
        legendgroup = "2",
        text = paste0(
          "Target: ",
          EnSectorExports$Target,
          " MW\nDate: ",
          format(EnSectorExports$Date, "%Y")
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
          range = c(min(EnSectorExports$Date) - 100, max(EnSectorExports$Date) +
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
  
  output$EnSectorTurnover.png <- downloadHandler(
    filename = "EnSectorTurnover.png",
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
  
  output$EnSectorTurnoverTable = renderDataTable({
    EnSectorExports <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 12, n_max = 5)
    
    EnSectorExports <- EnSectorExports[12:14]
    
    names(EnSectorExports) <- c("Date", "Capacity (MW)", "Number of Installations")
    
    EnSectorExports$Date <- format(EnSectorExports$Date, "%b %Y")
    
    datatable(
      EnSectorExports,
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
    toggle("EnSectorTurnoverTable")
  })
  
  output$EnSectorGVASubtitle <- renderText({
    EnSectorGVA <- read_excel("Structure/CurrentWorking.xlsx", 
                                  sheet = "Energy economy", col_names = FALSE, 
                                  skip = 12, n_max = 9)
    
    EnSectorGVA <- as.data.frame(t(EnSectorGVA))
    
    EnSectorGVA <- tail(EnSectorGVA[c(1,6)],-1)
    EnSectorGVA %<>% lapply(function(x) as.numeric(as.character(x)))
    EnSectorGVA <- as.data.frame(EnSectorGVA)
    names(EnSectorGVA) <- c("Year", "GVA")
    
    EnSectorGVA <- EnSectorGVA[which(EnSectorGVA$Year >= 2008),]
    
    EnSectorGVA$Year <- as.numeric(substr(EnSectorGVA$Year, 1,4))
    
    EnSectorGVA$GVA <- EnSectorGVA$GVA /1000
    
    paste("Scotland,", min(EnSectorGVA$Year),"-", max(EnSectorGVA$Year))
  })
  
  output$EnSectorGVAPlot <- renderPlotly  ({
    
    EnSectorGVA <- read_excel("Structure/CurrentWorking.xlsx", 
                                  sheet = "Comm & locally owned ren", col_names = TRUE, 
                                  skip = 40, n_max = 8)
    
    EnSectorGVA <- EnSectorGVA[1:2]
    
    names(EnSectorGVA) <- c("Tech", "Capacity")
    
    EnSectorGVA$Capacity <- as.numeric(EnSectorGVA$Capacity)
    
    EnSectorGVA[is.na( EnSectorGVA)] <- 0.99
        
    EnSectorGVA$Tech <- paste0("<b>", EnSectorGVA$Tech, "</b>")
    
    EnSectorGVA$Tech <- factor(EnSectorGVA$Tech, levels = c(as.character(EnSectorGVA$Tech)))
  
    ChartColours <- c("#a3d65c", "#FF8500")
    
    p <- plot_ly(
      data = EnSectorGVA,
      y = ~Tech,
      x = ~Capacity,
      text = paste0(EnSectorGVA$Tech,
        "\nCapacity: ",
        ifelse(EnSectorGVA$Capacity <1, "<1", EnSectorGVA$Capacity), " MW"
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
  
  output$EnSectorGVA.png <- downloadHandler(
    filename = "EnSectorGVA.png",
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
  
  output$EnSectorGVATable = renderDataTable({
    EnSectorExports <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 40, n_max = 9)
    
    EnSectorExports <- EnSectorExports[1:5]
    
    EnSectorExports[9,1] <- "Total"
    
    names(EnSectorExports) <- c("Tech", "Capacity (MW)", "%", "Number of Installations", "%")
    
      datatable(
      EnSectorExports,
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
    toggle("EnSectorGVATable")
  })
  
  output$EnSectorExportsSubtitle <- renderText({
    EnSectorExports <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Energy economy", col_names = FALSE, 
                               skip = 12, n_max = 9)
    
    EnSectorExports <- as.data.frame(t(EnSectorExports))
    
    EnSectorExports <- tail(EnSectorExports[c(1,6)],-1)
    EnSectorExports %<>% lapply(function(x) as.numeric(as.character(x)))
    EnSectorExports <- as.data.frame(EnSectorExports)
    names(EnSectorExports) <- c("Year", "GVA")
    
    EnSectorExports <- EnSectorExports[which(EnSectorExports$Year >= 2008),]
    
    EnSectorExports$Year <- as.numeric(substr(EnSectorExports$Year, 1,4))
    
    EnSectorExports$GVA <- EnSectorExports$GVA /1000
    
    paste("Scotland,", min(EnSectorExports$Year),"-", max(EnSectorExports$Year))
  })
  
  output$EnSectorExportsPlot <- renderPlotly  ({
    
    EnSectorExports <- read_excel("Structure/CurrentWorking.xlsx", 
                               sheet = "Comm & locally owned ren", col_names = TRUE, 
                               skip = 47, n_max = 4)
    
    EnSectorExports <- EnSectorExports[11:13]
    
    names(EnSectorExports) <- c("Type", "Capacity", "%")
    
    EnSectorExports$Capacity <- as.numeric(EnSectorExports$Capacity)
    
    EnSectorExports[is.na( EnSectorExports)] <- 0.99
    
    EnSectorExports$Type <- paste0("<b>", str_wrap(EnSectorExports$Type, 16), "</b>")
    
    ChartColours <- c("#a3d65c", "#FF8500")
    
    p <- plot_ly(
      data = EnSectorExports,
      y = ~Type,
      x = ~Capacity,
      text = paste0(EnSectorExports$Type,
                    "\nCapacity: ",
                    ifelse(EnSectorExports$Capacity <1, "<1", EnSectorExports$Capacity), " MW"
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
                     ticktext = as.list(EnSectorExports$Type),
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
  
  output$EnSectorExports.png <- downloadHandler(
    filename = "EnSectorExports.png",
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
  
  output$EnSectorExportsTable = renderDataTable({
    EnSectorExports <- read_excel("Structure/CurrentWorking.xlsx", 
                                 sheet = "Comm & locally owned ren", col_names = TRUE, 
                                 skip = 47, n_max = 5)
    
    EnSectorExports <- EnSectorExports[11:13]
    
    EnSectorExports[5,1] <- "Total"
    
    names(EnSectorExports) <- c("Output", "Capacity (MW)", "%")
    
    datatable(
      EnSectorExports,
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
    toggle("EnSectorExportsTable")
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
        title = "Estimated capacity of operational community and locally owned renewable installations by type of Owner",
        dom = 'ltBp',
        buttons = list(
          list(extend = 'copy'),
          list(
            extend = 'excel',
            title = 'Estimated capacity of operational community and locally owned renewable installations by type of Owner',
            header = TRUE
          ),
          list(extend = 'csv',
               title = 'Estimated capacity of operational community and locally owned renewable installations by type of Owner')
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
                     paste(readtext("Structure/3 - Local Energy/CommunityRenewables.txt")[2])
                     
                   )))
  })
  
  observeEvent(input$ToggleText, {
    toggle("Text")
  })
  
  
}
