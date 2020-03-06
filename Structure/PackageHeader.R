library(readxl)
library(data.table)
library(plyr)
library(plotly)
library(dygraphs)
library(reshape2)
library(lubridate)
library(xts)
library(RColorBrewer)
library(readr)
library("ggplot2")
library(extrafont)
library(ggflags)
library(png)
library(grid)
library(scales)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(ggrepel)
library(magrittr)
library(tidyr)


##########################################################################################################

TargetChart <-
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
          y = ifelse(dataMax$Renewables > 0, 0, -.015),
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
          y = ifelse(dataMax$Renewables > 0, 0, -.015),
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
          y = ifelse(dataMax$Renewables > 0, 0, -.015),
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

TargetChart2 <-
  function(data,
           plottitle,
           sourcecaption,
           ChartColours) {
    ######
    data[2] <- round(data[2], digits = 3) # Round Numberss
    data$Percentage <-
      percent(data$Renewables, accuracy = 0.1) # Create Percentage Output for Labels
    data$Target <-
      percent(data$Tgt) # Create Percentage Output for Labels
    
    
    ### Get Max Year with Actual Data ###
    dataMax <- data
    dataMax[3] <- NULL # Remove Target Values
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
          y = 0,
          x = min(Year),
          label = min(Year),
          colour = "Renewables",
          hjust = 0.5,
          vjust = ifelse(max(data$Tgt, na.rm = TRUE) > 0, 2.2, -0.2),
          fontface = 2
        ),
        family = "Century Gothic"
      ) +
      
      geom_text(
        aes(
          y = 0,
          x = dataMax$Year,
          label = dataMax$Year,
          colour = "Renewables",
          hjust = 0.5,
          vjust = ifelse(max(data$Tgt, na.rm = TRUE) > 0, 2.2, -0.2),
          fontface = 2
        ),
        
        family = "Century Gothic"
      ) +
      geom_text(
        aes(
          y = 0,
          x = Year,
          label = ifelse(Tgt > 0 | Tgt < 0, Year, ""),
          # Labels only years with a Target Value
          colour = "Target",
          hjust = 0.5,
          vjust = ifelse(Tgt > 0, 2.2, -0.2),
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
PercentLabel <- function(x) {
  paste0(x * 100, "%")
}

LinePercentChart <-
  function(chart,
           data,
           plottitle,
           sourcecaption,
           ChartColours) {
    chart +
      ### Axis Settingss
      scale_x_continuous(breaks = seq(min(data[1]), max(data[1]), 2)) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = ChartColours) +
      
      ### Label Plot
      labs(caption = sourcecaption) +
      labs(
        title = plottitle,
        face = 2,
        subtitle = paste("Scotland,", min(data$Year), "-", max(data$Year))
      ) +
      
      ### Theme Options ###
      
      ggplot2::theme(
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
      )
  }

StackedArea <-
  function(chart,
           data,
           plottitle,
           sourcecaption,
           ChartColours) {
    chart +
      ### Axis Settingss
      #scale_x_continuous(breaks = seq(min(data[1]), max(data[1]), 2)) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = ChartColours) +
      
      ### Label Plot
      labs(caption = sourcecaption) +
      labs(
        title = plottitle,
        face = 2,
        subtitle = paste("Scotland,", min(data$Year), "-", max(data$Year))
      )       +
      
      ### Theme Options ###
      
      ggplot2::theme(
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
      )
  }

HorizontalChart <-
  function(chart,
           data,
           plottitle,
           sourcecaption,
           ChartColours) {
    chart +
      ### Axis Settingss
      scale_x_continuous(breaks = seq(min(data[1]), max(data[1]), 2)) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = ChartColours) +
      
      ### Label Plot
      labs(caption = sourcecaption) +
      labs(
        title = plottitle,
        face = 2,
        subtitle = paste("Scotland,", min(data$Year), "-", max(data$Year))
      ) +
      
      ### Theme Options ###
      
      ggplot2::theme(
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
      #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
      
      
      ### Plot Borders
      annotate(
        geom = 'segment',
        y = Inf,
        yend = -Inf,
        color = ChartColours[1],
        x = Inf,
        xend = Inf,
        size = 1.5
      ) +
      annotate(
        geom = 'segment',
        y = Inf,
        yend = -Inf,
        color = ChartColours[1],
        x = -Inf,
        xend = -Inf,
        size = 1
      )
  }

PieChart <-
  function(chart,
           data,
           plottitle,
           sourcecaption,
           ChartColours) {
    chart +
      ### Axis Settingss
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = ChartColours) +
      
      ### Label Plot
      labs(caption = sourcecaption) +
      labs(title = plottitle,
           face = 2) +
      
      ### Theme Options ###
      
      ggplot2::theme(
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
      )
    #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
    
  }

BaselineChart <-
  function(chart,
           data,
           plottitle,
           sourcecaption,
           ChartColours) {
    chart +
      ### Axis Settingss
      scale_x_continuous(breaks = seq(min(data[1]), max(data[1]), 2)) +
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = ChartColours) +
      
      ### Label Plot
      labs(caption = sourcecaption) +
      labs(
        title = plottitle,
        face = 2,
        subtitle = paste("Scotland,", min(data$Year), "-", max(data$Year))
      ) +
      
      ### Theme Options ###
      
      ggplot2::theme(
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
      #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
      
      
      ### Plot Borders
      annotate(
        geom = 'segment',
        y = Inf,
        yend = -Inf,
        color = ChartColours[1],
        x = Inf,
        xend = Inf,
        size = 1.5
      ) +
      annotate(
        geom = 'segment',
        y = Inf,
        yend = -Inf,
        color = ChartColours[1],
        x = -Inf,
        xend = -Inf,
        size = 1
      )
  }

StackedBars <-
  function(chart,
           data,
           plottitle,
           sourcecaption,
           ChartColours) {
    chart +
      ### Axis Settingss
      scale_y_continuous(labels = scales::percent) +
      scale_color_manual(values = ChartColours) +
      
      ### Label Plot
      labs(caption = sourcecaption) +
      labs(title = plottitle,
           face = 2) +
      
      ### Theme Options ###
      
      ggplot2::theme(
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
      #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
      
      
      ### Plot Borders
      annotate(
        geom = 'segment',
        y = Inf,
        yend = -Inf,
        color = ChartColours[1],
        x = Inf,
        xend = Inf,
        size = 1.5
      ) +
      annotate(
        geom = 'segment',
        y = Inf,
        yend = -Inf,
        color = ChartColours[1],
        x = -Inf,
        xend = -Inf,
        size = 1
      )
  }
DailyChart <-
  function(chart,
           data,
           plottitle,
           sourcecaption,
           ChartColours) {
    chart +
      ### Axis Settingss
      scale_color_manual(values = ChartColours) +
      
      ### Label Plot
      labs(caption = sourcecaption) +
      labs(
        title = plottitle,
        face = 2,
        subtitle = paste(
          "Scotland,",
          format(min(data$Year), format = "%b %Y"),
          "-",
          format(max(data$Year), format = "%b %Y")
        )
      ) +
      
      ### Theme Options ###
      
      ggplot2::theme(
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
      
      
      ### Plot Borders
      annotate(
        geom = 'segment',
        y = Inf,
        yend = Inf,
        color = ChartColours[1],
        x = min(data$Year) - 10000000000,
        xend = max(data$Year) + 10000000000,
        size = 1.5
      ) +
      annotate(
        geom = 'segment',
        y = -Inf,
        yend = -Inf,
        color = ChartColours[1],
        x = min(data$Year) - 10000000000,
        xend = max(data$Year) + 10000000000,
        size = 1
      )
  }

TimeChart <-
  function(chart,
           data,
           plottitle,
           sourcecaption,
           ChartColours) {
    chart +
      ### Axis Settingss
      scale_color_manual(values = ChartColours) +
      
      ### Label Plot
      labs(caption = sourcecaption) +
      labs(
        title = plottitle,
        face = 2
      ) +
      
      ### Theme Options ###
      
      ggplot2::theme(
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
      )+
      
      
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
      )
  }



ScotlandFlag <-
  readPNG(
    "Structure/ScotlandFlag.png"
  )
ScotFlag <- rasterGrob(ScotlandFlag, interpolate = TRUE)

LatviaFlag <-
  readPNG(
    "Structure/LatviaFlag.png"
  )
LatviaFlag <- rasterGrob(LatviaFlag, interpolate = TRUE)

EUFlagLookup <- read_csv("Structure/EUFlagLookup.csv")


##########################################################################################################

PackageHeader <- 1
