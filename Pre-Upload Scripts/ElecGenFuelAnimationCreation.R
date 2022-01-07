### Load Packages and Functions

if (exists("PackageHeader") == 0) {
  source(
    "Structure/PackageHeader.R"
  )
}
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggrepel)
library(gganimate)

print("Elec gen by fuel")
###### Elec Gen Fuel ######

# ElecGenFuel <-
#   read_csv(
#     "J:/ENERGY BRANCH/Statistics/Energy Strategy - Stats Publication/2019/Graphs/Data/ElecGenFuel.csv"
#   )

DataScot <- read_csv("Processed Data/Output/Electricity Generation/ScotlandFuelElecGenProportion.csv")

DataScot$`Other Renewables` <- DataScot$Renewables - DataScot$Hydro - DataScot$Wind - DataScot$`Solar PV` - DataScot$`Bioenergy and Waste`

DataScot <- select(DataScot,
                   Year,
                   Hydro,
                   Wind,
                   'Solar PV',
                   'Pumped hydro',
                   Nuclear,
                   Coal,
                   Oil,
                   Gas,
                   Other, 
                   'Other Renewables',
                   'Bioenergy and Waste'
)
DataScot$Sector <- "1.8"

DataScot <-
  DataScot[c(
    "Sector",
    "Year",
    "Wind",
    "Hydro",
    "Bioenergy and Waste",
    "Solar PV",
    "Other Renewables",
    "Nuclear",
    "Pumped hydro",
    "Other",
    "Coal",
    "Oil",
    "Gas"
  )]

DataEW <- read_csv("Processed Data/Output/Electricity Generation/EWFuelElecGenProportion.csv")

DataEW$`Other Renewables` <- DataEW$Renewables - DataEW$Hydro - DataEW$Wind - DataEW$`Solar PV` - DataEW$`Bioenergy and Waste`

DataEW <- select(DataEW,
                 Year,
                 Hydro,
                 Wind,
                 'Solar PV',
                 'Pumped hydro',
                 Nuclear,
                 Coal,
                 Oil,
                 Gas,
                 Other, 
                 'Other Renewables',
                 'Bioenergy and Waste'
)
DataEW$Sector <- "1"

DataEW <-
  DataEW[c(
    "Sector",
    "Year",
    "Wind",
    "Hydro",
    "Bioenergy and Waste",
    "Solar PV",
    "Other Renewables",
    "Nuclear",
    "Pumped hydro",
    "Other",
    "Coal",
    "Oil",
    "Gas"
  )]





ElecGenFuel <- rbind(DataEW, DataScot)

#ElecGenFuel$Sector <- ifelse(ElecGenFuel == "1", 1, 2)

ElecGenFuel <- ElecGenFuel[complete.cases(ElecGenFuel),]

ElecGenFuel$Sector <- as.numeric(ElecGenFuel$Sector)

ElecGenFuel <- melt(ElecGenFuel, id.vars = c("Sector", "Year"))

ElecGenFuel$variable <-
  factor(ElecGenFuel$variable,
         levels = rev(unique(ElecGenFuel$variable)),
         ordered = TRUE)

### Calculate Pumped hydro addition for low carbon in Scotland after 2017

HydroAddition <- ElecGenFuel[which(ElecGenFuel$variable == "Pumped hydro"),]

HydroAddition[which(HydroAddition$Year < 2017),]$value <- 0

HydroAddition[which(HydroAddition$Year >= 2017 & HydroAddition$Sector == 1.0),]$value <- 0



ElecGenFuel <- ElecGenFuel %>%
  group_by(Year, Sector)  %>%
  mutate(pos = cumsum(value) - value / 2) %>%
  mutate(top = sum(value)) %>% 
  mutate(RenLineX = Sector + 0.23) %>% 
  mutate(RenLineY = sum(value[which(variable %in% c("Wind", "Hydro", "Bioenergy and Waste", "Solar PV", "Other Renewables"))])) %>% 
  mutate(RenText = 1 - sum(value[which(variable %in% c("Coal", "Oil", "Gas", "Other", "Pumped hydro", "Nuclear"))])) %>% 
  mutate(LCLineX = Sector - 0.23) %>% 
  mutate(LCLineY = sum(value[which(variable %in% c("Wind", "Hydro", "Bioenergy and Waste",  "Solar PV", "Other Renewables", "Nuclear"))])) %>% 
  mutate(LCText = 1 - sum(value[which(variable %in% c("Coal", "Oil", "Gas", "Other", "Pumped hydro"))])) %>% 
  mutate(FossilLineX = Sector - 0.23) %>% 
  mutate(FossilLineY = sum(value[which(variable %in% c("Coal", "Oil", "Gas"))])) %>% 
  mutate(FossilText = sum(value[which(variable %in% c("Coal", "Oil", "Gas"))])) %>% 
  mutate(Infinite = Inf)

for(i in 2017: max(ElecGenFuel$Year)){
  ElecGenFuel[which(ElecGenFuel$Year == i & ElecGenFuel$Sector == 1.8),]$LCLineY <- max(ElecGenFuel[which(ElecGenFuel$Year == i & ElecGenFuel$Sector == 1.8),]$LCLineY) + HydroAddition[which(HydroAddition$Year == i & HydroAddition$Sector == 1.8),]$value
  
  ElecGenFuel[which(ElecGenFuel$Year == i & ElecGenFuel$Sector == 1.8),]$LCText <- max(ElecGenFuel[which(ElecGenFuel$Year == i & ElecGenFuel$Sector == 1.8),]$LCText) + HydroAddition[which(HydroAddition$Year == i & HydroAddition$Sector == 1.8),]$value
  
}
  
CountryList <- c("Scotland", "England and Wales")
plottitle <-
  "Proportion of electricity generation by fuel"
sourcecaption <- "Source: BEIS"

ChartColours <- c("#39ab2c", "#FF8500")
BarColours <-
  c(
    "#005a32",
    "#238b45",
    "#41ab5d",
    "#74c476",
    "#a1d99b",
    "#6baed6",
    "#969696",
    "#fdae6b",
    "#f16913",
    "#d94801",
    "#696969"
  )

#ElecGenFuelChart <- ElecGenFuelChart[which(ElecGenFuel$Year > 2016)]

ElecGenFuelChart <- ElecGenFuel %>%
  ggplot(aes(x = Sector, y = value, fill = variable), family = "Century Gothic") +
  scale_fill_manual(
    "variable",
    values = c(
      "Wind" = BarColours[1],
      "Hydro" = BarColours[2],
      "Bioenergy and Waste" = BarColours[3],
      "Solar PV" = BarColours[4],
      "Other Renewables" = BarColours[5],
      "Nuclear" = BarColours[6],
      "Pumped hydro" = BarColours[7],
      "Coal" = BarColours[8],
      "Oil" = BarColours[9],
      "Gas" = BarColours[10],
      "Other" = BarColours[11]
    )
  ) +
  geom_bar(stat = "identity", width = .4) +
  geom_text(aes(
    y = pos,
    label = ifelse(value > 0.03, percent(value, accuracy = 1), " "),
    fontface = 2
   
    
  ),
  colour = "white",
  size = 7,
  family = "Century Gothic") +
  geom_text(
    aes(
      x = Sector,
      y = -0.08,
      label = str_wrap(ifelse(Sector == 1, "England and Wales", "Scotland"), width = 9),
      fontface = 2
    ),
    colour = ChartColours[1],
    size = 8,
    family = "Century Gothic"
  )  +
  # annotate(
  #   "text",
  #   x = 2.375,
  #   y =  (sum(ElecGenFuel$value[which(
  #     ElecGenFuel$variable != "Gas" &
  #       ElecGenFuel$variable != "Oil" &
  #       ElecGenFuel$variable != "Coal" &
  #       ElecGenFuel$variable != "Pumped hydro" &
  #       ElecGenFuel$variable != "Nuclear" &
  #       ElecGenFuel$Sector == "1"
  #   )])) / 2,
  #   label = paste("Renewables:",
  #                 percent(1 - sum(ElecGenFuel$value[which(
  #                   ElecGenFuel$variable != "Wind" &
  #                     ElecGenFuel$variable != "Hydro" &
  #                     ElecGenFuel$variable != "Bioenergy and Waste" &
  #                     ElecGenFuel$variable != "Other Renewables" &
  #                     ElecGenFuel$Sector == "1"
  #                 )]))),
  #   colour =  BarColours[3],
  #   family = "Century Gothic",
  #   fontface = 2
  # ) +
geom_segment(mapping = aes(
    x = RenLineX,
    xend = RenLineX,
    y = RenLineY,
    yend = RenLineY - RenLineY),
    colour =  BarColours[2],
    size = 1.5
  ) +
  geom_text(aes(
    x =  RenLineX + 0.1,
    y = RenLineY * 0.5,
    label = paste("Renewables:", percent(RenText, accuracy = .1))
  ),
  size = 8,
  colour = BarColours[2],
  family = "Century Gothic",
  fontface = 2
  ) +
  
  geom_segment(mapping = aes(
    x = LCLineX,
    xend = LCLineX,
    y = LCLineY,
    yend = LCLineY - LCLineY),
    colour =  BarColours[6],
    size = 1.5
  ) +
  
  geom_text(aes(
    x =  LCLineX - 0.121,
    y = LCLineY * 0.5,
    label = paste("Low Carbon:", percent(LCText, accuracy = .1))),
    size = 8,
    colour = BarColours[6],
    family = "Century Gothic",
    fontface = 2
  ) +
  
  geom_segment(mapping = aes(
    x = FossilLineX,
    xend = FossilLineX,
    y = top,
    yend = top - FossilLineY),
    colour =  BarColours[9],
    size = 1.5
  ) +
  
  geom_text(aes(
    x =  FossilLineX - 0.121,
    y = top - (FossilLineY * 0.5),
    label = paste("Fossil Fuels:", percent(FossilText, accuracy = .1))),
    size = 8,
    colour = BarColours[9],
    family = "Century Gothic",
    fontface = 2
  ) +
  geom_text(aes(
    x = 2.375,
    y = ((0 / 10) *1.15) - 0.121,
    label = "Wind"
  ),
  colour =  BarColours[1],
  size = 7,
  family = "Century Gothic") +
  geom_text(
    aes(
      x = 2.375,
      y = ((1 / 10) *1.15) - 0.121,
      label = "Hydro"
    ),
    colour =  BarColours[2],
    size = 7,
    family = "Century Gothic"
  ) +
  geom_text(
    aes(
      x = 2.375,
      y = ((2 / 10) *1.15) - 0.121,
      label = "Bioenergy\n& wastes"
    ),
    colour =  BarColours[3],
    size = 7,
    family = "Century Gothic"
  ) +
  geom_text(
    aes(
      x = 2.375,
      y = ((3 / 10) *1.15) - 0.121,
      label = "Solar\nPV"
    ),
    colour =  BarColours[4],
    size = 7,
    family = "Century Gothic"
  ) +
  geom_text(
    aes(
      x = 2.375,
      y = ((4 / 10) *1.15) - 0.121,
      label = "Other\nRenewables"
    ),
    colour =  BarColours[5],
    size = 7,
    family = "Century Gothic"
  ) +
  geom_text(
    aes(
      x = 2.375,
      y = ((5 / 10) *1.15) - 0.121,
      label = "Nuclear"
    ),
    colour =  BarColours[6],
    size = 7,
    family = "Century Gothic"
  ) +
  geom_text(
    aes(
      x = 2.375,
      y = ((6 / 10) *1.15) - 0.121,
      label = "Pumped\nHydro"
    ),
    colour =  BarColours[7],
    size = 7,
    family = "Century Gothic"
  ) +
  geom_text(
    aes(
      x = 2.375,
      y = ((7 / 10) *1.15) - 0.121,
      label = "Other"
    ),
    colour =  BarColours[11],
    size = 7,
    family = "Century Gothic"
  ) +
  geom_text(
    aes(
      x = 2.375,
      y = ((8 / 10) *1.15) - 0.121,
      label = "Coal"
    ),
    colour =  BarColours[8],
    size = 7,
    family = "Century Gothic"
  ) +
  geom_text(aes(
    x = 2.375,
    y = ((9 / 10) *1.15) - 0.121,
    label = "Oil"
  ),
  colour =  BarColours[9],
  size = 7,
  family = "Century Gothic") +
  geom_text(aes(
    x = 2.375,
    y = ((10 / 10) *1.15) - 0.121,
    label = "Gas"
  ),
  colour =  BarColours[10],
  size = 7,
  family = "Century Gothic") +
  geom_text(aes(x = 2.8,
                y = 0,
                label = " ")) +
  geom_text(aes(
    x = 2.65,
    y = -0.15,
    label = "Proportion of electricity generation by fuel",
    fontface = 2,
    hjust = 0
  ),
  colour =  ChartColours[1],
  size = 10,
  family = "Century Gothic") +
  geom_text(aes(
    x = 2.65,
    y = 1.05,
    label = paste("Year:", Year),
    fontface = 2,
    hjust = 1
  ),
  colour =  ChartColours[1],
  size = 8,
  family = "Century Gothic") +
  geom_text(aes(
    x = 0.4,
    y = 1.05,
    label = paste("Source: BEIS"),
    hjust = 1
  ),
  colour =  ChartColours[1],
  size = 8,
  family = "Century Gothic") +
  geom_segment(
    aes(x = 0.5,
        xend = 0.5,
        y = -0.15,
        yend = 1.05),
    colour = ChartColours[1],
    size = 2
  )+
  geom_segment(
    aes(x = 2.52,
        xend = 2.52,
        y = -0.15,
        yend = 1.05),
    colour = ChartColours[1],
    size = 2
  )+
  coord_flip()+
  ### Axis Settingss
  scale_y_continuous(labels = scales::percent) +
  
  scale_color_manual(values = ChartColours) +

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
    plot.title = ggplot2::element_text(face = "bold")
  ) +

  #geom_hline(yintercept=.52, color = ChartColours[2], alpha = 0.7)+
  
  
  ## Plot Borders

  # annotate(
  #   geom = 'segment',
  #   y = Inf,
  #   yend = -Inf,
  #   color = ChartColours[1],
  #   x = -Inf,
  #   xend = -Inf,
  #   size = 1
  # )+
  transition_time(Year) +
  ease_aes('linear') 

animate(ElecGenFuelChart, height = (650), width = (1200), res = 75, end_pause = 20, detail = 200)

anim_save("Structure/2 - Renewables/Electricity/ElecGenAnimation.gif")

