### Load Packages
library(readr)
library("maptools")
library(tmaptools)
library(tmap)
library("sf")
library("leaflet")
library("rgeos")
library(readxl)
library(ggplot2)
library(rgdal)

### Add Simplified shape back to the Shapefile
LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")

LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
############ RENEWABLE ELECTRICITY ################################################

AverageBillMap <- read_csv("Processed Data/Output/Charging Points/Points.csv")[1:4]

names(AverageBillMap) <- c("CODE", "LocalAuthority", "Points", "Rapid Points")

AverageBillMap <- AverageBillMap[which(substr(AverageBillMap$CODE, 1,3)== "S12"),]

AverageBillMap$Content <- paste0("<b>",AverageBillMap$LocalAuthority, "</b><br/>Charging Points:<br/><em>", round(AverageBillMap$Points, digits = 0),"</em>" )

AverageBillMap$Hover <- paste0(AverageBillMap$LocalAuthority, " - ", round(AverageBillMap$Points, digits = 2))

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
AverageBillMap <- AverageBillMap[order(AverageBillMap$CODE),]

### Combine Data with Map data
LAMap <-
  merge(LA, AverageBillMap)


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "Points",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0,50,100,150,200),
    style = "cont"
  ) +
  tm_borders(alpha = .1)

#Export to PDF, to be used in Inkscape
tmap_save(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandChargingPoints.svg")

#Subset the Central Belt
LACentral <-
  subset(
    LAMap,
    CODE == "S12000045"   # East Dunbartonshire
    | CODE == "S12000039" # West Dunbartonshite
    | CODE == "S12000014" # Falkirk
    | CODE == "S12000018" # Inverclyde
    | CODE == "S12000049" # Glasgow City
    | CODE == "S12000038" # Renfrewshire
    | CODE == "S12000050" # North Lanarkshire
    | CODE == "S12000040" # West Lothian
    | CODE == "S12000036" # City of Edinburgh
    | CODE == "S12000011" # East Renfrewshire
  )

# Create Central Belt Map
CentralMap <- tm_shape(LACentral) +
  tm_fill(
    "Points",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0,50,100,150,200),
    style = "cont"
  ) +
  tm_borders(alpha = .1) 

#Export to PDF
tmap_save(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralChargingPoints.svg")
