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

print("MapsandGasGrid")

# This is unlikely to change from 2012
yearstart <- 2012

### Set the final year for the loop as the current year ###
yearend <- format(Sys.Date(), "%Y")

### Load Shapefile
LAinitial <- readShapePoly("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority.shp")

LAinitial$CODE <- as.character(LAinitial$CODE)
LAinitial$CODE[LAinitial$CODE == "S12000015"] <- "S12000047"
LAinitial$CODE[LAinitial$CODE == "S12000024"] <- "S12000048"
LAinitial$CODE[LAinitial$CODE == "S12000044"] <- "S12000050"
LAinitial$CODE[LAinitial$CODE == "S12000046"] <- "S12000049"
LAinitial$CODE <- as.factor(LAinitial$CODE)

### Simplify Shapefile for later image manipulation
LAsimple <- gSimplify(LAinitial, tol = 200, topologyPreserve = TRUE)

### Add Simplified shape back to the Shapefile
LA = SpatialPolygonsDataFrame(LAsimple, data = LAinitial@data)
############ RENEWABLE ELECTRICITY ################################################

AverageBillLA <- read_excel(
  "Structure/CurrentWorking.xlsx",
  sheet = "Average Bill",
  skip = 22
)[c(2,1,4,5,6)]

names(AverageBillLA) <- c("LocalAuthority", "CODE", "AverageElectricityBill", "AverageGasBill", "AverageTotalBill")


### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
AverageBillLA <- AverageBillLA[order(AverageBillLA$CODE),]

### Combine Data with Map data
LAMap <-
  append_data(LA, AverageBillLA, key.shp = "CODE", key.data = "CODE")

### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "AverageTotalBill",
    title = "LA Renewable Percentages",
    palette = "Blues",
    breaks = c(1000, 1200, 1400, 1600, 1800, 2000),
    style = "cont"
  ) +
  tm_borders(alpha = .1) +
  tm_text("LocalAuthority", size = .5) # Set the variable to use as Labels, and the text size.

#Export to PDF, to be used in Inkscape
save_tmap(ScotlandMap, filename = "Pre-Upload Scripts/Maps/Scotland.svg")

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
    "AverageTotalBill",
    title = "LA Renewable Percentages",
    palette = "Blues",
    breaks = c(1000, 1200, 1400, 1600, 1800, 2000),
    style = "cont"
  ) +
  tm_borders(alpha = .1) +
  tm_text("LocalAuthority", size = 1) # Bigger Text size

#Export to PDF
save_tmap(CentralMap, filename = "Pre-Upload Scripts/Maps/Central.svg")