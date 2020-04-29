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

EconomyMeter <- read_delim("Processed Data/Output/Restricted Meters/RestrictedMetersProp.txt", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)

EconomyMeter <- EconomyMeter[c(1,2,ncol(EconomyMeter))]

names(EconomyMeter) <- c("LocalAuthority", "CODE", "Meters")

EconomyMeter <- EconomyMeter[which(substr(EconomyMeter$CODE, 1,3)== "S12"),]

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
EconomyMeter <- EconomyMeter[order(EconomyMeter$CODE),]

### Combine Data with Map data
LAMap <-
  append_data(LA, EconomyMeter, key.shp = "CODE", key.data = "CODE")

### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "Meters",
    title = "LA Renewable Percentages",
    palette = "Blues",
    breaks = c(0,.1,.2,.3,.4,.5),
    style = "cont"
  ) +
  tm_borders(alpha = .1) +
  tm_text("LocalAuthority", size = .5) # Set the variable to use as Labels, and the text size.

#Export to PDF, to be used in Inkscape
save_tmap(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandEconomyMeter.svg")

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
    "Meters",
    title = "LA Renewable Percentages",
    palette = "Blues",
    breaks = c(0,.1,.2,.3,.4,.5),
    style = "cont"
  ) +
  tm_borders(alpha = .1) +
  tm_text("LocalAuthority", size = 1) # Bigger Text size

#Export to PDF
save_tmap(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralEconomyMeter.svg")