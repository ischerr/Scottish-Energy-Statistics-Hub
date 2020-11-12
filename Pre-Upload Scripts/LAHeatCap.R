
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

### Add Simplified shape back to the Shapefile
LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")

LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))
############ RENEWABLE ELECTRICITY ################################################


RenHeatLA <- read_delim("Processed Data/Output/Renewable Heat/RenHeatLA.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)

names(RenHeatLA)[1:2] <- c("LAName", "CODE")

RenHeatLA <- RenHeatLA[which(RenHeatLA$variable == "Operational Capacity (GW)"),]

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
RenHeatLA <- RenHeatLA[order(RenHeatLA$CODE),]

### Combine Data with Map data
LAMap <-
  merge(LA, RenHeatLA)


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0, 0.3),
    style = "cont"
  ) +
  tm_borders(alpha = .1) 

#Export to PDF, to be used in Inkscape
tmap_save(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandLAHeatCap.svg")

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
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0, 0.3),
    style = "cont"
  ) +
  tm_borders(alpha = .1) 

#Export to PDF
tmap_save(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralLAHeatCap.svg")





RenHeatLA <- read_delim("Processed Data/Output/Renewable Heat/RenHeatLA.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)

names(RenHeatLA)[1:2] <- c("LAName", "CODE")

RenHeatLA <- RenHeatLA[which(RenHeatLA$variable == "Output (GWh)"),]

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
RenHeatLA <- RenHeatLA[order(RenHeatLA$CODE),]

### Combine Data with Map data
LAMap <-
  merge(LA, RenHeatLA)


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0, 1000),
    style = "cont"
  ) +
  tm_borders(alpha = .1) 

#Export to PDF, to be used in Inkscape
tmap_save(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandLAHeatOutput.svg")

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
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0, 1000),
    style = "cont"
  ) +
  tm_borders(alpha = .1) 

#Export to PDF
tmap_save(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralLAHeatOutput.svg")

