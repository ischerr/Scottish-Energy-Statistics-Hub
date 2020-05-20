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

AverageBillMap <- read_delim("Processed Data/Output/Charging Points/AmountCharged.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)

AverageBillMap <- AverageBillMap[c(1,2,ncol(AverageBillMap))]

names(AverageBillMap) <- c("LocalAuthority", "CODE", "AmountCharged")

AverageBillMap$AmountCharged <- AverageBillMap$AmountCharged /1000

AverageBillMap <- AverageBillMap[which(substr(AverageBillMap$CODE, 1,3)== "S12"),]

AverageBillMap$Content <- paste0("<b>",AverageBillMap$LocalAuthority, "</b><br/>Charging AmountCharged:<br/><em>", round(AverageBillMap$AmountCharged, digits = 0),"</em>" )

AverageBillMap$Hover <- paste0(AverageBillMap$LocalAuthority, " - ", round(AverageBillMap$AmountCharged, digits = 2))

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
AverageBillMap <- AverageBillMap[order(AverageBillMap$CODE),]

### Combine Data with Map data
LAMap <-
  append_data(LA, AverageBillMap, key.shp = "CODE", key.data = "CODE")


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "AmountCharged",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0,500,1000,1500),
    style = "cont"
  ) +
  tm_borders(alpha = .1) +
  tm_text("LocalAuthority", size = .5) # Set the variable to use as Labels, and the text size.

#Export to PDF, to be used in Inkscape
save_tmap(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandChargingAmountCharged.svg")

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
    "AmountCharged",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0,500,1000,1500),
    style = "cont"
  ) +
  tm_borders(alpha = .1) +
  tm_text("LocalAuthority", size = 1) # Bigger Text size

#Export to PDF
save_tmap(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralChargingAmountCharged.svg")