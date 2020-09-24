
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


EnConsumptionLA <- read_csv("Processed Data/Output/Consumption/CorrectedFinalConsumptionbyLA.csv")

Year <- max(EnConsumptionLA$Year)

unique(EnConsumptionLA$Year)

EnConsumptionLAMap <- EnConsumptionLA[which(EnConsumptionLA$Year == Year),]

EnConsumptionLAMap <- EnConsumptionLAMap[c(2,1,31,32,33,30)]

names(EnConsumptionLAMap) <- c("LocalAuthority", "CODE", "Industry & Commercial", "Domestic", "Transport", "Total")

EnConsumptionLAMap[order(substr(EnConsumptionLAMap$`CODE`,1,3), EnConsumptionLAMap$`LocalAuthority`),]


EnConsumptionLAMap <- EnConsumptionLAMap[c(1,2,ncol(EnConsumptionLAMap))]

EnConsumptionLAMap <- EnConsumptionLAMap[which(substr(EnConsumptionLAMap$CODE, 1,3)== "S12"),]

EnConsumptionLAMap[is.na(EnConsumptionLAMap)] <- 0

EnConsumptionLAMap$Content <- paste0("<b>",EnConsumptionLAMap$LocalAuthority, "</b><br/>Total final energy consumption:<br/><em>", ifelse(EnConsumptionLAMap$Total > 0,paste0(format(round(EnConsumptionLAMap$Total,0 ),  big.mark = ",")," GWh</em>"),"N/A" ))

EnConsumptionLAMap$Hover <- paste0(EnConsumptionLAMap$LocalAuthority, " - ", ifelse(EnConsumptionLAMap$Total > 0,paste0(format(round(EnConsumptionLAMap$Total,0 ),  big.mark = ",")," GWh"),"N/A" ))




### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
EnConsumptionLAMap <- EnConsumptionLAMap[order(EnConsumptionLAMap$CODE),]

### Combine Data with Map data
LAMap <- merge(LA, EnConsumptionLAMap)


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0,5000,10000,15000,20000),
    style = "cont"
  ) +
  tm_borders(alpha = .1) +
  tm_text("LAName", size = .5) # Set the variable to use as Labels, and the text size.

#Export to PDF, to be used in Inkscape
save_tmap(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandLAConsump.svg")

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
    breaks = c(0,5000,10000,15000,20000),
    style = "cont"
  ) +
  tm_borders(alpha = .1) +
  tm_text("LAName", size = 1) # Bigger Text size

#Export to PDF
save_tmap(CentralMap, filename = "Pre-Upload Scripts/Maps/LAConsump.svg")
