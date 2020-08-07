
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


LARenGen <- read_delim("Processed Data/Output/Renewable Capacity/LARenCapSimple.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)

LARenGen <-  melt(LARenGen, id.vars = c("LACode", "LAName", "Year"))

names(LARenGen)[1] <- "CODE"

LARenGen <- LARenGen[which(LARenGen$Year == max(LARenGen$Year)),]

LARenGen <- LARenGen[which(substr(LARenGen$CODE,1,3) == "S12"),]

LARenGen <- LARenGen[which(LARenGen$variable == "Total Renewable"),]

LARenGen$Content <- paste0("<b>",LARenGen$LAName, "</b><br/>", LARenGen$variable[1], " Generation:<br/><em>", round(LARenGen$value, digits = 0)," GWh</em>" )

LARenGen$Hover <- paste0(LARenGen$LAName, " - ", round(LARenGen$value, digits = 2), " GWh")

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
LARenGen <- LARenGen[order(LARenGen$CODE),]

### Combine Data with Map data
LAMap <-
  append_data(LA, LARenGen, key.shp = "CODE", key.data = "CODE")


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0, 2500),
    style = "cont"
  ) +
  tm_borders(alpha = .1) +
  tm_text("LAName", size = .5) # Set the variable to use as Labels, and the text size.

#Export to PDF, to be used in Inkscape
save_tmap(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandLARenCap.svg")

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
    breaks = c(0, 2500),
    style = "cont"
  ) +
  tm_borders(alpha = .1) +
  tm_text("LAName", size = 1) # Bigger Text size

#Export to PDF
save_tmap(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralLARenCap.svg")
