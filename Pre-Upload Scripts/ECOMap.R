
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



ECOLA <- read_delim("Processed Data/Output/ECO/ECOMeasuresLA.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

names(ECOLA)[1:2] <- c("CODE", "LAName")

#"Carbon Saving Target (CERO)"       "ECO measures per 1,000 households" "Affordable Warmth (HHCRO)"         "ECO measures installed"            "Carbon Savings Community (CSCO)"  
ECOLA <- ECOLA[which(ECOLA$variable == "ECO measures per 1,000 households"),]

ECOLA$Content <- paste0("<b>",ECOLA$LAName, "</b><br/>", ECOLA$variable[1], " Generation:<br/><em>", round(ECOLA$value, digits = 0)," GWh</em>" )

ECOLA$Hover <- paste0(ECOLA$LAName, " - ", round(ECOLA$value, digits = 2), " GWh")

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
ECOLA <- ECOLA[order(ECOLA$CODE),]

### Combine Data with Map data
LAMap <-
  merge(LA, ECOLA)


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0, 400),
    style = "cont"
  ) +
    tm_borders(alpha = .1)
   

#Export to PDF, to be used in Inkscape
tmap_save(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandECOper1000.svg")

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
    breaks = c(0, 100,200,300,400),
    style = "cont"
  ) +
    tm_borders(alpha = .1)
    

#Export to PDF
tmap_save(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralECOper1000.svg")

### Add Simplified shape back to the Shapefile
LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")

LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))







ECOLA <- read_delim("Processed Data/Output/ECO/ECOMeasuresLA.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

names(ECOLA)[1:2] <- c("CODE", "LAName")

#"Carbon Saving Target (CERO)"       "ECO measures per 1,000 households" "Affordable Warmth (HHCRO)"         "ECO measures installed"            "Carbon Savings Community (CSCO)"  
ECOLA <- ECOLA[which(ECOLA$variable == "Carbon Saving Target (CERO)"),]

ECOLA$Content <- paste0("<b>",ECOLA$LAName, "</b><br/>", ECOLA$variable[1], " Generation:<br/><em>", round(ECOLA$value, digits = 0)," GWh</em>" )

ECOLA$Hover <- paste0(ECOLA$LAName, " - ", round(ECOLA$value, digits = 2), " GWh")

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
ECOLA <- ECOLA[order(ECOLA$CODE),]

### Combine Data with Map data
LAMap <-
  merge(LA, ECOLA)


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0, 20000),
    style = "cont"
  ) +
    tm_borders(alpha = .1)
   

#Export to PDF, to be used in Inkscape
tmap_save(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandECOCERO.svg")

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
    breaks = c(0, 20000),
    style = "cont"
  ) +
    tm_borders(alpha = .1)
    

#Export to PDF
tmap_save(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralECOCERO.svg")


### Add Simplified shape back to the Shapefile
LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")

LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))






ECOLA <- read_delim("Processed Data/Output/ECO/ECOMeasuresLA.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

names(ECOLA)[1:2] <- c("CODE", "LAName")

#"Carbon Saving Target (CERO)"       "ECO measures per 1,000 households" "Affordable Warmth (HHCRO)"         "ECO measures installed"            "Carbon Savings Community (CSCO)"  
ECOLA <- ECOLA[which(ECOLA$variable == "Affordable Warmth (HHCRO)"),]

ECOLA$Content <- paste0("<b>",ECOLA$LAName, "</b><br/>", ECOLA$variable[1], " Generation:<br/><em>", round(ECOLA$value, digits = 0)," GWh</em>" )

ECOLA$Hover <- paste0(ECOLA$LAName, " - ", round(ECOLA$value, digits = 2), " GWh")

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
ECOLA <- ECOLA[order(ECOLA$CODE),]

### Combine Data with Map data
LAMap <-
  merge(LA, ECOLA)


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0, 16000),
    style = "cont"
  ) +
    tm_borders(alpha = .1)
   

#Export to PDF, to be used in Inkscape
tmap_save(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandECOAW.svg")

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
    breaks = c(0, 16000),
    style = "cont"
  ) +
    tm_borders(alpha = .1)
    

#Export to PDF
tmap_save(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralECOAW.svg")

### Add Simplified shape back to the Shapefile
LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")

LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))






ECOLA <- read_delim("Processed Data/Output/ECO/ECOMeasuresLA.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

names(ECOLA)[1:2] <- c("CODE", "LAName")

#"Carbon Saving Target (CERO)"       "ECO measures per 1,000 households" "Affordable Warmth (HHCRO)"         "ECO measures installed"            "Carbon Savings Community (CSCO)"  
ECOLA <- ECOLA[which(ECOLA$variable == "ECO measures installed"),]

ECOLA$Content <- paste0("<b>",ECOLA$LAName, "</b><br/>", ECOLA$variable[1], " Generation:<br/><em>", round(ECOLA$value, digits = 0)," GWh</em>" )

ECOLA$Hover <- paste0(ECOLA$LAName, " - ", round(ECOLA$value, digits = 2), " GWh")

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
ECOLA <- ECOLA[order(ECOLA$CODE),]

### Combine Data with Map data
LAMap <-
  merge(LA, ECOLA)


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0, 48000),
    style = "cont"
  ) +
    tm_borders(alpha = .1)
   

#Export to PDF, to be used in Inkscape
tmap_save(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandECOInstalled.svg")

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
    breaks = c(0, 48000),
    style = "cont"
  ) +
    tm_borders(alpha = .1)
    

#Export to PDF
tmap_save(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralECOInstalled.svg")





### Add Simplified shape back to the Shapefile
LA <- readOGR("Pre-Upload Scripts/Maps/Shapefile/LocalAuthority2.shp")

LA <- spTransform(LA, CRS("+proj=longlat +datum=WGS84"))

ECOLA <- read_delim("Processed Data/Output/ECO/ECOMeasuresLA.txt", 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

names(ECOLA)[1:2] <- c("CODE", "LAName")

#"Carbon Saving Target (CERO)"       "ECO measures per 1,000 households" "Affordable Warmth (HHCRO)"         "ECO measures installed"            "Carbon Savings Community (CSCO)"  
ECOLA <- ECOLA[which(ECOLA$variable == "Carbon Savings Community (CSCO)"),]

ECOLA$Content <- paste0("<b>",ECOLA$LAName, "</b><br/>", ECOLA$variable[1], " Generation:<br/><em>", round(ECOLA$value, digits = 0)," GWh</em>" )

ECOLA$Hover <- paste0(ECOLA$LAName, " - ", round(ECOLA$value, digits = 2), " GWh")

### Change LA$CODE to string
LA$CODE <- as.character(LA$CODE)

### Order LAs in Shapefile
LA <- LA[order(LA$CODE),]

### Order LAs in Data
ECOLA <- ECOLA[order(ECOLA$CODE),]

### Combine Data with Map data
LAMap <-
  merge(LA, ECOLA)


### Create Full Scotland Map
ScotlandMap <- tm_shape(LAMap) +
  tm_fill(
    "value",
    title = "LA Renewable Percentages",
    palette = "Greens",
    breaks = c(0, 13000),
    style = "cont"
  ) +
    tm_borders(alpha = .1)
   

#Export to PDF, to be used in Inkscape
tmap_save(ScotlandMap, filename = "Pre-Upload Scripts/Maps/ScotlandECOCSCO.svg")

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
    breaks = c(0, 13000),
    style = "cont"
  ) +
    tm_borders(alpha = .1)
    

#Export to PDF
tmap_save(CentralMap, filename = "Pre-Upload Scripts/Maps/CentralECOCSCO.svg")
