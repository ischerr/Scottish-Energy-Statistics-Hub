Sources <- read_excel("Structure/Sources.xlsx")

Sources$HTML <- "Hi"

for (i in 1:nrow(Sources)){
  
  ifelse(substr(Sources[i,]$`Direct URL`,1,1) == "h", Sources[i,10] <- paste(a(Sources[i,]$Source, href = Sources[i,]$`Direct URL`, target="_blank")),Sources[i,10] <-  paste(p(Sources[i,]$Source, "(Unpublished)")))
  }    


SourceLookup <- function(x){
  
  
    
  tagList(
    
    
   HTML(Sources[which(Sources$Code == x),]$HTML),
    br(),
  )
}

DateLookup <- function(x){
  
 y <- min(Sources[which(Sources$Code %in% x),]$`Update Expected`, na.rm = TRUE)
  
  tagList(
    ifelse(is.infinite(y), "To Be Confirmed", format(y, "%B %Y")),
    br()
  )
} 



UpdatedLookup <- function(x){
  
  y <- max(Sources[which(Sources$Code %in% x),]$`Last Updated`, na.rm = TRUE)
  
  tagList(
    ifelse(is.infinite(y), " ", format(y, "%B %Y")),
    br()
  )
} 





css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
                



GenSupplyReadable <- read_delim("Processed Data/Output/Renewable Generation/GenSupplyReadable.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)

BalanceMultipliers <- read_excel("Structure/1 - Whole System/BalanceMultipliers.xlsx")

BalanceDropdown <- reactiveValues(Unit = "ktoe")

LARenGen <- read_delim("Processed Data/Output/Renewable Generation/LARenGen.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)

LARenCap <- read_delim("Processed Data/Output/Renewable Capacity/LAOperationalRenCap.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)

RenHeatDropdown <- reactiveValues(Measure = "Capacity")

ULEVbyLA <- read_delim("Processed Data/Output/Vehicles/ULEVbyLA.txt", 
                       "\t", escape_double = FALSE, trim_ws = TRUE)

LAHeatMap <- read_csv("Processed Data/Output/Consumption/HeatConsumptionbyLAMap.csv")

ECOMeasuresLACategories <- unique(read_delim("Processed Data/Output/ECO/ECOMeasuresLA.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)$variable)

LCRE <- read_csv("Processed Data/Output/LCRE/LCRE.csv") 

TransportMultipliers <- data.frame(Unit = c("GWh", "ktoe"),
                                   Multiplier = c(1, (1/0.01163)))

TransportDropdown <- reactiveValues(Unit = "GWh")