Sources <- read_excel("Structure/Sources.xlsx")

SourceLookup <- function(x){
  tagList(
    a(Sources[which(Sources$Code == x),][2], href = Sources[which(Sources$Code == x),][3], target="_blank"),
    br()
  )
}

DateLookup <- function(x){
  
 y <- min(Sources[which(Sources$Code %in% x),]$`Update Expected`, na.rm = TRUE)
  
  tagList(
    ifelse(is.infinite(y), "To Be Confirmed", format(y, "%B %Y")),
    br()
  )
} 


css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML
                



GenSupplyReadable <- read_delim("Processed Data/Output/Renewable Generation/GenSupplyReadable.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)

BalanceMultipliers <- read_excel("Structure/1 - Whole System/BalanceMultipliers.xlsx")