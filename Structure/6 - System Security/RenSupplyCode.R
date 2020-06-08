library(readr)
library(readxl)

GenSupplyReadable <- read_delim("Processed Data/Output/Renewable Generation/GenSupplyReadableProcessed.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)


RenSupplyGen <- read_excel("Structure/6 - System Security/RenSupplyGen.xlsx")


Country <- "Scotland"

Year <- 2018


GenSupplyReadableProcessed <- GenSupplyReadable[which(GenSupplyReadableProcessed$Country == Country),]

GenSupplyReadableProcessed <- GenSupplyReadableProcessed[which(GenSupplyReadableProcessed$Year == Year),]

RenSupplyGen[c(3,6)]

RenSupplyGen$title <- 0

RenSupplyGen$title[1] <- GenSupplyReadableProcessed$`Total generated`

RenSupplyGen$title[2] <- -(GenSupplyReadableProcessed$`Electricity transferred to England (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Northern Ireland (net of receipts)`+ GenSupplyReadableProcessed$`Electricity transferred to Europe (net of receipts)`)

RenSupplyGen$title[3] <- RenSupplyGen$title[1] + RenSupplyGen$title[2]

RenSupplyGen$title[4] <- -GenSupplyReadableProcessed$`Transfers from other generators to public supply`

RenSupplyGen$title[5] <- -GenSupplyReadableProcessed$`Consumption by autogenerators`

RenSupplyGen$title[6] <- -(GenSupplyReadableProcessed$`Own use by Other generators`+GenSupplyReadableProcessed$`Used in pumping at pumped storage and other own use by MPPs`)

RenSupplyGen$title[7] <- -(GenSupplyReadableProcessed$`Transmission losses` + GenSupplyReadableProcessed$`Distribution losses and theft`)

RenSupplyGen$title[8] <- sum(RenSupplyGen$title[c(1,4, 5, 6)])
  
RenSupplyGen$title[10] <- sum(RenSupplyGen$title[c(3,6:7)])

RenSupplyGen$title[9] <- sum(RenSupplyGen$title[c(10,5)])

RenSupplyGen$size <- (abs(RenSupplyGen$title) / max(abs(RenSupplyGen$title))) * 75



