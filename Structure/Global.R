Sources <- read_excel("Structure/Sources.xlsx")

SourceLookup <- function(x){
  tagList(
    a(Sources[which(Sources$Code == x),][2], href = Sources[which(Sources$Code == x),][3], target="_blank"),
    br()
  )
}