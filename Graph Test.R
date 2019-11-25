ElecConEx <- read_csv("Structure/3 - Electricity/ElecConEx.csv")

YearLow <- min(ElecConEx$Year) 
YearHigh <- max(ElecConEx$Year +1.1)

################################################

g <- ElecConEx %>%  ggplot(aes(x=Year)) +
  geom_line(aes(y = Exports, colour = "Exports" )) +
  geom_line(aes(y = Imports, colour = "Imports" )) +
  scale_x_continuous(breaks=seq(YearLow,YearHigh,2))+
  labs(y="Amount £")+
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    , legend.title = ggplot2::element_blank()
    , axis.text = element_text(colour = "black")
    , axis.title = element_text(colour = "black")
    , legend.text = element_text(colour = "black")
    , axis.ticks = ggplot2::element_blank()
    , panel.grid.major = element_blank()
  ) +
  geom_hline(yintercept=0, color = "black", alpha = 0.4) 

ggsave(file="test.svg", plot=g, width=10, height = 8)

g <- ggplotly(g, tooltip = c("Year","y")) %>% layout(legend = list(
  font = list(color = "black")),
  hoverlabel = list(
    font = list(color = "black")))
g
