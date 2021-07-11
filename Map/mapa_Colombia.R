rm(list=ls())
library(readxl)
library(ggplot2)
library(scales)

load("Map/ShapeColombiaDeptos.RData")
unique(ShapeColombiaDeptos$id)

pobreza <- read_excel("Map/Pobreza_2020Colombia.xlsx")

pobreza_mapa <- merge(ShapeColombiaDeptos, pobreza, by = "id")

mapa <- ggplot(pobreza_mapa) + 
  geom_polygon(aes(x=long, y=lat, 
                   group = group, 
                   fill = Pobreza/100),
               colour ="grey",
               size = 0.1) +
  labs(x="",y="", fill = "",
       title="Incidencia de pobreza de Colombia",
       caption = "@hanwengutierrez") +
  scale_fill_gradient2(
    low ='chartreuse4', 
    mid="darkgoldenrod2", 
    high='coral2',
    midpoint=0.47,
    labels = percent,
    na.value="gray70")  + 
  theme_void() +
  theme(plot.title=element_text(size=16, hjust=2, face="bold", vjust=-1)) 

ggsave(mapa, filename = "Map/mapa_colombia.jpeg")
