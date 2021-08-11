rm(list=ls())
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)

load("Map/ChileComunasRM_Shape.RData")

apruebo <- read_excel("Map/ChileComunasRM_valores.xlsx")

apruebo_mapa <- full_join(x.RM, apruebo, by = "nombre_comuna")

# Mapa de RM del porcentaje de APRUEBO
mapa <- ggplot() + 
  geom_polygon(data=apruebo_mapa, 
               aes(x=long, y=lat, group = group, fill = Apruebo), 
               colour ="white", size = 0.1) +
  labs(fill = "% APRUEBO") +
  labs(x="",y="") +
  scale_fill_gradient2(low ='gold1', mid="darkolivegreen1", 
                       midpoint=mean(apruebo_mapa$Apruebo) - 20,
                       high='forestgreen',
                       na.value = "lightgrey") + 
  theme_void() +
  theme(plot.title=element_text(size=16, hjust=0.5, face="bold", vjust=-1)) 

ggsave(mapa, filename = "Map/mapa_comunas_RM_Chile.jpeg", width = 30, height = 20, units = "cm")
