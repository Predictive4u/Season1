library(rvest)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Página de Wiki 
url1 <- "https://es.wikipedia.org/wiki/Anexo:Estad%C3%ADsticas_de_la_Copa_América"
tmp1 <- read_html(url1)
tmp1 <- html_nodes(tmp1, "table")
length(tmp1) # Hay en total 27 tablas
sapply(tmp1, function(x) dim(html_table(x, fill = TRUE)))
Copa <- html_table(tmp1[[7]], fill = TRUE)

url2 <- "https://es.wikipedia.org/wiki/Selección_de_fútbol_de_Colombia"
tmp2 <- read_html(url2)
tmp2 <- html_nodes(tmp2, "table")
length(tmp2) # Hay en total 21 tablas
sapply(tmp2, function(x) dim(html_table(x, fill = TRUE)))
Colombia <- html_table(tmp2[[7]], fill = TRUE)
Colombia <- Colombia[-16, c(-1,-7)]
Colombia$Partidos <- as.numeric(Colombia$Partidos)
Colombia <- Colombia %>% arrange(desc(Partidos))

colores <- colorRampPalette(brewer.pal(12, "Paired"))(15)

ggplot(Colombia, aes(x = reorder(Nombre,(-Partidos)), 
                     y = Partidos, fill = Nombre)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  scale_fill_manual(values = colores) +
  theme_void() +
  theme(legend.position = "none") + 
  coord_polar(direction = -1) +
  geom_bar(aes(y = I(8)), stat = "identity", width = 1, fill = "white") +
  # Agregar dos aros blancos transparentes en la mitad de la gráfica 
  geom_bar(aes(y = I(20)), stat="identity", 
           width = 1, fill = "white", alpha = 0.2) +
  geom_bar(aes(y = I(40)), stat="identity", 
           width = 1, fill = "white", alpha = 0.1) +
  # Agregar el número de los partidos jugados
  geom_text(aes(label = Partidos, 
                y = Partidos * 0.8),
            data = Colombia, color = "white",
            fontface = "bold", vjust = 1, 
            family = "Helvetica", size = seq(4, 2, length.out=15)) +
  # Agregar nombre de los jugadores
  geom_text(aes(label = Nombre, y = Partidos * 1.3),
            # color = "blue",
            vjust = seq(1.3, 0.2, length.out = 15),
            # hjust = c(0.4, 0.2, 0.1, 0.15, 0.1),
            fontface = "bold", family = "Helvetica",
            size = c(7, seq(4, 2, length.out = 14))) +
  # Colocar títulos, subtítulos, etc. 
  labs(title = "Jugadores con más participaciones",
       caption = "Datos tomados de: https://es.wikipedia.org/wiki/Selección_de_fútbol_de_Colombia") +
  # Apariencia de títulos, subtítulos, etc. 
  theme(plot.title=element_text(size=26, hjust=0.5, 
                                face="bold", vjust=-100),
        plot.caption = element_text(size=8, hjust=0.5, 
                                    face="bold", color='grey', vjust=14))
