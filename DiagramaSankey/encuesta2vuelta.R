# https://elpais.com/america-colombia/elecciones-presidenciales/2022-06-05/petro-y-rodolfo-en-empate-tecnico-segun-las-primeras-encuestas.html
devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(ggplot2)
library(dplyr)

candidatos1 <- c("Gustavo Petro", "Rodolfo Hernández", "Federico Gutiérrez", "Sergio Fajardo")
candidatos2 <- c("Gustavo Petro", "Rodolfo Hernández", "Blanco", "No votará")

m <- matrix(c(0.96, 0.02, 0.01, 0.01,
              0.04, 0.93, 0.02, 0.01,
              0.07, 0.83, 0.07, 0.03,
              0.38, 0.49, 0.13, 0),
            byrow = TRUE,
            nrow = 4, ncol = 4)

rownames(m) <- candidatos1
colnames(m) <- candidatos2
m

n <- c(8527768,5953209, 5058010, 888585) # votos primera vuelta
cambios <- round(matrix(n,nrow = 4, ncol = 4) *m)

Primera <- c(rep("Gustavo Petro", n[1]), 
             rep("Rodolfo Hernández", n[2]),
             rep("Federico Gutiérrez", n[3]),
             rep("Sergio Fajardo", n[4])
             )
datos <- data.frame(Primera)
datos$Segunda <- NA

datos$Segunda[which(datos$Primera == "Gustavo Petro")] <- 
  c(rep("Gustavo Petro",cambios[1,1]),
    rep("Rodolfo Hernández", cambios[1,2]),
    rep("Blanco", cambios[1,3]),
    rep("No votará", cambios[1,4]))

datos$Segunda[which(datos$Primera == "Rodolfo Hernández")] <- 
  c(rep("Gustavo Petro",cambios[2,1]),
    rep("Rodolfo Hernández", cambios[2,2]),
    rep("Blanco", cambios[2,3]),
    rep("No votará", cambios[2,4]+1))

datos$Segunda[which(datos$Primera == "Federico Gutiérrez")] <- 
  c(rep("Gustavo Petro",cambios[3,1]),
    rep("Rodolfo Hernández", cambios[3,2]),
    rep("Blanco", cambios[3,3]),
    rep("No votará", cambios[3,4]))

datos$Segunda[which(datos$Primera == "Sergio Fajardo")] <- 
  c(rep("Gustavo Petro",cambios[4,1]),
    rep("Rodolfo Hernández", cambios[4,2]),
    rep("Blanco", cambios[4,3]),
    rep("No votará", cambios[4,4]))
View(datos)

# datos1 <- datos %>% 
#   make_long(Primera, Segunda) 

datos1 <- datos[sample(nrow(datos), 10000),] %>% 
  make_long(Primera, Segunda) 
View(datos1)


colores <- c("#D5E1D6", # Voto blanco
             "#71D9A3", # Fico
             "#FF6843", # Petro
             "#DCD7A8", # No votará
             "#2577D9", # Rodolfo
             "#BFBF69" # Fajardo
    )

p <- datos1%>% 
  ggplot( aes(x = x, 
              next_x = next_x, 
              node = node, 
              next_node = next_node,
              fill = node,
              label = node)) +
  geom_sankey(alpha = 0.6) + 
  geom_sankey_label(size = 3,  fill= "white") +
  scale_fill_manual(values = colores) +
  ggtitle("Elección presidencial de Colombia 2022", 
          subtitle="Flujo de votos entre primera y segunda vuelta ") +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, face = "bold"),
         legend.position = "none"
        )

p

