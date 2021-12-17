rm(list=ls())
library(biscale)
library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(sf)
# ---------------------------------------------------------------------
# Mapa 1 - Resultado primera vuelta elección presidencial Chile C
#          Comunas de la R.M.
# ----------------------------------------------------------------------
library(chilemapas)

# Por comunas de la R.M.
shape.RM <- mapa_comunas %>% filter(codigo_region == "13") %>% 
  select(-codigo_provincia, -codigo_region)
shape.RM$codigo_comuna <- as.numeric(shape.RM$codigo_comuna)
eleccion.RM <- read_excel("~/Dropbox/1. youtube/Season1/Mapa_Bivariada/Presi_2021_Chile.xlsx", 
                          sheet = "comunas_RM")
eleccion.RM <- eleccion.RM %>% rename(codigo_comuna = id)
shape.RM <- shape.RM %>% left_join(eleccion.RM, by = "codigo_comuna")

k <- 3
datos.RM.bi <- bi_class(shape.RM, y = Kast, x = Boric, dim = k)


# Colores usados para el mapa
color <- "GrPink"

map.RM <- ggplot() + 
  geom_sf(data=datos.RM.bi, 
          aes(fill = bi_class, geometry = geometry), 
          colour ="white", 
          size = 0.1) +
  bi_scale_fill(pal = color, dim = k) +
  bi_theme()+
  theme(legend.position = "none")

# Crear la leyenda para el mapa
legend1 <- bi_legend(pal = color,
                    dim = k, 
                    xlab = "% Boric",
                    ylab = "% Kast",
                    size = 8)

mapa1 <- ggdraw() +
  draw_plot(map.RM, 0, 0, 1, scale=0.7) +
  draw_plot(legend1, 0.11, 0.55, 0.2, 0.2, scale=1.5)+
  draw_text("Kast vs. Boric. Resultados primera vuelta Chile 2021", 
            vjust = -13, size = 18)

mapa1

# ---------------------------------------------------------------------
# Mapa 2 - % población de raza blanca vs. % votos a favor de Biden
#          Estados de los Estados Unidos
# ----------------------------------------------------------------------
library(maps)
shape.usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
eleccion.EEUU <- read_excel("~/Dropbox/1. youtube/Season1/Mapa_Bivariada/Presi_2020_EEUU.xlsx")
shape.usa <- shape.usa %>% left_join(eleccion.EEUU, by = "ID")

k <- 2
datos.EEUU.bi <- bi_class(shape.usa, y = `Electoral Votes` , x = `% white`, dim = k)

# Colores usados para el mapa
color <- "DkCyan"

map.EEUU <- ggplot(data=datos.EEUU.bi) + 
  geom_sf(aes(fill = bi_class, geometry = geom), 
          colour ="white", 
          size = 0.1) +
  bi_scale_fill(pal = color, dim = k) +
  bi_theme()+
  theme(legend.position = "none")

# Crear la leyenda para el mapa
legend2 <- bi_legend(pal = color,
                     dim = k, 
                     xlab = "% blancos",
                     ylab = "Voto electoral",
                     size = 8)

mapa2 <- ggdraw() +
  draw_plot(map.EEUU, 0, 0, 1) +
  draw_plot(legend2, 0.8, 0.15, 0.2, 0.2, scale=1.5) +
  draw_text("Los estados de EE.UU. según el número de votos electorales y el porcentaje de blancos", 
            vjust = -13, size = 18)

mapa2

# ---------------------------------------------------------------------
# Mapa 3 - Pobreza monetaria vs. Desempleo
#          Departamentos de Colombia
# ----------------------------------------------------------------------
# devtools::install_github("nebulae-co/colmaps")
library(colmaps)

datos.deptos.col <- read_excel("~/Dropbox/1. youtube/Season1/Mapa_Bivariada/Pobreza_Desempleo_Colombia.xlsx")
shape.deptos <- read_sf("~/Dropbox/1. youtube/Season1/Mapa_Bivariada/depto.shp")
datos.deptos.col <- shape.deptos %>% left_join(datos.deptos.col, by = "DPTO")

k <- 3
datos.depto.bi <- bi_class(datos.deptos.col, y = Pobreza, x = Desempleo, dim = k)

# Colores usados para el mapa
color <- "DkBlue"

map.deptos <- ggplot() + 
  geom_sf(data=datos.depto.bi, 
          aes(fill = bi_class), 
          colour ="white", 
          size = 0.1) +
  bi_scale_fill(pal = color, dim = k) +
  bi_theme()+
  theme(legend.position = "none")

# Crear la leyenda para el mapa
legend3 <- bi_legend(pal = color,
                     dim = k, 
                     xlab = "Desempleo",
                     ylab = "Pobreza",
                     size = 8)

mapa3 <- ggdraw() +
  draw_plot(map.deptos, 0, 0, 1, 1) +
  draw_plot(legend3, 0.7, 0.6, 0.2, 0.2, scale = 1.2) + 
  draw_text("Pobreza monetaria y desempleo en Colombia",
            vjust = -17, size = 15)

mapa3

ggplot(datos.deptos.col, aes(y = Pobreza, x = Desempleo)) + 
  geom_point() +
  ggrepel::geom_text_repel(aes(label = Departamento)) + 
  theme_void() + 
  geom_segment(aes(x = max(Desempleo,na.rm = T), 
                   y = mean(Pobreza,na.rm = T), 
                   xend = min(Desempleo, na.rm = T), 
                   yend = mean(Pobreza,na.rm = T)),
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(xend = max(Desempleo,na.rm = T), 
                   yend = mean(Pobreza,na.rm = T), 
                   x = min(Desempleo, na.rm = T), 
                   y = mean(Pobreza,na.rm = T)),
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(xend = mean(Desempleo,na.rm = T)+14, 
                   yend = max(Pobreza,na.rm = T), 
                   x = mean(Desempleo, na.rm = T)+14, 
                   y = min(Pobreza,na.rm = T)),
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_segment(aes(x = mean(Desempleo,na.rm = T)+14, 
                   y = max(Pobreza,na.rm = T), 
                   xend = mean(Desempleo, na.rm = T)+14, 
                   yend = min(Pobreza,na.rm = T)),
               arrow = arrow(length = unit(0.5, "cm"))) +
geom_text(aes(y = max(Pobreza,na.rm = T)+2, 
              x = mean(Desempleo, na.rm = T)+14), 
          col = 2,
          label = "Más pobreza",
          size = 10) + 
  geom_text(aes(y = min(Pobreza,na.rm = T)-1, 
                x = mean(Desempleo, na.rm = T)+14), 
            col = 3,
            label = "Menos pobreza",
            size = 10) + 
  geom_text(aes(y = mean(Pobreza,na.rm = T), 
                x = min(Desempleo, na.rm = T) - 7.5), 
            col = 3,
            label = "Menos desempleo",
            angle = 90,
            size = 10) + 
  geom_text(aes(y = mean(Pobreza,na.rm = T), 
                x = max(Desempleo, na.rm = T) + 5), 
            label = "Más desempleo",
            col = 2,
            angle = 270,
            size = 10)

# ---------------------------------------------------------------------
# Mapa 4 - Pobreza monetaria vs. error muestral
#          Regiones de Chile
# ----------------------------------------------------------------------

# Por regiones
library(rgeos)
shape.regiones.chile <- generar_regiones(mapa_comunas)
shape.regiones.chile$codigo_region <- as.numeric(shape.regiones.chile$codigo_region)
pobreza.regiones <- read_excel("~/Dropbox/1. youtube/Season1/Mapa_Bivariada/Pobreza_2020_Chile.xlsx", 
                       sheet = "regiones")
shape.regiones.chile <- shape.regiones.chile %>% left_join(pobreza.regiones, by = "codigo_region")
k <- 2
datos.regiones.bi <- bi_class(shape.regiones.chile, y = pobreza, x = error, dim = k)

# Colores usados para el mapa
color <- "DkViolet"

map.region <- ggplot() + 
  geom_sf(data=datos.regiones.bi, 
          aes(fill = bi_class), 
          colour ="white", 
          size = 0.1) +
  bi_scale_fill(pal = color, dim = k) +
         bi_theme()+
  theme(legend.position = "none")

# Crear la leyenda para el mapa
legend4 <- bi_legend(pal = color,
                     dim = k, 
                     xlab = "error estándar",
                     ylab = "pobreza",
                     size = 8)

mapa4 <- ggdraw() +
  draw_plot(map.region, 0, 0, 1) +
  draw_plot(legend4, x = 0.5, y = 0.5, width = 0.2, height = 0.2) + 
  draw_text("Pobreza monetaria en las regiones de Chile",
             vjust = -23, hjust= -0.1, size = 12)

mapa4
