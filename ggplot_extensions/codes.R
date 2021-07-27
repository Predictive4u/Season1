## Crear animaciones con gganimate
library(dplyr)
library(gapminder)

## ---------------------------------------------
## Combinar diferentes gráfics
## ---------------------------------------------
library(patchwork)
gap.americas.2007 <- gapminder %>% filter(continent == "Americas", year == 2007)
r1 <- ggplot(gap.americas.2007, aes(y = lifeExp, x = gdpPercap)) + 
      geom_point() 
r2 <- ggplot(gap.americas.2007, aes(y = lifeExp)) + geom_boxplot()
r3 <- ggplot(gap.americas.2007) +
  geom_col(aes(x = country, y = gdpPercap)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8))
r1 + r2
r1/r2
(r1 + r2) / r3 

## ------------------------------------------------------------
## Mejorar la apariencia de textos en las gráfics
# PIB per cápita vs. esperanza vida para 2007 en Américas
## ------------------------------------------------------------
library(ggrepel)
gapC07 <- gapminder %>% 
  filter(year == 2007, str_detect(country, "^C"))
r1 <- ggplot(gapC07, aes(y = lifeExp, x = gdpPercap, label = country)) + 
  geom_point(aes(size = pop)) +
  ggtitle("Amercias and Oceania, 2017") +
  theme_classic()

r1 + geom_text()
r1 + geom_text_repel()
r1 + geom_label()
r1 + geom_label_repel()

# Diferentes colores
r1 + geom_text_repel(aes(col = continent))
r1 + geom_label_repel(aes(fill = continent), col = "blue")

# Resaltar algunos paises
r1 + geom_label_repel(data = gapC07 %>% 
                      filter(country %in% c("Chile", "Colombia", "Canada")))

## ---------------------------------------------
## Gráfica de burbujas de Hans Rosling
## ---------------------------------------------
library(gganimate)
p1 <- ggplot(gapminder %>% filter(country != "Kuwait"),
            aes(y = lifeExp, x = gdpPercap)) + 
  geom_point(aes(col = continent, size = pop), alpha = 1/2) + 
  scale_size(range = c(.1, 20), guide = "none") +  
  geom_text(aes(label = as.factor(year)), x = 40000, y =55, size = 12, col = "grey") +
  theme_classic() + 
  transition_time(year) 
animate(p1)
anim_save("ggplot_extensions/burbujas.gif")

# Gráfica de linea para PIB per cápita de China e India
p2 <- ggplot(gapminder %>% filter(country %in% c("China", "India")),
            aes(y = gdpPercap, x = year, col = country)) + 
  geom_line(size = 4) + 
  theme_classic() + 
  transition_reveal(year) 
animate(p2)
anim_save("ggplot_extensions/lineas.gif")

# Gráfica de burbujas para el 2007, continente a continente
p3 <- ggplot(gapminder %>% filter(year == 2007),
             aes(y = lifeExp, x = gdpPercap)) + 
      geom_point(aes(col = continent, size = pop)) +
      scale_size(range = c(.1, 20), guide = "none") +  
      theme_classic() + 
      transition_states(continent) + 
      enter_fade() + 
      exit_fade()
animate(p3)
anim_save("ggplot_extensions/puntos.gif")

p4 <- ggplot(gapminder %>% filter(country != "Kuwait"),
             aes(y = lifeExp, x = gdpPercap)) + 
  geom_point(aes(col = continent, size = pop), alpha = 1/2) + 
  scale_size(range = c(.1, 20), guide = "none") +  
  geom_text(aes(label = as.factor(year)), x = 40000, y = 55, size = 12, col = "grey") +
  geom_label_repel(data = gapminder %>% filter(country %in% c("Colombia", "Germany", "China")), 
                   aes(label = country, col = continent)) +
  theme_classic() + 
  transition_time(year) 
animate(p4)
anim_save("ggplot_extensions/burbujaslabel.gif")

# devtools::install_github("metacran/cranlogs")
library(cranlogs)
descarga.dia <- cran_downloads(packages = c("gganimate", "ggrepel", "patchwork"), from = "2021-07-23")
descarga.dia %>% 
  group_by(package) %>% 
  summarise(descarga = sum(count)) %>%
  arrange(desc(descarga))
