library(dplyr)
library(gapminder)

## Crear animaciones con gganimate
## Gráfica de burbujas de Hans Rosling
library(gganimate)
p <- ggplot(gapminder %>% filter(country != "Kuwait"),
            aes(y=lifeExp, x=gdpPercap)) + 
  geom_point(aes(col=continent, size=pop), alpha = 1/2) + 
  scale_size(range = c(.1, 20), guide = "none") +
  theme_classic()
p <- p +  geom_text(aes(label = as.factor(year)), x = 40000, y =55, size = 12, col = "grey")
p <- p + transition_time(year) 
animate(p)
anim_save("ggplot_extensions/burbujas.gif")

## Mejorar la apariencia de textos en las gráfics
library(ggrepel)
