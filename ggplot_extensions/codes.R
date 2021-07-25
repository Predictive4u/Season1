library(dplyr)
library(gapminder)

## Crear animaciones con gganimate
## Gráfica de burbujas de Hans Rosling
library(gganimate)
p1 <- ggplot(gapminder %>% filter(country != "Kuwait"),
            aes(y=lifeExp, x=gdpPercap)) + 
  geom_point(aes(col=continent, size=pop), alpha = 1/2) + 
  scale_size(range = c(.1, 20), guide = "none") +  
  geom_text(aes(label = as.factor(year)), x = 40000, y =55, size = 12, col = "grey") +
  theme_classic() + 
  transition_time(year) 
animate(p1)
anim_save("ggplot_extensions/burbujas.gif")

p2 <- ggplot(gapminder %>% filter(country %in% c("China", "India")),
            aes(y = gdpPercap, x = year)) + 
  geom_line(aes(col = country), size =4) + 
  # scale_size(range = c(.1, 20), guide = "none") +  
  theme_classic() + 
  transition_reveal(year) 
animate(p2)
anim_save("ggplot_extensions/lineas.gif")

p3 <- ggplot(gapminder %>% filter(year == 2007),
             aes(y=lifeExp, x=gdpPercap)) + 
      geom_point(aes(col = continent, size = pop)) +
      scale_size(range = c(.1, 20), guide = "none") +  
      theme_classic() + 
      transition_states(continent) + 
      enter_fade() + 
      exit_fade()
animate(p3)
anim_save("ggplot_extensions/puntos.gif")

## Mejorar la apariencia de textos en las gráfics
library(ggrepel)
r1 <- ggplot(gapminder %>% 
               filter(year == 2007, continent == "Americas"),
             aes(y=lifeExp, x=gdpPercap)) + 
  geom_point(aes(size = pop)) +
  scale_size(range = c(.1, 20), guide = "none")  

r1 + geom_text(aes(label = country))
