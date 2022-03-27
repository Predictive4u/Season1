library(rvest)
library(tidyverse)


# Bajar información de las 10 mil películas mejor calificados de género DRAMA de IMDb

n.resultados <- 10000

n.por.pagina <- 50

url1 <- "https://www.imdb.com/search/title/?title_type=movie&genres=drama&sort=user_rating,desc&start="
url2 <- "&explore=title_type,genres&ref_=adv_nxt"
secuencia <- seq(from=1, to = (n.resultados-n.por.pagina+1), by=50)
url.all <- str_c(url1, secuencia, url2)

prueba <- url.all[1]

# Extraer el título
extraer_title <- function(html){
  html %>% 
    read_html() %>% 
    html_nodes('.lister-item-header a') %>%
    html_text()
}

# Extraer el ranking
extraer_ranking <- function(html){
  ranking <- html %>% 
    read_html() %>%
    html_nodes('.text-primary') %>% 
    html_text()
  ranking  %>% 
    gsub("\\," , "", .)  %>% # Eliminar la coma, separador de mil
    gsub("\\.","",.) %>% # Eliminar el punto
    as.numeric() 
}

# Extraer la calificación
extraer_rate <- function(html){
  rate <- html %>% 
    read_html() %>%
    html_nodes('.ratings-imdb-rating strong') %>% 
    html_text()
  rate %>% as.numeric()
}

# Extraer el número de votos
extraer_vote <- function(html){
  vote <- html %>% 
    read_html()  %>% 
    html_nodes('.sort-num_votes-visible span:nth-child(2)')  %>% 
    html_text()
  vote %>% 
    gsub(",", "" , . ) %>% # Eliminar la coma
    as.numeric() 
}

# Extraer el año
extraer_year <- function(html){
  year <- html %>% 
    read_html()  %>% 
    html_nodes('.text-muted.unbold')  %>% 
    html_text()
  year  %>% 
    gsub("\\(", "" , .)  %>% 
    gsub("\\)", "" , .)  %>%  # eliminar paréntesis
    str_sub(-4, -1) %>% # extraer los últimos 4 dígitos
    as.numeric()  
}


# Combinar información de todas las variables
extraer_info <- function(html){
  ranking <- extraer_ranking(html)
  vote <- extraer_vote(html)
  rate <- extraer_rate(html)
  year <- extraer_year(html)
  title <- extraer_title(html)
  data.frame(ranking = ranking,
             nombre = title,
             calificacion = rate,
             votos = vote, 
             ano = year)
}

# Extraer información de todos las páginas
extraer_url <- function(url){
  url %>% map(extraer_info) %>% bind_rows()
}

x <- extraer_url(url.all)
