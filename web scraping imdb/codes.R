rm(list=ls())
library(rvest)
library(tidyverse)

# Bajar información de las 10 mil películas mejor calificados de género DRAMA de IMDb

n.resultados <- 10000
n.por.pagina <- 50

url <- "https://www.imdb.com/search/title/?title_type=movie&genres=drama&sort=user_rating,desc&explore=title_type,genres"

url1 <- "https://www.imdb.com/search/title/?title_type=movie&genres=drama&sort=user_rating,desc&start="
url2 <- "&explore=title_type,genres&ref_=adv_nxt"

url.all <- str_c(url1, seq(from=1, to = (n.resultados-n.por.pagina+1), by=50), url2)

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

# Extraer el género  
extraer_genero <- function(html){
  genero <- html %>%
    read_html() %>%
    html_nodes('.genre') %>%
    html_text()
  genero2 <- genero %>% gsub("\\\n", "", .) %>% # eliminar \n 
    gsub(" ","" , .) # eliminar los espacios
  
  p <- length(genero2)
  res <- matrix(NA, p, 3) # 3 es el número máximo de géneros para cada película
  for(i in 1:p){
    aux <- strsplit(genero2, split = ",")[[i]]
    res[i, 1:length(aux)] <- aux
  }
  res
}

# Extraer el ranking y el director (es que en el director hay missing)
extraer_director <- function(html){
  info <- html %>%
    read_html() %>%
    html_nodes('.text-primary , .text-muted+ p a:nth-child(1)') %>%
    html_text()

  # si no hay missing en el director, entonces se alterna número y nombre
  # si hay números consecutivos, entonces sí hay missing en el directors
  ind <- c()
  for(j in 1:length(info)){
    letras <- c(letters, LETTERS)
    ind[j] <- prod(str_detect(info[j], letras, negate = TRUE))
    # valor 1 indica que solo contiene números, valor 0 indica que contiene letras, es nombre del director
  }
  m <- which(diff(ind)==0)
  
  if(length(m) == 0){ # si no hay missing en director
    director <- html %>%
      read_html() %>%
      html_nodes('.text-muted+ p a:nth-child(1)') %>%
      html_text()
  }
  
  if(length(m) > 0){ # si hay missing en director
    for(jj in 1:length(m)){
      info <- c(info[1:(m[jj] + jj - 1)],NA, info[-(1:(m[jj]+ jj -1))])  
    }
    director <- info[seq(2, length(info), 2)]
  }

  director
}


# Combinar información de todas las variables
extraer_info <- function(html){
  ranking <- extraer_ranking(html)
  vote <- extraer_vote(html)
  rate <- extraer_rate(html)
  year <- extraer_year(html)
  title <- extraer_title(html)
  genero <- extraer_genero(html)
  director <- extraer_director(html)
  data.frame(ranking = ranking,
             nombre = title,
             calificacion = rate,
             votos = vote, 
             ano = year,
             director = director,
             genero = genero)
}

# Extraer información de todos las páginas
extraer_url <- function(url.all){
  url.all %>% map(extraer_info) %>% bind_rows()
}


x <- extraer_url(url.all)

