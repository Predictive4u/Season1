#############################################################
#    1. Cargar paquetes y leer datos de CASEN 2020
#############################################################
library(haven)
library(dplyr)
library(lme4)

# Lectura de datos de CASEN 2020
casen2020 <- read_dta("~/Dropbox/Hanwen/Material enseñanza/Datos/Casen 2020/Casen en Pandemia 2020 STATA.dta")
# muestra en 324 de las 346 comunas en Chile, 22 comunas de difícil acceso
length(unique(casen2020$comuna)) 

#############################################################
#    2. Seleccionar variables y preparación de la base CASEN
#############################################################
# Seleccionar solo variables: folio, región, comuna, sexo, etnia, ingreso total
c1 <- casen2020 %>% 
  select(folio, region, comuna, sexo, etnia, ypchtotcor)

# Convertir todas las variables a factores y eliminar NA en la variable ingreso
vars <- c(2:5)
c1[,vars] <- lapply(c1[,vars] , as_factor)
c1 <- c1 %>%
  filter(!is.na(ypchtotcor))

#############################################################
#    3. Determinar cómo entra cada covariable al modelo
#############################################################
# Verificar si región entra al modelo como efecto fijo o aleatorio
c1 %>% 
  group_by(region) %>%
  count()
# región entra como efecto fijo

# Verificar si comuna entra al modelo como efecto fijo o aleatorio
c1 %>% 
  group_by(comuna) %>%
  count() %>%
  arrange(n)
# comuna entra como efecto aleatorio

# Verificar si sexo entra al modelo como efecto fijo o aleatorio
c1 %>% 
  group_by(sexo) %>%
  count()
# sexo entra como efecto fijo

# Verificar si etnia entra al modelo como efecto fijo o aleatorio
c1 %>% 
  group_by(etnia) %>%
  count() 
# etnia entra como efecto fijo

# Ajustar el modelo de efectos aleatorios
modelo <- lmer(ypchtotcor ~ region + (1 | comuna) + sexo + etnia, data = c1)
summary(modelo)

#################################################################
#    4. Definir los 3 tipos de comunas usados para el ejercicio
#################################################################

# comunas con buen tamaño de muestra smuestra
# Arica y Santiago
comunas1 <- c(15101, 13101) 
casen2020 %>% 
  filter(comuna %in% comunas1) %>% 
  group_by(comuna) %>%
  count() 
 

# comunas con poca muestra
# Sierra Gorda y San Pedro
comunas2 <- c(2103, 13505) 
casen2020 %>% 
  filter(comuna %in% comunas2)  %>% 
  group_by(comuna) %>%
  count() 


# comunas sin muestra por ser área de difícil acceso (no están incluidas en el Marco Muestral del INE)
# Chaitén y Futaleufú
comunas3 <- c(10401, 10402) 

#################################################################
#    5. Cargar datos del censo 2017 de Chile
#################################################################
load("/Users/hanwen/Dropbox/Hanwen/Material enseñanza/Datos/Censo2017/Censo_Personas.RData")
censo <- censo %>% 
  select(REGION, COMUNA, P08, P16) %>%
  rename(region = REGION,
         comuna = COMUNA, 
         sexo = P08,
         etnia = P16) %>%
  filter(etnia!=99) 

# Calcular el número de personas en la población en cada combinación de comuna, sexo y etnia
censo.Arica <- censo %>% filter(comuna == 15101)
conteo.Arica <- censo.Arica %>% 
  group_by(comuna, sexo, etnia) %>% 
  count() %>%
  mutate(sexo = case_when(sexo == 1 ~ "Hombre", 
                          sexo == 2 ~ "Mujer"),
         etnia = case_when(etnia == 1 ~ "Pertenece a pueblos indígenas", 
                           etnia == 2 ~ "No pertenece a ninguno pueblo indígena"))
conteo.Arica

Arica.X <- conteo.Arica[,-4]
Arica.X$region <- "Región de Arica y Parinacota"
Arica.X$comuna <- "Arica"
Arica.X

ingreso.pred <- predict(modelo, newdata = Arica.X, allow.new.levels = TRUE)
ingreso.pred
sum(ingreso.pred * conteo.Arica$n)/sum(conteo.Arica$n)
casen2020 %>% filter(comuna == 15101) %>%
  filter(!is.na(ypchtotcor)) %>%
  summarise(sum(ypchtotcor * expr)/sum(expr))

#### comuna con poco dato
# Calcular el número de personas en la población en cada combinación de comuna, sexo y etnia
censo.SG <- censo %>% filter(comuna == 2103)
conteo.SG <- censo.SG %>% 
  group_by(comuna, sexo, etnia) %>% 
  count() %>%
  mutate(sexo = case_when(sexo == 1 ~ "Hombre", 
                          sexo == 2 ~ "Mujer"),
         etnia = case_when(etnia == 1 ~ "Pertenece a pueblos indígenas", 
                           etnia == 2 ~ "No pertenece a ninguno pueblo indígena"))
conteo.SG

SG.X <- conteo.SG[,-4]
SG.X$region <- "Región de Antofagasta"
SG.X$comuna <- "Sierra Gorda"
SG.X

ingreso.pred <- predict(modelo, newdata = SG.X, allow.new.levels = TRUE)
ingreso.pred
sum(ingreso.pred * conteo.SG$n)/sum(conteo.SG$n)
casen2020 %>% filter(comuna == 2103) %>%
  filter(!is.na(ypchtotcor)) %>%
  summarise(sum(ypchtotcor * expr)/sum(expr))

#### comuna sin dato
# Calcular el número de personas en la población en cada combinación de comuna, sexo y etnia
censo.Chaiten <- censo %>% filter(comuna == 10401)
conteo.Chaiten <- censo.Chaiten %>% 
  group_by(comuna, sexo, etnia) %>% 
  count() %>%
  mutate(sexo = case_when(sexo == 1 ~ "Hombre", 
                          sexo == 2 ~ "Mujer"),
         etnia = case_when(etnia == 1 ~ "Pertenece a pueblos indígenas", 
                           etnia == 2 ~ "No pertenece a ninguno pueblo indígena"))
conteo.Chaiten

Chaiten.X <- conteo.Chaiten[,-4]
Chaiten.X$region <- "Región de Los Lagos"
Chaiten.X$comuna <- "Chaitén"
Chaiten.X

ingreso.pred <- predict(modelo, newdata = Chaiten.X, allow.new.levels = TRUE)
sum(ingreso.pred * conteo.Chaiten$n)/sum(conteo.Chaiten$n)


