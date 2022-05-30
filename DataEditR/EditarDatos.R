rm(list=ls())
direccion <- "https://github.com/Predictive4u/Season1/blob/main/DataEditR/Jeep_RM_20220518.rda?raw=TRUE"
load(url(direccion))

# Instalar el paquete
install.packages("DataEditR")
library(DataEditR)

# con tibble no funciona muy bien
jeep <- as.data.frame(jeep)
# Definir la nueva base de datos
jeep.nuevo <- data_edit(x = jeep)


# OJO!!!!
# 1. No usar tibble, convertir a data.frame
# precio menor a 6 millones

# 2. No usar más de 2 filtros
jeep %>% filter(precio < 6000000,
                modelo2 == "Jeep Grand Cherokee", 
                año < 2005,
                año > 2000) 

jeep.nuevo2 <- data_edit(x = jeep.nuevo)

# 3. Cuando no hay ningún registro que cumple con las condiciones, no funciona!!!
jeep %>% filter(precio < 15000000,
                      modelo2 == "Jeep Compass", 
                      año > 2017) %>%
  dim()

