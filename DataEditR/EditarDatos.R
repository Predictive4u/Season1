rm(list=ls())
direccion <- "https://github.com/Predictive4u/Season1/blob/main/Mazda_20211112.rda?raw=TRUE"
load(url(direccion))

# Opción 1. 
mazda2 <- edit(mazda)

# Opción 2.


library(editData)
mazda
result <- editData(mtcars)
