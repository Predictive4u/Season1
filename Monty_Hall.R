set.seed(20210808)
# número de simulaciones
n.sim <- 1000000 
# número de puertas
k <- 3 
escenarios <- seleccion <- matrix(0, n.sim, k)
for(i in 1:n.sim){
  # el número de puerta con premio
  j <- sample(1:k, 1) 
  escenarios[i,j] <- 1
  # el número de puerta seleccionada
  m <- sample(1:k, 1) 
  seleccion[i,m] <- 1
}
# probabilidad de ganar 
# al quedar con la selección inicial
prob1 <- mean(rowSums(escenarios * seleccion)) 
# probabilidad de ganar 
# al cambiar la selección
prob2 <- 1 - prob1
