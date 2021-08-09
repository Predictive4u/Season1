set.seed(20210808)
n.sim <- 1000000 # número de simulaciones
k <- 3 # número de puertas
escenarios <- seleccion <- matrix(0, n.sim, k)
for(i in 1:n.sim){
  j <- sample(1:k, 1) # el número de puerta con premio
  escenarios[i,j] <- 1
  m <- sample(1:k, 1) # el número de puerta elegida
  seleccion[i,m] <- 1
}
# probabilidad de ganar al quedar con la selección inicial
prob1 <- mean(rowSums(escenarios * seleccion)) 
# probabilidad de ganar al cambiar la selección
prob2 <- 1 - prob1
