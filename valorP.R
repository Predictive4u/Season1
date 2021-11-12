# 3. El valor p no indica qué tan compatible son los datos con la hipótesis nula, 
# sino qué tan compatible con el modelo estadístico
set.seed(20210827)
n.sim <- 10000
count <- 0
n <- 20
lambda <- 0.1
for(i in 1:n.sim){
  x <- rpois(n, lambda)
  count <- count + 
    ifelse(t.test(x, mu = lambda)$p.value < 0.05, 1, 0)
}
count/n.sim

# 4. Traslape de dos IC no implica que las medias sean iguales
set.seed(20210827)
x1 <- rnorm(50, 5, 1)
x2 <- rnorm(50, 4.2, 1)
t.test(x1, x2, var.equal = T)
t.test(x1)
t.test(x2)

