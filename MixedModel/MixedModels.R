rm(list = ls())
set.seed(20220202)
library(lme4)
library(dplyr)

# 1. Generando la población -----------------------------------------------
N <- 10000
x <- c(rep(1, N / 2), rep(0, N / 2))
e <- rnorm(N)

a0 <- 0.5
a1 <- 0.2
y <- a0 + a1 * x + e

datos <- data.frame(y = y,
                    x = as.factor(x))

b1 <- round(mean(y[x == 0]), 2)
b2 <- round(beta1hat <- mean(y[x == 1]), 2)
b1 # promedio en grupo 0
b2 # promedio en grupo 1

# ------------------------------------------
# Muestra aleatoria simple -----------------
# ------------------------------------------
n = 800
datos.s <- sample_n(datos, n)
n0 <- sum(datos.s$x == 0)
n1 <- n - n0

# Ajustar un modelo de regresión sin intercepto, 2 variables dummy
mef <- lm(y ~ 0 + x, data = datos.s)
round(bhat <- coef(mef), 2)

# Ajustar un modelo de interceptos aleatorios
mea <- lmer(y ~ 1 + (1 | x), data = datos.s, REML = T)
summary(mea)
beta.est <- fixef(mea)

sigma_u <- as.data.frame(summary(mea)$var)[1, 5]
sigma_e <- as.data.frame(summary(mea)$var)[2, 5]

gamma_0 = sigma_u^2/(sigma_u^2 + (sigma_e^2/n0))
gamma_1 = sigma_u^2/(sigma_u^2 + (sigma_e^2/n1))

# Estimación con modelo de efectos aleatorios
beta.est * (1 - gamma_0) + gamma_0 * (mean(datos.s$y[datos.s$x == 0]))
beta.est * (1 - gamma_1) + gamma_1 * (mean(datos.s$y[datos.s$x == 1]))


# ------------------------------------------
# Muestra no balanceada 
# ------------------------------------------
n0 = 380
datos.2 <- datos %>% 
  filter(x == 0) %>% 
  sample_n(n0)

n1 = 10
datos.1 <- datos %>% 
  filter(x == 1) %>% 
  sample_n(n1)

datos.s <- rbind(datos.1, datos.2)

# Ajustar un modelo de regresión sin intercepto, 2 variables dummy
mef <- lm(y ~ 0 + x, data = datos.s)
round(bhat <- coef(mef), 2)

# Ajustar un modelo de interceptos aleaotrios
mea <- lmer(y ~ 1 + (1 | x), data = datos.s, REML = T)
summary(mea)
beta.est <- fixef(mea)

sigma_u <- as.data.frame(summary(mea)$var)[1, 5]
sigma_e <- as.data.frame(summary(mea)$var)[2, 5]

gamma_0 = sigma_u^2/(sigma_u^2 + (sigma_e^2/n0))
gamma_1 = sigma_u^2/(sigma_u^2 + (sigma_e^2/n1))

# Estimación con modelo de efectos aleatorios
beta.est * (1 - gamma_0) + gamma_0 * (mean(datos.s$y[datos.s$x == 0]))
beta.est * (1 - gamma_1) + gamma_1 * (mean(datos.s$y[datos.s$x == 1]))