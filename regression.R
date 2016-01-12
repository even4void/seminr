## regression.R
## Illustration pour le modèle linéaire
##

source("./mobilite.R")

library(MASS)

d <- mvrnorm(6053, mu = rep(0, 8), Sigma = mobility.cov, empirical = TRUE)

cov(d)

d <- as.data.frame(d)


m <- sem(mobility.model, data = d)

b <- coef(m)

pp <- predict(m)

pp <- as.data.frame(pp)

pp$PersMobility <- d$PersMobility

reg <- lm(PersMobility ~ PsychSocLV + PsyHealthLV, data = pp)
summary(reg)

b[6:7]
