## mediation.R
## Inspired from QuantPsyc package
##

d <- read.csv2("data/beliefs.csv")

names(d)

dim(d)

d <- d[,c("beliefs", "attitudes", "intentions")]

names(d) <- c("x", "m", "y")

## modèle 1 : m ~ x
m1 <- lm(m ~ x, d)

## modèle 2 : y ~ x + m
m2 <- lm(y ~ x + m, d)

## décomposition des effets
## y = tp * x + b * m
## m = a *x

a <- coef(m1)[2]    ## effet de X sur M
b <- coef(m2)[3]    ## effet de M sur Y en controllant X
tp <- coef(m2)[2]   ## effet de X sur Y en controllant M

ind.effect <- a * b ## effet indirect
dir.effect <- tp    ## effet direct

## effet total de X sur Y
ind.effect + dir.effect


library(mediation)

## m1 <- 
## m2 <- 

mm <- mediate(m1, m2, treat = "x", mediator = "m")

summary(mm)

plot(mm)