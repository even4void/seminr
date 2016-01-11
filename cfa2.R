## cfa2.R
## Rappels CFA
##
## load.r : chargement des données

source("./load.r")

## Holzinger & Swineford's data
d <- HS[,7:15]
names(d)

psych::describe(d)

## matrice de corrélation
cor(d)

## matrice de covariance, voir aussi cov2cor()
cov(d)

library(lavaan)

## modèle 1 facteur
m0 <- 'F =~ a*visual + b*cubes + c*paper + d*paragrap + e*sentence + f*wordm + g*addition + h*counting + i*straight'

r0 <- cfa(m0, data = d, std.lv = TRUE)
r0

coef(r0)

fitted(r0)

residuals(r0, type = "raw")$cov

## modèle 3 facteurs corrélés
cov(d)

m1 <- 'Visual =~ visual + cubes + paper 
       Verbal =~ paragrap + sentence + wordm
       Speed  =~ addition + counting + straight'

## saisie à partir des données individuelles
r1 <- cfa(m1, data = d)
r1

fitted(r1)

## saisie à partir d'une matrice de covariance
## cf. getCov() pour construire automatiquement une matrice de covariance
## à partir de données empiriques.
C <- cov(d)
N <- nrow(d)

r <- cfa(m1, std.lv = TRUE, sample.cov = cov(d), sample.nobs = N)



## utilisation de la covariance empirique
inspect(r1, "sampstat")

r1b <- cfa(m1, data = d, likelihood = "wishart")

fitted(r1b)

## indices de modification
modificationIndices(r1)

m1 <- 'Visual =~ visual + cubes + paper 
       Verbal =~ paragrap + sentence + wordm
       Speed  =~ addition + counting + straight'

m2 <- 'Visual =~ visual + cubes + paper 
       Verbal =~ paragrap + sentence + wordm
       Speed  =~ addition + counting + straight
       addition ~~ counting'

r1 <- cfa(m1, data = d)
r2 <- cfa(m2, data = d)

anova(r1, r2)

## contraintes

r3 <- cfa(m1, data = HS, group = "school")
r3

r4 <- cfa(m1, data = HS, group = "school", group.equal = "loadings")
summary(r4)

## sem

m3 <- 'Visual =~ visual + cubes + paper 
       Verbal =~ paragrap + sentence + wordm
       Speed  =~ addition + counting + straight
       sex ~ Visual'

r5 <- cfa(m3, data = HS)

semPaths(r5)
