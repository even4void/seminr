## efa.R
## Illustration EFA
##
## load.r : chargement des données

source("./load.r")


## EFA avec psych
efa <- fa(HS[,c("visual", "cubes", "paper")], nfactors = 1)
efa$communality
efa$uniquenesses

structure.diagram(efa, errors = TRUE)

## analyse d'items
d <- HS[,7:15]
names(d)

describe(d)

fa.parallel(d, fm = "pa", fa = "fa", main = "")

## modèle avec 6 variables
m1 <- fa(d, nfactors = 1, fm ="pa")
m2 <- fa(d, nfactors = 2, fm ="pa")
m3 <- fa(d, nfactors = 3, fm ="pa")

print(m3, sort = TRUE)

m1$loadings
m2$loadings
m3$loadings
