## cfa.R
## Illustration CFA
##
## load.r : chargement des données

source("./load.r")

library(psych)

d <- HS[,7:15]
names(d)

describe(d)

library(lavaan)
library(semPlot)

## modèle avec facteurs corrélés
m <- 'Visual =~ visual + cubes + paper 
      Verbal =~ paragrap + sentence + wordm
      Speed  =~ addition + counting + straight'

r <- cfa(m, data = d)
r

semPaths(r, whatLabels = "est")

summary(r, fit.measures = TRUE)

summary(r, fit.measures = TRUE, ci = TRUE)
coef(r)
resid(r, type = "normalized")

summary(r, fit.measures = TRUE, standardized = TRUE)
standardizedSolution(r)

summary(r, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
modindices(r)

r <- cfa(m, data = d, std.lv = TRUE)
semPaths(r, whatLabels = "std")

## modèle avec facteurs non corrélés
r2 <- cfa(m, data = d, orthogonal = TRUE)
r2

## modèle 2nd ordre
m3 <- 'Visual =~ visual + cubes + paper 
       Verbal =~ paragrap + sentence + wordm
       Speed  =~ addition + counting + straight
       F =~ Visual + Verbal + Speed'

r3 <- cfa(m3, data = d)
r3
semPaths(r3, whatLabels = "std")

## reliability du facteur de 2nd ordre
library(semTools)
reliabilityL2(r3, "F")

## modèle bifactoriel

## approche exploratoire et confirmatoire à l'aide du package psych
omega(d)
omegaSem(d)

## analyse préliminaire avec lavaan
m4 <- 'Visual =~ visual + cubes + paper 
       Verbal =~ paragrap + sentence + wordm
       Speed  =~ addition + counting + straight
       G =~ visual + cubes + paper + paragrap + sentence + wordm + addition + counting + straight
       Visual ~~ 0*Verbal
       Visual ~~ 0*Speed
       Verbal ~~ 0*Speed
       G ~~ 0*Visual
       G ~~ 0*Verbal
       G ~~ 0*Speed'

r4 <- cfa(m4, data = d, std.lv = TRUE, information="observed")
r4
semPaths(r4, whatLabels = "std")
