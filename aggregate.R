## aggregate.r
## Statistiques descriptives
##
## load.r : chargement des données

source("./load.r")

## sélection de sous-ensemble d'observations
subset(HS, subset = sex == "M")
subset(HS, select = c(ageyr, agemo))

## calcul de moyennes/ety conditionnels
aggregate(visual ~ sex, data = HS, mean)
aggregate(visual ~ sex, data = HS, sd)

aggregate(visual ~ sex + grade, HS, mean)
aggregate(cbind(visual,cubes,paper) ~ sex, HS, mean)

## passage du format large au format long
library(reshape2)

dw <- HS[,c("id","sex","visual","cubes","paper")]

dl <- melt(dw)                                     ## long
dw <- dcast(dl, sex + id ~ variable)               ## wide

## opérations par lignes/colonnes
apply(HS[,c("visual","cubes","paper")], 1, sum)
apply(HS[,c("visual","cubes","paper")], 2, mean)

