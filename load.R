## load.r
## Chargement des données Holzinger & Swineford (1939)
##

options(digits = 3)

## importation des données
data(HolzingerSwineford1939, package="lavaan")
HS <- HolzingerSwineford1939
names(HS)[7:15] <- c("visual", "cubes", "paper", 
                     "paragrap", "sentence", "wordm", 
                     "addition", "counting", "straight")

head(HS)

## indexation d'observations
HS[5,3]
HS[5,"ageyr"]

HS[10,7:9]
HS[10,c("visual", "cubes", "paper")]

## recodage des variables catégorielles
HS <- within(HS, {
  id <- factor(id)
  sex <- factor(sex, levels = c(1, 2), labels = c("M", "F"))
  grade <- factor(grade)
})


