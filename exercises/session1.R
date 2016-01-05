## Analyse des données d'EFA/CFA

library(lavaan)
data("HolzingerSwineford1939")

HS <- HolzingerSwineford1939

head(HS)

names(HS)[7:15] <- c("visual", "cubes", "paper", 
                     "paragrap", "sentence", "wordm", 
                     "addition", "counting", "straight")

HS$id <- factor(HS$id)
HS$grade <- factor(HS$grade)
HS$sex <- factor(HS$sex, levels = c(1,2), labels = c("M","F"))

summary(HS)


## Calcul de la moyenne du score visual par sexe et par tranche d'âge
## aggregate(y ~ x, data = , function)

aggregate(visual ~ sex, data = HS, mean)
aggregate(visual ~ ageyr, data = HS, mean)

aggregate(visual ~ sex + ageyr, data = HS, mean)

aggregate(visual ~ ageyr, data = subset(HS, sex == "M"), mean)

## Test de Student
t.test(visual ~ sex, data = HS)

## Test du chi-deux
xtabs(~ sex + ageyr, data = HS)

summary(xtabs(~ sex + ageyr, data = HS))

chisq.test(xtabs(~ sex + ageyr, data = HS))

xtabs(~ sex + grade, data = HS)

prop.table(xtabs(~ sex + grade, data = HS), 1)
prop.table(xtabs(~ sex + grade, data = HS), 2)

summary(xtabs(~ sex + grade, data = HS))

xtabs(~ ageyr, data = HS)

## ANOVA
HS$agec <- cut(HS$ageyr, breaks = c(11,12,13,16), 
               include.lowest = TRUE, 
               labels = c("11-12", "13", "14-16"))

summary(HS$agec)

aggregate(visual ~ agec, data = HS, mean)
aggregate(visual ~ agec, data = HS, sd)

summary(aov(visual ~ agec, data = HS))


cor.test(~ visual + ageyr, data = HS)








