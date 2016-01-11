## anxiety.R
## Analyse CFA sur données catégorielles
##
## Packages : psych, ltm, polycor, lavaan

anx <- read.table("data/Anxiety.dat", header = TRUE)

anx$age <- factor(anx$age, levels = c(0,1), labels = c("<65", "65+"))
anx$gender <- factor(anx$gender, levels = c(0,1), labels = c("Male", "Female"))
anx$education <- factor(anx$education, levels = c(0,1), 
                        labels = c("College+", "HighSchool-"))

anx$education <- relevel(anx$education, ref = "HighSchool-")

d <- anx[, 4:32]

## statistiques descriptives
library(psych)
describe(d)

ltm::descript(d)

## ACP et analyse parallèle
pca <- principal(d, rotate = "none")
fa.parallel(d)

as.numeric(pca$loadings)

## score total
s <- apply(d, 1, sum)

ds <- data.frame(score = s, sex = anx$gender)

library(ggplot2)

p <- ggplot(data = ds, aes(x = score))
p + geom_histogram() + facet_grid(~ sex)

aggregate(score ~ sex, data = ds, summary)

wilcox.test(score ~ sex, data = ds)

## matrice de corrélation
library(polycor)

## corrélation de Pearson
C <- hetcor(d)

dc <- as.data.frame(lapply(d, as.ordered))

ltm::descript(dc)

## corrélation polysériale
C <- hetcor(dc)

round(C$correlations, 2)

pca2 <- principal(C$correlations, rotate = "none")
as.numeric(pca2$loadings)

cc <- data.frame(Pearson = as.numeric(pca$loadings), 
                 Polychoric = as.numeric(pca2$loadings))

p <- ggplot(data = cc, aes(x = Pearson, y = Polychoric))
p + geom_point() + geom_abline(intercept = 0, slope = 1)


## écriture du modèle
m <- paste('F =~', paste(names(d), collapse = " + "))

## modèle basé sur la matrice de corrélation, estimateur ML
r <- cfa(model = m, sample.cov = C$correlations, sample.nobs = nrow(anx), std.lv = TRUE)
r

parameterEstimates(r, ci = FALSE, standardized = TRUE)

coef(r)

## modèle basé sur le data frame (facteurs ordonnés), estimateur WLS
r <- cfa(model = m, data = dc, std.lv = TRUE)
r

library(semPlot)

semPaths(r, whatLabels = "std", thresholds = FALSE, intercepts = FALSE)


rm <- cfa(model = m, subset(dc, anx$gender == "Male"), std.lv = TRUE)
rm

rf <- cfa(model = m, subset(dc, anx$gender == "Female"), std.lv = TRUE)
rf

params <- data.frame(id = paste("R", 1:29, sep = ""), 
                     male = coef(rm)[1:29], 
                     female = coef(rf)[1:29])
library(reshape2)
params <- melt(params)

p <- ggplot(data = params, aes(x = id, y = value, colour = variable))
p + geom_point()

## invariance de mesure
library(semTools)

## données catégorielles
dc$sex <- anx$gender
measurementInvarianceCat(model = m, data = dc, group = "sex", estimator = "wlsmv")

## données continues (strict = + test sur résidus)
d$sex <- anx$gender
measurementInvariance(model = m, data = d, group = "sex", strict = TRUE)

## voir exemples HS dans help(partialInvariance)

################################################################################
###############################  IRT & IFA #####################################
################################################################################

## analyse sur variables binaires
db <- ifelse(d < 3, 0, 1)

## package psych
ifa <- irt.fa(db)

rb <- fa.poly(db, scores = TRUE)

## package ltm (Rasch model)
library(ltm)
onepl <- rasch(db, constraint = cbind(ncol(db) + 1, 1))

plot(onepl)

fs <- factor.scores(onepl)
plot.fscores(fs, include.items = TRUE)
