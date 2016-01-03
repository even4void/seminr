## pca.R
## Illustration ACP
##
## load.r : chargement des données

source("./load.r")

## Statistiques descriptives
HS$spatial <- apply(HS[,c("visual", "cubes", "paper")], 1, sum)

cor(HS[,c("spatial", "visual", "cubes", "paper")])

d <- HS[,c("spatial", "visual", "cubes", "paper")]
head(d)

library(reshape2)
d <- melt(HS[,c("id", "spatial", "visual", "cubes", "paper")])
head(d)

library(ggplot2)
p <- ggplot(data = d, aes(x = value))
p + geom_density(colour = "transparent", fill = "grey30") + facet_wrap(~ variable, nrow = 2)

## ACP
library(FactoMineR)
pca <- PCA(HS[,c("visual", "cubes", "paper")], scale.unit = TRUE, graph = FALSE)
1.722
pca$eig
pca$var$coord
head(pca$ind$coord)

pc1 <- pca$ind$coord[,"Dim.1"]

d <- data.frame(total=HS$spatial, pc=pc1)

p <- ggplot(data = d, aes(x = total, y = pc))
p + geom_point() + labs(x = "Score non pondéré", y = "Score pondéré")

library(ggbiplot)
ggbiplot(pca, groups = HS$sex, varname.size = 4, alpha = .5) + theme_bw()

p <- ggbiplot(pca, circle = TRUE, alpha = 0) + theme_void() 
p + geom_hline(yintercept = 0, colour = "grey90") + geom_vline(xintercept = 0, colour = "grey90")

## ACP avec le package psych
library(psych)

pcb <- principal(HS[,c("visual", "cubes", "paper")], nfactors = 3, rotate = "none")
pcb

biplot(pcb)


