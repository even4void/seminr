## visualize.R

rm(list=ls())


## chargement des données
source("./load.r")

library(ggplot2)

## composition graphique
p <- ggplot(data = HS, aes(x = ageyr, y = visual))
p + geom_point()

p + geom_point(position = position_jitter(width = .1))
p + geom_point(position = position_jitter(width = .1), alpha = .5)

p <- p + geom_point(position = position_jitter(width = .1), alpha = .5)
p + scale_y_continuous(limits = c(0,10)) + 
  labs(x = "Age (années)", y = "Score visuel")

p <- p + scale_y_continuous(limits = c(0,10)) + 
  labs(x = "Age (années)", y = "Score visuel")
p + theme_bw(base_size = 12, base_family = "Times")

## histogrammes et courbes de densité
p <- ggplot(data = HS, aes(x = visual))
p + geom_histogram(binwidth = .5, fill = "grey30", colour = "white")
p + geom_histogram(aes(y = ..density..), binwidth = .5, fill = "grey30", colour = "white")

p + geom_line(stat = "density", size = .7)
p + geom_line(stat = "density", size = .7) + expand_limits(x = c(0,10))
p + geom_line(stat = "density", adjust = 2, size = .7)
p + geom_density(fill = "grey30", adjust = 1, colour = "transparent")

p <- ggplot(data = HS, aes(x = visual, colour = sex))
p + geom_line(stat = "density", size = .7) + expand_limits(x = c(0,10))

p <- p + geom_line(stat = "density", size = .7) + expand_limits(x = c(0,10))
p + scale_colour_manual(values = c("cornflowerblue", "orange"), name = "Sexe")

## diagrammes en barres et en points
p <- ggplot(data = HS, aes(x = factor(ageyr)))
p + geom_bar(width = .7, fill = "grey30") + labs(x = "Age")

p <- ggplot(data = HS, aes(x = factor(ageyr)))
p + geom_bar(width = .7, aes(fill = sex), position = position_dodge()) + labs(x = "Age")

d <- aggregate(visual ~ sex + grade, data = HS, mean)
p <- ggplot(data = d, aes(x = sex, y = visual, colour = grade))
p + geom_point(size = 3) + ylim(c(4.5,5.5)) + coord_flip()

## facettes
p <- ggplot(data = HS, aes(x = cubes, y = visual, colour = sex))
p + geom_point()
p + geom_point() + facet_grid(~ ageyr)

p <- ggplot(data = HS, aes(x = cubes, y = visual))
p + geom_point() + facet_grid(sex ~ ageyr)

p <- p + geom_point() + facet_grid(sex ~ ageyr)
p + geom_smooth(method = "lm", se = FALSE, colour = "orange")

p <- ggplot(data = subset(HS, complete.cases(grade)), 
            aes(x = cubes, y = visual, colour = school))
p <- p + geom_point(alpha = .5) + geom_smooth(span = 1.5, size = .7, se = FALSE) 
p + scale_x_continuous(limits = c(2,10)) + facet_wrap(sex ~ grade)

## extensions
library(GGally)
ggpairs(HS[,c("visual","cubes","paper")])

library(factoextra)
pca <- prcomp(HS[,7:12])
v <- get_pca_var(pca)
fviz_pca(pca, label = "var", pointsize = 1) + theme_minimal()

library(ggfortify)
pca <- prcomp(HS[,7:12])
autoplot(pca, data = HS, colour = "sex")
autoplot(cluster::pam(HS[,7:12], k = 3), frame = TRUE, frame.type = "norm")
