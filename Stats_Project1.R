library(datasets)
library(ggplot2)
library(MASS)
attach(iris)
str(iris)

p = ggplot(iris, aes(iris$Sepal.Length, iris$Sepal.Width, size = iris$Species,colour=iris$Species))
p + geom_point()

petal.dens = kde2d(iris$Petal.Length, iris$Petal.Width)
contour(petal.dens)
image(petal.dens)