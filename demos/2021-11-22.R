head(iris)
iris.mat <- as.matrix(iris[,1:4])
dimRed::dimRedMethodList()
method.name <- "MDS"
embed_result <- dimRed::embed(iris.mat, method.name)
low.dim.mat <- embed_result@data@data
head(low.dim.mat)

library(ggplot2)
library(data.table)
low.dim.dt <- data.table(
  low.dim.mat,
  Species=iris$Species)
ggplot()+
  geom_point(aes(
    MDS1, MDS2, color=Species),
    data=low.dim.dt)
## For homework please use zip data and three methods in a facet plot.

dimRed::dimRedQualityList()
dimRed::quality(embed_result, "Q_global")

## list vignettes in package:
vignette(package="dimRed")

## open one of those vignettes:
vignette("dimensionality-reduction", package="dimRed")
