library(data.table)
library(ggplot2)
head(iris)
iris.mat <- as.matrix(iris[,1:4])
str(iris.mat)

embed_result <- dimRed::embed(iris.mat, "LLE")
str(embed_result)
low.dim.mat <- embed_result@data@data
str(low.dim.mat)

low.dim.dt <- data.table(
  low.dim.mat,
  Species=iris$Species)
ggplot()+
  geom_point(aes(
    LLE1, LLE2, color=Species),
    data=low.dim.dt)

pca_result <- dimRed::embed(iris.mat, "PCA", ndim=3)
dim(pca_result@data@data)

embed_result <- dimRed::embed(iris.mat, "LLE", ndim=3)
dim(embed_result@data@data)

dimRed::dimRedMethodList() # for embed()
dimRed::dimRedQualityList()# for quality()

dimRed::quality(embed_result, "reconstruction_rmse")
dimRed::quality(pca_result, "reconstruction_rmse")

dimRed::quality(embed_result, "Q_global")
dimRed::quality(pca_result, "Q_global")
str(pca_result)

vignette(package="dimRed")
vignette("dimensionality-reduction",package="dimRed")
