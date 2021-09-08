head(iris)
data.mat <- as.matrix(
  iris[, c("Petal.Width", "Petal.Length")])
head(data.mat)
str(data.mat)
library(ggplot2)
ggplot()+
  geom_point(aes(
    x=Petal.Length,
    color=Species,
    y=Petal.Width),
    data=iris)

library(mclust)
n.clusters <- 3
mclust.result <- mclust::Mclust(
  data.mat, n.clusters, "VVI", verbose=FALSE)
mclust.result$loglik
mclust.result$classification
