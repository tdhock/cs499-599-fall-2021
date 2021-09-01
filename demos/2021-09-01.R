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

kmeans.result <- stats::kmeans(data.mat, 2)
options(width=60)
kmeans.result
names(kmeans.result)
str(kmeans.result)
kmeans.result$tot.withinss
kmeans.result[["tot.withinss"]]
name.of.element <- "tot.withinss"
kmeans.result[[name.of.element]]
kmeans.result$name.of.element

kmeans.result.dt <- data.table(
  data.mat,
  cluster=factor(kmeans.result[["cluster"]]))
ggplot()+
  geom_point(aes(
    x=Petal.Length,
    color=cluster,
    y=Petal.Width),
    data=kmeans.result.dt)
