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

kmeans.result <- stats::kmeans(data.mat, 4)
options(width=60)
kmeans.result
names(kmeans.result)
str(kmeans.result)
kmeans.result$tot.withinss
kmeans.result[["tot.withinss"]]
name.of.element <- "tot.withinss"
kmeans.result[[name.of.element]]
kmeans.result$name.of.element
library(data.table)
kmeans.result.dt <- data.table(
  data.mat,
  cluster=factor(kmeans.result[["cluster"]]))
ggplot()+
  geom_point(aes(
    x=Petal.Length,
    color=cluster,
    y=Petal.Width),
    data=kmeans.result.dt)

iris.n.clusters.list <- list()
for(n.clusters in 1:10){
  kmeans.result <- stats::kmeans(data.mat, n.clusters)
  iris[,"Species"]
  iris[["Species"]]
  iris$Species
  pdfCluster::adj.rand.index(
    iris[,"Species"], kmeans.result$cluster)
  iris.n.clusters.list[[paste(n.clusters)]] <- data.table(
    n.clusters,
    error=kmeans.result[["tot.withinss"]])
}
iris.n.clusters.list[[1]]
iris.n.clusters.list[[2]]
rbind(
  iris.n.clusters.list[[1]],
  iris.n.clusters.list[[2]])
## Python equivalent of do.call: star operator.
## L = [ ... ]
## f(*L) by index/order
## D = { ... }
## f(**D) by name f(arg=value)
iris.n.clusters <- do.call(rbind, iris.n.clusters.list)

gg <- ggplot()+
  geom_point(aes(
    x = n.clusters,
    y = error),
    data = iris.n.clusters)
png(
  "2021-09-03-error.png",
  width=3, height=3,
  units="in",
  res=200)# I want to start drawing.
print(gg)
dev.off()# I'm done drawing.
