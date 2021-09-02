data.mat <- as.matrix(
  iris[, c("Petal.Length", "Petal.Width")])
head(data.mat)
library(data.table)
data.dt <- data.table(data.mat)
library(ggplot2)
gg <- ggplot()+
  geom_point(aes(
    Petal.Length, Petal.Width),
    data=data.dt)+
  coord_equal()
gg
ggplot2:::print.ggplot(gg)
ggplot2::print.ggplot(gg)

K <- 3
set.seed(1)
centers.mat <- data.mat[sample(1:nrow(data.dt), K), ]
(centers.dt <- data.table(
  centers.mat,
  cluster=factor(1:nrow(centers.mat))))
gg+
  geom_point(aes(
    Petal.Length, Petal.Width, color=cluster),
    data=centers.dt)

## Requires O(N K) memory where N=number of data and K=number of clusters.
(dist.dt <- data.table(expand.grid(
  centers.i=1:nrow(centers.dt),
  data.i=1:nrow(data.dt))))
dist.dt[, data.mat[data.i,] ]
dist.dt[, centers.mat[centers.i,] ]
dist.dt[, data.mat[data.i,]-centers.mat[centers.i,] ]
dist.dt[, (data.mat[data.i,]-centers.mat[centers.i,])^2 ]

dist.dt[, error := {
  rowSums((data.mat[data.i,]-centers.mat[centers.i,])^2)
}]
dist.dt[, min(error)]
dist.dt[, which.min(error)]
dist.dt[, .SD[which.min(error)] ]
assignment.dt <- dist.dt[, {
  .SD[which.min(error)]
}, by=data.i ]
dist.dt[, {
  data.table(closest=centers.i[which.min(error)])
}, by=data.i ]
## Next step: for every cluster, compute new center.
assignment.dt[, {
  browser(expr= centers.i==3)#R debugger
  data.mat[data.i,]
}, by=centers.i]
new.mean.dt <- assignment.dt[, {
  data.table(t(colMeans(data.mat[data.i,])))
}, by=centers.i]
centers.mat
centers.mat <- new.mean.dt[, colnames(centers.mat), with=FALSE]




dist.dt[, error := {
  rowSums((data.mat[data.i,]-centers.mat[centers.i,])^2)
}]
assignment.dt <- dist.dt[, {
  .SD[which.min(error)]
}, by=data.i ]
new.mean.dt <- assignment.dt[, {
  data.table(t(colMeans(data.mat[data.i,])))
}, by=centers.i]
(centers.mat <-
   new.mean.dt[, colnames(centers.mat), with=FALSE])

kmeans.result <- stats::kmeans(data.mat, K)
pdfCluster::adj.rand.index(
  iris$Species, kmeans.result$cluster)
kmeans.result$tot.withinss # R's total error.
sum(assignment.dt$error) # our total error.
# Your KMEANS function should return:
list(
  tot.withinss=sum(assignment.dt$error),
  cluster=assignment.dt$centers.i)

## [[ same as $ but better for programming.
sum(assignment.dt[["error"]]) # our total error.
some.variable <- "error"
sum(assignment.dt[[some.variable]]) # our total error.
assignment.dt$some.variable


Rcpp::Rcpp.package.skeleton()
Rcpp::compileAttributes("anRpackage/")
install.packages("anRpackage/", repo=NULL, type="source")
