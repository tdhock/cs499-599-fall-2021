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

dist.dt <- data.table(expand.grid(
  centers.i=1:nrow(centers.dt),
  data.i=1:nrow(data.dt)))
dist.dt[, error := rowSums((data.mat[data.i,]-centers.mat[centers.i,])^2)]

clust.id.vec <- anRpackage::find_closest_center(
  data.mat, centers.mat)

## two ways to compute new means. #1 tapply.
(mean.list <- tapply(
  seq_along(clust.id.vec), clust.id.vec, function(i){
    colMeans(data.mat[i,])
  }))
(centers.new <- do.call(rbind, mean.list))

##2 data.table by.
mean.dt <- data.table(
  data.i=seq_along(clust.id.vec), cluster=clust.id.vec
)[, {
  data.table(t(colMeans(data.mat[data.i,])))
}, keyby=cluster]
(centers.new <- as.matrix(mean.dt[,-1]))
