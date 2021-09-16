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

anRpackage::rcpp_hello_world(data.mat, centers.mat) # all ones, bug?
