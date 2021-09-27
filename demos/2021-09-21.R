data.mat <- as.matrix(
  iris[, c("Petal.Length", "Petal.Width")])

## k-means initialization.
K <- 3
set.seed(1)
(centers.mat <- data.mat[sample(1:nrow(data.mat), K), ])

## k-means assignment in C++ code.
(clust.id.vec <- anRpackage::find_closest_center_interface(
  data.mat, centers.mat))

anRpackage::find_closest_center_interface(
  data.mat, centers.mat[,1,drop=FALSE])

anRpackage::find_closest_center_interface(
  data.mat[0,], centers.mat)

anRpackage::find_closest_center_interface(
  data.mat, centers.mat[0,])

anRpackage::find_closest_center_interface(
  data.mat[,0], centers.mat[,0])

## several ways to compute new means. #1 tapply.
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

##3 C++ code, no big time/space savings over R code, but maybe a fun
##exercise for the reader.
centers.new <- anRpackage::compute_centers(
  data.mat, clust.id.vec)
