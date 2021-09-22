head(iris)
data.mat <- as.matrix(iris[,1:4])
n.folds <- 4 # 75% train, 25% validation.
set.seed(1)
fold.id.vec <- sample(rep(1:n.folds, l=nrow(iris)))
valid.fold <- 1
is.valid <- fold.id.vec == valid.fold
is.train <- !is.valid
(set.vec <- ifelse(is.train, "train", "validation"))
set.vec <- ifelse(is.train, "train", "validation")
set.vec
table(set.vec, iris$Species)
n.clusters <- 5
kmeans.result <- kmeans(data.mat[is.train,], n.clusters)
centers.mat <- kmeans.result[["centers"]]
kmeans(data.mat[is.train,], centers.mat, iter.max=0)
library(data.table)
pair.dt <- data.table(expand.grid(
  center.i=1:nrow(centers.mat),
  data.i=1:nrow(data.mat)))
pair.dt[, dist := {
  rowSums((data.mat[data.i,]-centers.mat[center.i,])^2)
}]
#DT[i/WHERE/rows, j/SELECT/cols, by/GROUP BY]
pair.dt[, data.table(
  closest=center.i[which.min(dist)]
), by=data.i]
# .SD = subset of data table corresponding to one unique value of by=data.i
closest.dt <- pair.dt[, .SD[which.min(dist)], by=data.i]
closest.dt[, set := set.vec]
head(closest.dt, 20)
closest.dt[, .(
  total.error=sum(dist)
), by=set]
kmeans.result$tot.withinss
