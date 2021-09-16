data.mat <- as.matrix(
  iris[, c("Petal.Width", "Petal.Length")])
str(data.mat)
small.data.mat <- data.mat[c(1:5, 51:55, 101:105),]
str(small.data.mat)
row.indices <- 1:nrow(small.data.mat)
library(data.table)
pair.dt <- data.table(expand.grid(
  row.i=row.indices,
  row.j=row.indices))
pair.dt[, dist := rowSums(abs(
  small.data.mat[row.i,]-small.data.mat[row.j,]))]
pair.dt

d.mat <- matrix(
  pair.dt[["dist"]],
  nrow(small.data.mat), nrow(small.data.mat))
str(d.mat)
d.mat[1:5, 1:5]

## tall/long to wide data reshape.
wide.dt <- data.table::dcast(pair.dt, row.i ~ row.j)
d.mat <- as.matrix(wide.dt[, -1])
str(d.mat)
d.mat[1:5, 1:5]

d.mat <- matrix(
  NA, nrow(small.data.mat), nrow(small.data.mat))
for(row.i in 1:nrow(small.data.mat)){
  d.mat[row.i,] <- colSums(abs(
    small.data.mat[row.i,]-t(small.data.mat)))
}
str(d.mat)
d.mat[1:5, 1:5]

which.min(d.mat) #does not ignore diagonal!
diag(d.mat) <- NA
d.mat[1:5, 1:5]
min.indices <- which(min(d.mat, na.rm=TRUE) == d.mat)
min.index.dt <- data.table(
  min.indices,
  row=row(d.mat)[min.indices],
  col=col(d.mat)[min.indices])
first.min <- min.index.dt[1]
obs.in.each.cluster <- as.list(1:15)
index.to.keep <- first.min[["row"]]
index.to.remove <- first.min[["col"]]
join.indices <- c(index.to.keep, index.to.remove)
d.mat.copy <- d.mat
new.cluster.distances <- #single linkage criterion.
  apply(d.mat[join.indices,], 2, min)
d.mat.copy[index.to.keep,] <- new.cluster.distances
d.mat.copy[,index.to.keep] <- new.cluster.distances
d.mat.new <- d.mat.copy[-index.to.remove, -index.to.remove]
str(d.mat.new)
d.mat.new[1:5, 1:5]
obs.in.each.cluster.copy <- obs.in.each.cluster
obs.in.each.cluster.copy[[index.to.keep]] <-
  unlist(obs.in.each.cluster[join.indices])
obs.in.each.cluster.new <-
  obs.in.each.cluster.copy[-index.to.remove]
## convert list to cluster label integer vector
cluster.id.vec <- integer(nrow(small.data.mat))
for(cluster.id in seq_along(obs.in.each.cluster.new)){
  obs.indices <- obs.in.each.cluster.new[[cluster.id]]
  cluster.id.vec[obs.indices] <- cluster.id
}

## join operations in data table.
lower.dt <- pair.dt[row.i < row.j]
lower.dt[which.min(dist)]
