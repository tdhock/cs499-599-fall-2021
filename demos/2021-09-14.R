data.mat <- as.matrix(
  iris[, c("Petal.Width", "Petal.Length")])
str(data.mat)
small.data.mat <- data.mat[c(1:5, 51:55, 101:105),]
str(small.data.mat)
row.indices <- 1:nrow(small.data.mat)
library(data.table)
pair.dt <- data.table(expand.grid(
  row.i=row.indices, row.j=row.indices))
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

## join operations in data table.
lower.dt <- pair.dt[row.i < row.j]
lower.dt[which.min(dist)]
