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
?mclustModelNames
system.time({
  mclust.result <- mclust::Mclust(
    data.mat, n.clusters, "VVI", verbose=FALSE)
})
mclust.result$loglik
mclust.result$classification

rand.pairs <- mclust::hcRandomPairs(data.mat)
system.time({
  mclust.result <- mclust::Mclust(
    data.mat, n.clusters, "VVI", verbose=FALSE,
    initialization = list(hcPairs=rand.pairs))
})

rand.mat <- matrix(
  runif(nrow(data.mat)*n.clusters),
  nrow=nrow(data.mat),
  ncol=n.clusters)
head(rand.mat)
prob.mat <- rand.mat/rowSums(rand.mat)
head(prob.mat)

cluster.param.list <- list()
for(cluster in 1:n.clusters){
  prob.vec <- prob.mat[, cluster]
  mean.vec <- colSums(data.mat * prob.vec)/sum(prob.vec)
  mean.mat <- matrix(
    mean.vec, nrow(data.mat), ncol(data.mat), byrow=TRUE)
  diff.mat <- data.mat - mean.mat
  diff.mat[1,] %*% t(diff.mat[1,])
  dim(diff.mat)
  unconstrained.cov.mat <-
    t(diff.mat) %*% (diff.mat*prob.vec) / sum(prob.vec)
  constrained.cov.mat <- diag(diag(unconstrained.cov.mat))
  colSums(diff.mat^2 * prob.vec)/sum(prob.vec)
  this.cluster.params <- list(
    prior.weight=mean(prob.vec),
    mean.vec=mean.vec,
    cov.mat=constrained.cov.mat)
  cluster.param.list[[cluster]] <- this.cluster.params
}
density.mat <- matrix(NA, nrow(data.mat), n.clusters)
dnorm.mat <- matrix(NA, nrow(data.mat), n.clusters)
for(cluster in 1:n.clusters){
  params <- cluster.param.list[[cluster]]
  density.mat[,cluster] <- mvtnorm::dmvnorm(
    data.mat, params$mean.vec, params$cov.mat
  )*params$prior.weight
  mean.mat <- matrix(
    params$mean.vec, nrow(data.mat), ncol(data.mat),
    byrow=TRUE)
  sd.mat <- matrix(
    sqrt(diag(params$cov.mat)), nrow(data.mat), ncol(data.mat),
    byrow=TRUE)
  ## storing the vector of diagonal entries of the covariance matrix,
  ## and using dnorm, is much more efficient in terms of time and
  ## space, compared to storing the full p x p matrix, and using
  ## mvtnorm::dmvnorm.
  dnorm.mat[,cluster] <-
    apply(dnorm(data.mat, mean.mat, sd.mat), 1, prod)*params$prior.weight
  with(cluster.param.list[[cluster]], mvtnorm::dmvnorm(
    data.mat, mean.vec, cov.mat)*prior.weight)
}
total.density.vec <- rowSums(density.mat)
print(log.lik <- sum(log(total.density.vec)))
prob.mat <- density.mat/total.density.vec


destfile <- "zip.test.gz"
datasets.url <-
  "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/"
if(!file.exists(destfile)){
  zip.url <- paste0(datasets.url, destfile)
  download.file(zip.url, destfile)
}
if(!requireNamespace("R.utils")){
  install.packages("R.utils")
}
zip.dt <- data.table::fread(file=destfile)
library(data.table)
data.mat <- as.matrix(zip.dt[,-1])

## demo of how to avoid numerical issues in large data by computing
## density values in log space:
rand.mat <- matrix(
  runif(nrow(data.mat)*n.clusters),
  nrow=nrow(data.mat),
  ncol=n.clusters)
head(rand.mat)
prob.mat <- rand.mat/rowSums(rand.mat)
head(prob.mat)

cluster.param.list <- list()
for(cluster in 1:n.clusters){
  prob.vec <- prob.mat[, cluster]
  mean.vec <- colSums(data.mat * prob.vec)/sum(prob.vec)
  mean.mat <- matrix(
    mean.vec, nrow(data.mat), ncol(data.mat), byrow=TRUE)
  diff.mat <- data.mat - mean.mat
  tolerance <- 1e-6
  ##https://stats.stackexchange.com/questions/219302/singularity-issues-in-gaussian-mixture-model
  this.cluster.params <- list(
    prior.weight=mean(prob.vec),
    mean.vec=mean.vec,
    cov.vec=colSums(diff.mat^2 * prob.vec)/sum(prob.vec)+tolerance)
  cluster.param.list[[cluster]] <- this.cluster.params
}
log.density.mat <- matrix(NA, nrow(data.mat), n.clusters)
prior.weight.mat <- matrix(NA, nrow(data.mat), n.clusters)
for(cluster in 1:n.clusters){
  params <- cluster.param.list[[cluster]]
  mean.mat <- matrix(
    params$mean.vec, nrow(data.mat), ncol(data.mat),
    byrow=TRUE)
  sd.vec <- sqrt(params$cov.vec)
  sd.mat <- matrix(
    sd.vec, nrow(data.mat), ncol(data.mat),
    byrow=TRUE)
  dnorm.mat <- dnorm(data.mat, mean.mat, sd.mat, log=TRUE)
  sd.vec[which(colMeans(!is.finite(dnorm.mat))==1)]
  log.density.mat[,cluster] <- rowSums(dnorm.mat)
  prior.weight.mat[,cluster] <- params$prior.weight
  ##exp(log.density.vec)*params$prior.weight
}
max.vec <- apply(log.density.mat, 1, max)
log.sum.vec <- log(
  rowSums(prior.weight.mat*exp(log.density.mat - max.vec))
)
log.total.density.vec <- max.vec + log.sum.vec
which(!is.finite(log.total.density.vec))
in.exp <- log(prior.weight.mat)+log.density.mat-log.total.density.vec
prob.mat <- exp(in.exp)
print(log.lik <- sum(log.total.density.vec))
##head(prob.mat)
which(!is.finite(prob.mat))
