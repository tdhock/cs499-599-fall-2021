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
    diag(params$cov.mat), nrow(data.mat), ncol(data.mat),
    byrow=TRUE)
  dnorm.mat[,cluster] <- rowSums(dnorm(data.mat, mean.mat, sd.mat))
  with(cluster.param.list[[cluster]], mvtnorm::dmvnorm(
    data.mat, mean.vec, cov.mat)*prior.weight)
}
total.density.vec <- rowSums(density.mat)
print(log.lik <- sum(log(total.density.vec)))
prob.mat <- density.mat/total.density.vec
