library(data.table)
library(mclust)
head(iris)
data.mat <- as.matrix(iris[,1:4])
n.folds <- 4 # 75% train, 25% validation.
set.seed(1)
(uniq.folds <- 1:n.folds)
(fold.id.vec <- sample(rep(uniq.folds, l=nrow(iris))))
err.dt.list <- list()
for(valid.fold in uniq.folds){
  is.valid <- fold.id.vec == valid.fold
  is.train <- !is.valid
  set.vec <- ifelse(is.train, "train", "validation")
  n.clusters <- 5
  mclust.result <- Mclust(
    data.mat[is.train,], n.clusters, "EII")
  log.lik <- mclust::dens(
    mclust.result$modelName,
    data.mat,
    logarithm = TRUE,
    parameters = mclust.result$parameters)
  (lik.dt <- data.table(
    set=set.vec,
    log.lik))
  err.dt.list[[paste(n.clusters, valid.fold)]] <-
    data.table(
      n.clusters,
      valid.fold,
      lik.dt[, .(
        total.error=sum(-log.lik)
      ), by=set])
  ## select sum(dist) as total.error from closest.dt group by set
}
(err.dt <- do.call(rbind, err.dt.list))
mean(err.dt[set=="validation", total.error])
(mean.over.folds <- err.dt[, .(
  mean.err=mean(total.error)
), by=set])
