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
system.time({
  mclust.result <- mclust::Mclust(
    data.mat, n.clusters, "VVI", verbose=FALSE)
})
mclust.time <- system.time({
  mclust.result <- mclust::Mclust(
    data.mat, n.clusters, "VVI", verbose=FALSE)
})
names(mclust.time)
str(mclust.time)
mclust.time[["elapsed"]]
mclust.result$loglik
mclust.result$classification
?mclust::Mclust
mclust.options("subset")

big.data.mat <- data.mat[rep(1:nrow(data.mat), 10),]
nrow(data.mat)
nrow(big.data.mat)
str(big.data.mat)
system.time({
  mclust.result <- mclust::Mclust(
    big.data.mat, n.clusters, "VVI", verbose=FALSE)
})

system.time({
  kmeans(data.mat, n.clusters)
})
system.time({
  kmeans(big.data.mat, n.clusters)
})

system.time({
  mclust.result <- mclust::Mclust(
    big.data.mat, n.clusters, "VVI", verbose=FALSE,
    initialization = list(
      subset=1:150))
})

library(data.table)
(n.data.vec <- as.integer(10^seq(1, 3, by=0.5))) #logarithmic sequence (GOOD for timings experiments)
seq(10, 1000, l=5) #linear sequence (BAD)
all.timings.list <- list()
for(n.data in n.data.vec){
  n.data.mat <- big.data.mat[1:n.data,]
  n.timings <- microbenchmark::microbenchmark(
    mclust.init={
      mclust.result <- mclust::Mclust(
        n.data.mat, n.clusters, "VVI", verbose=FALSE,
        initialization = list(
          subset=1:min(150, nrow(n.data.mat))))
    },
    mclust.default={
      mclust.result <- mclust::Mclust(
        n.data.mat, n.clusters, "VVI", verbose=FALSE)
    },
    kmeans={
      kmeans(n.data.mat, n.clusters)
    },
    times=10)
  class(n.timings)
  all.timings.list[[paste(n.data)]] <- data.table(
    n.data,
    n.timings)
}
(all.timings <- do.call(rbind, all.timings.list))
all.timings[, seconds := time/1e9]
all.timings[, algorithm := expr]

library(ggplot2)
ggplot()+#linear scale, not super helpful.
  geom_point(aes(
    n.data, seconds, color=algorithm),
    data=all.timings)

ggplot()+#log scales, more helpful.
  geom_point(aes(
    n.data, seconds, color=algorithm),
    data=all.timings)+
  scale_x_log10()+
  scale_y_log10()
#larger asymptotic time complexity appears as larger slopes on the log-log scale.
