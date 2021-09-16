head(iris)
data.mat <- as.matrix(
  iris[, c("Petal.Width", "Petal.Length")])
head(data.mat)
str(data.mat)
dist.mat <- stats::dist(data.mat)
N <- nrow(data.mat)
N
N*(N-1)/2 #number of pairs of N objects.

linkage.list <- list("single", "complete")
hclust.err.values.list <- list()
for(linkage in linkage.list){
  hc.tree <- stats::hclust(dist.mat, linkage)
  str(hc.tree)
  head(hc.tree$merge)
  n.clusters.vec <- 1:20
  cut.result <- stats::cutree(hc.tree, k=n.clusters.vec)
  library(data.table)
  for(n.clusters in n.clusters.vec){
    print(names(hclust.err.values.list))
    hclust.err.values.list[[paste(linkage, n.clusters)]] <-
      data.table(
        n.clusters,
        linkage,
        ARI=pdfCluster::adj.rand.index(
          cut.result[, n.clusters],
          iris[["Species"]]))
  }
}
(hclust.err.values <- do.call(
  rbind, hclust.err.values.list))

library(ggplot2)
ggplot()+
  geom_line(aes(
    color=linkage,
    x=n.clusters,
    y=ARI),
    data=hclust.err.values)


hc.tree.gg.list <- ggdendro::dendro_data(hc.tree)
str(hc.tree.gg.list)

ggplot()+
  geom_segment(aes(
    x=x,
    y=y,
    xend=xend,
    yend=yend),
    data=hc.tree.gg.list$segments)+
  geom_text(aes(
    x=x,
    y=y,
    label=label),
    data=hc.tree.gg.list$labels)

ggplot()+
  geom_segment(aes(
    x=y,
    y=x,
    xend=yend,
    yend=xend),
    data=hc.tree.gg.list$segments)+
  geom_text(aes(
    x=y,
    y=x,
    label=label),
    size=2,
    data=hc.tree.gg.list$labels)

(labels.dt <- data.table(hc.tree.gg.list$labels))
labels.dt[, Species := iris$Species[as.integer(label)] ]

ggplot()+
  geom_segment(aes(
    x=y,
    y=x,
    xend=yend,
    yend=xend),
    data=hc.tree.gg.list$segments)+
  geom_point(aes(
    color=Species,
    x=y,
    y=x),
    data=labels.dt)

