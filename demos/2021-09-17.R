head(iris)
data.mat <- as.matrix(iris[, 1:4])
head(data.mat)
str(data.mat)
library(data.table)
data.dt <- data.table(
  row=as.integer(row(data.mat)),
  col=as.integer(col(data.mat)),
  cm=as.numeric(data.mat))

library(ggplot2)
ggplot()+
  geom_raster(aes(
    col, row, fill=cm),
    data=data.dt)+
  scale_fill_gradient(low="white", high="red")

set.seed(1)
rand.mat <- data.mat[sample(1:nrow(data.mat)),]
rand.dt <- data.table(
  row=as.integer(row(rand.mat)),
  col=as.integer(col(rand.mat)),
  cm=as.numeric(rand.mat))
ggplot()+
  geom_raster(aes(
    col, row, fill=cm),
    data=rand.dt)+
  scale_fill_gradient(low="white", high="red")

dist.mat <- stats::dist(rand.mat)
hc.tree <- stats::hclust(dist.mat)
segs.and.labels <- ggdendro::dendro_data(hc.tree)

ggplot()+
  geom_segment(aes(
    x, y, xend=xend, yend=yend),
    data=segs.and.labels[["segments"]])+
  geom_text(aes(
    x, y, label=label),
    data=segs.and.labels[["labels"]])

ggplot()+
  geom_segment(aes(
    y, x, xend=yend, yend=xend),
    data=segs.and.labels[["segments"]])+
  geom_text(aes(
    y, x, label=label),
    size=2,
    data=segs.and.labels[["labels"]])

labels.dt <- data.table(segs.and.labels[["labels"]])
labels.dt[rand.dt, on=.(label=row)]
labels.dt[, row := as.integer(label)]
labels.dt[rand.dt, on=.(row)]
labels.ord <- labels.dt[rand.dt, on=.NATURAL]

ggplot()+
  geom_raster(aes(
    col, x, fill=cm),
    data=data.table(
      graph="heatmap",
      labels.ord))+
  scale_fill_gradient(low="white", high="red")+
  geom_segment(aes(
    y, x, xend=yend, yend=xend),
    data=data.table(
      graph="tree",
      segs.and.labels[["segments"]]))+
  facet_grid(. ~ graph, scales="free")
