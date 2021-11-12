destfile <- "zip.train.gz"
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
dim(zip.dt)
zip.dt[1]

zip.label.col.i <- 1
zip.no.label <- as.matrix(
  zip.dt[, -zip.label.col.i, with=FALSE])
str(zip.no.label)

system.time({
  zip.pca.full <- prcomp(
    zip.no.label,
    rank=2)
})
str(zip.pca.full)

## verify that reconstructions works as expected.
mean.mat <- matrix(
  zip.pca.full$center,
  nrow(zip.no.label),
  ncol(zip.no.label),
  byrow=TRUE)
mean.mat[1:4, 1:4]

pc.dt <- data.table(
  zip.pca.full$x[,1:2])
library(ggplot2)
ggplot()+
  geom_point(aes(
    PC1, PC2),
    data=pc.dt)

zip.pca.full$rotation[1:2,1:2]
zip.pca.full$x[1:2,1:2]

n.components <- 0
pc.reconstruction <- if(n.components==0){
  0
}else{
  some.directions <-
    zip.pca.full$rotation[, 1:n.components]
  some.components <- zip.pca.full$x[, 1:n.components]
  some.components %*% t(some.directions)
}
some.reconstruction <-
  mean.mat + pc.reconstruction
sum((zip.no.label - some.reconstruction)^2)

row.i <- 5
N.pixels <- 16
row.intensity.dt <- data.table(
  example=row.i,
  col=1:N.pixels, 
  row=rep(N.pixels:1, each=N.pixels),
  intensity=as.numeric(zip.no.label[row.i,]))

ggplot()+
  geom_tile(aes(
    col, row, fill=intensity),
    data=row.intensity.dt)+
  scale_fill_gradient(high="white", low="black")

reconstruction.dt <- data.table(
  example=row.i,
  col=1:N.pixels, 
  row=rep(N.pixels:1, each=N.pixels),
  intensity=as.numeric(some.reconstruction[row.i,]))
ggplot()+
  geom_tile(aes(
    col, row, fill=intensity),
    data=reconstruction.dt)+
  scale_fill_gradient(high="white", low="black")+
  coord_equal()

## Exercise: visualize virtual images on PCA grid.
table(zip.dt[[1]])
zip.label.vec <- zip.dt[[zip.label.col.i]]
is.one.class <- zip.label.vec == 3
zip.one.class <- zip.no.label[is.one.class,]
zip.pca.one.class <- prcomp(
  zip.one.class,
  rank=2)

pc.dt <- data.table(
  zip.pca.one.class$x[,1:2])
ggplot()+
  geom_point(aes(
    PC1, PC2),
    data=pc.dt)
sapply(pc.dt, range)

pc.grid.list <- lapply(pc.dt, function(values){
  seq(min(values), max(values), l=5)
})

grid.dt <- data.table(do.call(
  expand.grid, pc.grid.list))
ggplot()+
  geom_point(aes(
    PC1, PC2),
    data=pc.dt)+
  geom_point(aes(
    PC1, PC2),
    color="red",
    data=grid.dt)

pc.grid.mat <- as.matrix(grid.dt)
some.directions <- zip.pca.one.class$rotation
mean.mat <- matrix(
  zip.pca.one.class$center,
  nrow(pc.grid.mat),
  length(zip.pca.one.class$center),
  byrow=TRUE)
some.reconstruction <-
  mean.mat + pc.grid.mat %*% t(some.directions)
## TODO viz images in some.reconstruction!
