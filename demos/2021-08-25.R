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
zip.no.label = fread(file=destfile, drop=1)
dim(zip.no.label)
zip.no.label.mat <- as.matrix(zip.no.label)
zip.no.label.mat[1:4, 1:4]
zip.only.label = fread(file=destfile, select=1)
dim(zip.only.label)

some.mat <- t(zip.no.label.mat[1:2,])
N.pixels <- 16
some.dt <- data.table(
  example=as.integer(col(some.mat)),
  col=1:N.pixels, 
  row=rep(N.pixels:1, each=N.pixels),
  intensity=as.numeric(some.mat))
library(ggplot2)
ggplot(data=some.dt, aes(x=col, y=row)) +
  facet_grid(. ~ example, labeller=label_both)+
  geom_tile(aes(fill = intensity))+
  scale_fill_gradient(low="black", high="white")+
  coord_equal()

show.label <- 5
examples.per.label <- 4
some.indices <- which(zip.only.label==show.label)[1:examples.per.label]
some.mat <- t(zip.no.label.mat[some.indices,])
N.pixels <- 16
some.dt <- data.table(
  example=as.integer(col(some.mat)),
  col=1:N.pixels, 
  row=rep(N.pixels:1, each=N.pixels),
  intensity=as.numeric(some.mat))
library(ggplot2)
ggplot(data=some.dt, aes(x=col, y=row)) +
  facet_grid(. ~ example, labeller=label_both)+
  geom_tile(aes(fill = intensity))+
  scale_fill_gradient(low="black", high="white")+
  coord_equal(expand=FALSE)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))

## all.
show.dt.list <- list()
examples.per.label <- 4
for(show.label in 4:6){
  some.indices <- which(zip.only.label==show.label)[1:examples.per.label]
  some.mat <- t(zip.no.label.mat[some.indices,])
  N.pixels <- 16
  show.dt.list[[paste(show.label)]] <- data.table(
    label=show.label,
    example=as.integer(col(some.mat)),
    col=1:N.pixels, 
    row=rep(N.pixels:1, each=N.pixels),
    intensity=as.numeric(some.mat))
}
show.dt <- do.call(rbind, show.dt.list)
library(ggplot2)
ggplot(data=show.dt, aes(x=col, y=row)) +
  facet_grid(label ~ example, labeller=label_both)+
  geom_tile(aes(fill = intensity))+
  scale_fill_gradient(low="black", high="white")+
  coord_equal(expand=FALSE)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))

