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

zip.no.label <- as.matrix(zip.dt[, -1])
str(zip.no.label)

## look at min/max/mean/sd value of each feature
apply(zip.no.label, 2, range)
apply(zip.no.label, 2, mean)
apply(zip.no.label, 2, sd)

## if features have very different mean/sd values then you can use scale to transform the data to mean=0, sd=1.
zip.no.label.sc <- scale(zip.no.label)
apply(zip.no.label.sc, 2, range)
apply(zip.no.label.sc, 2, mean)
apply(zip.no.label.sc, 2, sd)

zip.pca.full <- prcomp(zip.no.label, center=TRUE)
str(zip.pca.full)

## verify that reconstructions works as expected.
mean.mat <- matrix(
  zip.pca.full$center,
  nrow(zip.no.label),
  ncol(zip.no.label),
  byrow=TRUE)
mean.mat[1:4, 1:4]

lambda.times.V <- with(zip.pca.full, x %*% t(rotation))
dim(mean.mat)
dim(lambda.times.V)
reconstruction <- mean.mat + lambda.times.V
all.equal(reconstruction, zip.no.label)
rbind(
  reconstruction=reconstruction[1,],
  data=zip.no.label[1,])

dim(zip.pca.full$x)
zip.pca.full$x[1:4,1:4]

pc.dt <- data.table(
  zip.pca.full$x[,1:2])

library(ggplot2)
ggplot()+
  geom_point(aes(
    PC1, PC2),
    data=pc.dt)

n.components <- 256
some.directions <- zip.pca.full$rotation[, 1:n.components]
 some.components <- zip.pca.full$x[, 1:n.components]
some.reconstruction <- mean.mat + some.components %*% t(some.directions)
sum((zip.no.label - some.reconstruction)^2)
