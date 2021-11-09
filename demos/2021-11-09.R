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
zip.no.label[1,]
zip.label.vec <- zip.dt[[1]]
table(zip.label.vec)
label.dt <- data.table(
  label=zip.label.vec,
  row=seq_along(zip.label.vec))
label.dt[, example.i := seq_along(row), by=label]
label.dt[1:20]
subset.indices <- label.dt[example.i <= 10, row]

zip.no.label.subset <- zip.no.label[subset.indices,]
dim(zip.no.label.subset)
zip.label.subset <- zip.label.vec[subset.indices]
str(zip.label.subset)

pca.subset <- prcomp(zip.no.label.subset)
str(pca.subset)
principal.components.mat <- pca.subset[["x"]][, 1:2]
str(principal.components.mat)
principal.directions <- pca.subset[["rotation"]]
str(principal.directions)

for(n.components in 0:ncol(principal.directions)){
  reconstruction <- if(n.components==0){
    matrix(
      pca.subset$center,
      nrow(zip.no.label.subset),
      ncol(zip.no.label.subset),
      byrow=TRUE)
  }else{
    pca.subset$rotation[,n.components] %*% pca.subset$x[,n.components] + reconstruction TODO
  }
  squared.error <-
    sum((reconstruction - zip.no.label.subset)^2)
}
