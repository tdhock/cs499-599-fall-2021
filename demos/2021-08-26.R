destfile <- "zip.train.gz"
zip.url = paste0(
  "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/",
  destfile)
if(!file.exists(destfile)){
  download.file(zip.url, destfile)
}
zip.dt <- data.table::fread(destfile)
zip.dt <- fread(destfile)
if(FALSE){
  install.packages("data.table")
}

zip.dt[1] #first argument of [ is row subset.
zip.mat <- as.matrix(zip.dt[, -1]) #second argument of [ is col subset.
search()
library(data.table) #attach package to search() path.
search()

ggplot2::ggplot(ggplot2::faithfuld, ggplot2::aes(waiting, eruptions)) +
  ggplot2::geom_raster(ggplot2::aes(fill = density))

library(ggplot2)
ggplot(data=faithfuld, aes(x=waiting, y=eruptions)) +
  geom_raster(aes(fill = density))
ggplot(data=faithfuld, aes(x=waiting, y=eruptions)) +
  geom_tile(aes(fill = density))

digit.to.show <- 1
N.pixels <- 16
number.image.data.table <- data.table(
  intensity=zip.mat[digit.to.show, ],
  row=rep(N.pixels:1, each=N.pixels),
  col=rep(1:N.pixels, times=N.pixels))
ggplot(data=number.image.data.table, aes(x=col, y=row)) +
  geom_tile(aes(fill = intensity))+
  scale_fill_gradient(low="black", high="white") +
  ggtitle(
    "one digit from zip data",
    subtitle=paste0("label=", zip.dt[digit.to.show, 1]))
