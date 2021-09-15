## TODO full data set

## TODO other choices, compare trees

data(spirals, package="kernlab")

## TODO gene expression clustering.

local.file <- "khan.xtrain"
if(!file.exists(local.file)){
  data.url <- paste0(
    "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/", local.file)
  download.file(data.url, local.file)
}
data.dt <- data.table::fread(local.file)
data.mat <- as.matrix(data.dt)
str(data.mat)
