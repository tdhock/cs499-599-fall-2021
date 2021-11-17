library(data.table)
library(keras)
destfile <- "zip.train.gz"
datasets.url <-
  "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/"
if(!file.exists(destfile)){
  zip.url <- paste0(datasets.url, destfile)
  download.file(zip.url, destfile)
}
zip.dt <- data.table::fread(file=destfile)
zip.no.label <- as.matrix(zip.dt[1:100, -1])
dim(zip.no.label)

n.input.output <- ncol(zip.no.label)
n.intermediate <- 50
my_activation <- activation_relu
i.model <- keras::keras_model_sequential() %>%
  keras::layer_dense(
    input_shape = n.input.output,
    units=n.intermediate,
    activation = my_activation) %>%
  keras::layer_dense(
    name="code",
    units = 2, 
    activation = my_activation) %>%
  keras::layer_dense(
    units=n.intermediate,
    activation=my_activation) %>%
  keras::layer_dense(
    units = n.input.output,
    activation=my_activation)

i.compiled.model <- keras::compile(
  i.model,
  optimizer=keras::optimizer_sgd(lr=0.01),
  loss=keras::loss_mean_squared_error)
