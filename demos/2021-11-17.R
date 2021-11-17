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
model <- keras::keras_model_sequential() %>%
  keras::layer_dense(
    input_shape = n.input.output,
    units=n.intermediate,
    use_bias = FALSE,
    activation = my_activation) %>%
  keras::layer_dense(
    name="code",
    use_bias = FALSE,
    units = 2, 
    activation = my_activation) %>%
  keras::layer_dense(
    use_bias = FALSE,
    units=n.intermediate,
    activation=my_activation) %>%
  keras::layer_dense(
    use_bias = FALSE,
    units = n.input.output,
    activation=my_activation)

compiled.model <- keras::compile(
  model,
  optimizer=keras::optimizer_sgd(learning_rate=0.01),
  loss=keras::loss_mean_squared_error)

recompiled.model <- keras::compile(
  compiled.model,
  optimizer=keras::optimizer_adadelta(),
  loss=keras::loss_mean_squared_error)
fit.history <- keras::fit(
  recompiled.model,
  zip.no.label, zip.no.label, epochs=20)
plot(fit.history)

## predictions at code layer
code_layer_model <- keras::keras_model(
  inputs = compiled.model$input,
  outputs = keras::get_layer(compiled.model, "code")$output)
code_mat <- predict(code_layer_model, zip.no.label)
