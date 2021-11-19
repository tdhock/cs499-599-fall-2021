library(data.table)
library(ggplot2)
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
range(zip.no.label)
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
    activation=activation_linear)

model

compiled.model <- keras::compile(
  model,
  optimizer=keras::optimizer_sgd(learning_rate=0.01),
  loss=keras::loss_mean_squared_error)
recompiled.model <- keras::compile(
  compiled.model,
  optimizer=keras::optimizer_adadelta(),
  loss=keras::loss_mean_squared_error)
## both compiled.model and recompiled.model are pointers to the same neural network. if you call fit on either one, it will update the weights of that network.

fit.history <- keras::fit(
  recompiled.model,
  zip.no.label, #input/first layer
  zip.no.label, #output/last layer
  epochs=20)
plot(fit.history)

as.data.table(fit.history)

pca.fit <- prcomp(zip.no.label, rank=2)
mean.mat <- matrix(
  pca.fit$center,
  nrow(zip.no.label),
  ncol(zip.no.label),
  byrow=TRUE)
mean.mat[1:4,1:4]
reconstruction.list <- list(
  pca=mean.mat + pca.fit$x %*% t(pca.fit$rotation),
  autoencoder=predict(recompiled.model, zip.no.label))
mse.dt.list <- list()
for(model.name in names(reconstruction.list)){
  pred.mat <- reconstruction.list[[model.name]]
  mse.dt.list[[model.name]] <- data.table(
    model.name,
    mse=mean((zip.no.label-pred.mat)^2))
}
(mse.dt <- do.call(rbind, mse.dt.list))
as.data.table(fit.history)[.N]

## initialize list once which keeps history of loss for all epochs of training, over multiple calls to fit function.
train.loss.list <- list()

## execute this block to re-train the network for a few more epochs.
recompiled.model <- keras::compile(
  compiled.model,
  optimizer=keras::optimizer_adadelta(
    learning_rate=0.5),
  loss=keras::loss_mean_squared_error)
num.epochs <- 1000
for(epoch in 1:num.epochs){
  cat(sprintf(
    "%4d / %4d epochs\n",
    epoch, num.epochs))
  keras::fit(
    recompiled.model,
    zip.no.label, #input/first layer
    zip.no.label, #output/last layer
    epochs=1)
  pred.mat <- predict(recompiled.model, zip.no.label)
  epoch.i <- length(train.loss.list)+1
  train.loss.list[[epoch.i]] <-
    data.table(
      epoch.i,
      mse=mean((zip.no.label-pred.mat)^2))
}
train.loss <- do.call(rbind, train.loss.list)
ggplot()+
  geom_hline(aes(
    yintercept=mse, color=model.name),
    data=mse.dt[model.name=="pca"])+
  geom_line(aes(
    epoch.i, mse, color=model.name),
    data=data.table(
      model.name="autoencoder", train.loss))


## predictions at code layer
code_layer_model <- keras::keras_model(
  inputs = compiled.model$input,
  outputs = keras::get_layer(
    compiled.model, "code")$output)
code_mat <- predict(code_layer_model, zip.no.label)
