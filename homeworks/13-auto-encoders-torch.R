## https://mlverse.github.io/torchbook_materials/vaes.html
## https://torch.mlverse.org/start/guess_the_correlation/
library(data.table)
library(ggplot2)
destfile <- "zip.train.gz"
datasets.url <-
  "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/"
if(!file.exists(destfile)){
  zip.url <- paste0(datasets.url, destfile)
  download.file(zip.url, destfile)
}
zip.dt <- data.table::fread(file=destfile)
some.zip.dt <- zip.dt[1:100,]
some.zip.X <- as.matrix(some.zip.dt[,-1])

## zip_dataset is a class which represents a zip data set.
zip_dataset <- torch::dataset(
  name = "zip_dataset",
  initialize = function(zip.X.mat) {
    self$x <- torch::torch_tensor(zip.X.mat)
  },
  .getitem = function(i) {
    ## Need list with x=y=same thing to use luz::fit.
    list(x=self$x[i,], y=self$x[i,])
  },
  .length = function() {
    self$x$size()[[1]]
  }
)

## zip_train is an instance of zip_dataset, which is a wrapper around
## the some.zip.X matrix.
zip_train <- zip_dataset(some.zip.X)
length(zip_train)
zip_train[1:2]

## set seed to control random number generation.
torch::torch_manual_seed(777)

## Define neural network architecture, number of units per layer.
autoencoder <- torch::nn_module(
  "autoencoder",
  initialize =
    function(n.input.output=ncol(some.zip.X),
             n.intermediate=50, n.code=2) {
    self$encoder <- torch::nn_sequential(
      torch::nn_linear(n.input.output, n.intermediate),
      torch::nn_relu(),
      torch::nn_linear(n.intermediate, n.code),
      torch::nn_relu()
    )
    self$decoder <- torch::nn_sequential(
      torch::nn_linear(n.code, n.intermediate),
      torch::nn_relu(),
      torch::nn_linear(n.intermediate, n.input.output)
    )
  },
  encode = function(x) {
    self$encoder(x)
  },
  decode=function(z) {
    self$decoder(z)
  },
  forward = function(x) {
    z <- self$encode(x)
    self$decode(z)
  }
)


## dataloader is used to specify batch size.
train_dl <- torch::dataloader(
  zip_train, batch_size = 10, shuffle = TRUE)

## luz package provides high level interface for training, similar to
## keras.
after.setup <- luz::setup(
  autoencoder,
  loss=torch::nnf_mse_loss,
  optimizer=torch::optim_adadelta)
fitted <- luz::fit(after.setup, train_dl, epochs=100)

## Train MSE plot, compare with PCA.
pca.fit <- prcomp(some.zip.X, rank=2)
mean.mat <- matrix(
  pca.fit$center,
  nrow(some.zip.X),
  ncol(some.zip.X),
  byrow=TRUE)
mean.mat[1:4,1:4]
reconstruction.list <- list(
  pca=mean.mat + pca.fit$x %*% t(pca.fit$rotation),
  autoencoder=fitted$model$forward(zip_train$x))
mse.dt.list <- list()
for(model.name in names(reconstruction.list)){
  pred.mat <- reconstruction.list[[model.name]]
  mse.dt.list[[model.name]] <- data.table(
    model.name,
    mse=as.numeric(mean((some.zip.X-pred.mat)^2)))
}
(mse.dt <- do.call(rbind, mse.dt.list))
(train.loss <- luz::get_metrics(fitted))
ggplot()+
  scale_y_continuous("Mean Squared Error")+
  geom_hline(aes(
    yintercept=mse, color=model.name),
    data=mse.dt[model.name=="pca"])+
  geom_line(aes(
    epoch, value, color=model.name),
    data=data.table(
      model.name="autoencoder", train.loss))

low.dim.tensor <- fitted$model$encode(zip_train$x)
low.dim.mat <- as.matrix(low.dim.tensor)
low.dim.dt <- data.table(
  label=factor(some.zip.dt[[1]]),
  low.dim.mat)
ggplot()+
  geom_text(aes(
    V1, V2, label=label),
    data=low.dim.dt)
