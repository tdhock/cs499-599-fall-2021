ls()
data(neuroblastoma, package="neuroblastoma")
ls()

library(data.table)
(nb.dt <- data.table(neuroblastoma$profiles))
(data.dt <- nb.dt[profile.id=="1" & chromosome=="1"])

library(ggplot2)
ggplot()+
  geom_point(aes(
    position, logratio),
    data=data.dt)

data.dt[, data.i := 1:.N]
ggplot()+
  geom_point(aes(
    data.i, logratio),
    data=data.dt)

N.data <- nrow(data.dt)
max.segments <- 4
loss.mat <- matrix(NA, N.data, max.segments)
## best loss = sum of squares - sum^2/N.data.
cum.vec <- cumsum(data.dt[["logratio"]])
cum.N <- 1:N.data
loss <- function(sum.vec, N.vec){
  -sum.vec^2/N.vec
}
loss.mat[,1] <- loss(cum.vec, cum.N)

loss1.dt <- data.table(
  loss=loss.mat[,1],
  N.data=cum.N)
ggplot()+
  geom_point(aes(
    N.data, loss),
    data=loss1.dt)

up.to.t <- 10
N.segments <- 2
(possible.first.end <- seq(1, up.to.t-1))
(prev.loss <- loss.mat[possible.first.end,N.segments-1])
(N.last.segs <- up.to.t-possible.first.end)
(sum.last.segs <- cum.vec[up.to.t]-cum.vec[possible.first.end])
(last.loss <- loss(sum.last.segs, N.last.segs))
(total.loss <- prev.loss + last.loss)
best.index <- which.min(total.loss)
loss.mat[up.to.t, N.segments] <- total.loss[best.index]
