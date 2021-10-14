data(neuroblastoma, package="neuroblastoma")
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
last.mean.mat <- matrix(NA, N.data, max.segments)
last.change.mat <- matrix(NA, N.data, max.segments)
## best loss = sum of squares - sum^2/N.data.
cum.vec <- cumsum(data.dt[["logratio"]])
cum.N <- 1:N.data
loss <- function(sum.vec, N.vec){
  -sum.vec^2/N.vec
}
loss.mat[,1] <- loss(cum.vec, cum.N)
last.mean.mat[,1] <- cum.vec/cum.N

loss1.dt <- data.table(
  loss=loss.mat[,1],
  N.data=cum.N)
ggplot()+
  geom_point(aes(
    N.data, loss),
    data=loss1.dt)

## computation of best loss in N.segments up.to.t
## => L_{N.segments, up.to.t}
for(N.segments in 2:max.segments){
  for(up.to.t in N.segments:N.data){
    (possible.prev.end <-
       seq(N.segments-1, up.to.t-1)) #t'
    (prev.loss <- loss.mat[possible.prev.end,N.segments-1])
    (N.last.segs <- up.to.t-possible.prev.end)
    (sum.last.segs <-
       cum.vec[up.to.t]-cum.vec[possible.prev.end])
    data.dt[up.to.t, logratio]
    (last.loss <- loss(sum.last.segs, N.last.segs))
    (total.loss <- prev.loss + last.loss)
    best.index <- which.min(total.loss)
    last.mean.mat[up.to.t,N.segments] <-
      (sum.last.segs/N.last.segs)[best.index]
    last.change.mat[up.to.t, N.segments] <-
      possible.prev.end[best.index]
    loss.mat[up.to.t, N.segments] <- total.loss[best.index]
  }
}
data.table(loss.mat)
data.table(last.change.mat)
data.table(last.mean.mat)

last.change.mat[N.data, max.segments]
last.mean.mat[N.data, max.segments]
mean(data.dt[461:N.data, logratio])

last.change.mat[460, 3]
last.mean.mat[460,3]
mean(data.dt[438:460, logratio])

last.change.mat[437, 2]

seg.end <- N.data
seg.dt.list <- list()
for(seg.i in max.segments:1){
  prev.end <- last.change.mat[seg.end, seg.i]
  seg.start <- if(seg.i==1)1 else prev.end+1
  seg.dt.list[[seg.i]] <- data.table(
    seg.start, seg.end,
    seg.mean=last.mean.mat[seg.end, seg.i])
  seg.end <- prev.end
}
(seg.dt <- do.call(rbind, seg.dt.list))

ggplot()+
  geom_segment(aes(
    seg.start, seg.mean,
    xend=seg.end, yend=seg.mean),
    color="red",
    size=2,
    data=seg.dt)+
  geom_point(aes(
    data.i, logratio),
    shape=1,
    data=data.dt)

## For comparing loss values with other packages (binsegRcpp, jointseg) you need to add the sum of squares term which we ignored in the definition of loss().
loss.mat[N.data,]+sum(data.dt$logratio^2)
