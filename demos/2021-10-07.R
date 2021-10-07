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

data.dt[, cum.data := cumsum(logratio)]
data.dt[, .(logratio, cum.data)]
data.dt
sum(data.dt$logratio)
(possible.dt <- data.table(
  first_seg_end = seq(1, nrow(data.dt)-1)))
possible.dt[, first_seg_mean := {
  data.dt$cum.data[first_seg_end]/first_seg_end
}]
## Loss = sum of squares - sum^2/n.
loss <- function(cum.sum.vec, cum.square.vec, N.data.vec){
  cum.square.vec-cum.sum.vec^2/N.data.vec
}
data.dt[, cum.square := cumsum(logratio^2)]
possible.dt[, first_seg_loss := {
  data.dt[
    first_seg_end,
    loss(cum.data, cum.square, first_seg_end)
  ]  
}]
data.dt
possible.dt[, cum.data.after := {
  data.dt[.N, cum.data]-data.dt[first_seg_end, cum.data]
}]
possible.dt[, cum.square.after := {
  data.dt[.N, cum.square]-
    data.dt[first_seg_end, cum.square]
}]
possible.dt[, N.data.after := nrow(data.dt)-first_seg_end]
possible.dt[, second_seg_loss := {
  loss(cum.data.after, cum.square.after, N.data.after)
}]
possible.dt[order(second_seg_loss)]
possible.dt[, total_loss := {
  first_seg_loss + second_seg_loss
}]
possible.dt[order(total_loss)]

ggplot()+
  geom_point(aes(
    data.i, logratio),
    data=data.table(panel="data", data.dt))+
  geom_point(aes(
    first_seg_end+0.5, total_loss),
    data=data.table(panel="loss", possible.dt))+
  facet_grid(panel ~ ., scales="free")

new.segs <- possible.dt[which.min(total_loss), rbind(
  data.table(start=1, end=first_seg_end),
  data.table(start=first_seg_end+1, end=nrow(data.dt)))]
for(seg.i in 1:nrow(new.segs)){
  one.seg <- new.segs[seg.i]
  possible.dt <- one.seg[, data.table(
    first_seg_end=seq(start, end-1))]
  possible.dt[, first_seg_loss := TODO]
  possible.dt[, second_seg_loss := TODO]
  possible.dt[, split_loss := {
    first_seg_loss+second_seg_loss
  }]
  possible.dt[, no_split_loss := TODO]
  possible.dt[, loss_decrease := no_split_loss-split_loss]
}
all.possible.dt[which.max(loss_decrease)]
