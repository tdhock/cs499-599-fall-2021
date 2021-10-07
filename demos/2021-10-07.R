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

## option 1: use a single data table with one row per split point
## (includes both segments that could be split). This is a great
## option because it includes the first step as a special case.
segs.after.first.split <- possible.dt[which.min(total_loss), rbind(
  data.table(first_seg_start=1, second_seg_end=first_seg_end),
  data.table(first_seg_start=first_seg_end+1, second_seg_end=nrow(data.dt))
)]
segs.before.first.split <- data.table(
  first_seg_start=1, second_seg_end=nrow(data.dt))
make.possible <- function(segs.dt){
  segs.dt[, data.table(
    first_seg_end=seq(first_seg_start, second_seg_end-1)
  ), by=.(first_seg_start, second_seg_end)]
}
make.possible(segs.before.first.split)
(all.new.possible <- make.possible(segs.after.first.split))
all.new.possible[, loss_decrease := TODO]
all.new.possible[which.max(loss_decrease)]

## option 2: for loop over two segments that could be split. Inside
## for loop compute data table of only splits on that segment.
new.segs <- possible.dt[which.min(total_loss), rbind(
  data.table(start=1, end=first_seg_end),
  data.table(start=first_seg_end+1, end=nrow(data.dt)))]
for(seg.i in 1:nrow(new.segs)){
  one.seg <- new.segs[seg.i]
  new.possible.dt <- one.seg[, data.table(
    first_seg_end=seq(start, end-1))]
  new.possible.dt[, first_seg_loss := TODO]
  new.possible.dt[, second_seg_loss := TODO]
  new.possible.dt[, split_loss := {
    first_seg_loss+second_seg_loss
  }]
  new.possible.dt[, no_split_loss := TODO]
  new.possible.dt[, loss_decrease := no_split_loss-split_loss]
}
all.new.possible.dt[which.max(loss_decrease)]
