# Question 1.1
data(neuroblastoma, package="neuroblastoma")
library(data.table)
data.dt <- data.table(neuroblastoma$profiles)
(one.sequence <- data.dt[
  chromosome=="2" & profile.id=="79"])
sets <- c("subtrain", "validation")
one.sequence[, set := sample(
  rep(sets,length.out=nrow(one.sequence)))]
(table(one.sequence[,set]))
# Question 1.2

K.max <- 10
(subtrain.data <- one.sequence[set == "subtrain"])
(pos.before <- subtrain.data[1:(.N-1), position])
(pos.after <- subtrain.data[2:.N, position])
(pos.change <- (pos.before+pos.after)/2)
(first.start <- one.sequence$position[1])
(last.end <- one.sequence[.N, position])
pred.start <- c(first.start, pos.change)
pred.end <- c(pos.change, last.end)
subtrain.data[, change := c(NA, pos.change)]
binseg <- binsegRcpp::binseg_normal(
  subtrain.data$logratio,K.max)
(segs.dt <- coef(binseg))
head(segs.dt)
segs.dt[, pos.start := pred.start[start] ]
subtrain.data[10:11, position]
segs.dt[, pos.end := pred.end[end] ]

library(ggplot2)
ggplot()+
  geom_point(aes(
    position, logratio),
    data=one.sequence)

ggplot()+
  geom_point(aes(
    position, logratio),
    data=subtrain.data)

model.color <- "red"
ggplot()+
  geom_point(aes(
    position, logratio),
    data=subtrain.data)+
  geom_vline(aes(
    xintercept=pos.start),
    color=model.color,
    size=1,
    data=segs.dt[start>1])+
  geom_segment(aes(
    pos.start, mean,
    xend=pos.end, yend=mean),
    data=segs.dt,
    color=model.color)+
  facet_grid(segments ~ .)

## What if we put the changepoint at the end/start rather than the middle?
change.list <- list(
  before=pos.before,
  middle=(pos.before+pos.after)/2)
loss.options.dt.list <- list()
for(pos.name in names(change.list)){
  pos.change <- change.list[[pos.name]]
  pred.start <- c(first.start, pos.change+1)
  pred.end <- c(pos.change, last.end)
  segs.dt[, pos.start := pred.start[start] ]
  segs.dt[, pos.end := pred.end[end] ]
  ## DT1[DT2] is a join. DT[i,j, by]
  ## columns of DT1 can be referred to as x.colName
  ## columns of DT2 can be referred to as i.colName
  ## on=.(DT1 column >= == <= DT2 column, ...)
  data.and.means <- one.sequence[
    segs.dt,
    data.table(
      logratio, segments, set, mean),
    on=.(position >= pos.start, position <= pos.end)]
  set.loss <- data.and.means[, .(
    squared.error=sum((logratio-mean)^2),
    n.data=.N
  ), by=.(set, segments)]
  if(!all.equal(
    set.loss[set=="subtrain", squared.error]
    ,binseg$loss)){
    stop("subtrain error computed incorrectly")
  }
  loss.options.dt.list[[pos.name]] <- data.table(
    pos.name,
    set.loss)
}
loss.options.dt <- do.call(rbind, loss.options.dt.list)

min.dt <- loss.options.dt[
, .SD[which.min(squared.error)],
  by=.(set, pos.name)]
ggplot()+
  geom_line(aes(
    segments, squared.error, color=set),
    size=2,
    data=loss.options.dt)+
  geom_point(aes(
    segments, squared.error, fill=set),
    data=min.dt,
    size=5,
    shape=21)+
  facet_grid(pos.name ~ .)

label.dt <- data.table(neuroblastoma$annotations)
for(seq.i in 1:nrow(label.dt)){
  label.row <- label.dt[seq.i]
  one.sequence <- data.dt[
    label.row, on=.(profile.id, chromosome)]
  model <- binsegRcpp::binseg_normal(
    one.sequence$logratio, K.max)
  penaltyLearning::modelSelection(model, complexity="segments")
}
