data(neuroblastoma, package="neuroblastoma")
library(data.table)
(nb.dt <- data.table(neuroblastoma$profiles))
(data.dt <- nb.dt[profile.id=="4" & chromosome=="2"])

library(ggplot2)
ggplot()+
  geom_point(aes(
    position, logratio),
    data=data.dt)

set.seed(1)
data.dt[, set := sample(
  rep(c("subtrain","validation"), l=.N))]
ggplot()+
  geom_point(aes(
    position, logratio, color=set),
    data=data.dt)

data.dt[, orig.row := .I]
data.dt

ggplot()+
  geom_point(aes(
    orig.row, logratio, color=set),
    data=data.dt)

(subtrain.dt <- data.dt[set=="subtrain"])
## where would the change be if this was the seg end?
change.indices <- subtrain.dt[, c(
  floor(orig.row[-.N]+diff(orig.row)/2)+0.5)]
start.indices <- c(1, change.indices)
end.indices <- c(change.indices, nrow(data.dt))
max.segments <- 3
(binseg.model <- binsegRcpp::binseg_normal(
  subtrain.dt[["logratio"]], max.segments))
(segments.dt <- coef(binseg.model))
segments.dt[, start.index := start.indices[start] ]
segments.dt[, end.index := end.indices[end] ]
segments.dt

model.color <- "blue"
ggplot()+
  geom_point(aes(
    orig.row, logratio, color=set),
    data=data.dt)+
  geom_segment(aes(
    start.index, mean,
    xend=end.index, yend=mean),
    color=model.color,
    size=1,
    data=segments.dt)+
  facet_grid(segments ~ .)

names(data.dt)
names(segments.dt)
## DT1[DT2, on=.(columnFromDT1 <= == >= columnFromDT2,...)] join.
data.and.means <- data.dt[
  segments.dt,
  data.table(logratio, mean, segments, set),
  on=.(orig.row >= start.index, orig.row <= end.index)]
## O( S N ) S=models, N=data
nrow(data.dt)*3
(error.dt <- data.and.means[, .(
  loss=sum((mean-logratio)^2)
), by=.(set, segments)])
subtrain.loss <- error.dt[set=="subtrain", loss]
if(!identical(
  all.equal(binseg.model$loss, subtrain.loss), TRUE
)){
  stop("subtrain loss not correct")
}

## join and summarize in one step.
error.segs <- data.dt[set=="validation"][
  segments.dt,
  data.table(segments, loss.seg=sum((mean-logratio)^2)),
  by=.EACHI,
  on=.(orig.row >= start.index, orig.row <= end.index)]
(error.efficient <- error.segs[, .(
  loss=sum(loss.seg)
), by=segments])
error.dt[set=="validation"]

## AIC/BIC (use full data set).
(full.model <- binsegRcpp::binseg_normal(
  data.dt[["logratio"]], max.segments))
penalty <- 2
full.model[, loss + penalty*segments]

for(penalty.name in c("AIC", "BIC")){
  penalty <- TODO
  full.model[, loss + penalty*segments]
}
