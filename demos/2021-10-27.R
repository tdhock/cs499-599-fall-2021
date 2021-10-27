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
(subtrain.dt <- data.dt[set=="subtrain"])
## where would the change be if this was the seg end?
subtrain.dt[, change.pos := {
  c(orig.row[-.N]+diff(orig.row)/2, NA)
}]
max.segments <- 10
(binseg.model <- binsegRcpp::binseg_normal(
  subtrain.dt[["logratio"]], max.segments))
(segments.dt <- coef(binseg.model))
(some.segs <- head(segments.dt))
subtrain.dt[some.segs$end]
