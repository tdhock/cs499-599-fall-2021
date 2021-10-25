data(neuroblastoma, package="neuroblastoma")
library(data.table)
(nb.dt <- data.table(neuroblastoma$profiles))
(data.dt <- nb.dt[profile.id=="4" & chromosome=="2"])

library(ggplot2)
ggplot()+
  geom_point(aes(
    position, logratio),
    data=data.dt)

N.data <- nrow(data.dt)
sampled.data <- sample(data.dt$logratio, N.data/2)
plot(sampled.data) #out of order.

set.seed(1)
data.dt[, set := sample(
  rep(c("subtrain","validation"), l=.N))]
ggplot()+
  geom_point(aes(
    position, logratio, color=set),
    data=data.dt)

subtrain.dt <- data.dt[set=="subtrain"]
max.segments <- 10
(binseg.model <- binsegRcpp::binseg_normal(
  subtrain.dt[["logratio"]], max.segments))
segments.dt <- coef(binseg.model)
