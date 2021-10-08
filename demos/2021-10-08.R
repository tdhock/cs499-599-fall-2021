data(neuroblastoma, package="neuroblastoma")
library(data.table)
ls()
(nb.dt <- data.table(neuroblastoma$profiles))
(data.dt <- nb.dt[profile.id=="1" & chromosome=="1"])

library(ggplot2)
ggplot()+
  geom_point(aes(
    position, logratio),
    data=data.dt)

max.segments <- 20
normal.model <- binsegRcpp::binseg_normal(
  data.dt[["logratio"]], max.segments)
normal.model

ggplot()+
  geom_point(aes(
    segments, loss),
    data=normal.model)

plot(normal.model)

selected.segments <- 5
str(selected.segments)
selected.segments <- 5L
str(selected.segments)
selected.segments <- as.integer(5)
str(selected.segments)
(segs.dt <- coef(normal.model, selected.segments))

## For simplicity instead of x=position (as in problem 1), use x=seq_along(logratio)
model.color <- "blue"
(vline.dt <- segs.dt[2:selected.segments])
(vline.dt <- segs.dt[2:.N])
(vline.dt <- segs.dt[start>1])
ggplot()+
  geom_point(aes(
    x=seq_along(logratio),
    y=logratio),
    data=data.dt)+
  geom_segment(aes(
    x=start,
    y=mean,
    xend=end,
    yend=mean),
    color=model.color,
    size=2,
    data=segs.dt)+
  geom_vline(aes(
    xintercept=start-0.5),
    color=model.color,
    size=1,
    data=vline.dt)
## Use geom_segment to show segment means and geom_vline to show changepoints.
