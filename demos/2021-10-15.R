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

max.segments <- 5
DP.model <- jointseg::Fpsn(
  data.dt[["logratio"]], max.segments)
str(DP.model)
(DP.loss <- data.table(
  algorithm="DP",
  segments=1:max.segments,
  loss=DP.model[["J.est"]]))
binseg.model <- binsegRcpp::binseg_normal(
  data.dt[["logratio"]], max.segments)
str(binseg.model)
(binseg.loss <- data.table(
  algorithm="binseg",
  segments=1:max.segments,
  loss=binseg.model[["loss"]]))
(both.loss <- rbind(DP.loss, binseg.loss))

library(ggplot2)
ggplot()+
  geom_line(aes(
    x=segments,
    y=loss,
    size=algorithm,
    color=algorithm),
    data=both.loss)+
  scale_size_manual(values=c(binseg=2, DP=1))+
  scale_color_manual(values=c(binseg="red", DP="black"))

selected.segments <- 3L
coef(binseg.model, selected.segments)

(end.mat <- DP.model[["t.est"]])
(n.segments.mat <- row(end.mat))
DP.segs <- data.table(
  segments=as.integer(n.segments.mat),
  end=as.integer(end.mat)
)[!is.na(end)][order(segments, end)]
# data table has segments, end columns, also need mean, start columns to plot with geom_segment.
for(segment in DP.segs$segments){
  print(segment)
}
c(1, DP.segs$end[-nrow(DP.segs)])
c(1, DP.segs[-.N, end])
unique(DP.segs$segments) # loop?
DP.segs[, start := c(1, 1+end[-.N]), by=segments]
DP.segs[, start := 1+c(0, end[-.N]), by=segments]
DP.segs
DP.segs[, sum := sum(data.dt[start:end, logratio])]
cum.vec <- cumsum(data.dt[["logratio"]])
head(cum.vec)
head(data.dt[["logratio"]])
cum.vec[1]
cum.vec[0]
DP.segs[, sum := cum.vec[end]-cum.vec[start-1] ]
DP.segs[, sum := cum.vec[end]-ifelse(start==1, 0, cum.vec[start-1]) ] ##??

cum.vec <- c(0, cumsum(data.dt[["logratio"]]))
DP.segs[, sum := cum.vec[end+1]-cum.vec[start] ]
DP.segs[, mean := sum/(end-start+1)]

ggplot()+
  geom_point(aes(
    data.i, logratio),
    data=data.dt)+
  geom_segment(aes(
    start, mean,
    xend=end, yend=mean),
    data=DP.segs,
    color="red")
