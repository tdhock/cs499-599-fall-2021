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

end.mat <- DP.model[["t.est"]]
row(end.mat)
data.table(
  segments=as.integer(row(end.mat)),
  end=as.integer(end.mat)
)[!is.na(end)][order(segments, end)]
# data table has segments, end columns, also need mean, start columns to plot with geom_segment.
