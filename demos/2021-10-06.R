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
