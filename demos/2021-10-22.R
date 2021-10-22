data(neuroblastoma, package="neuroblastoma")
library(data.table)
(nb.dt <- data.table(neuroblastoma$profiles))
(data.dt <- nb.dt[profile.id=="4" & chromosome=="2"])

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

n.states <- 2
hmm.spec <- depmixS4::depmix(
  logratio ~ 1, data=data.dt, nstates=n.states)
depmixS4::getpars(hmm.spec)
hmm.learned <- depmixS4::fit(hmm.spec)
learned.parameters <- depmixS4::getpars(hmm.learned)

## Q2.
(lik.dt <- data.table(
  n.states,
  neg.log.lik=-depmixS4::logLik(hmm.learned)))

## Q3.
data.dt[, best.state := factor(hmm.learned@posterior[,1]) ]
ggplot()+
  geom_point(aes(
    data.i, logratio, color=best.state),
    data=data.dt)

data.dt[, .(data.i, best.state)]

diff()#lagged differences.
rle() #run length encoding.
rle.list <- rle(as.integer(data.dt$best.state))
cumsum(rle.list$lengths)
change.dt <- data.table(
  end=which(diff(as.integer(data.dt$best.state)) != 0))
ggplot()+
  geom_point(aes(
    data.i, logratio, color=best.state),
    data=data.dt)+
  geom_vline(aes(
    xintercept=end+0.5),
    data=change.dt)

(mean.sd.params <- learned.parameters[
  names(learned.parameters) %in% c("(Intercept)", "sd")])
param.mat <- matrix(
  mean.sd.params, n.states, byrow=TRUE,
  dimnames=list(
    state=1:n.states,
    parameter=c("mean", "sd")))
seg.dt <- data.table(
  state=rle.list$values,
  end=cumsum(rle.list$lengths),
  param.mat[rle.list$values,])
seg.dt <- with(rle.list, data.table(
  state=values,
  end=cumsum(lengths),
  param.mat[values,]))
seg.dt[, start := 1+c(0, end[-.N])]
seg.dt

ggplot()+
  geom_point(aes(
    data.i, logratio, color=best.state),
    data=data.dt)+
  geom_vline(aes(
    xintercept=end+0.5),
    data=change.dt)+
  geom_segment(aes(
    x=start-0.5, xend=end+0.5,
    y=mean, yend=mean),
    data=seg.dt)+
  geom_rect(aes(
    xmin=start-0.5,
    xmax=end+0.5,
    ymin=mean-sd,
    ymax=mean+sd),
    alpha=0.5,
    data=seg.dt)

cpt.model <- changepoint::cpt.meanvar(
  data.dt$logratio,
  method="SegNeigh", penalty="Manual",
  Q=nrow(seg.dt))
(segment.ends <- cpt.model@cpts)
slotNames(cpt.model)
cpt.segs <- do.call(data.table, cpt.model@param.est)
cpt.segs[, sd := sqrt(variance)]
cpt.segs[, start := TODO]
cpt.segs[, end := segment.ends]
cpt.segs
