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

hmm.spec <- depmixS4::depmix(
  logratio ~ 1, data=data.dt, nstates=100)
model.or.null <- tryCatch({
  depmixS4::fit(hmm.spec)
}, error=function(e){
  NULL
})
if(!is.null(model.or.null)){
  Do the homework
}

set.seed(1)
n.states <- 3
hmm.spec <- depmixS4::depmix(
  logratio ~ 1, data=data.dt, nstates=n.states)
depmixS4::getpars(hmm.spec)
hmm.learned <- depmixS4::fit(hmm.spec)
(log.lik <- depmixS4::logLik(hmm.learned))
data.table(n.states, log.lik)

(learned.param.vec <- depmixS4::getpars(hmm.learned))
learned.param.list <- list()
param.type.vec <- c(mean="(Intercept)", sd="sd")
for(param.type in names(param.type.vec)){
  param.name <- param.type.vec[[param.type]]
  learned.param.list[[param.type]] <-
    learned.param.vec[names(learned.param.vec)==param.name]
}
learned.param.list

data.table(hmm.learned@posterior)
data.dt[, best.state := factor(hmm.learned@posterior[,1]) ]
state.rle <- rle(as.integer(data.dt$best.state))
str(state.rle)
seg.dt <- data.table(
  seg.size=state.rle$lengths,
  cluster.i=state.rle$values)
seg.dt[, end := cumsum(seg.size)]
seg.dt[, start := c(1, end[-.N]+1)]
for(param.type in names(param.type.vec)){
  param.name <- param.type.vec[[param.type]]
  param.values <- 
    learned.param.vec[names(learned.param.vec)==param.name]
  set(
    seg.dt,
    j=param.type,
    value=param.values[seg.dt$cluster.i])
}
seg.dt

seg.dt[, state := factor(cluster.i)]
ggplot()+
  geom_rect(aes(
    xmin=start-0.5,
    xmax=end+0.5,
    ymin=mean-sd,
    ymax=mean+sd,
    fill=state),
    alpha=0.5,
    data=seg.dt)+
  geom_vline(aes(
    xintercept=start-0.5),
    data=seg.dt[-1])+
  geom_segment(aes(
    start-0.5, mean,
    xend=end+0.5, yend=mean,
    color=state),
    size=2,
    data=seg.dt)+
  geom_point(aes(
    data.i, logratio, fill=best.state),
    shape=21,
    data=data.dt)

cpt.model <- suppressWarnings(changepoint::cpt.meanvar(
  data.dt$logratio,
  method="SegNeigh", penalty="Manual", Q=nrow(seg.dt)))
cpt.model@param.est
(cpt.segs <- data.table(
  end=cpt.model@cpts))
cpt.segs[, start := c(1, end[-.N]+1)]
cpt.segs
cpt.model@param.est$sd <-
  sqrt(cpt.model@param.est$variance)
for(param.type in names(cpt.model@param.est)){
  param.values <- cpt.model@param.est[[param.type]]
  set(
    cpt.segs,
    j=param.type,
    value=param.values)
}
ggplot()+
  geom_rect(aes(
    xmin=start-0.5,
    xmax=end+0.5,
    ymin=mean-sd,
    ymax=mean+sd),
    alpha=0.5,
    data=cpt.segs)+
  geom_vline(aes(
    xintercept=start-0.5),
    data=cpt.segs[-1])+
  geom_segment(aes(
    start-0.5, mean,
    xend=end+0.5, yend=mean),
    size=2,
    data=cpt.segs)+
  geom_point(aes(
    data.i, logratio),
    shape=21,
    data=data.dt)+
  geom_rect(aes(
    xmin=start-0.5,
    xmax=end+0.5,
    ymin=mean-sd,
    ymax=mean+sd,
    fill=state),
    alpha=0.5,
    data=seg.dt)+
  geom_vline(aes(
    xintercept=start-0.5),
    data=seg.dt[-1])+
  geom_segment(aes(
    start-0.5, mean,
    xend=end+0.5, yend=mean,
    color=state),
    size=2,
    data=seg.dt)
  
data.dt[, mean_HMM := TODO]
data.dt[, sd_HMM := TODO]
data.dt[, mean_cpt := TODO]
data.dt[, sd_cpt := TODO]
for(model.name in c("cpt", "HMM")){
  mean.vec <- data.dt[[paste0("mean_", model.name)]]
  sd.vec <- TODO
  neg.log.lik <- -sum(dnorm(
    data.dt$logratio, mean.vec, sd.vec, log=TRUE))
}
