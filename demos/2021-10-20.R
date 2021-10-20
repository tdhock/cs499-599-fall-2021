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

hmm.spec <- depmixS4::depmix(
  logratio ~ 1, data=data.dt, nstates=2)
depmixS4::getpars(hmm.spec)
hmm.learned <- depmixS4::fit(hmm.spec)
depmixS4::getpars(hmm.learned)
viterbi.state.vec <- hmm.learned@posterior[,1]
