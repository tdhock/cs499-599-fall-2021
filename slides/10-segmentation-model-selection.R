suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
set.colors <- c(subtrain="black", validation="red")
theme_set(
  theme_bw()+
    theme(
      panel.spacing=grid::unit(0, "lines"),
      text=element_text(size=20)))
geom.text.size <- 5
data(neuroblastoma, package="neuroblastoma")
a <- function(min, max, annotation){
  data.frame(min, max, annotation)
}
nb.ann <- rbind(
  data.table(pid.chr="4.11", rbind(
    a(0, 100, "0changes"),
    a(130, 145, "1change"))),
  data.table(pid.chr="4.2", rbind(
    a(0, 50, "1change"),
    a(120, 155, "0changes"))))
setkey(nb.ann, pid.chr)
nb.dt <- data.table(neuroblastoma[["profiles"]])
nb.dt[, data.i := rank(position), keyby=.(profile.id, chromosome)]
nb.dt[, pid.chr := paste0(profile.id, ".", chromosome)]
setkey(nb.dt, pid.chr)

max.segments <- 4
data.dt.list <- list()
model.dt.list <- list()
cv.dt.list <- list()
penalty.dt.list <- list()
selected.dt.list <- list()
model.segs.list <- list()
label.error.list <- list()
subtrain.segs.list <- list()
some.ids <- c(
  ##"4.2",
  "4.11")
for(id in some.ids){
  chrom.dt <- nb.dt[id]
  fit.list <- jointseg::Fpsn(chrom.dt$logratio, max.segments)
  loss <- fit.list[["J.est"]]
  n.data <- nrow(chrom.dt)
  first.pos <- chrom.dt[1, position]
  last.pos <- chrom.dt[.N, position]
  pos.between <- chrom.dt[, position[-1]-diff(position)/2]
  chrom.dt[, start.pos := c(first.pos, pos.between)]
  chrom.dt[, end.pos := c(pos.between, last.pos)]
  data.dt.list[[id]] <- data.table(sequence.id=id, chrom.dt)
  chrom.segs <- with(fit.list, data.table(
    end=as.integer(t.est),
    segments=as.integer(row(t.est)))
    )[is.finite(end)][order(segments, end)]
  cum.vec <- c(0, cumsum(chrom.dt$logratio))
  chrom.segs[, start := c(0, end[-.N])+1, by=segments]
  chrom.segs[, changes := segments-1]
  chrom.segs[, mean := (cum.vec[end+1]-cum.vec[start])/(end-start+1) ]
  for(start.or.end in c("start", "end")){
    start.or.end.pos <- paste0(start.or.end, ".pos")
    indices <- chrom.segs[[start.or.end]]
    set(
      chrom.segs,
      j=start.or.end.pos,
      value=chrom.dt[[start.or.end.pos]][indices])
  }
  chrom.ann <- nb.ann[id]
  print(chrom.ann)
  change.dt <- chrom.segs[start>1]
  change.dt[, pid.chr := id]
  show.models <- data.table(
    pid.chr=id,
    sequence.id=id,
    segments=1:max.segments)
  show.models[, changes := segments-1]
  err.list <- penaltyLearning::labelError(
    show.models,
    chrom.ann,
    change.dt,
    change.var="start",
    problem.vars="pid.chr",
    model.vars="segments")
  label.error.list[[id]] <- err.list$label.errors
  model.segs.list[[id]] <- data.table(sequence.id=id, chrom.segs)
}
plotData <- function(id){
  ggplot()+
    scale_x_continuous(
      "Position/index in data sequence")+
    scale_y_continuous(
      "logratio (approximate DNA copy number)")+
    geom_point(aes(
      data.i, logratio),
      data=data.dt.list[[id]])
}
plotData("4.2")
plotData("4.11")

scale.fill <- scale_fill_manual(
  "label",
  values=c("1change"="violet", "0changes"="orange"))
plotDataLabel <- function(id){
  pro.ann <- nb.ann[id]
  ggplot()+
    scale_x_continuous(
      "Position on chromosome (bases)")+
    scale_y_continuous(
      "logratio (approximate DNA copy number)")+
    geom_rect(aes(
      xmin=min, xmax=max,
      ymin=-Inf, ymax=Inf,
      fill=annotation),
      alpha=0.5,
      data=pro.ann)+
    geom_point(aes(
      data.i, logratio),
      shape=1,
      data=data.dt.list[[id]])+
    scale.fill
}
plotDataLabel("4.11")
model.color <- "blue"
plotDataLabelModels <- function(id){
  seg.dt <- model.segs.list[[id]]
  change.dt <- seg.dt[start>1]
  data.dt <- data.dt.list[[id]]
  ggplot()+
    facet_grid(segments ~ .)+
    geom_segment(aes(
      start, mean,
      xend=end, yend=mean),
      color=model.color,
      size=2,
      data=seg.dt)+
    geom_vline(aes(
      xintercept=start),
      data=change.dt,
      size=1,
      color=model.color)+
    scale_x_continuous(
      "Position on chromosome (bases)")+
    scale_y_continuous(
      "logratio (approximate DNA copy number)")+
    geom_point(aes(
      data.i, logratio),
      shape=1,
      data=data.dt)+
    geom_rect(aes(
      xmin=min, xmax=max,
      ymin=-Inf, ymax=Inf,
      fill=annotation),
      alpha=0.5,
      data=nb.ann[id])+
    geom_rect(aes(
      xmin=min, xmax=max,
      ymin=-Inf, ymax=Inf,
      linetype=status),
      color="black",
      size=2,
      fill="transparent",
      data=label.error.list[[id]])+
    scale_linetype_manual(
      "error type",
      values=c(
        correct=0,
        "false negative"=3,
        "false positive"=1))+
    scale.fill
}
plotDataLabelModels("4.11")

seg.dt <- do.call(rbind, model.segs.list)[segments>1]
err.dt <- do.call(rbind, label.error.list)[segments>1]
change.dt <- seg.dt[start>1]
data.dt <- do.call(rbind, data.dt.list)
ggplot()+
  facet_grid(changes ~ sequence.id, labeller=label_both, scales="free", space="free")+
  geom_rect(aes(
    xmin=min, xmax=max,
    ymin=-Inf, ymax=Inf,
    fill=annotation),
    alpha=0.5,
    color="grey",
    data=err.dt)+
  geom_segment(aes(
    start-0.5, mean,
    xend=end+0.5, yend=mean),
    color=model.color,
    size=2,
    data=seg.dt)+
  geom_vline(aes(
    xintercept=start-0.5),
    data=change.dt,
    size=1,
    color=model.color)+
  scale_x_continuous(
    "Index/position in data sequence")+
  scale_y_continuous(
    "logratio (approximate DNA copy number)")+
  geom_point(aes(
    data.i, logratio),
    shape=1,
    color="grey30",
    data=data.dt)+
  geom_rect(aes(
    xmin=min, xmax=max,
    ymin=-Inf, ymax=Inf,
    linetype=status),
    color="black",
    size=2,
    fill="transparent",
    data=err.dt)+
  scale_linetype_manual(
    "error type",
    values=c(
      correct=0,
      "false negative"=3,
      "false positive"=1))+
  scale.fill

gg <- ggplot()+
  facet_grid(changes ~ ., labeller=label_both, scales="free", space="free")+
  geom_rect(aes(
    xmin=min, xmax=max,
    ymin=-Inf, ymax=Inf,
    fill=annotation),
    alpha=0.5,
    color="grey",
    data=err.dt)+
  geom_segment(aes(
    start-0.5, mean,
    xend=end+0.5, yend=mean),
    color=model.color,
    size=2,
    data=seg.dt)+
  geom_vline(aes(
    xintercept=start-0.5),
    data=change.dt,
    size=1,
    color=model.color)+
  scale_x_continuous(
    "Index/position in data sequence along chromosome")+
  scale_y_continuous(
    "Noisy DNA copy number signal")+
  geom_rect(aes(
    xmin=min, xmax=max,
    ymin=-Inf, ymax=Inf,
    linetype=status),
    color="black",
    size=2,
    fill="transparent",
    data=err.dt)+
  geom_point(aes(
    data.i, logratio),
    shape=21,
    fill="grey",
    color="grey30",
    data=data.dt)+
  ## geom_line(aes(
  ##   data.i, logratio),
  ##   color="grey30",
  ##   data=data.dt)+
  scale_linetype_manual(
    "error type",
    values=c(
      correct=0,
      "false negative"=3,
      "false positive"=1))+
  scale.fill
png("10-segmentation-model-selection.png", height=5, width=10, res=200, units="in")
print(gg)
dev.off()
