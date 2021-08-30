destfile <- "zip.train.gz"
datasets.url <-
  "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/"

if(!file.exists(destfile)){
  zip.url <- paste0(datasets.url, destfile)
  download.file(zip.url, destfile)
}

if(!requireNamespace("R.utils")){
  install.packages("R.utils")
}

zip.dt <- data.table::fread(file=destfile)
library(data.table)
zip.no.label = fread(file=destfile, drop=1)
dim(zip.no.label)
zip.no.label.mat <- as.matrix(zip.no.label)
zip.no.label.mat[1:4, 1:4]
zip.only.label = fread(file=destfile, select=1)
dim(zip.only.label)

library(ggplot2)
data(WorldBank, package="animint2")
wb.dt <- data.table(WorldBank)
wb1975 <- wb.dt[year==1975]
wb1970.75 <- wb.dt[1970 <= year & year <= 1975]

ggplot()+
  geom_point(aes(
    x = life.expectancy,
    y = fertility.rate,
    color = region),
    data = wb1975)

ggplot()+
  geom_point(aes(
    x = life.expectancy,
    y = log10(GDP.per.capita.Current.USD),
    color = region),
    data = wb1975)

ggplot()+
  geom_point(aes(
    x = life.expectancy,
    y = fertility.rate,
    color = region),
    data = wb1975) +
  geom_path(aes(
    x = life.expectancy,
    y = fertility.rate,
    color = region,
    group = country),
    data = wb1970.75)
    
ggplot(mapping=aes(
  x = life.expectancy,
  y = fertility.rate,
  color = region))+
  geom_point(
    data = wb1975) +
  geom_path(aes(
    group = country),
    data = wb1970.75)
    
ggplot(mapping=aes(
  x = life.expectancy,
  y = fertility.rate))+
  geom_point(
    data = wb1975) +
  geom_path(aes(
    group = country),
    data = wb1970.75)

points.dt.list <- list()
path.dt.list <- list()
for(show.year in c(1975, 1985, 1995)){
  points.dt.list[[paste(show.year)]] <-
    data.table(show.year, wb.dt[year==show.year])
  path.dt.list[[paste(show.year)]] <- data.table(
    show.year, wb.dt[show.year-5 <= year & year <= show.year])
}
(points.dt <- do.call(rbind, points.dt.list))
(path.dt <- do.call(rbind, path.dt.list))

ggplot(mapping=aes(
  x = life.expectancy,
  y = fertility.rate,
  color = region))+
  geom_point(
    data = points.dt) +
  geom_path(aes(
    group = country),
    data = path.dt)+
  facet_grid(. ~ show.year)

ggplot(mapping=aes(
  x = life.expectancy,
  y = fertility.rate,
  color = region))+
  geom_point(
    data = points.dt) +
  geom_path(aes(
    group = country),
    data = path.dt)+
  facet_grid(show.year ~ .)

ggplot(mapping=aes(
  x = life.expectancy,
  y = fertility.rate))+
  geom_point(
    data = points.dt) +
  geom_path(aes(
    group = country),
    data = path.dt)+
  facet_grid(show.year ~ region)

digit.dt <- data.table(
  row=rep(16:1, each=16),
  col=rep(1:16, times=16),
  intensity=zip.no.label.mat[1,])
ggplot()+
  geom_tile(aes(
    x = col,
    y = row,
    fill = intensity),
    data=digit.dt)+
  scale_fill_gradient(low="white", high="black")
