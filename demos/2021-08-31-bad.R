destfile <- "zip.test.gz"
datasets.url <- "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/"
if(!file.exists(destfile)){
  file.link <- paste0(datasets.url, destfile)
  download.file(file.link, destfile)
}
test.dt <- data.table::fread(destfile, drop = 1)
utils::str(test.dt)
data.mat = as.matrix(test.dt)

## Three issues with code block below (see General Usage
## Rubric). 1. repeated logic, 2. Repeated constants (16), 3. rbind
## inside for loop is slow, see
## https://tdhock.github.io/blog/2017/rbind-inside-outside/
number.grids <- data.frame(row = rep(16:1, each=16),
                          col = rep(1:16, times=16),
                          intensity = data.mat[1,],
                          grid.number = 1)
for(num in 2:facets){
  number.grids <- rbind(number.grids, data.frame(row = rep(16:1, each=16),
                                  col = rep(1:16, times=16),
                                  intensity = data.mat[num,],
                                 grid.number = num))
}

number.grids.mat <- as.matrix(number.grids)

ggplot() +
  geom_tile(aes(
    x = col,
    y = row,
    fill = intensity),
    data = number.grids
  )+
  scale_fill_gradient(low="orange", high="black")+
  facet_wrap(vars(grid.number))


##-5 for repetitive code blocks / variable names that could be replaced by a loop or another less repetitive programming technique. solution: use for loop and a list with named elements, or another programming technique. CH29, CH4
##BAD:
result2 <- kmeans(X, 2); result3 <- kmeans(X, 3)
##GOOD:
result.list <- list(); for(n.clusters in 2:3) result.list[[paste(n.clusters)]] <- kmeans(X, n.clusters)

show.digits <- c(5,10,15)
first.row.visual <- data.table(
  image.order = show.digits[1],
  data=test.matrix[show.digits[1],],
  row = rep(N.pixels:1, each = N.pixels),
  col= rep(1:N.pixels, times = N.pixels))
second.row.visual <- data.table(
  image.order = show.digits[2],
  data=test.matrix[show.digits[2],],
  row = rep(N.pixels:1, each = N.pixels),
  col= rep(1:N.pixels, times = N.pixels))
third.row.visual <- data.table(
  image.order = show.digits[3],
  data=test.matrix[show.digits[3],],
  row = rep(N.pixels:1, each = N.pixels),
  col= rep(1:N.pixels, times = N.pixels))

## -5 for using multiple geoms in a ggplot when you could use a single geom with a bigger data set. CH35
## BAD:
geom_line(aes(x, y), data=DT[algorithm=="kmeans"], color="blue")+
geom_line(aes(x, y), data=DT[algorithm=="hclust"], color="red")
## GOOD:
geom_line(aes(x, y, color=algorithm), data=DT)+
scale_color_manual(values=c(kmeans="blue", hclust="red"))

ggplot() +
  geom_tile(data=first.row.visual,
            aes(x=col,y=row,fill=data))+
  geom_tile(data=second.row.visual,
            aes(x=col,y=row,fill=data))+
  geom_tile(data=third.row.visual,
            aes(x=col,y=row,fill=data))+
  facet_grid(.~image.order) +
  scale_fill_gradient(low='black',high = 'white')
