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
