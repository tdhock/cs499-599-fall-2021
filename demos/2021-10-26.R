# Question 1.1
data(neuroblastoma, package="neuroblastoma")
library(data.table)
data.dt <- data.table(neuroblastoma$profiles)
one.sequence <- data.dt[chromosome=="2" & profile.id=="79"]
sets <- c("train", "validation")
one.sequence[, set := sample(rep(sets,length.out=nrow(one.sequence)))]
(table(one.sequence[,set]))
# Question 1.2
K.max <- 10
train.data <- one.sequence[set == "train"]
binseg <- binsegRcpp::binseg_normal(train.data$logratio,K.max)
segs.dt <- coef(binseg)

