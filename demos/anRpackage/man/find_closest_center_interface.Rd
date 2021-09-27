\name{find_closest_center_interface}
\docType{package}
\title{
  Find closest centers for each data point
}
\description{
  For each row in the data matrix,
  we compute the distances with all
  rows in the centers matrix,
  using the L2/euclidean distance.
}
\usage{
find_closest_center_interface(data_mat, centers_mat)
}
\examples{

  data.mat <- as.matrix(
    iris[, c("Petal.Length", "Petal.Width")])
  K <- 3
  set.seed(1)
  (centers.mat <- data.mat[sample(1:nrow(data.mat), K), ])
  (cluster.id.vec <-
    anRpackage::find_closest_center_interface(
      data.mat, centers.mat))

}
