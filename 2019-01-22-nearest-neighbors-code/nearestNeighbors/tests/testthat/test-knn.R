library(testthat)
library(nearestNeighbors)
data(mixture.example, package="ElemStatLearn")

test_that("NN is same as R computation for one test input", {
  prob.i <- 4
  max.neighbors <- 20L
  C.pred.vec <- with(mixture.example, Predict1toMaxNeighbors(
    x, y, x[prob.i,], max.neighbors))
  R.pred.vec <- rep(NA, max.neighbors)
  for(neighbors in 1:max.neighbors){
    res.mat <- t(t(mixture.example$x)-mixture.example$x[prob.i,])
    (dist.vec <- sqrt(rowSums(res.mat * res.mat)))
    (sorted.indices <- order(dist.vec))
    head(dist.vec[sorted.indices])
    neighbor.indices <- sorted.indices[1:neighbors]
    (neighbor.y <- mixture.example$y[neighbor.indices])
    R.pred.vec[neighbors] <- mean(neighbor.y)
  }
  rbind(C.pred.vec, R.pred.vec)
  expect_equal(C.pred.vec, R.pred.vec)
})

test_that("NN Matrix predictions same as class::knn", {
  max.neighbors <- 2L
  C.pred.mat <- with(mixture.example, Predict1toMaxNeighborsMatrix(
    x, y, x, max.neighbors))
  R.pred.mat <- matrix(NA, nrow(C.pred.mat), ncol(C.pred.mat))
  for(neighbors in 1:max.neighbors){
    pred.fac <- with(mixture.example, class::knn(
      x, x, y, k=neighbors, prob=TRUE))
    prob.winning <- attr(pred.fac, "prob")
    R.pred.mat[,neighbors] <- ifelse(
      pred.fac=="1", prob.winning, 1-prob.winning)
  }
  expect_equal(C.pred.mat, R.pred.mat)
})


