library(testthat)
library(nearestNeighbors)
data(zip.train, package="ElemStatLearn")

test_that("C++ and R agree", {
  some.zip <- zip.train[1:10,]
  some.feature.mat <- some.zip[,-1]
  some.label.vec <- as.integer(some.zip[,1])
  test.mat <- some.feature.mat[2, ,drop=FALSE]
  max.neighbors <- 10L
  pred.mat <- Predict1toMaxNeighborsMatrixMultiClass(
    some.feature.mat, some.label.vec,
    test.mat, max.neighbors)
  res.mat <- t(t(some.feature.mat)-as.numeric(test.mat))
  (dist.vec <- sqrt(rowSums(res.mat * res.mat)))
  (sorted.indices <- order(dist.vec))
  sorted.label.vec <- some.label.vec[sorted.indices]
  pred.vec <- integer(max.neighbors)
  for(neighbors in 1:max.neighbors){
    lab.tab <- sort(-table(sorted.label.vec[1:neighbors]))
    print(lab.tab)
    pred.vec[[neighbors]] <- as.integer(names(lab.tab)[1])
  }
  rbind(C=pred.mat, R=pred.vec)
  expect_equal(as.integer(pred.mat), as.integer(pred.vec))
})

if(FALSE){

  set.seed(1)
  zip.feature.mat <- zip.train[,-1]
  zip.label.vec <- as.integer(zip.train[,1])
  fold.vec <- sample(rep(1:5, l=length(zip.label.vec)))
  validation.fold <- 1
  is.train <- fold.vec != validation.fold
  pred.mat <- Predict1toMaxNeighborsMatrixMultiClass(
    zip.feature.mat[is.train,], zip.label.vec[is.train],
    zip.feature.mat, 50L)

  library(data.table)
  library(ggplot2)
  set.list <- list(train=is.train, validation=!is.train)
  set.dt.list <- list()
  for(set.name in names(set.list)){
    is.set <- set.list[[set.name]]
    set.pred.mat <- pred.mat[is.set,]
    set.label.vec <- zip.label.vec[is.set]
    set.dt.list[[set.name]] <- data.table(
      set.name,
      neighbors=1:ncol(set.pred.mat),
      loss=colMeans(set.pred.mat != set.label.vec))
  }
  set.dt <- do.call(rbind, set.dt.list)
  ggplot()+
    geom_line(aes(
      neighbors, loss, group=set.name, color=set.name),
      data=set.dt)

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

}
