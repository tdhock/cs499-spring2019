
devtools::install_github(
  "mertayD/Coding_Project_1_NN",
  build_vignettes=TRUE,
  force=TRUE,
  local=FALSE)#to remove object files mistakenly put in repo.
library(codungProject1)

devtools::install_github(
  "Suttungr/ML-Project1",
  build_vignettes=TRUE,
  force=TRUE,
  local=FALSE)#to remove object files mistakenly put in repo.
library(nearestNeighbors)

devtools::install_github(
  "SixianZhang/CS499-Coding-Project-1",
  build_vignettes=TRUE,
  force=TRUE,
  local=FALSE)#to remove object files mistakenly put in repo.
library(NearestNeighbors)

data(mixture.example, package="ElemStatLearn")

catch <- function(code){
  tryCatch(code, error=identity)
}

max.neighbors <- 200L
n.test <- 2
pred.mat <- catch({
  NN1toKmaxPredict(
    mixture.example$x, mixture.example$y,
    mixture.example$x[1:n.test,,drop=FALSE], max.neighbors)
})

xtest <- mixture.example$x[1,,drop=FALSE]
dist.vec <- colSums(abs(t(mixture.example$x) - as.numeric(xtest)))
sorted.index.vec <- order(dist.vec)
R.pred.vec <- cumsum(mixture.example$y[sorted.index.vec])/(1:max.neighbors)
C.pred.vec <- catch({
  NN1toKmaxPredict(
    mixture.example$x, mixture.example$y,
    xtest, max.neighbors)
})


nn1.pred.vec <- catch({
  NN1toKmaxPredict(
    mixture.example$x, mixture.example$y,
    mixture.example$x, 1L)
})


cv.xnew.pred <- catch({
  cv.fit <- NNLearnCV(mixture.example$x, mixture.example$y)
  cv.fit$predict(mixture.example$xnew)
})

set.seed(1)
n.folds <- 10
cv.fold.vec <- sample(rep(1:n.folds, l=length(mixture.example$y)))
cv.max.neighbors <- 100L
cv.fold.xnew.pred <- catch({
  cv.fold.fit <- NNLearnCV(
    mixture.example$x, mixture.example$y, cv.max.neighbors, cv.fold.vec)
  cv.fold.fit$predict(mixture.example$xnew)
})

## 2 points for each of the following that are TRUE.
rbind(
  "your function returns a valid numeric prediction matrix with the expected dimensions"=is.matrix(pred.mat) && is.numeric(pred.mat) && nrow(pred.mat)==n.test && ncol(pred.mat)==max.neighbors,
  "all 1NN predictions are correct for training data"=isTRUE(all.equal(as.numeric(nn1.pred.vec), as.numeric(mixture.example$y))),
  "correct predictions using L1/Manhattan distance"=isTRUE(all.equal(as.numeric(R.pred.vec), as.numeric(C.pred.vec))),
  "correct dim/type for NNLearnCV prediction matrix when no fold vector given"=is.numeric(cv.xnew.pred) && length(cv.xnew.pred)==nrow(mixture.example$xnew),
  "correct dim/type for NNLearnCV prediction matrix when fold vector given"=is.numeric(cv.fold.xnew.pred) && length(cv.fold.xnew.pred)==nrow(mixture.example$xnew))

