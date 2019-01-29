Predict1toMaxNeighborsMatrixMultiClass <- function
### Nearest neighbor predictions for an entire test matrix, and
### multi-class classification problem.
(X.train, y.train, X.test, max.neighbors){
  if(!all(
    is.matrix(X.train),
    is.numeric(X.train)
  )){
    stop("X.train must be a numeric matrix")
  }
  if(!all(
    is.integer(y.train),
    length(y.train)==nrow(X.train),
    0 <= y.train
  )){
    stop("y.train must be integer vector of size nrow(X.train)")
  }
  max.y <- max(y.train)
  n.classes <- max.y+1
  if(!all(
    is.matrix(X.test),
    is.numeric(X.test),
    ncol(X.test)==ncol(X.train)
  )){
    stop("x.test must be a numeric vector of size ncol(X.train)")
  }
  if(!all(
    is.integer(max.neighbors),
    length(max.neighbors)==1
  )){
    stop("max.neighbors must be an integer scalar")
  }
  res <- .C(
    "Predict1toMaxNeighborsMatrixMultiClass_interface",
    as.double(X.train), as.integer(y.train),
    nrow(X.train), ncol(X.train), as.integer(max.neighbors), nrow(X.test),
    as.integer(n.classes),
    as.double(t(X.test)),
    test.predictions=integer(max.neighbors*nrow(X.test)),
    PACKAGE="nearestNeighbors")
  matrix(res$test.predictions, nrow(X.test), max.neighbors, byrow=TRUE)
}


NearestNeighborsMultiClassCV <- structure(function
### Fit nearest neighbors MultiClass model using k-fold CV.
(input.mat, label.vec, max.neighbors, fold.vec, n.folds=5L, LAPPLY=parallel::mclapply){
  if(!all(
    is.matrix(input.mat),
    is.numeric(input.mat)
  )){
    stop("input.mat must be a numeric matrix")
  }
  if(!all(
    is.integer(label.vec),
    length(label.vec)==nrow(input.mat)
  )){
    stop("label.vec must be integer vector of size nrow(input.mat)")
  }
  if(missing(fold.vec)){
    if(!all(
      is.integer(n.folds),
      length(n.folds)==1,
      is.finite(n.folds),
      1 < n.folds,
      n.folds <= nrow(input.mat)
    )){
      stop("if fold.vec is unspecified, n.folds must be integer scalar")
    }
    fold.vec <- sample(rep(1:n.folds, l=nrow(input.mat)))
  }
  if(!all(
    is.integer(max.neighbors),
    length(max.neighbors)==1
  )){
    stop("max.neighbors must be an integer scalar")
  }
  OneFold <- function(validation.fold){
    is.train <- fold.vec != validation.fold
    pred.mat <- Predict1toMaxNeighborsMatrixMultiClass(
      input.mat[is.train,],
      label.vec[is.train],
      input.mat,
      max.neighbors)
    set.list <- list(train=is.train, validation=!is.train)
    loss.dt.list <- list()
    for(set.name in names(set.list)){
      is.set <- set.list[[set.name]]
      set.pred.mat <- pred.mat[is.set,]
      set.label.vec <- label.vec[is.set]
      loss <- colMeans(set.pred.mat != set.label.vec)
      loss.dt.list[[set.name]] <- data.table(
        validation.fold,
        set.name,
        neighbors=1:max.neighbors,
        loss)
    }
    do.call(rbind, loss.dt.list)
  }
  err.dt <- do.call(rbind, LAPPLY(unique(fold.vec), OneFold))
  mean.dt <- err.dt[set.name=="validation", list(
    mean.loss=mean(loss),
    sd.loss=sd(loss)
  ), by=list(neighbors, set.name)]
  selected.neighbors <- mean.dt[which.min(mean.loss), neighbors]
  list(
    fold.loss=err.dt,
    mean.loss=mean.dt,
    input.mat=input.mat,
    label.vec=label.vec,
    selected.neighbors=selected.neighbors,
    predict=function(test.input.mat){
      pred.mat <- Predict1toMaxNeighborsMatrixMultiClass(
        input.mat, label.vec, test.input.mat, selected.neighbors)
      pred.mat[, selected.neighbors]
    })
}, ex=function(){

  data(zip.train, package="ElemStatLearn")
  set.seed(1)
  zip.feature.mat <- zip.train[,-1]
  zip.label.vec <- as.integer(zip.train[,1])
  max.neighbors <- 20L

  library(nearestNeighbors)
  fit <- NearestNeighborsMultiClassCV(
    zip.feature.mat, zip.label.vec, max.neighbors)

  library(ggplot2)
  mean.loss <- data.table(fit$mean.loss, validation.fold="mean")
  ggplot()+
    facet_grid(validation.fold ~ .)+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    geom_line(aes(
      neighbors, loss, color=set.name, group=set.name),
      data=fit$fold.loss)+
    geom_line(aes(
      neighbors, mean.loss),
      data=mean.loss)+
    geom_point(aes(
      neighbors, mean.loss),
      data=mean.loss[neighbors==fit$selected.neighbors],
      shape=21,
      fill="white")

})

