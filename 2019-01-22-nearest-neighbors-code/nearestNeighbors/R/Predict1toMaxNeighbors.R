Predict1toMaxNeighbors <- function(X.train, y.train, x.test, max.neighbors){
  if(!all(
    is.matrix(X.train),
    is.numeric(X.train)
  )){
    stop("X.train must be a numeric matrix")
  }
  if(!all(
    is.numeric(y.train),
    length(y.train)==nrow(X.train)
  )){
    stop("y.train must be a numeric vector of size nrow(X.train)")
  }
  if(!all(
    is.numeric(x.test),
    length(x.test)==ncol(X.train)
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
    "Predict1toMaxNeighbors_interface",
    as.double(X.train), as.double(y.train),
    nrow(X.train), ncol(X.train), as.integer(max.neighbors),
    as.double(x.test),
    test.predictions=double(max.neighbors),
    PACKAGE="nearestNeighbors")
  res$test.predictions
}

Predict1toMaxNeighborsMatrix <- function
### Nearest neighbor predictions for an entire test matrix.
(X.train, y.train, X.test, max.neighbors){
  if(!all(
    is.matrix(X.train),
    is.numeric(X.train)
  )){
    stop("X.train must be a numeric matrix")
  }
  if(!all(
    is.numeric(y.train),
    length(y.train)==nrow(X.train)
  )){
    stop("y.train must be a numeric vector of size nrow(X.train)")
  }
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
    "Predict1toMaxNeighborsMatrix_interface",
    as.double(X.train), as.double(y.train),
    nrow(X.train), ncol(X.train), as.integer(max.neighbors), nrow(X.test),
    as.double(t(X.test)),
    test.predictions=double(max.neighbors*nrow(X.test)),
    PACKAGE="nearestNeighbors")
  matrix(res$test.predictions, nrow(X.test), max.neighbors, byrow=TRUE)
}

loss.function.list <- list(
  mse=function(pred, label){
    (pred-label)^2
  },
  misclassification=function(pred.prob, label){
    pred.label <- ifelse(pred.prob<0.5, 0, 1)
    pred.label != label
  })

NearestNeighborsCVError <- structure(function
### Nearest neighbor cross-validation error.
(input.mat, label.vec, max.neighbors, fold.vec, n.folds=5L, loss.function=NULL){
  if(is.null(loss.function)){
    loss.name <- if(all(label.vec %in% c(0,1))){
      "misclassification"
    }else{
      "mse"
    }
    loss.function <- loss.function.list[[loss.name]]
  }
  if(!all(
    is.matrix(input.mat),
    is.numeric(input.mat)
  )){
    stop("input.mat must be a numeric matrix")
  }
  if(!all(
    is.numeric(label.vec),
    length(label.vec)==nrow(input.mat)
  )){
    stop("label.vec must be a numeric vector of size nrow(input.mat)")
  }
  if(missing(fold.vec)){
    if(!all(
      is.integer(n.folds),
      length(n.folds),
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
    pred.mat <- Predict1toMaxNeighborsMatrix(
      input.mat[is.train,],
      label.vec[is.train],
      input.mat,
      max.neighbors)
    loss.mat <- loss.function(pred.mat, label.vec)
    set.list <- list(train=is.train, validation=!is.train)
    loss.dt.list <- list()
    for(set.name in names(set.list)){
      is.set <- set.list[[set.name]]
      set.mat <- loss.mat[is.set,]
      loss.dt.list[[set.name]] <- data.table(
        validation.fold,
        set.name,
        neighbors=1:max.neighbors,
        loss=colMeans(set.mat))
    }
    do.call(rbind, loss.dt.list)
  }
  do.call(rbind, parallel::mclapply(unique(fold.vec), OneFold))
}, ex=function(){

  err.dt <- with(mixture.example, NearestNeighborsCVError(x, y, 20L))

  library(ggplot2)
  ggplot()+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(validation.fold ~ .)+
    geom_line(aes(
      neighbors, loss, color=set.name),
      data=err.dt)
  
})

NearestNeighborsCV <- structure(function
### Fit nearest neighbors model using k-fold CV.
(input.mat, label.vec, max.neighbors, fold.vec, n.folds=5L, loss.function=NULL){
  err.dt <- NearestNeighborsCVError(
    input.mat, label.vec, max.neighbors, fold.vec, n.folds, loss.function)
  mean.dt <- err.dt[set.name=="validation", list(
    mean.loss=mean(loss)
  ), by=list(neighbors)]
  selected.neighbors <- mean.dt[which.min(mean.loss), neighbors]
  list(
    input.mat=input.mat,
    label.vec=label.vec,
    selected.neighbors=selected.neighbors,
    predict=function(test.input.mat){
      pred.mat <- Predict1toMaxNeighborsMatrix(
        input.mat, label.vec, test.input.mat, selected.neighbors)
      pred.mat[, selected.neighbors]
    })
}, ex=function(){

  max.neighbors <- 50L
  set.seed(1)
  fold.vec <- sample(rep(1:5, l=nrow(mixture.example$x)))
  result.dt.list <- list()
  for(test.fold in unique(fold.vec)){
    is.test <- fold.vec == test.fold
    y.train.valid <- mixture.example$y[!is.test]
    fit <- NearestNeighborsCV(
      mixture.example$x[!is.test,], y.train.valid, max.neighbors)
    pred.prob <- fit$predict(mixture.example$x[is.test,])
    pred.label <- ifelse(pred.prob<0.5, 0, 1)
    result.dt.list[[test.fold]] <- data.table(
      test.fold,
      baseline.error=mean(y.train.valid != mixture.example$y[is.test]),
      KNN.error=mean(pred.label != mixture.example$y[is.test]),
      neighbors=fit$selected.neighbors)
  }
  do.call(rbind, result.dt.list)

})

