data(zip.train, package="ElemStatLearn")
library(data.table)
library(ggplot2)

set.seed(1)
zip.feature.mat <- zip.train[,-1]
zip.label.vec <- zip.train[,1]
fold.vec <- sample(rep(1:5, l=length(zip.label.vec)))
validation.fold <- 1
is.train <- fold.vec != validation.fold
max.neighbors <- 20

loss.dt.list <- list()
for(neighbors in 1:max.neighbors){
  if(! neighbors %in% names(loss.dt.list)){
    print(neighbors)
    pred.vec <- class::knn(
      zip.feature.mat[is.train,],
      zip.feature.mat,
      zip.label.vec[is.train],
      k=neighbors)
    loss.dt.list[[paste(neighbors)]] <- data.table(
      is.error=pred.vec != zip.label.vec,
      set=ifelse(is.train, "train", "validation"))[, data.table(
        neighbors,
        loss=mean(is.error)
      ), by=list(set)]
  }
}

