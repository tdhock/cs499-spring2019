data(zip.train, package="ElemStatLearn")
library(data.table)
library(ggplot2)

zip.train.label.vec <- zip.train[,1]
zip.train.feature.mat <- zip.train[,-1]
is.01 <- zip.train.label.vec < 2
zip.01.label.vec <- zip.train.label.vec[is.01]
zip.01.feature.mat <- zip.train.feature.mat[is.01,]
set.seed(1)
fit.auc <- nearestNeighbors::NearestNeighborsCV(
  zip.01.feature.mat, zip.01.label.vec, 100L,
  LAPPLY=lapply,
  ##LAPPLY=parallel::mclapply,
  loss.function=function(pred.mat, label.vec){
    apply(pred.mat, 2, function(pred.vec){
      roc.df <- WeightedROC::WeightedROC(pred.vec, label.vec)
      -WeightedROC::WeightedAUC(roc.df)
    })
  })
mean.loss <- data.table(fit.auc$mean.loss, validation.fold="mean")
ggplot()+
  facet_grid(validation.fold ~ .)+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_line(aes(
    neighbors, loss, color=set.name, group=set.name),
    data=fit.auc$fold.loss)+
  geom_line(aes(
    neighbors, mean.loss),
    data=mean.loss)+
  geom_point(aes(
    neighbors, mean.loss),
    data=mean.loss[neighbors==fit.auc$selected.neighbors],
    shape=21,
    fill="white")

## Compare with class::knn.
set.seed(1)
fold.vec <- sample(rep(1:5, l=length(zip.01.label.vec)))
fit <- nearestNeighbors::NearestNeighborsCV(
  zip.01.feature.mat, zip.01.label.vec, 100L, fold.vec)

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
    
## looks fine.
validation.fold <- 1
neighbors <- 100
is.train <- fold.vec != validation.fold
pred.vec <- class::knn(
  zip.01.feature.mat[is.train,],
  zip.01.feature.mat,
  zip.01.label.vec[is.train],
  k=neighbors)
data.table(
  is.error=pred.vec != zip.01.label.vec,
  set=ifelse(is.train, "train", "validation"))[, data.table(
    loss=mean(is.error)
  ), by=list(set)]
select.dt <- data.table(validation.fold, neighbors)
fit$fold.loss[select.dt, on=list(validation.fold, neighbors)]

