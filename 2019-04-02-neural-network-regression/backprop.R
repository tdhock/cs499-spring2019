library(ggplot2)
library(data.table)
## ElemStatLearn::prostate [97 x 8] output is lpsa column, ignore train column.
## ElemStatLearn::ozone [111 x 3] output is first column (ozone).
data(ozone, package="ElemStatLearn") 
head(ozone)
X.unscaled.mat <- as.matrix(ozone[,-1])
head(X.unscaled.mat)
y.vec <- ozone[,1]
step.size <- 0.02
n.hidden.units <- 200 # u
n.folds <- 5
max.iterations <- 500
sigmoid <- function(a){
  1/(1+exp(-a))
}
set.seed(1)
unique.folds <- 1:n.folds
loss.dt.list <- list()
fold.vec <- rep(unique.folds, l=nrow(X.scaled.mat))
for(validation.fold in unique.folds){
  is.train <- fold.vec != validation.fold
  X.train.unscaled <- X.unscaled.mat[is.train, ]
  X.train.scaled <- scale(X.train.unscaled)
  y.train <- y.vec[is.train]
  (V <- matrix(rnorm(ncol(X.scaled.mat)*n.hidden.units), ncol(X.scaled.mat), n.hidden.units))
  (w <- rnorm(n.hidden.units))
  set.dt <- data.table(
    set=ifelse(is.train, "train", "validation"))
  for(iteration in 1:max.iterations){
    pred.vec <- as.numeric(sigmoid(X.scaled.mat %*% V) %*% w)
    pred.dt <- data.table(
      set.dt,
      loss=0.5*(pred.vec - y.vec)^2)
    loss.dt.list[[paste(validation.fold, iteration)]] <- pred.dt[, data.table(
      iteration,
      validation.fold,
      mean.loss=mean(loss)
    ), by=list(set)]
    head(A <- X.train.scaled %*% V) #1
    head(Z <- sigmoid(A)) #2
    head(b <- as.numeric(Z %*% w))
    head(delta.w <-  b - y.train)
    head(A.deriv <- Z * (1-Z))
    diag(w)
    head(delta.v <- diag(delta.w) %*% A.deriv %*% diag(as.numeric(w)))
    head(grad.w <- t(Z) %*% delta.w / nrow(X.train.scaled))
    head(grad.V <- t(X.train.scaled) %*% delta.v / nrow(X.train.scaled))
    ## take a step.
    (w <- w - step.size * grad.w)
    (V <- V - step.size * grad.V)
  }
}
(loss.dt <- do.call(rbind, loss.dt.list))

## Plot train/validation loss for each fold.
min.dt <- loss.dt[, .SD[which.min(mean.loss)], by=list(validation.fold, set)]
ggplot()+
  theme_bw()+
  facet_grid(validation.fold ~ .)+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_line(aes(
    iteration, mean.loss, color=set),
    data=loss.dt)+
  geom_point(aes(
    iteration, mean.loss, color=set),
    shape=1,
    data=min.dt)

## Plot mean validation loss over all folds.
mean.dt <- loss.dt[set=="validation", list(
  mean.loss=mean(mean.loss)
), by=list(iteration)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_line(aes(
    iteration, mean.loss),
    data=mean.dt)+
  geom_point(aes(
    iteration, mean.loss),
    shape=1,
    data=mean.dt[which.min(mean.loss)])
