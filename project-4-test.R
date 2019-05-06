library(ggplot2)
library(directlabels)

"git clone https://github.com/SixianZhang/CS499-Coding-Project-4.git"
install.packages("CS499-Coding-Project-4", repo=NULL, type="source")

"git clone https://github.com/ShoeRider/CS499_MachineLearning_TeamProjects.git"
install.packages("MachineLearningProjects/Project4", repo=NULL, type="source")

library(linearmodels)

library(LinearModelL1)

data(prostate, package="ElemStatLearn")
y.vec <- as.numeric(prostate$lpsa)
set.seed(1)
X.mat <- cbind(as.matrix(prostate[,1:8]), rnorm(nrow(prostate)))

X.scaled <- scale(X.mat)
w.vec <- rep(0, ncol(X.scaled))
grad.vec <- t(X.scaled) %*% (X.scaled %*% w.vec - y.vec) / nrow(X.scaled)
(lmax <- max(abs(grad.vec)))

## 2 points if return weight vector is all 0 except for first entry.
(zero.fit <- LinearModelL1(
  X.scaled, y.vec, penalty=0.9,
  opt.thresh=1e-4, initial.weight.vec=rep(0, ncol(X.scaled)+1)))

## 2 points if returned weight vector has at least one non-zero
## element (other than the first).
(nonzero.fit <- LinearModelL1(
  X.scaled, y.vec, penalty=0.8,
  opt.thresh=1e-4, initial.weight.vec=rep(0, ncol(X.scaled)+1)))

## 2 points if there is an informative error message when there is a
## negative threshold.
LinearModelL1(
  X.scaled, y.vec, penalty=0.8,
  opt.thresh=-5,
  initial.weight.vec=rep(0, ncol(X.scaled)+1))

## 2 points if w.mat correct size, 10 x 12
pen.vec <- cumprod(c(0.9, rep(0.75, l=10), 0))
w.mat <- LinearModelL1penalties(
  X.mat[prostate$train,], y.vec[prostate$train],
  penalty.vec=pen.vec, step.size=0.75)
str(w.mat)

## 2 points if informative error message for increasing penalties.
LinearModelL1penalties(
  X.mat[prostate$train,], y.vec[prostate$train],
  penalty.vec=rev(pen.vec), step.size=0.75)

## 2 points if train loss always decreasing with model complexity.
pred.mat <- cbind(1, X.mat) %*% w.mat
squared.res.mat <- (pred.mat - y.vec)^2
set.list <- with(prostate, list(
  train=train,
  validation=!train))
err.list <- list()
min.dt.list <- list()
for(set.name in names(set.list)){
  is.set <- set.list[[set.name]]
  set.df <- data.frame(
    loss=colMeans(squared.res.mat[is.set, ]),
    set.name,
    penalty=pen.vec)
  err.list[[set.name]] <- set.df
  min.dt.list[[set.name]] <- subset(set.df, loss==min(loss))
}
err <- do.call(rbind, err.list)
min.dt <- do.call(rbind, min.dt.list)
diff(err.list$train$loss) < 0 # should be all TRUE

## 2 points if validation error decreases and then increases.
gg <- ggplot()+
  geom_line(aes(
    -log(penalty), loss, color=set.name),
    data=err)+
  geom_point(aes(
    -log(penalty), loss, color=set.name),
    shape=21,
    size=3,
    fill="white",
    data=min.dt)
directlabels::direct.label(gg, "first.polygons")

## 2 points if CV prediction function returns vector of correct size [97]
fit <- LinearModelL1CV(X.mat[prostate$train,], y.vec[prostate$train], penalty.vec=pen.vec)
pred.vec <- fit$predict(X.mat)
str(pred.vec)

## 2 points if train loss always decreasing with complexity.
complexity <- -log(fit$penalty.vec+1)
plot(complexity, fit$mean.train.loss.vec, type="l", lwd=4)

## 2 points if validation loss decreases and then increases.
lines(complexity, fit$mean.validation.loss.vec, col="red", lwd=2)

