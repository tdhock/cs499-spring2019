library(NeuralNetwork)

data(spam, package="ElemStatLearn")
X.mat <- as.matrix(subset(spam, select=-spam))
y.vec <- ifelse(spam$spam=="spam", 1, 0)

max.iterations <- 100L
n.hidden.units <- 10L
set.seed(1)
n.folds <- 2L
unique.folds <- 1:n.folds
fold.vec <- sample(rep(unique.folds, l=nrow(X.mat)))
validation.fold <- 1
is.train <- fold.vec != validation.fold
it.list <- NeuralNetwork::NNetIterations(
  X.mat, y.vec, max.iterations=max.iterations,
  is.train=is.train,
  step.size=1, n.hidden.units=n.hidden.units)

##1 pred.mat has the right number of rows. (2 points)
rbind(
  pred.mat.rows=nrow(it.list$pred.mat),
  expected=nrow(X.mat))

##2 pred.mat has the right number of columns. (2 points)
rbind(
  pred.mat.cols=ncol(it.list$pred.mat),
  expected=max.iterations)

##3 V.mat has the right number of columns (2 points)
rbind(
  V.mat.cols=ncol(it.list$V.mat),
  expected=n.hidden.units)

##4 V.mat has the right number of rows (2 points), +1 is OK if they
## used an intercept at the second level.
rbind(
  V.mat.rows=nrow(it.list$V.mat),
  expected=ncol(X.mat))

##5 prediction function gives vector of right size (2 points)
pred <- it.list$predict(X.mat)
rbind(
  length.pred=length(pred),
  expected=nrow(X.mat))

##6 prediction function returns probabilities (2 points)
rbind(
  range.pred=range(pred),
  expected=c(0,1))

##7 informative error for size mismatch. (2 points)
NeuralNetwork::NNetIterations(
  X.mat, y.vec[1:100], max.iterations=max.iterations,
  is.train=is.train,
  step.size=1, n.hidden.units=n.hidden.units)

set.seed(1)
X.mat <- cbind(
  as.matrix(subset(iris, select=-Species)),
  matrix(rnorm(nrow(iris)*ncol(iris)), nrow(iris), ncol(iris)))
y.vec <- ifelse(iris$Species=="versicolor", 1, 0)
max.iterations <- 10000L
data.i <- 1:nrow(X.mat)
fit <- NeuralNetwork::NNetEarlyStoppingCV(
  X.mat[data.i,], y.vec[data.i], n.folds=n.folds,
  max.iterations=max.iterations,
  step.size=0.2,
  n.hidden.units=10L)

##8 mean.validation.loss.vec is right size, 2 points
rbind(
  length.validation=length(fit$mean.validation.loss.vec),
  expected=max.iterations)

##9 train loss always decreases, 2 points
plot(fit$mean.train.loss.vec, type="l")
v <- fit$mean.validation.loss.vec
lines(v, col="red")

##10 validation loss min (red point) same as selected steps (red
##vertical line), 2 points
i <- which.min(v)
points(i, v[i], col="red")
abline(v=fit$selected.steps, col="red")
rbind(
  selected.steps=fit$selected.steps,
  expected=i)
