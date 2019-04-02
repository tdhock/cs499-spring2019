library(LinearModel)#TODO change depending on group pkg name.
library(LinearModels)

## Regression.
data(ozone, package="ElemStatLearn")
head(ozone)
y.vec <- ozone[,1]
X.mat <- scale(as.matrix(ozone[,-1]))
pen.value <- 5
w.wrong.size.result <- tryCatch({
  LMSquareLossL2(
    X.mat, y.vec, penalty=pen.value, opt.thresh=1e-4,
    initial.weight.vec=0)
}, error=function(e){
  e
})
y.wrong.size.result <- tryCatch({
  LMSquareLossL2(
    X.mat, 0, penalty=pen.value, opt.thresh=1e-4,
    initial.weight.vec=rep(0, ncol(X.mat)))
}, error=function(e){
  e
})
w.vec <- tryCatch({
  LMSquareLossL2(
    X.mat, y.vec, penalty=pen.value, opt.thresh=1e-4,
    initial.weight.vec=rep(0, ncol(X.mat)))
}, error=function(e){
  e
})
pen.dec.error <- tryCatch({
  LMSquareLossL2CV(X.mat, y.vec, penalty.vec=10^seq(-1, 1, l=10))
}, error=function(e){
  e
})
pred.vec <- tryCatch({
  fit <- LMSquareLossL2CV(X.mat, y.vec, penalty.vec=10^seq(1, -1, l=10))
  fit$predict(X.mat)
}, error=function(e){
  e
})

##Binary class.
data(spam, package="ElemStatLearn")
head(spam)
some.spam <- spam[as.integer(seq(1, nrow(spam), l=100)),]
y.vec <- ifelse(unlist(subset(some.spam, select=spam))=="spam", 1, 0)
X.mat <- as.matrix(subset(some.spam, select=-spam))
max.it <- 10
W.mat <- tryCatch({
  LMLogisticLossIterations(X.mat, y.vec, max.iterations=max.it)
}, error=function(e){
  e
})
opt.thresh <- 1e-4
logistic.l2.vec <- tryCatch({
  LMLogisticLossL2(X.mat, y.vec, penalty=pen.value, opt.thresh=opt.thresh, initial.weight.vec=rep(0, ncol(X.scaled)))
}, error=function(e){
  e
})

grad.small <- tryCatch({
  y.tilde <- ifelse(y.vec==0, -1, 1)
  (inside.exp <- y.tilde * (X.mat %*% logistic.l2.vec))
  (y.term <- as.numeric(-y.tilde/(1+exp(inside.exp))))
  (loss.grad.vec <- as.numeric(y.term %*% X.mat))
  (pen.grad.vec <- 2*pen.value*logistic.l2.vec)
  (total.grad.vec <- pen.grad.vec + loss.grad.vec)
  sum(abs(total.grad.vec)) < opt.thresh
}, error=function(e){
  FALSE
})

is.zero <- function(x)is.finite(x) & x==0
rbind(
  LMLogisticLossL2.grad.small.L1.norm=grad.small,
  LMLogisticLossL2.weights.correct.size=length(logistic.l2.vec)==ncol(X.mat),
  LMLogisticLossIterations.weights.nrow.correct=is.matrix(W.mat) && nrow(W.mat)==ncol(X.mat)+1,
  LMLogisticLossIterations.weights.ncol.correct=is.matrix(W.mat) && ncol(W.mat)==max.it,
  LMLogisticLossIterations.first.iteration.zero=is.matrix(W.mat) && all(is.zero(W.mat[,1])),
  LMSquareLossL2.error.for.increasing.penalties=is(pen.dec.error, "error"),
  LMSquareLossL2.error.for.initial.weight.vec.wrong.size=is(w.wrong.size.result, "error"),
  LMSquareLossL2.error.for.y.vec.wrong.size=is(y.wrong.size.result, "error"),
  LMSquareLossL2CV.pred.right.size=length(pred.vec) == length(y.vec),
  LMSquareLossL2.result.right.size=length(w.vec)==3)
