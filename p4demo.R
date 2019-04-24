data(prostate, package="ElemStatLearn")

X.mat <- as.matrix(prostate[, 1:8])
y.vec <- prostate$lpsa

set.seed(1)
n.folds <- 5
fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
validation.fold <- 1
is.train <- fold.vec != validation.fold
X.train <- X.mat[is.train,]
y.train <- y.vec[is.train]
X.sc <- scale(X.train)
X.int <- cbind(1, X.sc)

IntervalRegressionInternal <- function
### Solve the squared hinge loss interval regression problem for one
### regularization parameter: w* = argmin_w L(w) + regularization *
### ||w||_1 where L(w) is the average squared hinge loss with respect
### to the targets, and ||w||_1 is the L1-norm of the weight vector
### (excluding the first element, which is the un-regularized
### intercept or bias term). This function performs no scaling of
### input features, and is meant for internal use only! To learn a
### regression model, try IntervalRegressionCV or
### IntervalRegressionUnregularized.
(features,
### Scaled numeric feature matrix (problems x features). The first
### column/feature should be all ones and will not be regularized.
 targets,
### Numeric target matrix (problems x 2).
 initial.param.vec=rep(0, ncol(features)),
### initial guess for weight vector (features).
 regularization,
### Degree of L1-regularization.
 threshold=1e-3,
### When the stopping criterion gets below this threshold, the
### algorithm stops and declares the solution as optimal.
 max.iterations=1e3,
### If the algorithm has not found an optimal solution after this many
### iterations, increase Lipschitz constant and max.iterations.
 weight.vec=NULL,
### A numeric vector of weights for each training example.
 Lipschitz=NULL,
### A numeric scalar or NULL, which means to compute Lipschitz as the
### mean of the squared L2-norms of the rows of the feature matrix.
 verbose=2,
### Cat messages: for restarts and at the end if >= 1, and for every
### iteration if >= 2.
 margin=1
### Margin size hyper-parameter, default 1.
 ){
  if(!(
    is.numeric(margin) &&
    length(margin)==1 &&
    is.finite(margin)
    )){
    stop("margin must be finite numeric scalar")
  }
  stopifnot(is.matrix(features))
  stopifnot(is.numeric(features))
  n.features <- ncol(features)
  n.problems <- nrow(features)
  stopifnot(is.numeric(targets))
  stopifnot(length(targets) == n.problems)
  if(is.null(weight.vec)){
    weight.vec <- rep(1, n.problems)
  }
  stopifnot(is.numeric(weight.vec))
  stopifnot(length(weight.vec) == n.problems)
  if(is.null(Lipschitz)){
    Lipschitz <- mean(rowSums(features * features) * weight.vec)
  }
  stopifnot(is.numeric(Lipschitz))
  stopifnot(length(Lipschitz) == 1)
  stopifnot(is.numeric(max.iterations))
  stopifnot(length(max.iterations) == 1)
  stopifnot(is.numeric(threshold))
  stopifnot(length(threshold) == 1)
  stopifnot(is.numeric(initial.param.vec))
  stopifnot(length(initial.param.vec) == n.features)
  ## Return 0 for a negative number and the same value otherwise.
  positive.part <- function(x){
    ifelse(x<0, 0, x)
  }
  calc.loss <- function(x){
    0.5 * mean((as.numeric(features %*% x) - targets)^2)
  }
  calc.grad <- function(x){
    t(features) %*% (as.numeric(features %*% x) - targets) / nrow(features)
  }
  calc.penalty <- function(x){
    regularization * sum(abs(x[-1]))
  }
  calc.cost <- function(x){
    calc.loss(x) + calc.penalty(x)
  }
  soft.threshold <- function(x,thresh){
    ifelse(abs(x) < thresh, 0, x-thresh*sign(x))
  }
  ## do not threshold the intercept.
  prox <- function(x,thresh){
    x[-1] <- soft.threshold(x[-1],thresh)
    x
  }
  ## p_L from the fista paper.
  pL <- function(x,L){
    grad <- calc.grad(x)
    prox(x - grad/L, regularization/L)
  }
  step <- function(x, size){
    grad <- calc.grad(x)
    prox(x - grad*size, regularization*size)
  }
  dist2subdiff.opt <- function(w,g){
    ifelse(w==0,positive.part(abs(g)-regularization),
           ifelse(w<0,abs(-regularization+g),abs(regularization+g)))
  }
  iterate.count <- 1
  stopping.crit <- threshold
  last.iterate <- this.iterate <- y <- initial.param.vec
  this.t <- 1
  while({
    ##browser(expr=is.na(stopping.crit))
    ##str(stopping.crit)
    stopping.crit >= threshold
  }){
    ## here we implement the FISTA method with constant step size, as
    ## described by in the Beck and Tebolle paper.
    last.iterate <- this.iterate
    this.iterate <- step(this.iterate, 0.1)
    ## this.iterate <- pL(this.iterate, Lipschitz)
    ## this.iterate <- pL(y, Lipschitz)
    ## last.t <- this.t
    ## this.t <- (1+sqrt(1+4*last.t^2))/2
    ## y <- this.iterate + (last.t - 1)/this.t*(this.iterate-last.iterate)
    ## here we calculate the subgradient optimality condition, which
    ## requires 1 more gradient evaluation per iteration.
    after.grad <- calc.grad(this.iterate)
    w.dist <- dist2subdiff.opt(this.iterate[-1],after.grad[-1])
    zero.at.optimum <- c(abs(after.grad[1]),w.dist)
    stopping.crit <- max(zero.at.optimum)
    if(verbose >= 2){
      cost <- calc.cost(this.iterate)
      cat(sprintf("%10d cost %10f crit %10.7f\n",
                  iterate.count,
                  cost,
                  stopping.crit))
    }
    iterate.count <- iterate.count + 1
    if(any(!is.finite(this.iterate)) || 1e20 < stopping.crit){
      if(verbose >= 1){
        cat("restarting with bigger Lipschitz.\n")
      }
      iterate.count <- 1
      stopping.crit <- threshold
      last.iterate <- this.iterate <- y <- initial.param.vec
      this.t <- 1
      Lipschitz <- Lipschitz * 1.5
    }
    if(iterate.count > max.iterations){
      if(verbose >= 1){
        cat(max.iterations, "iterations, increasing Lipschitz and iterations.",
            "crit =", stopping.crit, "\n")
      }
      Lipschitz <- Lipschitz * 1.5
      iterate.count <- 1
      max.iterations <- max.iterations * 2
    }
  }
  if(verbose >= 1){
    cat("solution with crit =", stopping.crit, "\n")
  }
  this.iterate
### Numeric vector of scaled weights w of the affine function f_w(X) =
### X %*% w for a scaled feature matrix X with the first row entirely
### ones.
}

lambda <- 0.1
fit <- IntervalRegressionInternal(
  X.int, y.train, regularization=lambda,
  verbose=2)

it <- 1
w.vec <- rep(0, ncol(X.int))
ppart <- function(x){
  ifelse(x<0, 0, x)
}
subdiff.crit <- function(w, d){
  ifelse(
    w==0,
    ppart(abs(d)-lambda),
    abs(d-sign(w)*lambda))
}
step.size <- 0.1
soft <- function(x, l){
  sign(x)*ppart(abs(x)-l)
}
prox <- function(x, l){
  c(x[1], soft(x[-1], l))
}

res.vec <- X.int %*% w.vec - y.train
g.vec <- t(X.int) %*% res.vec / nrow(X.int)
d.vec <- -g.vec
crit.vec <- c(abs(d.vec[1]), subdiff.crit(w.vec[-1], d.vec[-1]))
it <- it+1
pstep <- function(size){
  prox(w.vec+d.vec*size, lambda*size)
}
pcost <- function(size){
  w <- pstep(size)
  res.vec <- X.int %*% w - y.train
  0.5 * mean(res.vec^2) + lambda*sum(abs(w[-1]))
}
curve(sapply(x, pcost), 0, 10)
step.fac <- 2
while(pcost(step.size/step.fac) < pcost(step.size)){
  step.size <- step.size/step.fac
}
while(pcost(step.size*step.fac) < pcost(step.size)){
  step.size <- step.size*step.fac
}
cat(sprintf("it=%d cost=%f crit=%f size=%f\n", it, pcost(0), max(crit.vec), step.size))
points(step.size, pcost(step.size))
w.vec <- pstep(step.size)
