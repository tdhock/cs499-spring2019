data(zip.train, package="ElemStatLearn")
table(zip.train[, 1])
table(zip.train[, 2])
all.y.vec <- zip.train[, 1]
is.01 <- all.y.vec %in% c(0,1)
table(is.01)
y.vec <- all.y.vec[is.01]
X.mat <- zip.train[is.01, -1]
str(y.vec)
str(X.mat)

set.seed(1)
n.folds <- 5
fold.vec <- sample(rep(1:n.folds, l=nrow(X.mat)))
validation.fold <- 1
is.train <- fold.vec != validation.fold
is.validation <- fold.vec == validation.fold

X.train <- X.mat[is.train,]
y.train <- y.vec[is.train]

X.sc <- scale(X.train)
y.tilde <- ifelse(y.train==1, 1, -1)
table(y.tilde, y.train)
X.filtered <- X.sc[, attr(X.sc, "scaled:scale") != 0]
X.int <- cbind(1, X.filtered)

## NB: first element is bias/intercept.
w <- rep(0, l=ncol(X.int))
step.size <- 0.1
lambda <- 0.5
sigmoid <- function(z){
  1/(1+exp(-z))
}
soft <- function(x, lambda){
  sign(x) * pos.part(abs(x)-lambda)
}
pos.part <- function(x){
  ifelse(x<0, 0, x)
}
curve(soft(x, 1), -5, 5)
l1opt <- function(w.vec, d){
  ifelse(
    w.vec==0,
    pos.part(abs(d)-lambda),
    abs(d-sign(w.vec)*lambda))
}
it <- 1

pred.vec <- X.int %*% w
prob.vec <- sigmoid(-pred.vec * y.tilde)
grad.vec <- -t(X.int) %*% (y.tilde * prob.vec) / nrow(X.int)
d.vec <- -grad.vec
crit.vec <- c(
  abs(grad.vec[1]), ##intercept/bias/beta
  l1opt(w[-1], d.vec[-1])##weights
)
cost.weight <- function(w.vec){#cost of a given weight vector
  pred.vec <- X.int %*% w.vec
  loss.vec <- log(1+exp(- pred.vec * y.tilde))
  mean(loss.vec) + lambda * sum(abs(w.vec[-1]))
}
cost.step <- function(step){#cost of a given step size
  new.w <- w.step(step)
  cost.weight(new.w)
}
w.step <- function(step){#returns a weight vector for a given step size.
  u.vec <- w + step * d.vec
  c(u.vec[1], soft(u.vec[-1], step*lambda))
}
curve(sapply(x, cost.step), 0, 10)
cat(sprintf("it=%d crit=%f\n", it, max(crit.vec)))
it <- it+1
lmax <- max(abs(grad.vec[-1]))
while(cost.step(step.size/2) < cost.step(step.size)){
  step.size <- step.size/2
}
while(cost.step(step.size*2) < cost.step(step.size)){
  step.size <- step.size*2
}
points(step.size, cost.step(step.size))
w <- w.step(step.size)
