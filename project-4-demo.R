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

## NB: first element is bias/intercept.
w <- rep(0, l=ncol(X.filtered)+1)

sigmoid <- function(z){
  1/(1+exp(-z))
}
X.int <- cbind(1, X.filtered)
pred.vec <- X.int %*% w
prob.vec <- sigmoid(-pred.vec * y.tilde)
grad.vec <- -t(X.int) %*% (y.tilde * prob.vec)
d.vec <- -grad.vec
step.size <- 0.1
u.vec <- w + step.size * d.vec
soft <- function(x, lambda){
  sign(x) * (abs(x)-lambda)
}
lambda <- 10
w <- c(u.vec[1], soft(u.vec[-1], step.size*lambda))

