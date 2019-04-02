## ElemStatLearn::prostate [97 x 8] output is lpsa column, ignore train column.
## ElemStatLearn::ozone [111 x 3] output is first column (ozone).
data(ozone, package="ElemStatLearn") 
head(ozone)
X.unscaled.mat <- cbind(constant5=5, as.matrix(ozone[,-1]), constant9=9)
y.vec <- ozone[,1]
step.size <- 0.02
n.hidden.units <- 2 # u
n.folds <- 5
sigmoid <- function(a){
  1/(1+exp(-a))
}
set.seed(1)
unique.folds <- 1:n.folds
loss.dt.list <- list()
fold.vec <- rep(unique.folds, l=nrow(X.unscaled.mat))
is.train <- fold.vec != validation.fold
X.train <- X.unscaled.mat[is.train, ]
## TODO scaling.
y.train <- y.vec[is.train]
(V <- matrix(rnorm(ncol(X.scaled.mat)*n.hidden.units), ncol(X.scaled.mat), n.hidden.units))
(w <- rnorm(n.hidden.units))
head(A <- X.train %*% V) #1
head(Z <- sigmoid(A)) #2
head(b <- as.numeric(Z %*% w))
head(delta.w <-  b - y.train)
head(A.deriv <- Z * (1-Z))
diag(w)
head(delta.v <- diag(delta.w) %*% A.deriv %*% diag(as.numeric(w)))
(grad.w <- t(Z) %*% delta.w / nrow(X.train))
(grad.V <- t(X.train) %*% delta.v / nrow(X.train))
## take a step.
(w <- w - step.size * grad.w)
(V <- V - step.size * grad.V)
