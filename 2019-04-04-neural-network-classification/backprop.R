data(spam, package="ElemStatLearn") 
head(spam)

head(y.vec <- ifelse(spam$spam=="spam", 1, -1))
table(y.vec)
head(X.mat <- as.matrix(subset(spam, select=-spam)))

(is.binary <- all(y.vec %in% c(-1, 1)))
step.size <- 0.02
n.hidden.units <- 2 # u
sigmoid <- function(a){
  1/(1+exp(-a))
}

set.seed(1)
n.folds <- 2
(unique.folds <- 1:n.folds)
(fold.vec <- sample(rep(unique.folds, l=nrow(X.mat))))
validation.fold <- 1
is.train <- fold.vec != validation.fold
table(is.train)

X.train <- X.mat[is.train, ]
y.train <- y.vec[is.train]

##Scaling.
head(X.sc <- scale(X.train))
attr(X.sc, "scaled:center")
attr(X.sc, "scaled:scale")
(V <- matrix(rnorm(ncol(X.sc)*n.hidden.units), ncol(X.sc), n.hidden.units))
(w <- rnorm(n.hidden.units))

## gradent descent.
head(A <- X.sc %*% V) #1
head(Z <- sigmoid(A)) #2
head(b <- as.numeric(Z %*% w))
dw <- if(is.binary){
  -y.train * sigmoid(-y.train * b)
}else{
  b - y.train
}
head(A.deriv <- Z * (1-Z))
head(dv <- unname(dw * A.deriv * matrix(w, nrow(A.deriv), ncol(A.deriv), byrow=TRUE)))
(grad.w <- t(Z) %*% dw / nrow(X.sc))
(grad.V <- t(X.sc) %*% dv / nrow(X.sc))
## take a step.
(w <- w - step.size * grad.w)
(V <- V - step.size * grad.V)

predict.sc <- function(X.tilde){
  A.mat <- X.tilde %*% V
  sigmoid(A.mat) %*% w
}
predict1.orig <- function(X.unsc){
  X.tilde <- scale(
    X.unsc, attr(X.sc, "scaled:center"), attr(X.sc, "scaled:scale"))
  predict.sc(X.tilde)
}

V.orig <- V/attr(X.sc, "scaled:scale")
b.orig <- - t(V/attr(X.sc, "scaled:scale")) %*% attr(X.sc, "scaled:center")
V.with.intercept <- rbind(intercept=as.numeric(b.orig), V.orig)
predict2.orig <- function(X.unsc){
  A.mat <- cbind(1, X.unsc) %*% V.with.intercept
  sigmoid(A.mat) %*% w
}
rbind(
  as.numeric(head(predict.sc(X.sc))),
  as.numeric(head(predict1.orig(X.train))),
  as.numeric(head(predict2.orig(X.train))))

## train/validation error.
pred.vec <- as.numeric(predict2.orig(X.mat))
set.list <- list(
  train=is.train,
  validation=!is.train)
is.error <- ifelse(pred.vec > 0, 1, -1) != y.vec
log.loss <- log(1+exp(-y.vec * pred.vec))
sapply(names(set.list), function(set.name){
  is.set <- set.list[[set.name]]
  mean(is.error[is.set])
})
  
