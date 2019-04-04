library(ggplot2)
library(data.table)
library(directlabels)

data(spam, package="ElemStatLearn") 
head(spam)
head(y.vec <- ifelse(spam$spam=="spam", 1, -1))
table(y.vec)
head(X.mat <- as.matrix(subset(spam, select=-spam)))

data(ozone, package="ElemStatLearn") 
head(ozone)
head(y.vec <- ozone$ozone)
head(X.mat <- as.matrix(subset(ozone, select=-ozone)))

is.binary <- all(y.vec %in% c(1, -1))
line.search.factor <- NULL
line.search.factor <- 3
step.size <- 1
n.hidden.units <- 100 # u
n.folds <- 2
max.iterations <- 200
logistic.loss <- function(pred, y){
  log(1+exp(-y * pred))
}
sigmoid <- function(a){
  1/(1+exp(-a))
}
set.seed(2)
(unique.folds <- 1:n.folds)
fold.vec <- sample(rep(unique.folds, l=nrow(X.mat)))
validation.fold <- 1
is.train <- fold.vec != validation.fold
set.list <- list(
  train=is.train,
  validation=!is.train)
table(is.train)
X.train <- X.mat[is.train, ]
dim(X.mat)
dim(X.train)
y.train <- y.vec[is.train]
length(y.vec)
length(y.train)
##Scaling.
head(X.sc <- scale(X.train))
str(X.sc)
##attr(X.sc, "scaled:center")
attr(X.sc, "scaled:scale")
V <- matrix(rnorm(ncol(X.sc)*n.hidden.units), ncol(X.sc), n.hidden.units)
w <- rnorm(n.hidden.units)
iteration.dt.list <- list()
iteration.vec <- 1:max.iterations

iteration.vec <- seq(iteration+1, iteration+max.iterations)
for(iteration in iteration.vec){
  head(A <- X.sc %*% V) #1
  head(Z <- sigmoid(A)) #2
  head(b <- as.numeric(Z %*% w))
  ## Y <- diag(y.train)
  ## system.time({
  ##   head(delta.w <- as.numeric(-Y %*%  (sigmoid(-Y %*% b))))
  ## })
  dw <- if(is.binary){
    -y.train * sigmoid(-y.train * b)
  }else{
    b-y.train
  }
  ## all.equal(dw, delta.w)
  ## identical(dw, delta.w)
  head(A.deriv <- Z * (1-Z))
  ## system.time({
  ##   head(delta.v <- diag(dw) %*% A.deriv %*% diag(as.numeric(w)))
  ## })
  system.time({
    head(dv <- unname(dw * A.deriv * matrix(w, nrow(A.deriv), ncol(A.deriv), byrow=TRUE)))
  })
  ##all.equal(delta.v, dv)
  grad.w <- t(Z) %*% dw / nrow(X.sc)
  grad.V <- t(X.sc) %*% dv / nrow(X.sc)
  ## take a step.
  cost.w.V <- function(w.vec, V.mat){
    b <- sigmoid(X.sc %*% V.mat) %*% w.vec
    mean(logistic.loss(b, y.train))
  }
  cost.step <- function(step){
    cost.w.V(w-step*grad.w, V-step*grad.V)
  }
  if(is.numeric(line.search.factor)){
    while(cost.step(step.bigger <- step.size*line.search.factor) < cost.step(step.size)){
      step.size <- step.bigger
    }
    while(cost.step(step.smaller <- step.size/line.search.factor) < cost.step(step.size)){
      step.size <- step.smaller
    }
  }
  print(step.size)
  if(FALSE){
    curve(sapply(x, cost.step), 0, 300)
  }
  w <- w - step.size * grad.w
  V <- V - step.size * grad.V
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
  b.orig <- - t(V) %*% diag(attr(X.sc, "scaled:scale")) %*% attr(X.sc, "scaled:center")
  ## need to divide by scale (not multiply!)
  system.time({
    b.orig <- - t(V) %*% diag(1/attr(X.sc, "scaled:scale")) %*% attr(X.sc, "scaled:center")
  })
  system.time({
    b.orig <- - t(V/attr(X.sc, "scaled:scale")) %*% attr(X.sc, "scaled:center")
  })
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
  is.error <- ifelse(pred.vec > 0, 1, -1) != y.vec
  log.loss <- logistic.loss(pred.vec, y.vec)
  square.loss <- (y.vec-pred.vec)^2
  iteration.dt.list[[iteration]] <- print(data.table(
    set=ifelse(is.train, "train", "validation"),
    is.error, square.loss,
    log.loss)[, list(
      iteration,
      mean.square.loss=mean(square.loss),
      error.percent=mean(is.error)*100,
      mean.log.loss=mean(log.loss)
      ), by=list(set)])
}
iteration.dt <- do.call(rbind, iteration.dt.list)

iterations.tall <- melt(
  iteration.dt,
  measure.vars=c("error.percent", "mean.log.loss"))
min.tall <- iterations.tall[, .SD[which.min(value)], by=list(variable, set)]
set.colors <- c(
  train="black",
  validation="red")
gg <- ggplot()+
  ggtitle(paste(
    "Single layer neural network (57, 100, 1) for binary classification",
    "of spam data, N_train=2300, N_validation=2301, line search", sep="\n"))+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  scale_color_manual(values=set.colors)+
  facet_grid(variable ~ ., scales="free")+
  geom_line(aes(
    iteration, value, color=set),
    data=iterations.tall)+
  scale_shape_manual(values=c(min=1))+
  geom_point(aes(
    iteration, value, color=set, shape=Value),
    size=3,
    data=data.table(Value="min", min.tall))+
  ylab("")+
  scale_x_continuous(limits=c(0, 450))
dl <- direct.label(gg, "last.polygons")
png("figure-nnet-spam.png", 6, 6, units="in", res=100)
print(dl)
dev.off()

fit <- glmnet::glmnet(X.train, y.train, family="binomial", alpha=0)
pred.mat <- predict(fit, X.mat)
loss.mat <- logistic.loss(pred.mat, y.vec)
err.mat <- ifelse(pred.mat > 0, 1, -1) != y.vec
glmnet.dt.list <- list()
for(set.name in names(set.list)){
  is.set <- set.list[[set.name]]
  glmnet.dt.list[[set.name]] <- data.table(
    set=set.name,
    lambda=fit$lambda,
    lambda.i=seq_along(fit$lambda),
    error.percent=colMeans(err.mat[is.set,])*100,
    mean.log.loss=colMeans(loss.mat[is.set,]))
}
(glmnet.dt <- do.call(rbind, glmnet.dt.list))
gg <- ggplot()+
  theme_bw()+
  scale_color_manual(values=set.colors)+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_line(aes(
    lambda.i, mean.log.loss, color=set),
    data=glmnet.dt)
dl <- direct.label(gg, "last.polygons")
print(dl)

combine.dt <- rbind(
  glmnet.dt[, data.table(
    model="Linear model complexity -log(lambda)",
    x=-log(lambda), mean.log.loss, error.percent, set)],
  iteration.dt[, data.table(
    model="Neural network iterations",
    x=iteration, mean.log.loss, error.percent, set)])
blank.dt <- combine.dt[, {
  x.width <- max(x)-min(x)
  data.table(x=max(x)+x.width/5, mean.log.loss=Inf)
}, by=list(model)]
combine.tall <- melt(
  combine.dt, measure.vars=c("mean.log.loss", "error.percent"))
best.linear <- combine.tall[{
  grepl("Linear", model) & set=="validation"
}, {
  .SD[which.min(value)]
}, by=list(variable)]
gg <- ggplot()+
  theme_bw()+
  scale_color_manual(values=set.colors)+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(variable ~ model, scales="free")+
  geom_blank(aes(
    x, mean.log.loss),
    data=blank.dt)+
  geom_line(aes(
    x, value, color=set),
    data=combine.tall)+
  geom_hline(aes(
    yintercept=value, color=set),
    data=best.linear[, .(value, set, variable)],
    alpha=0.5)+
  geom_text(aes(
    -Inf, value, label=" best linear model", color=set),
    hjust=0,
    vjust=-0.5,
    data=best.linear)+
  xlab("")+
  ylab("")
dl <- direct.label(gg, "last.polygons")
png("figure-nnet-spam-compare-linear.png", 6, 6, units="in", res=100)
print(dl)
dev.off()



