library(data.table)
library(parallel)
fun.list <- list(
  constant=function(x)1,
  linear=function(x)x,
  quadratic=function(x)x*x,
  step=function(x)ifelse(x<1, 4, 1))
N <- 100
set.seed(1)
min.x <- -2
max.x <- 3
input.vec <- runif(N, min.x, max.x)
in.out.nofold <- data.table(fun.name=names(fun.list))[, {
  f <- fun.list[[fun.name]]
  true.vec <- f(input.vec)
  list(
    input=input.vec,
    true=true.vec,
    output=true.vec+rnorm(N))
}, by=list(fun.name)]

n.folds.dt <- data.table(n.folds=c(2:20))
neighbors.dt <- data.table(neighbors=1:(N/2))
fold.valid.dt <- n.folds.dt[, {
  print(n.folds)
  fold.dt <- data.table(validation.fold=1:n.folds)
  in.out.dt <- data.table(in.out.nofold)
  in.out.dt[, fold := rep(1:n.folds, l=.N), by=list(fun.name)]
  pred.dt <- do.call(rbind, mclapply(fold.dt$validation.fold, function(validation.fold){
    neighbors.dt[, {
      in.out.dt[, {
        is.train <- fold!=validation.fold
        in.mat <- cbind(input[is.train])
        fit <- caret::knnreg(in.mat, output[is.train], k=neighbors)
        data.table(
          validation.fold,
          input,
          output,
          pred=predict(fit, input),
          set=ifelse(is.train, "train", "validation"))
      }, by=list(fun.name)]
    }, by=list(neighbors)]
  }))
  rss.dt <- pred.dt[, {
    res.vec <- pred-output
    rss <- sum(res.vec*res.vec)
    data.table(rss, mse=rss/N)
  }, by=list(neighbors, validation.fold, fun.name, set)]
  rss.dt[set=="validation", list(
    mean=mean(mse),
    sd=sd(mse)
  ), by=list(neighbors, fun.name)]
}, by=list(n.folds)]
min.dt <- fold.valid.dt[, {
  .SD[which.min(mean)]
  }, by=list(n.folds, fun.name)]

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(n.folds ~ fun.name)+
  geom_ribbon(aes(
    neighbors, ymin=mean-sd, ymax=mean+sd),
    alpha=0.5,
    data=fold.valid.dt)+
  geom_line(aes(
    neighbors, mean),
    data=fold.valid.dt)+
  geom_point(aes(
    neighbors, mean),
    fill="white",
    shape=21,
    data=min.dt)+
  coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(paste(
    "Validation set mean squared error (mean +/- sd over K",
    "folds)"))

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ fun.name)+
  geom_point(aes(
    n.folds, neighbors),
    data=min.dt)

