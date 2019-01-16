library(animint2)
library(data.table)
fun.list <- list(
  constant=function(x)1,
  linear=function(x)x,
  quadratic=function(x)x*x,
  step=function(x)ifelse(x<1, 4, 1))
N <- 40
set.seed(1)
min.x <- -2
max.x <- 3

n.folds <- 4
input.vec <- runif(N, min.x, max.x)
in.out.dt <- data.table(fun.name=names(fun.list))[, {
  f <- fun.list[[fun.name]]
  true.vec <- f(input.vec)
  list(
    input=input.vec,
    true=true.vec,
    output=true.vec+rnorm(N),
    fold=1:n.folds)
}, by=list(fun.name)]

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(fold ~ fun.name)+
  geom_line(aes(
    input, true, color="truth"),
    size=2,
    data=in.out.dt)+
  geom_point(aes(
    input, output),
    data=in.out.dt)

fold.colors <- RColorBrewer::brewer.pal(5, "Set1")[-1]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ fun.name)+
  geom_point(aes(
    input, output, fill=factor(fold)),
    shape=21,
    color="black",
    data=in.out.dt)+
  scale_fill_manual(values=fold.colors)

fold.dt <- data.table(validation.fold=1:n.folds)
neighbors.dt <- data.table(neighbors=1:(N/n.folds*(n.folds-1)))
pred.dt <- fold.dt[, {
  neighbors.dt[, {
    in.out.dt[, {
      is.train <- fold!=validation.fold
      in.mat <- cbind(input[is.train])
      fit <- caret::knnreg(in.mat, output[is.train], k=neighbors)
      data.table(
        fold,
        input,
        output,
        pred=predict(fit, input),
        set=ifelse(is.train, "train", "validation"))
    }, by=list(fun.name)]
  }, by=list(neighbors)]
}, by=list(validation.fold)]

grid.dt <- fold.dt[, {
  neighbors.dt[, {
    in.out.dt[, {
      is.train <- fold!=validation.fold
      in.mat <- cbind(input[is.train])
      fit <- caret::knnreg(in.mat, output[is.train], k=neighbors)
      x <- seq(min.x, max.x, l=200)
      f <- fun.list[[fun.name]]
      data.table(
        input=x,
        truth=f(x),
        prediction=predict(fit, x))
    }, by=list(fun.name)]
  }, by=list(neighbors)]
}, by=list(validation.fold)]

rss.dt <- pred.dt[, {
  res.vec <- pred-output
  rss <- sum(res.vec*res.vec)
  data.table(rss, mse=rss/N)
}, by=list(neighbors, validation.fold, fun.name, set)]
min.dt <- rss.dt[set=="validation", .SD[which.min(mse)], by=list(fun.name, validation.fold)]
set.colors <- c(train="grey60", validation="black")
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(validation.fold ~ fun.name)+
  geom_line(aes(
    neighbors, rss, color=set),
    size=2,
    data=rss.dt)+
  geom_point(aes(
    neighbors, rss, color=set),
    shape=21,
    fill="white",
    data=min.dt)+
  scale_color_manual(values=set.colors)+
  scale_x_continuous()

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(validation.fold ~ fun.name)+
  geom_line(aes(
    neighbors, mse, color=set),
    data=rss.dt)+
  scale_x_continuous()

verr <- rss.dt[set=="validation", list(
  mean=mean(mse),
  sd=sd(mse)
), by=list(neighbors, fun.name)]
verr.min <- verr[, .SD[which.min(mean)], by=list(fun.name)]

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ fun.name)+
  geom_ribbon(aes(
    neighbors, ymin=mean-sd, ymax=mean+sd),
    alpha=0.5,
    data=verr)+
  geom_line(aes(
    neighbors, mean),
    data=verr)+
  scale_y_continuous(paste(
    "Validation set mean squared error (mean +/- sd over",
    n.folds, "folds)"))

pred.colors <- c(prediction="red", truth="blue")
(set.dt <- pred.dt[neighbors==1])

fold.dt[, {
  dt <- data.table(in.out.dt, validation.fold)
  dt[, set := ifelse(validation.fold==fold, "validation", "train")]
  dt
}, by=list(validation.fold)]

in.out.dt[, validation.fold := fold]
in.out.dt[, fold.fac := factor(fold)]
rss.dt[, fold.fac := factor(validation.fold)]
min.dt[, fold.fac := factor(validation.fold)]
grid.dt[, fun.type := "prediction"]
lab.dt <- grid.dt[fun.name=="constant" & input==min(input)]
(viz <- animint(
  title=paste(n.folds, "fold cross-validation"),
  data=ggplot()+
    ggtitle("Data and fold/set assignment")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=1000)+
    facet_grid(panel ~ fun.name)+
    scale_linetype_manual(values=c(prediction="solid"))+
    geom_text(aes(
      input, 6, label=paste0(
        neighbors,
        " nearest neighbor",
        ifelse(neighbors==1, "", "s"),
        " predictor")),
      color=pred.colors[["prediction"]],
      hjust=0,
      showSelected=c("neighbors", "validation.fold"),
      data=lab.dt)+
    geom_text(aes(
      input, 4, label=paste(
        "trained on fold",
        validation.fold)),
      color=pred.colors[["prediction"]],
      hjust=0,
      showSelected=c("neighbors", "validation.fold"),
      data=lab.dt)+
    geom_line(aes(
      input, prediction, linetype=fun.type),
      data=grid.dt,
      size=1,
      color=pred.colors[["prediction"]],
      showSelected=c("validation.fold", "neighbors"))+
    geom_point(aes(
      input, output, fill=fold.fac),
      shape=21,
      size=3,
      color="black",
      alpha=0.7,
      clickSelects="validation.fold",
      data=data.table(in.out.dt, panel="fold"))+
    geom_point(aes(
      input, output, color=set),
      fill=NA,
      size=3,
      showSelected="validation.fold",
      data=data.table(set.dt[order(set)], panel="set"))+
    scale_color_manual(values=set.colors)+
    scale_fill_manual("fold", values=fold.colors, guide=guide_legend(order=1)),
  verr=ggplot()+
    ggtitle("Validation error")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=1000)+
    facet_grid(panel ~ fun.name)+
    geom_ribbon(aes(
      neighbors, ymin=mean-sd, ymax=mean+sd),
      alpha=0.5,
      data=data.table(verr, panel="mean"))+
    geom_line(aes(
      neighbors, mean),
      data=data.table(verr, panel="mean"))+
    geom_point(aes(
      neighbors, mean),
      shape=21,
      fill="white",
      data=data.table(verr.min, panel="mean"))+
    geom_tallrect(aes(
      xmin=neighbors-0.5,
      xmax=neighbors+0.5),
      alpha=0.5,
      color=NA,
      fill=pred.colors[["prediction"]],
      clickSelects="neighbors",
      data=neighbors.dt)+
    geom_line(aes(
      neighbors, mse, color=fold.fac, group=fold.fac),
      clickSelects="validation.fold",
      size=3,
      alpha=0.8,
      data=data.table(rss.dt[set=="validation"], panel="folds"))+
    geom_point(aes(
      neighbors, mse, color=fold.fac),
      shape=21,
      fill="white",
      clickSelects="validation.fold",
      data=data.table(min.dt, panel="folds"))+
    scale_color_manual("fold", values=fold.colors)+
    ##coord_cartesian(ylim=c(0,2))+
    ylab("mean squared error in validation set")+
    scale_x_continuous()))
animint2gist(viz)
