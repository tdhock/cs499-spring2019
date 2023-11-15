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
library(data.table)
in.out.dt <- data.table(fun.name=names(fun.list))[, {
  f <- fun.list[[fun.name]]
  true.vec <- f(input.vec)
  list(
    input=input.vec,
    true=true.vec,
    output=true.vec+rnorm(N),
    set=rep(c("train", "validation"),l=length(input.vec)))
}, by=list(fun.name)]

library(animint2)
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(set ~ fun.name)+
  geom_line(aes(
    input, true, color="truth"),
    size=2,
    data=in.out.dt)+
  geom_point(aes(
    input, output),
    data=in.out.dt)

neighbors.dt <- data.table(neighbors=1:(N/2))
pred.dt <- neighbors.dt[, {
  in.out.dt[, {
    is.train <- set=="train"
    in.mat <- cbind(input[is.train])
    fit <- caret::knnreg(in.mat, output[is.train], k=neighbors)
    data.table(
      neighbors,
      input,
      output,
      pred=predict(fit, input),
      set)
  }, by=list(fun.name)]
}, by=list(neighbors)]

grid.dt <- neighbors.dt[, {
  in.out.dt[, {
    is.train <- set=="train"
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

pred.dt[, panel := neighbors]
in.out.dt[, panel := set]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(panel ~ fun.name)+
  geom_line(aes(
    input, true, color="truth"),
    size=2,
    data=in.out.dt)+
  geom_point(aes(
    input, output),
    data=in.out.dt)+
  geom_line(aes(
    input, pred, color="prediction"),
    data=pred.dt)

rss.dt <- pred.dt[, {
  res.vec <- pred-output
  rss <- sum(res.vec*res.vec)
  data.table(rss, mse=rss/length(res.vec))
  }, by=list(neighbors, fun.name, set)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ fun.name)+
  geom_line(aes(
    neighbors, rss, color=set),
    data=rss.dt)+
  scale_x_continuous()

ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ fun.name)+
  geom_line(aes(
    neighbors, mse, color=set),
    data=rss.dt)+
  scale_x_continuous()

min.dt <- rss.dt[set=="validation", .SD[which.min(mse)], by=list(fun.name)]

pred.dt[, fun.type := "prediction"]
grid.tall <- melt(
  grid.dt,
  measure.vars=c("prediction", "truth"),
  variable.name="fun.type")
in.out.dt[, fun.type := "truth"]
set.colors <- c(train="grey50", validation="black")
pred.colors <- c(prediction="red", truth="blue")
(viz <- animint(
  title="Nearest neighbors algorithm for regression",
  funs=ggplot()+
    ggtitle("Data and predictions")+
    theme_bw()+
    theme_animint(width=1000)+
    scale_fill_manual(values=set.colors)+
    scale_color_manual(values=pred.colors)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(set ~ fun.name)+
    ylab("output")+
    geom_point(aes(
      input, output, fill=set),
      size=4,
      data=in.out.dt)+
    geom_line(aes(
      input, value, color=fun.type),
      data=grid.tall[fun.type=="truth" & neighbors==1])+
    geom_line(aes(
      input, value,
      key="pred",
      color=fun.type),
      showSelected="neighbors",
      data=grid.tall[fun.type=="prediction"]),
  select=ggplot()+
    ggtitle("Train and validation errors")+
    theme_bw()+
    theme_animint(width=950, height=300)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ fun.name)+
    geom_line(aes(
      neighbors, mse, color=set, group=set),
      data=rss.dt)+
    geom_point(aes(
      neighbors, mse, color=set),
      fill="white",
      data=min.dt)+
    geom_text(aes(
      N/2, 0.3,
      key="neighbors",
      label=paste0(
        neighbors, " nearest neighbor", ifelse(neighbors==1, "", "s"))),
      showSelected="neighbors",
      color=pred.colors[["prediction"]],
      hjust=1,
      data=neighbors.dt)+
    geom_text(aes(
      N/2, ifelse(set=="train", 0.15, 0),
      color=set,
      key=set,
      label=sprintf(
        "%s error: %.4f", set, mse)),
      showSelected="neighbors",
      hjust=1,
      data=rss.dt)+
    ylab("mean squared error")+
    geom_tallrect(aes(
      xmin=neighbors-0.5,
      xmax=neighbors+0.5),
      alpha=0.5,
      clickSelects="neighbors",
      data=neighbors.dt)+
    scale_x_continuous()+
    coord_cartesian(ylim=c(0,2))+
    scale_color_manual(values=set.colors),
  out.dir="viz",
  duration=list(neighbors=1000),
  source="https://github.com/tdhock/cs499-spring2019/blob/master/2019-01-17-nearest-neighbors/viz.R"
  ))
if(FALSE){
  animint2pages(viz, "2019-01-nearest-neighbor-regression-one-split")
}
