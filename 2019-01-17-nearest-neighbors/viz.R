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
    set=c("train", "validation"))
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

neighbors.dt <- data.table(neighbors=1:50)
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
    data.table(
      neighbors,
      input=x,
      pred=predict(fit, x))
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
  list(
    rss=sum(res.vec*res.vec)
  )
  }, by=list(neighbors, fun.name, set)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ fun.name)+
  geom_line(aes(
    neighbors, rss, color=set),
    data=rss.dt)+
  scale_x_continuous()+
  coord_cartesian(ylim=c(0, 100))

pred.dt[, fun.type := "prediction"]
grid.dt[, fun.type := "prediction"]
in.out.dt[, fun.type := "truth"]
set.colors <- c(train="black", validation="grey50")
pred.colors <- c(prediction="red", truth="blue")
viz <- animint(
  title="Nearest neighbors algorithm for regression",
  funs=ggplot()+
    theme_bw()+
    theme_animint(width=1000)+
    scale_fill_manual(values=set.colors)+
    scale_color_manual(values=pred.colors)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(set ~ fun.name)+
    geom_line(aes(
      input, true, color=fun.type),
      size=2,
      data=in.out.dt)+
    geom_point(aes(
      input, output, fill=set),
      size=4,
      data=in.out.dt)+
    geom_line(aes(
      input, pred, color=fun.type),
      showSelected="neighbors",
      data=grid.dt),
  select=ggplot()+
    theme_bw()+
    theme_animint(width=950, height=300)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ fun.name)+
    geom_line(aes(
      neighbors, rss, color=set, group=set),
      data=rss.dt)+
    geom_text(aes(
      10, 20, label=paste0(
        neighbors, " nearest neighbor", ifelse(neighbors==1, "", "s"))),
      showSelected="neighbors",
      color=pred.colors[["prediction"]],
      hjust=0,
      data=neighbors.dt)+
    geom_text(aes(
      10, ifelse(set=="train", 10, 0), color=set, label=sprintf(
        "%s error: %.2f", set, rss)),
      showSelected="neighbors",
      hjust=0,
      data=rss.dt)+
    geom_tallrect(aes(
      xmin=neighbors-0.5,
      xmax=neighbors+0.5),
      alpha=0.5,
      clickSelects="neighbors",
      data=neighbors.dt)+
    scale_x_continuous()+
    scale_color_manual(values=set.colors)+
    coord_cartesian(ylim=c(0, 100)))
viz

animint2gist(viz)
