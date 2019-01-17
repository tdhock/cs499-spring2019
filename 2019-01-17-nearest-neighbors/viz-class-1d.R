library(data.table)
library(class)
fun.list <- list(
  constant=function(x)0.75,
  linear=function(x)(x+2)/4,
  quadratic=function(x){
    p <- x*x/4+0.2
    ifelse(p>1, 1, p)
  },
  sin=function(x)(sin(3*x)+1)/2)
N <- 100
set.seed(1)
min.x <- -2
max.x <- 2
input.vec <- runif(N, min.x, max.x)
in.out.dt <- data.table(fun.name=names(fun.list))[, {
  f <- fun.list[[fun.name]]
  prob.vec <- f(input.vec)
  list(
    input=input.vec,
    true.prob=prob.vec,
    output=ifelse(prob.vec<runif(N), 0, 1),
    set=c("train", "validation"))
}, by=list(fun.name)]
library(animint2)
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(set ~ fun.name)+
  geom_line(aes(
    input, true.prob, color="truth"),
    size=2,
    data=in.out.dt)+
  geom_point(aes(
    input, output),
    data=in.out.dt)

neighbors.dt <- data.table(neighbors=1:(N/2))
pred.dt <- neighbors.dt[, {
  in.out.dt[, {
    is.train <- set=="train"
    fit <- class::knn(
      cbind(input[is.train]),
      cbind(input),
      output[is.train],
      prob=TRUE,
      k=neighbors)
    data.table(
      neighbors,
      input,
      output,
      pred.class=fit,
      set)
  }, by=list(fun.name)]
}, by=list(neighbors)]

grid.dt <- neighbors.dt[, {
  in.out.dt[, {
    is.train <- set=="train"
    x <- seq(min.x, max.x, l=200)
    fit <- class::knn(
      cbind(input[is.train]),
      cbind(x),
      output[is.train],
      prob=TRUE,
      k=neighbors)
    f <- fun.list[[fun.name]]
    pred.winning <- attr(fit, "prob")
    data.table(
      input=x,
      true.prob=f(x),
      pred.prob=ifelse(fit==1, pred.winning, 1-pred.winning))
  }, by=list(fun.name)]
}, by=list(neighbors)]

pred.dt[, panel := neighbors]
in.out.dt[, panel := set]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(panel ~ fun.name)+
  geom_line(aes(
    input, true.prob, color="truth"),
    size=2,
    data=in.out.dt)+
  geom_point(aes(
    input, output),
    data=in.out.dt)+
  geom_line(aes(
    input, pred.prob, color="prediction"),
    data=grid.dt)

err.dt <- pred.dt[, {
  is.error <- pred.class != output
  total.errors <- sum(is.error)
  data.table(total.errors, percent.error=100*total.errors/length(is.error))
  }, by=list(neighbors, fun.name, set)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ fun.name)+
  geom_line(aes(
    neighbors, percent.error, color=set),
    data=err.dt)+
  scale_x_continuous()

min.dt <- err.dt[set=="validation", .SD[percent.error==min(percent.error)], by=list(fun.name)]
pred.dt[, fun.type := "prediction"]
grid.tall <- melt(
  grid.dt,
  measure.vars=c("pred.prob", "true.prob"),
  variable.name="fun.type")
in.out.dt[, fun.type := "truth"]
set.colors <- c(train="grey50", validation="black")
pred.colors <- c(pred.prob="red", true.prob="blue")
viz <- animint(
  title="Nearest neighbors algorithm for classification",
  funs=ggplot()+
    ggtitle("Data and predictions")+
    theme_bw()+
    theme_animint(width=1000)+
    scale_fill_manual(values=set.colors)+
    scale_color_manual(values=pred.colors)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(set ~ fun.name)+
    geom_hline(yintercept=0.5, color="grey")+
    ylab("output")+
    geom_point(aes(
      input, output, fill=set),
      size=4,
      data=in.out.dt)+
    geom_line(aes(
      input, value, color=fun.type),
      data=grid.tall[fun.type=="true.prob" & neighbors==1])+
    geom_line(aes(
      input, value, color=fun.type),
      showSelected="neighbors",
      data=grid.tall[fun.type=="pred.prob"]),
  select=ggplot()+
    ggtitle("Train and validation errors")+
    theme_bw()+
    theme_animint(width=950, height=300)+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(. ~ fun.name)+
    geom_line(aes(
      neighbors, percent.error, color=set, group=set),
      data=err.dt)+
    geom_point(aes(
      neighbors, percent.error, color=set),
      fill="white",
      data=min.dt)+
    geom_text(aes(
      N/2, 10, label=paste0(
        neighbors, " nearest neighbor", ifelse(neighbors==1, "", "s"))),
      showSelected="neighbors",
      color=pred.colors[["pred.prob"]],
      hjust=1,
      data=neighbors.dt)+
    geom_text(aes(
      N/2, ifelse(set=="train", 5, 0), color=set, label=sprintf(
        "%s error: %.0f%%", set, percent.error)),
      showSelected="neighbors",
      hjust=1,
      data=err.dt)+
    geom_tallrect(aes(
      xmin=neighbors-0.5,
      xmax=neighbors+0.5),
      alpha=0.5,
      clickSelects="neighbors",
      data=neighbors.dt)+
    scale_x_continuous()+
    scale_color_manual(values=set.colors))
viz

##animint2gist(viz)
