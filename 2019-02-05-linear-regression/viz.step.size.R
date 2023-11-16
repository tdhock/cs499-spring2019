library(animint2)
library(data.table)
ozone.url <- "https://hastie.su.domains/ElemStatLearn/datasets/ozone.data"
if(!file.exists("ozone.tsv")){
  download.file(ozone.url, "ozone.tsv")
}
ozone <- fread("ozone.tsv")
ozone.sc <- scale(ozone)
X.mat <- cbind(1, ozone.sc[, "temperature"])
y.vec <- ozone.sc[, "ozone"]
set.seed(3)
fold.vec <- sample(rep(1:2, l=nrow(X.mat)))
validation.fold <- 1
is.train <- fold.vec != validation.fold
X.train.mat <- X.mat[is.train,]
y.train.vec <- y.vec[is.train]
fit <- lm.fit(X.train.mat, y.train.vec)
fit.dt <- data.table(
  intercept=fit$coef[1],
  slope=fit$coef[2])
library(data.table)
ggplot()+
  geom_point(aes(
    temperature, ozone, color=is.train),
    shape=21,
    fill=NA,
    data=data.table(ozone.sc))+
  geom_abline(aes(
    intercept=intercept, slope=slope),
    data=fit.dt)

n.grid <- 200
min.int <- -0.2
max.int <- 0.1
min.slope <- -0.2
max.slope <- 1
grid.dt <- data.table(expand.grid(
  intercept=seq(min.int, max.int, l=n.grid),
  slope=seq(min.slope, max.slope, l=n.grid)))
grid.dt[, cost := {
  weight.mat <- rbind(intercept, slope) #p x g
  pred.mat <- X.train.mat %*% weight.mat #n x g
  res.mat <- pred.mat - y.train.vec
  0.5 * colMeans(res.mat * res.mat)
}]
ggplot()+
  theme_bw()+
  geom_contour(aes(
    intercept, slope, z=cost, color=..level..),
    breaks=seq(0.2, 1, by=0.01),
    data=grid.dt)+
  scale_color_gradient("cost", low="red", high="white")+
  geom_point(aes(
    intercept, slope),
    shape=21,
    fill="red",
    data=fit.dt)

max.it <- 30
cost.dt.list <- list()
weight.dt.list <- list()
step.size.vec <- 1e-3
step.size.vec <- round(10^seq(-3.75, -1.25, by=0.25), 4)
log10.step.size.vec <- log10(step.size.vec)
log10.diff.vec <- diff(log10.step.size.vec)/2
step.size.dt <- data.table(
  step.size=step.size.vec,
  log10.step.size=log10.step.size.vec,
  before=log10.step.size.vec-c(log10.diff.vec[1], log10.diff.vec),
  after=log10.step.size.vec+c(
    log10.diff.vec, log10.diff.vec[length(log10.diff.vec)]))
for(step.size in step.size.vec){
  weight.vec <- c(0, 0)
  for(iteration in c(1:max.it, Inf)){
    if(iteration==Inf){
      weight.vec <- fit$coef
    }
    all.res.vec <- X.mat %*% weight.vec - y.vec
    train.res.vec <- all.res.vec[is.train,]
    gradient.vec <- t(X.train.mat) %*% train.res.vec
    weight.dt.list[[paste(step.size, iteration)]] <- data.table(
      iteration,
      step.size,
      intercept=weight.vec[1],
      slope=weight.vec[2],
      intercept.gradient=gradient.vec[1],
      slope.gradient=gradient.vec[2])
    cost.dt.list[[paste(step.size, iteration)]] <- data.table(
      residual=as.numeric(all.res.vec),
      set=ifelse(is.train, "train", "validation"))[, data.table(
        parameter="iteration",
        iteration,
        step.size,
        cost=0.5 * mean(residual*residual)
      ), by=list(set)]
    weight.vec <- weight.vec - step.size * gradient.vec
  }
}
cost.dt <- do.call(rbind, cost.dt.list)
weight.dt <- do.call(rbind, weight.dt.list)
weight.dt[, intercept.end := intercept-intercept.gradient]
weight.dt[, slope.end := slope-slope.gradient]
min.dt <- cost.dt[iteration<Inf, .SD[which.min(cost)], by=list(set, step.size)]
ggplot()+
  coord_cartesian(ylim=c(0.2, 2))+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(step.size ~ .)+
  geom_line(aes(
    iteration, cost, group=set, color=set),
    data=cost.dt[iteration<Inf])+
  geom_point(aes(
    iteration, cost, color=set, fill=model),
    shape=21,
    data=data.table(cost.dt[iteration==Inf], model="least squares"))+
  scale_fill_manual(values=c(
    "min cost"="black",
    "least squares"="white"))+
  geom_point(aes(
    iteration, cost, color=set, fill=model),
    shape=21,
    data=data.table(min.dt, model="min cost"))

ggplot()+
  geom_point(aes(
    log10(step.size), cost, color=set),
    data=min.dt)

ozone.dt <- data.table(ozone.sc, set=ifelse(is.train, "train", "validation"))
max.cost <- 0.5
text.x <- -0.2
set.colors <- c(
  "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", 
  "#E5C494", "#B3B3B3")
viz <- animint(
  regression=ggplot()+
    ggtitle("Linear regression model")+
    xlab("input/feature: temperature")+
    ylab("output/label: ozone")+
    theme_bw()+
    theme_animint(height=300)+
    geom_point(aes(
      temperature, ozone, color=set),
      shape=21,
      fill=NA,
      data=ozone.dt)+
    scale_color_manual(values=set.colors)+
    geom_abline(aes(
      intercept=intercept, slope=slope, key=1),
      showSelected=c("iteration", "step.size"),
      data=weight.dt),
  contours=ggplot()+
    ggtitle("Level curves of mean squared error")+
    theme_bw()+
    theme_animint(height=300)+
    geom_contour(aes(
      intercept, slope, z=cost, color=..level..),
      breaks=seq(0.2, 1, by=0.01),
      data=grid.dt)+
    geom_segment(aes(
      intercept, slope,
      key=1,
      xend=intercept.end,
      yend=slope.end),
      showSelected=c("iteration", "step.size"),
      data=weight.dt)+
    scale_color_gradient("cost", low="red", high="white")+
    geom_point(aes(
      intercept, slope),
      shape=21,
      color="red",
      fill="black",
      data=fit.dt)+
    geom_path(aes(
      intercept, slope, key=1),
      showSelected="step.size",
      color="blue",
      size=3,
      data=weight.dt[iteration<Inf])+
    coord_cartesian(xlim=c(min.int, max.int), ylim=c(min.slope, max.slope))+
    geom_point(aes(
      intercept, slope, key=iteration),
      showSelected=c("step.size"),
      clickSelects="iteration",
      alpha=0.51,
      size=5,
      data=weight.dt)+
    ## geom_point(aes(
    ##   intercept, slope, key=1),
    ##   showSelected=c("iteration", "step.size"),
    ##   size=3,
    ##   data=weight.dt)+
    geom_text(aes(
      text.x, 0, key=1, label=sprintf("cost=%.4f", cost)),
      showSelected=c("iteration", "step.size"),
      hjust=0,
      data=cost.dt)+
    geom_text(aes(
      text.x, -0.1, key=1, label=paste0("iteration=", iteration)),
      showSelected=c("iteration", "step.size"),
      hjust=0,
      data=cost.dt)+
    geom_text(aes(
      text.x, -0.2, key=1, label=sprintf("step size=%.4f", step.size)),
      showSelected=c("iteration", "step.size"),
      hjust=0,
      data=cost.dt),
  cost=ggplot()+
    ggtitle("Train/validation error, select iteration and step size")+
    scale_color_manual(values=set.colors)+
    theme_bw()+
    theme_animint(width=800, height=300)+
    facet_grid(. ~ parameter, scales="free")+
    coord_cartesian(ylim=c(0.2, max.cost))+
    geom_point(aes(
      log10(step.size), cost, color=set, fill=model),
      data=data.table(min.dt, model="min cost")[, parameter := "log10(step size)"])+
    ylab("mean squared error")+
    xlab("")+
    geom_tallrect(aes(
      xmin=before, xmax=after),
      clickSelects="step.size",
      alpha=0.5,
      data=data.table(step.size.dt, parameter="log10(step size)"))+
    geom_line(aes(
      iteration,
      ifelse(max.cost < cost, Inf, cost),
      group=paste(set, step.size), key=set, color=set),
      showSelected="step.size",
      data=cost.dt[iteration<Inf])+
    geom_point(aes(
      iteration, cost, color=set, fill=model, key=set),
      shape=21,
      showSelected="step.size",
      data=data.table(cost.dt[iteration==Inf], model="un-regularized"))+
    scale_fill_manual(values=c(
      "un-regularized"="black",
      "min cost"="white"))+
    geom_point(aes(
      iteration, cost, color=set, fill=model, key=set),
      shape=21,
      showSelected="step.size",
      data=data.table(min.dt, model="min cost"))+
    geom_tallrect(aes(
      xmin=ifelse(iteration==Inf, max.it+0.5, iteration-0.5),
      xmax=ifelse(iteration==Inf, Inf, iteration+0.5)),
      alpha=0.5,
      clickSelects="iteration",
      data=cost.dt[set=="validation" & step.size==step.size[1] ]),
  duration=list(
    step.size=400,
    iteration=400),
  out.dir="viz.step.size",
  title="Gradient descent with several step sizes for 1d linear regression in ozone data",
  source="https://github.com/tdhock/cs499-spring2019/blob/master/2019-02-05-linear-regression/viz.step.size.R",
  time=list(
    variable="iteration",
    ms=500))
viz
if(FALSE){
  animint2pages(viz, "2019-02-05-linear-regression-1d-grad-desc-ozone-steps")
}
