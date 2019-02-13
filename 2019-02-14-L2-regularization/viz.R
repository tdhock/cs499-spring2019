library(animint2)
library(data.table)

data(ozone, package="ElemStatLearn")
plot(ozone)
plot(ozone ~ temperature, data=ozone)

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
min.int <- -1
max.int <- 1
min.slope <- 0
max.slope <- 1
grid.dt <- data.table(expand.grid(
  intercept=seq(min.int, max.int, l=n.grid),
  slope=seq(min.slope, max.slope, l=n.grid)))
grid.dt[, cost := {
  weight.mat <- rbind(intercept, slope) #p x g
  pred.mat <- X.train.mat %*% weight.mat #n x g
  res.mat <- pred.mat - y.train.vec
  0.5 * colSums(res.mat * res.mat)
}]
ggplot()+
  theme_bw()+
  geom_contour(aes(
    intercept, slope, z=cost, color=..level..),
    data=grid.dt)+
  scale_color_gradient("cost", low="red", high="white")+
  geom_point(aes(
    intercept, slope),
    shape=21,
    fill="red",
    data=fit.dt)

weight.vec <- c(0, 0)
step.size <- 0.0001
max.it <- 100
weight.dt.list <- list()
pen.grid.list <- list()
opt.thresh <- 1e-3
pen.diff <- -0.5
for(penalty in 10^seq(4, -1, by=pen.diff)){
  one.pen.grid <- data.table(expand.grid(
    intercept=int.grid <- seq(-0.8, 0.8, l=n.grid),
    slope=slope.grid <- seq(-0.7, 1, l=n.grid)))
  one.pen.grid[, cost := {
    weight.mat <- rbind(intercept, slope) #p x g
    pred.mat <- X.train.mat %*% weight.mat #n x g
    res.mat <- pred.mat - y.train.vec
    0.5 * colSums(res.mat^2) + 0.5*penalty*colSums(weight.mat^2)
  }]
  pen.grid.list[[paste(penalty)]] <- data.table(
    penalty, one.pen.grid)
  iteration <- 1
  grad.norm <- Inf
  while(opt.thresh < grad.norm){
    train.res.vec <- X.train.mat %*% weight.vec - y.train.vec
    gradient.vec <- t(X.train.mat) %*% train.res.vec + penalty * weight.vec
    grad.norm <- sum(gradient.vec^2)
    weight.dt.list[[paste(iteration, penalty)]] <- data.table(
      penalty,
      iteration,
      grad.norm,
      intercept=weight.vec[1],
      slope=weight.vec[2],
      intercept.gradient=gradient.vec[1],
      slope.gradient=gradient.vec[2])
    iteration <- iteration+1
    weight.vec <- weight.vec - step.size * gradient.vec
  }
}
pen.grid <- do.call(rbind, pen.grid.list)
(weight.dt <- do.call(rbind, weight.dt.list))

ggplot()+
  theme_bw()+
  facet_grid(. ~ penalty)+
  theme(
    panel.margin=grid::unit(0, "lines"))+
  scale_y_log10()+
  geom_point(aes(
    iteration, grad.norm),
    shape=21,
    data=weight.dt)

weight.dt[, intercept.end := intercept-intercept.gradient]
weight.dt[, slope.end := slope-slope.gradient]
opt.dt <- weight.dt[grad.norm < opt.thresh]
weight.mat <- opt.dt[, rbind(intercept, slope)]
pred.mat <- X.mat %*% weight.mat
res.mat <- pred.mat-y.vec
is.set.list <- list(
  train=is.train,
  validation=!is.train)
set.err.list <- list()
for(set.name in names(is.set.list)){
  is.set <- is.set.list[[set.name]]
  set.err.list[[set.name]] <- data.table(
    set.name,
    penalty=opt.dt$penalty,
    tse=0.5*colSums(res.mat[is.set,]^2))
}
(set.err <- do.call(rbind, set.err.list))
min.dt <- set.err[, .SD[which.min(tse)], by=list(set.name)]
ggplot()+
  geom_line(aes(
    penalty, tse, color=set.name),
    data=set.err)+
  geom_point(aes(
    penalty, tse, color=set.name),
    shape=21,
    data=min.dt,
    fill="white")+
  scale_x_log10()+
  scale_y_log10()

rad.seq <- seq(0, 2*pi, l=100)
ball.dt <- data.table(
  penalty=opt.dt$penalty,
  l2norm=sqrt(colSums(weight.mat^2)))[, data.table(
    intercept=l2norm*sin(rad.seq),
    slope=l2norm*cos(rad.seq)), by=list(penalty, l2norm)]
ggplot()+
  theme_bw()+
  geom_path(aes(
    intercept, slope, group=l2norm),
    data=ball.dt)+
  geom_contour(aes(
    intercept, slope, z=cost, color=..level..),
    breaks=set.err[set.name=="train", tse],
    data=grid.dt)+
  scale_color_gradient("cost", low="red", high="grey90")+
  geom_point(aes(
    intercept, slope),
    shape=21,
    fill="red",
    data=fit.dt)+
  geom_point(aes(
    intercept, slope),
    data=opt.dt)+
  geom_text(aes(
    intercept, slope, label=sprintf("%.0e ", penalty)),
    hjust=1, 
    data=opt.dt)


pchr <- function(x)sprintf("%.0e ", x)
pfac <- function(x){
  sorted <- sort(unique(x))
  factor(pchr(x), pchr(sorted))
}
pen.grid[, Penalty := pfac(penalty)]
opt.dt[, Penalty := pfac(penalty)]
weight.dt[, Penalty := pfac(penalty)]
pen.grid[, relative.cost := (cost-min(cost))/(max(cost)-min(cost))+1, by=list(penalty)]
ggplot()+
  theme_bw()+
  facet_wrap("Penalty")+
  theme(
    panel.margin=grid::unit(0, "lines"))+
  scale_color_gradient("relative
cost", low="red", high="grey90")+
  geom_contour(aes(
    intercept, slope, z=log10(relative.cost), color=..level..),
    data=pen.grid)+
  geom_point(aes(
    intercept, slope, fill=solution),
    shape=21,
    data=data.table(fit.dt, solution="Un-regularized"))+
  geom_path(aes(
    intercept, slope),
    data=weight.dt)+
  geom_point(aes(
    intercept, slope, fill=solution),
    shape=21,
    data=data.table(solution="L2-regularized", opt.dt))

my.contours <- pen.grid[, {
  L <- contourLines(
    int.grid, slope.grid,
    matrix(log10(relative.cost), n.grid, n.grid),
    levels=seq(0.05, 0.3, by=0.05))
  data.table(group.i=seq_along(L))[, {
    with(L[[group.i]], data.table(
      relative.cost=level, intercept=x, slope=y))
  }, by=list(group.i)]
}, by=list(penalty, Penalty)]
ggplot()+
  theme_bw()+
  facet_wrap("Penalty")+
  theme(
    panel.margin=grid::unit(0, "lines"))+
  scale_color_gradient("relative
cost", low="red", high="grey90")+
  geom_path(aes(
    intercept, slope, group=group.i, color=relative.cost),
    data=my.contours)+
  geom_point(aes(
    intercept, slope, fill=solution),
    shape=21,
    data=data.table(fit.dt, solution="Un-regularized"))+
  geom_path(aes(
    intercept, slope),
    data=weight.dt)+
  geom_point(aes(
    intercept, slope, fill=solution),
    shape=21,
    data=data.table(solution="L2-regularized", opt.dt))

    
ozone.dt <- data.table(ozone.sc, set=ifelse(is.train, "train", "validation"))
set.colors <- c(
  "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", 
  "#E5C494", "#B3B3B3")
set.err[, Penalty := pfac(penalty)]
ball.dt[, Penalty := pfac(penalty)]
weight.dt[, frame := 1:.N]
viz <- animint(
  title="L2-regularized (Ridge) least squares linear regression",
  ## regression=ggplot()+
  ##   ggtitle("Linear regression model")+
  ##   xlab("input/feature: temperature")+
  ##   ylab("output/label: ozone")+
  ##   theme_bw()+
  ##   theme_animint(height=300)+
  ##   theme(panel.margin=grid::unit(0, "lines"))+
  ##   facet_grid(set ~ .)+
  ##   geom_point(aes(
  ##     temperature, ozone),
  ##     shape=21,
  ##     fill=NA,
  ##     data=ozone.dt)+
  ##   scale_color_manual(values=set.colors, guide="none")+
  ##   geom_abline(aes(
  ##     intercept=intercept, slope=slope, key=1),
  ##     showSelected="iteration",
  ##     data=weight.dt),
  balls=ggplot()+
    coord_equal()+
    ggtitle("Un-regularized loss and constraint ball")+
    theme_bw()+
    geom_contour(aes(
      intercept, slope, z=cost, color=..level..),
      breaks=set.err[set.name=="train", tse],
      data=grid.dt)+
    geom_path(aes(
      intercept, slope, key=1, group=l2norm),
      showSelected="Penalty",
      data=ball.dt)+
    scale_color_gradient("Un-regularized loss", low="red", high="grey90")+
    geom_point(aes(
      intercept, slope, fill=solution),
      shape=21,
      size=3,
      data=data.table(solution="Un-regularized", fit.dt))+
    geom_path(aes(
      intercept, slope, key=1),
      showSelected="Penalty",
      color="grey",
      data=weight.dt)+
    geom_point(aes(
      intercept, slope, key=1, fill=solution),
      showSelected=c("frame"),
      size=3,
      data=data.table(weight.dt, solution="iteration"))+
    geom_point(aes(
      intercept, slope, key=1, fill=solution),
      shape=21,
      size=3,
      showSelected="Penalty",
      data=data.table(solution="L2-regularized", opt.dt)),
  contours=ggplot()+
    coord_equal()+
    ggtitle("Reg. cost = square loss + L2 pen.")+
    theme_bw()+
    theme(
      panel.margin=grid::unit(0, "lines"))+
    scale_color_gradient("Relative cost", low="red", high="grey90")+
    geom_path(aes(
      intercept, slope,
      key=paste(Penalty, relative.cost, group.i),
      group=group.i,
      color=relative.cost),
      showSelected="Penalty",
      data=my.contours)+
    geom_point(aes(
      intercept, slope, fill=solution),
      size=3,
      shape=21,
      data=data.table(fit.dt, solution="Un-regularized"))+
    geom_path(aes(
      intercept, slope, key=1),
      showSelected="Penalty",
      color="grey",
      data=weight.dt)+
    geom_point(aes(
      intercept, slope, key=1, fill=solution),
      size=3,
      showSelected=c("frame"),
      data=data.table(weight.dt, solution="iteration"))+
    geom_point(aes(
      intercept, slope, key=1, fill=solution),
      showSelected="Penalty",
      shape=21,
      size=3,
      data=data.table(solution="L2-regularized", opt.dt)),
  cost=ggplot()+
    ggtitle("Train/validation loss, select penalty")+
    scale_color_manual(values=set.colors)+
    theme_bw()+
    theme_animint(height=400, width=400)+
    ylab("log10(total squared error)")+
    geom_line(aes(
      log10(penalty), log10(tse), group=set.name, color=set.name),
      data=set.err)+
    scale_fill_manual(values=c(
      "un-regularized"="black",
      "min loss"="white"))+
    geom_point(aes(
      log10(penalty), log10(tse), color=set.name, fill=model),
      shape=21,
      data=data.table(min.dt, model="min loss"))+
    geom_tallrect(aes(
      xmin=log10(penalty)-0.5*abs(pen.diff),
      xmax=log10(penalty)+0.5*abs(pen.diff)),
      alpha=0.5,
      clickSelects="Penalty",
      data=set.err[set.name=="validation"]),
  iterations=ggplot()+
    ggtitle("Optimality criterion, select iteration")+
    theme_bw()+
    theme_animint(width=600)+
    ylab("Optimality criterion = log10(L2 norm of gradient)")+
    geom_line(aes(
      iteration, log10(grad.norm),
      key=1),
      showSelected="Penalty",
      data=weight.dt)+
    geom_hline(
      yintercept=log10(opt.thresh),
      color="grey")+
    geom_tallrect(aes(
      xmin=iteration-0.5,
      xmax=iteration+0.5,
      key=iteration),
      alpha=0.5,
      showSelected="Penalty",
      clickSelects="frame",
      data=weight.dt),
  duration=list(
    iteration=500,
    frame=500,
    Penalty=500))
print(viz)

animint2gist(viz)
