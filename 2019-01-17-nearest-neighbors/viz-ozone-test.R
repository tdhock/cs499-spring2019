library(data.table)
library(parallel)
data(ozone, package="ElemStatLearn")
library(animint2)

ggplot()+
  geom_point(aes(
    wind, temperature, fill=ozone),
    shape=21,
    data=ozone)+
  scale_fill_gradient(low="white", high="red")+
  theme_bw()

n.folds <- 3
set.seed(1)
in.out.dt <- data.table(
  ozone,
  fold=sample(rep(1:n.folds, l=nrow(ozone))))
neighbors.dt <- data.table(neighbors=1:20)
pred.dt <- do.call(rbind, parallel::mclapply(1:n.folds, function(validation.fold){
  neighbors.dt[, {
    in.out.dt[, {
      is.train <- fold!=validation.fold
      input.mat <- cbind(wind, temperature)
      output.vec <- ozone
      fit <- caret::knnreg(input.mat[is.train,], output.vec[is.train], k=neighbors)
      set <- ifelse(is.train, "train", "validation")
      pred.vec <- predict(fit, input.mat)
      res.vec <- pred.vec-output.vec
      data.table(
        res.vec,
        set)[, data.table(
          validation.fold,
          mse=mean(res.vec*res.vec)
        ), by=list(set)]
    }]
  }, by=list(neighbors)]
}))
valid.dt <- pred.dt[set=="validation"]
(min.dt <- valid.dt[, .SD[min(mse)==mse], by=list(validation.fold)])
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(validation.fold ~ .)+
  geom_line(aes(
    neighbors, mse, color=set, group=set),
    data=pred.dt)+
  geom_point(aes(
    neighbors, mse, color=set),
    data=min.dt,
    fill="white",
    shape=21)

mean.dt <- valid.dt[, data.table(
  mean=mean(mse),
  sd=sd(mse)
), by=list(neighbors, set)]
mean.min.dt <- mean.dt[, .SD[mean==min(mean)] ]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_ribbon(aes(
    neighbors, ymin=mean-sd, ymax=mean+sd),
    data=mean.dt,
    alpha=0.5)+
  geom_line(aes(
    neighbors, mean),
    data=mean.dt)+
  geom_point(aes(
    neighbors, mean),
    data=mean.min.dt,
    fill="white",
    shape=21)

n.grid <- 100
wind.grid <- in.out.dt[, seq(min(wind), max(wind), l=n.grid)]
temp.grid <- in.out.dt[, seq(min(temperature), max(temperature), l=n.grid)]
grid.dt <- data.table(expand.grid(
  wind=wind.grid,
  temperature=temp.grid))[, {
    grid.mat <- cbind(wind, temperature)
    do.call(rbind, lapply(1:n.folds, function(validation.fold){
      train.dt <- in.out.dt[fold!=validation.fold]
      neighbors.dt[, {
        fit <- caret::knnreg(
          train.dt[, cbind(wind, temperature)],
          train.dt$ozone,
          k=neighbors)
        pred.vec <- predict(fit, grid.mat)
        pred.mat <- matrix(pred.vec, n.grid, n.grid)
        contour.list <- contourLines(
          wind.grid, temp.grid, pred.mat, levels=seq(0, 180, by=5))
        do.call(rbind, lapply(seq_along(contour.list), function(group){
          with(contour.list[[group]], data.table(
            validation.fold,
            group, level, wind=x, temperature=y))
        }))
      }, by=list(neighbors)]
    }))
  }]
set.dt <- data.table(validation.fold=1:n.folds)[, {
  data.table(
    in.out.dt,
    row=seq_along(in.out.dt$fold),
    set=ifelse(validation.fold==in.out.dt$fold, "validation", "train"))
}, by=list(validation.fold)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(validation.fold ~ neighbors)+
  scale_color_gradient(
    "prediction",
    low="white", high="blue")+
  geom_path(aes(
    wind, temperature, color=level, group=group),
    data=grid.dt[neighbors<5])+
  geom_point(aes(
    wind, temperature, fill=ozone),
    shape=21,
    data=set.dt[set=="train"])+
  scale_fill_gradient(low="white", high="red")

pred.dt[, fold.fac := factor(validation.fold)]
min.dt[, fold.fac := factor(validation.fold)]
animint(
  title="Nearest neighbors for regression with 2D inputs",
  data=ggplot()+
    xlab("x1 = wind")+
    ylab("x2 = temperature")+
    geom_text(aes(
      min(wind.grid), max(temp.grid)+1,
      key=1,
      label=paste0(
        neighbors,
        " nearest neighbors for validation fold ",
        validation.fold)),
      showSelected=c("validation.fold", "neighbors"),
      hjust=0,
      data=pred.dt[set=="validation"])+
    ggtitle("Data and nearest neighbor regression model")+
    theme_bw()+
    scale_color_gradient(
      "prediction f(x)",
      low="white", high="red")+
    geom_path(aes(
      wind, temperature, color=level, group=group,
      key=seq_along(wind),
      tooltip=paste0("prediction=", level)),
      showSelected=c("neighbors", "validation.fold"),
      data=grid.dt)+
    geom_point(aes(
      wind, temperature, fill=ozone, size=set,
      key=row,
      tooltip=paste0(
        "ozone=", ozone,
        " fold=", fold,
        " set=", set)),
      data=set.dt,
      showSelected="validation.fold")+
    scale_size_manual(values=c(train=3, validation=2))+
    scale_fill_gradient(
      "y = ozone",
      low="white", high="red"),
  verr=ggplot()+
    ggtitle("Error for each fold/neighbors")+
    ylab("Mean squared error")+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    facet_grid(set ~ .)+
    scale_color_manual(
      "fold",
      values=RColorBrewer::brewer.pal(n.folds+1, "Set1")[-1])+
    ## geom_ribbon(aes(
    ##   neighbors, ymin=mean-sd, ymax=mean+sd),
    ##   data=mean.dt,
    ##   alpha=0.3)+
    ## geom_line(aes(
    ##   neighbors, mean),
    ##   data=mean.dt)+
    ## geom_point(aes(
    ##   neighbors, mean),
    ##   data=mean.min.dt,
    ##   fill="white",
    ##   shape=21)+
    geom_tallrect(aes(
      xmin=neighbors-0.5,
      xmax=neighbors+0.5),
      data=neighbors.dt,
      clickSelects="neighbors",
      alpha=0.5)+
    geom_line(aes(
      neighbors, mse, color=fold.fac, group=validation.fold),
      clickSelects="validation.fold",
      showSelected="set",
      alpha=0.8,
      size=4,
      data=pred.dt)+
    geom_point(aes(
      neighbors, mse, color=fold.fac),
      data=min.dt,
      showSelected="set",
      clickSelects="validation.fold",
      fill="white",
      shape=21),
  duration=list(validation.fold=200)
  )

##animint2gist(viz)
