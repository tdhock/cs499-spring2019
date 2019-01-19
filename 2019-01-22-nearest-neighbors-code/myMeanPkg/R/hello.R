my_mean_R <- function(x.vec){
  res.list <- .C(
    "my_mean_interface",
    as.integer(x.vec),
    length(x.vec),
    mean=numeric(1),
    PACKAGE="myMeanPkg")
  res.list$mean
}

eigen_mean_R <- function(x.vec){
  res.list <- .C(
    "eigen_mean_interface",
    as.double(x.vec),
    length(x.vec),
    mean=numeric(1),
    PACKAGE="myMeanPkg")
  res.list$mean
}
