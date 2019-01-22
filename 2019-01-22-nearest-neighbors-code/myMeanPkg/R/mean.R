#' Compute mean using pure C
#'
#' @param x.vec vector with at least 1 element, which is coerced to integer
#'
#' @return mean, numeric/double scalar
#' @export
#'
#' @examples
#' my_mean_R(c(1,2))
my_mean_R <- function(x.vec){
  res.list <- .C(
    "my_mean_interface",
    as.integer(x.vec),
    length(x.vec),
    mean=numeric(1),
    PACKAGE="myMeanPkg")
  res.list$mean
}

#' Compute mean using Eigen mean reduction method.
#'
#' @param x.vec vector of data which will be coerced using as.double
#'
#' @return mean of data
#' @export
#'
#' @examples
#' eigen_mean_R(c(1,2))
eigen_mean_R <- function(x.vec){
  res.list <- .C(
    "eigen_mean_interface",
    as.double(x.vec),
    length(x.vec),
    mean=numeric(1),
    PACKAGE="myMeanPkg")
  res.list$mean
}
