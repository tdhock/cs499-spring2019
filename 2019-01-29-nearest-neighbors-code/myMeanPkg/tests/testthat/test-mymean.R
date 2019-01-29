library(testthat)
library(myMeanPkg)

two.data <- c(2, 5)
set.seed(1)
rand.data <- rpois(30, 50)

test_that("my_mean_R works for 2 data", {
  mean.value <- my_mean_R(two.data)
  expect_equal(mean.value, 3.5)
})

test_that("my_mean_R works for random data", {
  mean.value <- my_mean_R(rand.data)
  expect_equal(mean.value, mean(rand.data))
})

test_that("my_mean_R errors for no data", {
  expect_error({
    my_mean_R(integer(0))
  }, "no data")
})

test_that("eigen_mean_R works for 2 data", {
  mean.value <- eigen_mean_R(two.data)
  expect_equal(mean.value, 3.5)
})

test_that("eigen_mean_R works for random data", {
  mean.value <- eigen_mean_R(rand.data)
  expect_equal(mean.value, mean(rand.data))
})

test_that("eigen_mean_R errors for no data", {
  expect_error({
    eigen_mean_R(integer(0))
  }, "no data")
})

