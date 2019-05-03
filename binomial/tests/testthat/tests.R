#library(testthat)


## Tests for Checker Functions
### check_prob

context("Tests for valid prob values")

test_that("prob values are valid", {

  expect_true(check_prob(0.5))
  expect_error(check_prob(-2), "invalid prob value")
  expect_error(check_prob(10), "invalid prob value")
})

### check_trials
context("Tests for valid trials values")

test_that("trials values are valid", {

  expect_true(check_trials(10))
  expect_error(check_trials(-2), "trials must be a positive value")
  expect_error(check_trials(10.5), "trials must be an integer")
})

### check_success
context("Tests for valid success values")

test_that("success values are valid", {

  expect_true(check_success(10, 10))
  expect_true(check_success(10, 11))
  expect_error(check_success(-2, 3), "success must be a positive value")
  expect_error(check_success(10.5, 11), "success must be an integer")
  expect_error(check_success(4, 3), "success must be less than or equal to trials")

})


## Test for Summary Measures
### aux_mean
context("Tests for valid aux_mean() values")

test_that("aux_mean() values are valid", {

  t = c(1, 2, 3, -5, 0.5)
  p = c(0.5, 0.3, 0.7, -20.5, 2, -0.5)

  expect_equal(aux_mean(t[1], p[1]), 0.5)
  expect_equal(aux_mean(t[2], p[2]), 0.6)
  expect_error(aux_mean(t[4], p[1]), "trials must be a positive value")
  expect_error(aux_mean(t[5], p[1]), "trials must be an integer")
  expect_error(aux_mean(t[1], p[5]), "invalid prob value")
  expect_error(aux_mean(t[1], p[6]), "invalid prob value")



})


### aux_variance
context("Tests for valid aux_variance() values")

test_that("aux_variance() values are valid", {

  t = c(1, 2, 3, -5, 0.5)
  p = c(0.5, 0.3, 0.7, -20.5, 2, -0.5)

  expect_equal(aux_variance(t[1], p[1]), 0.25)
  expect_equal(aux_variance(t[2], p[2]), 0.42)
  expect_error(aux_variance(t[4], p[1]), "trials must be a positive value")
  expect_error(aux_variance(t[5], p[1]), "trials must be an integer")
  expect_error(aux_variance(t[1], p[5]), "invalid prob value")
  expect_error(aux_variance(t[1], p[6]), "invalid prob value")



})


### aux_mode
context("Tests for valid aux_mode() values")

test_that("aux_mode() values are valid", {

  t = c(1, 2, 3, -5, 0.5)
  p = c(0.5, 0.3, 0.7, -20.5, 2, -0.5)

  expect_equal(aux_mode(t[1], p[1]), 1)
  expect_equal(aux_mode(t[2], p[2]), 0)
  expect_error(aux_mode(t[4], p[1]), "trials must be a positive value")
  expect_error(aux_mode(t[5], p[1]), "trials must be an integer")
  expect_error(aux_mode(t[1], p[5]), "invalid prob value")
  expect_error(aux_mode(t[1], p[6]), "invalid prob value")



})


### aux_skewness
context("Tests for valid aux_skewness() values")

test_that("aux_skewness() values are valid", {

  t = c(1, 2, 3, -5, 0.5)
  p = c(0.5, 0.3, 0.7, -20.5, 2, -0.5)

  expect_equal(aux_skewness(t[1], p[1]), 0)
  expect_equal(aux_skewness(t[2], p[2]), 0.6172134)
  expect_error(aux_skewness(t[4], p[1]), "trials must be a positive value")
  expect_error(aux_skewness(t[5], p[1]), "trials must be an integer")
  expect_error(aux_skewness(t[1], p[5]), "invalid prob value")
  expect_error(aux_skewness(t[1], p[6]), "invalid prob value")



})


### aux_kurtosis
context("Tests for valid aux_kurtosis() values")

test_that("aux_kurtosis() values are valid", {

  t = c(1, 2, 3, -5, 0.5)
  p = c(0.5, 0.3, 0.7, -20.5, 2, -0.5)

  expect_equal(aux_kurtosis(t[1], p[1]), -2)
  expect_equal(aux_kurtosis(t[3], p[3]), -0.4126984)
  expect_error(aux_kurtosis(t[4], p[1]), "trials must be a positive value")
  expect_error(aux_kurtosis(t[5], p[1]), "trials must be an integer")
  expect_error(aux_kurtosis(t[1], p[5]), "invalid prob value")
  expect_error(aux_kurtosis(t[1], p[6]), "invalid prob value")



})


## Check context for binomial
### bin_choose
context("Tests for valid bin_choose() values")

test_that("bin_choose() values are valid", {

  x = c(1, 4, 3, -5, 1.5)
  y = c(1, 2, -2, .2, -0.5)

  expect_equal(bin_choose(x[1], y[1]), 1)
  expect_equal(bin_choose(x[2], y[2]), 6)
  expect_error(bin_choose(x[4], y[1]), "success must be less than or equal to trials")
  expect_error(bin_choose(x[5], y[1]), "trials must be an integer")
  expect_error(bin_choose(x[1], y[4]), "success must be an integer")
  expect_error(bin_choose(x[1], y[2]), "success must be less than or equal to trials")
  expect_error(bin_choose(x[2], y[3]), "success must be a positive value")



})


### bin_probability
context("Tests for valid bin_probability() values")

test_that("bin_probability() values are valid", {

  x = c(1, 10, 5, 0.5, -10)
  y = c(1, 5, 10, 0.5, -10)
  z = c(0.5, 1, -.5, 2)

  expect_equal(bin_probability(x[1], y[1], z[1]), 0.5)
  expect_equal(bin_probability(x[3], y[3], z[2]), 0)

  expect_error(bin_probability(x[2], y[2], z[2]), "success must be less than or equal to trials")
  expect_error(bin_probability(x[4], y[1], z[1]), "success must be an integer")
  expect_error(bin_probability(x[5], y[1], z[1]), "success must be a positive value")

  expect_error(bin_probability(x[3], y[3], z[3]), "invalid prob value")
  expect_error(bin_probability(x[1], y[1], z[4]), "invalid prob value")

  expect_error(bin_probability(x[1], y[4], z[1]), "trials must be an integer")
  expect_error(bin_probability(x[1], y[5], z[1]), "trials must be a positive value")



})


### bin_distribution
context("Tests for valid bin_distribution() values")

test_that("bin_distribution() values are valid", {

  x = c(1, 5, 10, 0.5, -10)
  y = c(0.5, 1, -.5, 2)

  expect_equal(bin_distribution(x[1], y[1])$success, c(0,1))
  expect_equal(bin_distribution(x[1], y[1])$probability, c(0.5, 0.5))
  expect_length(bin_distribution(x[1], y[1])$success, 2)
  expect_length(bin_distribution(x[1], y[1])$probability, 2)

  expect_error(bin_distribution(x[4], y[1]), "trials must be an integer")
  expect_error(bin_distribution(x[5], y[1]), "trials must be a positive value")
  expect_error(bin_distribution(x[1], y[4]), "invalid prob value")
  expect_error(bin_distribution(x[1], y[3]), "invalid prob value")
  expect_type(bin_distribution(x[1], y[1]), "list")
  expect_s3_class(bin_distribution(x[1], y[1]), c("bindis", "data.frame"))




})


### bin_cumulative
context("Tests for valid bin_cumulative() values")

test_that("bin_cumulative() values are valid", {

  x = c(1, 5, 10, 0.5, -10)
  y = c(0.5, 1, -.5, 2)

  expect_equal(bin_cumulative(x[1], y[1])$success, c(0,1))
  expect_equal(bin_cumulative(x[1], y[1])$probability, c(0.5, 0.5))
  expect_equal(bin_cumulative(x[1], y[1])$cumulative, c(0.5, 1.0))
  expect_length(bin_cumulative(x[1], y[1])$success, 2)
  expect_length(bin_cumulative(x[1], y[1])$probability, 2)
  expect_length(bin_cumulative(x[1], y[1])$cumulative, 2)
  expect_type(bin_cumulative(x[1], y[1]), "list")
  expect_s3_class(bin_cumulative(x[1], y[1]), c("bincum", "data.frame"))

  # expect_equal(bin_distribution(x[2], y[2]), 6)
  expect_error(bin_cumulative(x[4], y[1]), "trials must be an integer")
  expect_error(bin_cumulative(x[5], y[1]), "trials must be a positive value")
  expect_error(bin_cumulative(x[1], y[4]), "invalid prob value")
  expect_error(bin_cumulative(x[1], y[3]), "invalid prob value")




})

