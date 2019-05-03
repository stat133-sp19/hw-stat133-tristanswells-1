# Workout03
##binomial function package

## Check Probability
### prob is some value, or probability, between 0 and 1
### check_prob checks whether this value is between 0 and one, else stop error
check_prob <- function(prob){

  if (max(prob) > 1 | min(prob) < 0){
    stop("invalid prob value")
  }
  else{
    return(TRUE)
  }
}

## Check Trials
### check_trials function checks if an input
###"trials" is a valid value (i.e. n is non-negative integer)

check_trials <- function(trials){

  if (min(trials) < 0){
    stop("trials must be a positive value")
  }

  if(trials != floor(trials)){
    stop("trials must be an integer")
  }
  else{
    return(TRUE)
  }
}

## Check Success
### check_success function checks whether the input "success"
### is a valid value for number of successes

check_success <- function(success, trials){
  for (i in 1:length(success)){
  if (success[i] < 0){
    stop("success must be a positive value")
  }

  if(success[i] != floor(success[i])){
    stop("success must be an integer")
  }

  if (success[i] > trials){

    stop("success must be less than or equal to trials")
  }
  else {
    return(TRUE)
  }
  }
}


# Private Auxiliary Functions

## Mean
### aux_mean calculates mean, given inputs "trials" and "prob"

aux_mean <- function(trials, prob){

  check_prob(prob)
  check_trials(trials)

  trials*prob

}

## Variance
### aux_variance is an internal function which calculates the variance,
###given inputs "trials" and "prob"

aux_variance <- function(trials, prob){

  check_prob(prob)
  check_trials(trials)

  trials*prob*(1-prob)

}

## Mode
### aux_mode is an internal function which calculates the mode,
###given inputs "trials" and "prob"

aux_mode <- function(trials, prob){

  check_prob(prob)
  check_trials(trials)

  mode = trials*prob + prob
  if (ceiling(mode) == mode){
    return(ceiling(mode))
  }

  else{
    return(floor(mode))
  }
}
## Skewness
### aux_skewness is an internal function which calculates the skewness,
###given inputs "trials" and "prob"

aux_skewness <- function(trials, prob){

  check_prob(prob)
  check_trials(trials)

  (1-2*prob)/sqrt(trials*prob*(1 - prob))
}

## Kurtosis
### aux_kurtosis is an internal function which calculates the kurtosis,
###given inputs "trials" and "prob

aux_kurtosis <- function(trials, prob){

  check_prob(prob)
  check_trials(trials)

  (1 - 6*prob*(1 - prob))/(trials*prob*(1 - prob))
}


# Main Functions
library(devtools)

## Bin Choose
### bin_choose function calculates the number of combinations
### in which k successes can occur in n trials.
#' @title Binomial "Choose"
#' @description that calculates number of combinations in which k successes can occur in n trials.
#' @param trials number of trials
#' @param prob probability
#' @return number of combinations in which k successes can occur in n trials.
#' @export
#' @examples
#' # default
#' choose <- bin_choose(3, 2)
#'

bin_choose <- function(n, k){

  check_success(k, n)
  check_trials(n)

  if (max(k) > n){
    stop("k cannot be greater than n")
  }
  else{
    factorial(n)/(factorial(k)*factorial(n-k))
  }
}

## Probability
### bin_probablity is a function which takes three arguments: success, trials, and prob,
### and raises error code if inputs are invalid values
#' @title Binomial Probability
#' @description that calculates the binomial probability
#' @param trials number of trials
#' @param prob probability
#' @param success successes of trial
#' @return binomial probability
#' @export
#' @examples
#' # default
#' prob <- bin_probability(3, 4, 0.5)
#'

bin_probability <- function(success, trials, prob){

  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)

  if (check_trials(trials) != TRUE){
    check_trials(trials)
  }

  if (check_prob(prob) != TRUE){
    check_prob(prob)
  }

  if (check_success(success, trials) != TRUE){
    check_success(success, trials)
  }

  else{

    bin_choose(trials, success)*(prob^success)*(1 - prob)^(trials - success)
  }
}


## Distribution
### bin_distribution is a function that takes inputs "trials," and "prob,"
### and will return a data frame with the probability distribution: sucesses in
### the first column, probability in the second column.
#' @title Bin Distribution
#' @description Creates an object of class \code{c("bindis", "data.frame")}
#' @param trials number of trials
#' @param prob probability
#' @return an object of class \code{c("bindis", "data.frame")}
#' @export
#' @examples
#' # default
#' bindis <- bin_distribution(5, .5)
#'
bin_distribution <- function(trials, prob){
  check_prob(prob)
  check_trials(trials)

  probability = 0
  success = 0
  for (i in 0:trials){

  probability[i+1] =  bin_probability(i, trials, prob)
  success[i+1] = i
  bindis <- data.frame(success, probability)

  }
  class(bindis) <- c("bindis", "data.frame")

  return(bindis)

}

## Binomial Distribution Plot
### plot.bindis is a function that graphs a barplot to display
### the probability histogram of a binomial distribution object "bindis"


#' @export

plot.bindis <- function(bindis){

  x = bindis$success
  y = bindis$probability
  barplot(y, names.arg = x)

}

#' @title Bin Cumulative
#' @description Creates an object of class \code{c("bincum", "data.frame")}
#' @param trials number of trials
#' @param prob probability
#' @return an object of class \code{c("bincum", "data.frame")}
#' @export
#' @examples
#' # default
#' bincum <- bin_cumulative(5, 0.5)
#'

bin_cumulative <- function(trials, prob){
  check_prob(prob)
  check_trials(trials)

  probability = 0
  success = 0
  cumulative = 0
  for (i in 0:trials){

    probability[i+1] =  bin_probability(i, trials, prob)
    success[i+1] = i
    cumulative[1] = probability[1]
    cumulative[i+1] = sum(probability[0:i+1])
    bincum <- data.frame(success, probability, cumulative)

  }
  class(bincum) <- c("bincum", "data.frame")

  return(bincum)

}

## Binomial Distribution Plot
### plot.bindis is a function that graphs a barplot to display
### the probability histogram of a binomial distribution object "bindis"


#' @export

plot.bincum <- function(bincum){

  x = bincum$success
  y = bincum$cumulative

  library(ggplot2)
  ggplot(bincum, aes(x, y)) +
   geom_point(shape = 1) +
   geom_line() +
   ylab("probability") +
   xlab("success")
}

#' @title Binonmial Variable
#' @description Creates an object of class \code{c("binvar", "data.frame")}
#' @param trials number of trials
#' @param prob probability
#' @return an object of class \code{c("binvar")} which is a list with named elements trials and prob
#' @export
#' @examples
#' # default
#' binvar <- bin_variable(5, 0.5)

bin_variable <- function(trials, prob){
  check_prob(prob)
  check_trials(trials)

  if (check_trials(trials) != TRUE){
    stop("invalid trials value")
  }
  if (check_prob(prob) != TRUE){
    stop("invalid prob value")
  }
  else{
  binvar <- list(trials, prob)
  class(binvar) <- "binvar"

  return(binvar)
  }

}

## print binvar
#' @export

print.binvar <- function(x, ...){

  cat('"Binomial Variable"\n\n')
  cat(sprintf('Parameters\n'))
  cat("-number of trials:", x[[1]], "\n")
  cat("-prob of success:", x[[2]], "\n")
  #cat(sprintf("num of %s:", x$coin$sides[2]), x$tails, "\n")
  invisible(x)

}

## summary of binvar
#' @export

summary.binvar <- function(x, ...){
  mean = aux_mean(x$trials, x$prob)
  variance = aux_variance(x$trials, x$prob)
  mode = aux_mode(x$trials, x$prob)
  skewness = aux_skewness(x$trials, x$prob)
  kurtosis = aux_kurtosis(x$trials, x$prob)

  summary.binvar <- list(trials = x$trials, prob = x$prob, mean, variance, mode, skewness, kurtosis)
  class(summary.binvar) <- "summary.binvar"
  return(summary.binvar)
}


## print summary of binvar
#' @export

print.summary.binvar <- function(x, ...){

  cat('"Summary Binomial"\n\n')
  cat(sprintf('Parameters\n'))
  cat("-number of trials:", x[[1]], "\n")
  cat("-prob of success :", x[[2]], "\n\n")
  cat(sprintf('Measures\n'))
  cat("-mean    :", aux_mean(x$trials, x$prob), "\n")
  cat("-variance:", aux_variance(x$trials, x$prob), "\n")
  cat("-mode    :", aux_mode(x$trials, x$prob), "\n")
  cat("-skewness:", aux_skewness(x$trials, x$prob), "\n")
  cat("-kurtosis:", aux_kurtosis(x$trials, x$prob), "\n")
  #cat(sprintf("num of %s:", x$coin$sides[2]), x$tails, "\n")
  invisible(x)

}


## Main Functions
#' @title Binonmial Mean
#' @description Creates binomial mean given "trials" and "prob"
#' @param trials number of trials
#' @param prob probability
#' @return binomial mean
#' @export
#' @examples
#' # default
#' mean <- bin_mean(5, .5)

bin_mean <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  mean = aux_mean(trials, prob)
  return(mean)

}

#' @title Binonmial Variance
#' @description Creates binomial variance given "trials" and "prob"
#' @param trials number of trials
#' @param prob probability
#' @return binomial variance
#' @export
#' @examples
#' # default
#' variance <- bin_variance(5, .5)

bin_variance <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  variance = aux_variance(trials, prob)
  return(variance)

}

#' @title Binonmial Mode
#' @description Creates binomial mode given "trials" and "prob"
#' @param trials number of trials
#' @param prob probability
#' @return binomial mode
#' @export
#' @examples
#' # default
#' mode <- bin_mode(5, .5)

bin_mode <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  mode = aux_mode(trials, prob)
  return(mode)

}

#' @title Binonmial Skewness
#' @description Creates binomial skewness given "trials" and "prob"
#' @param trials number of trials
#' @param prob probability
#' @return binomial skewness
#' @export
#' @examples
#' # default
#' skew <- bin_skewness(5, .5)

bin_skewness <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  skewness = aux_skewness(trials, prob)
  return(skewness)

}

#' @title Binonmial Kurtosis
#' @description Creates binomial kurtosis given "trials" and "prob"
#' @param trials number of trials
#' @param prob probability
#' @return binomial kurtosis
#' @export
#' @examples
#' # default
#' kurt <- bin_kurtosis(5, .5)

bin_kurtosis <- function(trials, prob){
  check_trials(trials)
  check_prob(prob)
  kurtosis = aux_kurtosis(trials, prob)
  return(kurtosis)

}


