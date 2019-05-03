README.Rmd
================

Overview
--------

### Binomial

This package contains functions for calculating probabilities of a Binomial random variable, and related calculations such as the probability distribution, the expected value, variance, etc.

Motivation
----------

This package was created and inspired by the impact it will have on my grade. Additionally, learning how to create an R package seems very useful.

Installation
------------

This package can be installed using the package "devtools" from github

``` r
#install.packages("devtools")

#devtools::install_github("tristanswells-1/binomial")
```

Usage
-----

### Identifying "trials", "prob", and "success"

The first step is to identify the probability (prob), the number of trials (trials), and the number of successes (success).

From here, you can calculate the binomial mean, mode, variance, skewness, and kurtosis using the "binomial" package as such:

``` r
trials = 10
prob = 0.3

binmean <- bin_mean(trials, prob)
binmean
#> [1] 3
```

note that the number of trials (trials) must be postive integers, prob must be between 0 and 1, and success must be a positive integer less than or equal to trials.

### Calculating the Binomial Distribution

With the function "bin\_distribution", you can create a list containing the binomial distribution of trials and probability

``` r
bindis_list <- bin_distribution(trials, prob)
bindis_list
#>    success  probability
#> 1        0 0.0282475249
#> 2        1 0.1210608210
#> 3        2 0.2334744405
#> 4        3 0.2668279320
#> 5        4 0.2001209490
#> 6        5 0.1029193452
#> 7        6 0.0367569090
#> 8        7 0.0090016920
#> 9        8 0.0014467005
#> 10       9 0.0001377810
#> 11      10 0.0000059049
```

### Calculating the cumulative binomial distribution

with bin\_cumulative, you can calulate the cumulative value of success given a probability and number of trials.

``` r
bincum <- bin_cumulative(trials, prob)
bincum
#>    success  probability cumulative
#> 1        0 0.0282475249 0.02824752
#> 2        1 0.1210608210 0.14930835
#> 3        2 0.2334744405 0.38278279
#> 4        3 0.2668279320 0.64961072
#> 5        4 0.2001209490 0.84973167
#> 6        5 0.1029193452 0.95265101
#> 7        6 0.0367569090 0.98940792
#> 8        7 0.0090016920 0.99840961
#> 9        8 0.0014467005 0.99985631
#> 10       9 0.0001377810 0.99999410
#> 11      10 0.0000059049 1.00000000
```

### Binomial variable

the bin\_variable function will return a binomial random variable object, given a number of trials, and probability. It will provide statistics for each set of inputs.

### n choose k

the bin\_choose function will caculate the number of combinations in which k successes can occur in n trials.

### Plots

The functions plot.bincum and plot.binvar can provide plots of the list values given for bin\_cumulative and bin\_variable.

``` r

bincum <- bin_cumulative(trials, prob)

bincum
#>    success  probability cumulative
#> 1        0 0.0282475249 0.02824752
#> 2        1 0.1210608210 0.14930835
#> 3        2 0.2334744405 0.38278279
#> 4        3 0.2668279320 0.64961072
#> 5        4 0.2001209490 0.84973167
#> 6        5 0.1029193452 0.95265101
#> 7        6 0.0367569090 0.98940792
#> 8        7 0.0090016920 0.99840961
#> 9        8 0.0014467005 0.99985631
#> 10       9 0.0001377810 0.99999410
#> 11      10 0.0000059049 1.00000000
plot(bincum)
```

![](README-unnamed-chunk-5-1.png)
