########################################################
# R functions that implement variance estimators in
# the context of greenhouse gas emissions.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: December 2019
########################################################

parametricDistributions <- c("normal", "uniform", "lognormal")
allowedDistributions <- c(parametricDistributions, "unknown")

#'
#' A class that contains a point and a variance estimates.
#'
#' @export estimate
estimate <- setClass("estimate", slots = c(mean = "numeric", variance = "numeric", stdDev = "numeric", dist = "character"))

#'
#' A class that extends the estimate class. In addition to the fields
#' of the super class, it also contains a list of realizations.
#'
#' @export monteCarloEstimate
monteCarloEstimate <- setClass("monteCarloEstimate", contains="estimate", slots = c(real = "numeric"))

#'
#' Constructs instance of the estimate class.
#'
#' @param mean the point estimate
#' @param variance the variance estimate
#' @param dist the distribution of the estimator ("normal", "uniform", "lognormal", or "unknown"). By default
#' the distribution is set to "normal"
#' @return an instance of the estimate class
#'
#' @export
createEstimate <- function(mean, variance, dist = "normal") {
  if (!(dist %in% allowedDistributions)) {
    stop(paste("The dist parameter should be either:", paste(allowedDistributions, collapse=" ")))
  }
  obj <- methods::new("estimate", mean = mean, variance = variance, stdDev = variance^.5, dist = dist)
  return(obj)
}

.createMonteCarloEstimate <- function(realizations) {
  var <- var(realizations)
  obj <- methods::new("monteCarloEstimate", mean = mean(realizations), variance = var, stdDev = var^.5, dist = "empirical", real = realizations)
  return(obj)
}

.generateDeviate <- function(est) {
  if (methods::is(est, "estimate") == F) {
    stop("Error .generateDeviate: The function requires an instance of the estimate class!")
  }
  if (!(est@dist %in% parametricDistributions)) {
    stop(paste("Error .generateDeviate: The estimate must follow one of these distributions:", paste(parametricDistributions, collapse=" ")))
  }
  if (est@dist == "normal") {
    return(stats::rnorm(1, mean=est@mean, sd=est@stdDev))
  } else if (est@dist == "uniform") {
    a = est@mean - 3^.5 * est@stdDev
    b = est@mean + 3^.5 * est@stdDev
    return(stats::runif(1, min = a, max = b))
  } else if (est@dist == "lognormal") {
    sigma2 = log(est@variance/est@mean^2 + 1)
    mu = log(est@mean) - sigma2 * .5
    return(stats::rlnorm(1, meanlog = mu, sdlog = sigma2^.5))
  }
}


#'
#' Computes the products of several estimates under different methods.
#'
#' @param ... a series of instances of the estimate class
#' @param factor a multiplicative factor. The mean and the variance of the resulting
#' estimate instance are multiplied by factor and factor^2, respectively. By default
#' this factor is set to 1.
#' @param method either "Goodman", "Naive", "Propagation","MonteCarlo", or "RescaledMonteCarlo". The
#' last two are based on resampling method and the resulting estimate is then a monteCarloEstimate instance
#' instead of an estimate instance
#' @param realMC the number of realizations in cases the method is "MonteCarlo" or "RescaledMonteCarlo"
#' @return an instance of the estimate or the monteCarloEstimate class
#'
#' @export
multiplyEstimates <- function(..., factor = 1, method="Goodman", realMC=5000) {
  if (!(method %in% c("Goodman", "Naive", "Propagation","MonteCarlo","RescaledMonteCarlo"))) {
    stop("The method should be one of the following: Goodman, Naive, Propagation, MonteCarlo or RescaledMonteCarlo")
  }
  estimates <- list(...)
  if (length(estimates) < 2) {
    stop("The first arguments of the function should be at least two instances of the estimate class!")
  }
  for (estimate in estimates) {
    if (methods::is(estimate, "estimate") == F) {
      stop("The first arguments of the function should be instances of the estimate class!")
    }
  }
  if (method %in% c("Goodman", "Naive", "Propagation")) {
    for (i in 2:length(estimates)) {
      if (i == 2) {
        estimate1 <- estimates[[i-1]]
      }
      estimate2 <- estimates[[i]]
      mean <- estimate1@mean * estimate2@mean
      variance <- estimate1@mean^2 * estimate2@variance + estimate1@variance * estimate2@mean^2
      if (method == "Naive") {
        variance <- variance + estimate2@variance * estimate1@variance
      } else if (method == "Goodman") {
        variance <- variance - estimate2@variance * estimate1@variance
      }
      estimate1 <- createEstimate(mean, variance)
    }
    return(createEstimate(estimate1@mean * factor, estimate1@variance * factor^2, dist = "unknown"))
  } else {
    real <- c()
    for (i in 1:realMC) {
      real.i <- 1
      for (estimate in estimates) {
        real.i <- real.i * .generateDeviate(estimate)
      }
      real <- c(real, real.i * factor)
    }
    meanMC <- mean(real)
    if (method == "RescaledMonteCarlo") {
      goodmanVariance <- multiplyEstimates(..., factor = factor)@variance
      naiveVariance <- multiplyEstimates(..., factor = factor, method="Naive")@variance
      real <- (goodmanVariance/naiveVariance)^.5 * (real - meanMC) + meanMC
    }
    finalEstimate <- .createMonteCarloEstimate(real)
    return(finalEstimate)
  }
}

#'
#' Extends the summary function to the estimate class.
#'
#' @param object an instance of the estimate class
#' @param ... useless parameters
#' @return a character string with the mean, the variance and the distribution of the estimate
#'
#' @export
summary.estimate <- function(object, ...) {
  estimate <- object
  if (methods::is(estimate, "estimate")) {
    print(paste("Mean =",estimate@mean,"; Variance =",estimate@variance, "; Distribution =", estimate@dist, sep=" "))
  } else {
    NextMethod("summary")
  }
}

#'
#' Plots the distribution of an estimate.
#'
#' @param estimate an instance of the estimate class
#'
#' @export
plotEstimate <- function(estimate) {
  if (methods::is(estimate, "estimate")) {
    if (methods::is(estimate, "monteCarloEstimate")) {
      graphics::hist(estimate@real, col="gray50", border="black", main = "Monte Carlo estimate distribution")
    } else if (estimate@dist == "unknown") {
      print("Cannot plot the distribution of this estimate since it is unknown!")
    } else {
      if (estimate@dist == "normal") {
        low <- estimate@mean - 4 * estimate@stdDev
        upp <- estimate@mean + 4 * estimate@stdDev
        values <- seq(low, upp, by=(upp - low) * 0.01)
        density <- stats::dnorm(values, mean = estimate@mean, sd = estimate@stdDev)
        graphics::plot(x=values, y = density, type = "l")
      } else if (estimate@dist == "uniform") {
        a <- estimate@mean - 3^.5 * estimate@stdDev
        b <- estimate@mean + 3^.5 * estimate@stdDev
        extent <- b - a
        values <- seq(a, b, by = extent * .01)
        density <- rep(1 / extent, length(values))
        graphics::plot(x=values, y = density, type = "l", xlim = c(a - 0.1 * extent, b + 0.1 * extent), ylim = c(0, density[1] * 1.3))
      } else if (estimate@dist == "lognormal") {
        sigma2 <- log(estimate@variance/estimate@mean^2 + 1)
        mu <- log(estimate@mean) - sigma2 * .5
        sigma <- sigma2^.5
        low <- mu - 4 * sigma
        upp <- mu + 4 * sigma
        logvalues <- seq(low, upp, by=(upp - low) * 0.01)
        values <- exp(logvalues)
        density <- stats::dlnorm(values, meanlog = mu, sdlog = sigma)
        graphics::plot(x=values, y = density, type = "l")
      }
    }
  } else {
    stop("Error plot.estimate: the function requires an instance of the estimate class!")
  }
}


