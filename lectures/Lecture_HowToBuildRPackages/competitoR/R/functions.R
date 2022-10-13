#' Logistic function
#'
#' This is the logistic function
#' which grows to a carrying capacity
#'
#' @param x time
#' @param N0 initial time
#' @param K carrying capacity
#' @param r0 intrinsic growth rate
#' @examples curve(logistic(x, .01, 1, 10))
#'
#' @export

logistic <- function(x, N0, K, r0){
  K/(1 + ((K - N0)/N0)*exp(-r0*x))
}

#' Fit Logistic Curve
#'
#' @example examples/fitLogisticExample.R
#' @export
fitLogistic <- function(data, y = "N", time = "Day", N0 = 1, K = 200, r0 = 0.75){
  Y <- with(data, get(y))
  X <- with(data, get(time))

  myfit <- nls(Y ~ logistic(X, N0, K, r0),
               start = list(N0 = N0, K = K, r0 = r0))

  summary(myfit)
}


#' Draw logistic fit
#'
#' @export
linesLogistic <- function(fit, ...){
  curve(logistic(x, N0 =  fit$coefficients[1,1],
                 K= fit$coefficients[2,1],
                 r0= fit$coefficients[3,1]), add = TRUE, ...)
}
