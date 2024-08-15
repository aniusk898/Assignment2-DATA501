#' Cook's Distance Measure Calculation
#'
#' This function calculates Cook's distance, which is a measure of influence used to assess the impact that an observation has on the estimates of a least squares regression model.

#'
#' @param model An object of class \code{lm} that is the linear model for which Cook's distance is going to be calculated.
#'
#' @return A numerical vector that contains the Cook's distance of each observation in the model.
#' @export
#'
#' @examples
#' data <- data.frame(
#' y <- c(4.2, 5.1, 7.3, 6.5, 8.0, 6.7, 10),
#' x1 <- c(2.1, 3.5, 5.1, 4.3, 6.0, 5.5, 7)
#' )
#' model <- lm(y ~ x1, data <- data)
#' cooks_d <- cooks.distance(model)
cooks_distance <- function(model) {
  if (!inherits(model, "lm")) {
    stop("the argument 'model' must be an object of class lm")
  }

  res <- resid(model)
  lev <- hatvalues(model)
  mse <- sum(res ^ 2) / df.residual(model)
  p <- length(model$coefficients)
  cdist <- (res ^ 2 / (p * mse)) * (lev / (1 - lev) ^ 2)
  return(cdist)
}

#' Difference in fits Measure Calculation
#'
#' This function calculates the difference in fits which is a metric used to identify influential data points by measuring how much the predicted value changes, in terms of standard deviations, when a specific observation is removed from the model.
#'
#' @inheritParams cooks_distance
#'
#' @return A numerical vector that contains the difference in fits of each observation in the model.
#' @export
#'
#' @examples
#' data <- data.frame(
#' y <- c(4.2, 5.1, 7.3, 6.5, 8.0, 6.7, 10),
#' x1 <- c(2.1, 3.5, 5.1, 4.3, 6.0, 5.5, 7)
#' )
#' model <- lm(y ~ x1, data <- data)
#' dfits <- diffits(model)
diffits <- function(model) {
  if (!inherits(model, "lm")) {
    stop("the argument 'model' must be an object of class lm")
  }

  res_stand <- rstudent(model)
  lev <- hatvalues(model)
  diffits <- res_stand * sqrt(lev / (1 - lev))
  return(diffits)
}

#' hadis_influence's Influence Measure Calculation
#'
#' This function calculates the hadis_influence's influence measure that considers that influential observations may occur in the response variable, the predictors, or in both.
#'
#' @inheritParams cooks_distance
#'
#' @return A numerical vector that contains hadis_influence's influence measure of each observation in the model.
#' @export
#'
#' @examples
#' data <- data.frame(
#' y <- c(4.2, 5.1, 7.3, 6.5, 8.0, 6.7, 10),
#' x1 <- c(2.1, 3.5, 5.1, 4.3, 6.0, 5.5, 7)
#' )
#' model <- lm(y ~ x1, data <- data)
#' hadis_influences <- hadis_influence(model)
hadis_influence <- function (model) {
  if (!inherits(model, "lm")) {
    stop("the argument 'model' must be an object of class lm")
  }

  res_stand <- rstudent(model)
  lev <- hatvalues(model)
  hadis_influence <- (res_stand ^ 2 * lev) / (1 - lev) ^ 2
  return(hadis_influence)
}


#' Validate Inputs and Outputs
#'
#' This function validates the inputs to ensure that the provided data and model are appropriate for calculating influence measures.
#'
#' @param data A dataframe  that contains the data  used to fit the linear model.
#' @param model  An object of class \code{lm} that is the linear model used to calculate the measures of influence (cooks_distance, diffits, hadis_influence).
#'
#' @return The function returns \code{TRUE} if all validations pass. If any validation fails, the function stops execution and returns an error message.
#' @export
#'
#' @examples
#' data <- data.frame(
#' y <- c(4.2, 5.1, 7.3, 6.5, 8.0, 6.7, 10),
#' x1 <- c(2.1, 3.5, 5.1, 4.3, 6.0, 5.5, 7)
#' )
#' model <- lm(y ~ x1, data <- data)
#'
#' valInputs(data, model)
valInputs <- function(data, model) {
  if (!inherits(model, "lm")) {
    stop("the argument 'model' must be an object of class lm")
  }
  if (!is.data.frame(data)) {
    stop("The data must be a data frame")
  }
  if (any(is.na(data)) || any(is.infinite(data))) {
    stop("The data contains NA or inf values")
  }
  if (!is.numeric(data)) {
    stop("The data format must be numeric")
  }
}








