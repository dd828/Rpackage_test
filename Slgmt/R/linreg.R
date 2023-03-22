#' Fit a linear regression model using matrix algebra
#'
#' Given two vectors, x and y, fit a simple linear regression model using matrix algebra
#' and return the coefficients, residuals, fitted values, and more.
#'
#' @param x A numeric vector of predictor values
#' @param y A numeric vector of response values
#' @return A list with elements including coefficients, residuals, fitted values, and more
#' @export
linreg.fit <- function(x, y) {
  n <- length(y)
  X <- matrix(c(rep(1, n), x), nrow = n, ncol = 2, byrow = FALSE)
  XtX <- t(X) %*% X
  invXtX <- solve(XtX)
  XtY <- t(X) %*% y
  beta <- invXtX %*% XtY
  residuals <- y - X %*% beta
  fitted.values <- X %*% beta
  r.squared <- 1 - sum(residuals^2) / sum((y - mean(y))^2)
  list(coefficients = as.vector(beta),
       residuals = residuals,
       fitted.values = fitted.values,
       r.squared = r.squared)
}


