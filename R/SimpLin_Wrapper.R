#' A function that fits a simple linear regression model
#'
#' This function takes two vectors, x and y, as inputs. It then fits a simple
#' regression model and returns a list with regression coefficients, their
#' corresponding standard errors and 95% confidence intervals, residuals,
#' and predicted values as a list.
#'
#' @param x A numeric vector serving as the explanatory variable.
#' @param y A numeric vector equal in length to x serving as the response
#' variable.
#'
#' @returns A list. [["coefficients"] will give the beta vector where the first
#' element is the intercept term and the second element is the slope term,
#' [["SE B0"]] will give SE of the intercept, will give the beta vector,
#' [["SE B1"]] will give SE of the slope, [["95% CI B0"]] will give a 95%
#' confidence for the intercept, [["95% CI B1"]] will give a 95% confidence
#' for the slope, [["yhat]] will give the predicted values, [["residuals"]]
#' will give the residuals.
#'
#' @export
SimpLinR <- function(x, y) {
  if (is.atomic(c(x,y)) == FALSE | is.numeric(c(x,y)) == FALSE) {
    stop(
      "x and y must be numeric vectors",
      call. = FALSE
    )
  }
  if (length(x) != length(y)) {
    stop(
      "x and y must be the same length",
      call. = FALSE
    )
  }
  if (length(x) <= 1) {
    stop(
      "x must contain at least 2 elements",
      call. = FALSE
    )
  }
  if(all((1/x[1])*x == rep(1, length(x)))) {
    stop(
      "must be at least 2 unique elements in x",
      call. = FALSE
    )
  }
  else {
    out_list <- .Call(`_SimpLin_SimpLinCpp`, x, y, PACKAGE = 'SimpLin' )
  }
  return(out_list)
}

