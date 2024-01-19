#' Inverse function of fit standard curve
#'
#' @param y MFI value.
#'
#' @param par parameters from standard curve fit.
#'
#'
#' @return \code{FUNinv} will return estimated dilution factor
#' based on MFI and std curve fit.
#'
#' @examples
#' R code examples of how to use your function
#'

FUNinv <- function(y, par) {
-par["Scale"]*log(((par["Aup"] - par["Alow"])/
                     (y - par["Alow"]))^(1/par["a"]) - 1) + par["Xmid"]
}
