#' Compute re-transformed regression fit statistics
#'
#' For a regression where the dependent variable is a power transformation of the original
#' variable of interest, standard error-like and R-squared-like statistics are computed
#' in the scale of the original variable of interest.
#' @param regObj A lm object
#' @param tranPwr Power transformation applied to the original variable of interest. Defaults to 0 (log).
#' @export
#' @examples
#' fm1 <- lm(log(Volume) ~ log(Girth), data = trees); rtrnstat(fm1,0)

rtrnstat <- function(regObj, tranPwr=0) {
  if (class(regObj) != "lm") {
    print("ERROR - first argument must be a 'lm' object")
    list(SE = "NA", R2 = "NA")
  } else {
    if (is.numeric(tranPwr)) {
      if (tranPwr == 0) {
        rtrnSE <- sqrt(sum((exp(regObj$model[,1])-exp(regObj$fitted.values))^2)/regObj$df.residual)
        rtrnR2 <- 1-sum((exp(regObj$model[,1])-exp(regObj$fitted.values))^2)/(var(exp(regObj$model[,1]))*(length(regObj$model[,1])-1))
        #return(list("SE" = rtrnSE, "R2" = rtrnR2))
        list(SE = rtrnSE, R2 = rtrnR2)
      } else {
        rtrnBy = 1.0/tranPwr
        rtrnSE <- sqrt(sum((regObj$model[,1]^rtrnBy-regObj$fitted.values^rtrnBy)^2)/regObj$df.residual)
        rtrnR2 <- 1-sum((regObj$model[,1]^rtrnBy-regObj$fitted.values^rtrnBy)^2)/(var(regObj$model[,1]^rtrnBy)*(length(regObj$model[,1])-1))
        #return(list("SE" = rtrnSE, "R2" = rtrnR2))
        list(SE = rtrnSE, R2 = rtrnR2)
      }
    } else {
      print("ERROR - second argument must be a number")
      list(SE = "NA", R2 = "NA")
    }
  }
}