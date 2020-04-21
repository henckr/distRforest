#' Error functions
#'
#' The group of loss functions used to calculate OOB errors.
#'
#' @param ytrue numeric vector of the true target values.
#' @param ypred numeric vector of the predicted target values.
#' @param wcase numeric vector of case weights.
#' @param method string specifying the type of random forest. One of the
#'   following: 'anova', 'class', 'poisson', 'gamma' or 'lognormal'.
#' @keywords internal
error_funcs <- function(ytrue, ypred, wcase = rep(1, length(ytrue)), method) {
  
  switch(method,
         'anova' = wgt_mse(ytrue, ypred, wcase),
         'class' = mcc(ytrue, ypred, wcase),
         'poisson' = dev_poiss(ytrue, ypred, wcase),
         'gamma' = dev_gamma(ytrue, ypred, wcase),
         'lognormal' = wgt_mse(ytrue, ypred, wcase),
         stop('Unknown model class. Please specify a correct one.'))
}


#' @describeIn error_funcs Poisson deviance loss function for Poisson regression
#' @keywords internal
dev_poiss <- function(ytrue, ypred, wcase) -2 * weighted.mean(dpois(ytrue, ypred, log = TRUE) - dpois(ytrue, ytrue, log = TRUE), wcase, na.rm = TRUE)


#' @describeIn error_funcs Gamma deviance loss function for gamma regression
#' @keywords internal
dev_gamma <- function(ytrue, ypred, wcase) -2 * weighted.mean(log(ytrue/ypred) - ((ytrue - ypred) / ypred), wcase, na.rm = TRUE)


#' @describeIn error_funcs Weighted mean squared error loss function for anova and lognormal regression
#' @keywords internal
wgt_mse <- function(ytrue, ypred, wcase) weighted.mean((ytrue - ypred)^2, wcase, na.rm = TRUE)


#' @describeIn error_funcs Matthews correlation coefficient for binary classification
#' @keywords internal
mcc <- function(ytrue, ypred, wcase) {

  # Collect the conditions and predictions
  comp <- data.frame('cond' = as.numeric(as.factor(ytrue)) - 1,
                     'pred' = round(ypred - 1),
                     'w' = wcase)
  # Deal with missings in the predictions
  comp <- comp[! is.na(comp$pred), ]
  # Calculate TP, TN, FP and FN values
  tp <- sum(comp[comp$cond == 1 & comp$pred == 1, 'w'])
  tn <- sum(comp[comp$cond == 0 & comp$pred == 0, 'w'])
  fp <- sum(comp[comp$cond == 0 & comp$pred == 1, 'w'])
  fn <- sum(comp[comp$cond == 1 & comp$pred == 0, 'w'])
  # Calculate the MCC numerator and denominator
  num <- (tp * tn) - (fp * fn)
  den <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  if (den == 0) den <- 1
  # Return the MCC value
  return(num / den)
}
