#' Predict a random forest
#'
#' This function obtains predictions from a random forest based on aggregating
#' the predictions from individual \code{\link{rpart}} trees. A majority vote is
#' taken for binary classification trees, while the predictions are averaged for
#' normal, poisson, gamma and lognormal regression trees.
#'
#' @param object fitted model object from the class \code{rforest}.
#' @param newdata data frame containing the observations to predict. This
#'   argument can only be missing when the random forest in \code{object} is
#'   trained with \code{keep_data = TRUE}. In that case, the original training
#'   data will be used to generate predictions.
#' @return numeric vector with the averaged predictions (for regression) or the
#'   majority vote (for classification) of the individual trees.
predict.rforest <- function(object, newdata) {
  
  # Deal with missing newdata argument
  if (missing(newdata)) {
    if (is.null(object[['data']])) stop('The argument newdata must be supplied when the rforest is trained with keep_data = FALSE.')
    newdata <- object[['data']]
  }
  
  # Initialize a vector to save the predictions for each observation
  pred_vec <- rep(0, nrow(newdata))
  # Iterate over the trees in the forest and update predictions accordingly
  for (rf_id in seq_len(length(object[['trees']]))) {
    pred_vec <- ((rf_id - 1) * pred_vec / rf_id) + (predict(object[['trees']][[rf_id]], newdata = newdata, type = 'vector') / rf_id)
  }
  # Convert to classes 0 and 1 for a classification forest
  if (object[['trees']][[1]]$method == 'class') pred_vec <- round(pred_vec - 1)
  
  # Return predictions
  return(pred_vec)
}
