#' Variable importance in a random forest
#'
#' This function acts as a user-friendly interface for the variable importance
#' scores in a random forest based on individual \code{\link{rpart}} trees.
#'
#' @param object fitted model object from the class \code{rforest}.
#' @return data frame with one row for each variable and four columns:
#'   \describe{ \item{variable}{the name of the variable.} \item{importance}{the
#'   average importance score over all the individual trees.}
#'   \item{scale_sum}{scaled scores which sum to one.} \item{scale_max}{scaled
#'   scores such that the maximum value is equal to one.}}
importance_rforest <- function(object){
  
  # Create list with the variable importance for each tree in the ensemble
  list_vi <- lapply(seq_len(length(object[['trees']])), function(i) object[['trees']][[i]]$variable.importance)
  if (all(unlist(lapply(list_vi, is.null)))) stop('Can not compute importance scores for a forest of only root-node-trees.')
  # Average the variable importance over all trees
  vi <- tapply(unlist(list_vi), names(unlist(list_vi)), sum)/length(object[['trees']])
  # Add the variables which are not selected
  vi[attr(object[['trees']][[1]]$terms, 'term.labels')[!(attr(object[['trees']][[1]]$terms, 'term.labels') %in% names(vi))]] = 0
  # Sort from most to least important
  vi <- sort(vi, decreasing = TRUE)
  # Collect everything in a data frame
  vi_df <- data.frame('variable' = names(vi),
                      'importance' = unname(vi),
                      'scale_sum' = unname(vi) / sum(vi),
                      'scale_max' = unname(vi) / max(vi))
  return(vi_df)
}
