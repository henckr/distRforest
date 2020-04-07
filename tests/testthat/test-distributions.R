context('New distributions')
library(distRforest)

# Use a gbm fit on the mtpl_be data to test the partial dependence function
if (!requireNamespace('CASdatasets', quietly = TRUE)) {
  stop('Package "CASdatasets" needed for this function to work. Please install it.',
       call. = FALSE)
}
library(CASdatasets)
data(ausprivauto0405)


test_that('Poisson tree with exposure is being calculated correctly', {
  # Fit a Poisson regression tree
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 1)
  tree_poiss <- distRforest::rpart(formula = cbind(Exposure, ClaimNb) ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                   data = ausprivauto0405, method = 'poisson', control = ctrl, parms = list('shrink' = 10000000))
  # Split the data in the left and right branch
  left_branch <- ausprivauto0405[ausprivauto0405$DrivAge %in% c('old people', 'oldest people'), ]
  right_branch <- ausprivauto0405[! ausprivauto0405$DrivAge %in% c('old people', 'oldest people'), ]
  # Get number of observations in both branches
  left_nobs <- tree_poiss$frame$n[2]
  right_nobs <- tree_poiss$frame$n[3]
  # Get prediction for both branches
  left_pred <- tree_poiss$frame$yval[2]
  right_pred <- tree_poiss$frame$yval[3]
  # Get sum of deviance in both branches
  left_dev <- tree_poiss$frame$dev[2]
  right_dev <- tree_poiss$frame$dev[3]
  # Define a function to calculate the Poisson deviance
  poiss_dev <- function(ytrue, ypred) -2 * sum(dpois(ytrue, ypred, log = TRUE) - dpois(ytrue, ytrue, log = TRUE), na.rm = TRUE)
  # Check whether the observed values match the expected empirical ones
  expect_equal(nrow(left_branch), left_nobs)
  expect_equal(nrow(right_branch), right_nobs)
  expect_equal(sum(left_branch$ClaimNb)/sum(left_branch$Exposure), left_pred)
  expect_equal(sum(right_branch$ClaimNb)/sum(right_branch$Exposure), right_pred)
  expect(poiss_dev(left_branch$ClaimNb, left_pred * left_branch$Exposure), left_dev)
  expect(poiss_dev(right_branch$ClaimNb, right_pred * right_branch$Exposure), right_dev)
})


test_that('Gamma tree without weights is being calculated correctly', {
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  # Fit a gamma regression tree
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 1)
  tree_gamma <- distRforest::rpart(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                   data = ausprivauto0405_claims, method = 'gamma', control = ctrl)
  # Split the data in the left and right branch
  left_branch <- ausprivauto0405_claims[! ausprivauto0405_claims$DrivAge == 'youngest people', ]
  right_branch <- ausprivauto0405_claims[ausprivauto0405_claims$DrivAge == 'youngest people', ]
  # Get number of observations in both branches
  left_nobs <- tree_gamma$frame$n[2]
  right_nobs <- tree_gamma$frame$n[3]
  # Get prediction for both branches
  left_pred <- tree_gamma$frame$yval[2]
  right_pred <- tree_gamma$frame$yval[3]
  # Get sum of deviance in both branches
  left_dev <- tree_gamma$frame$dev[2]
  right_dev <- tree_gamma$frame$dev[3]
  # Define a function to calculate the Poisson deviance
  gamma_dev <- function(ytrue, ypred, wcase = 1) -2 * sum(wcase * (log(ytrue/ypred) - ((ytrue - ypred) / ypred)), na.rm = TRUE)
  # Check whether the observed values match the expected empirical ones
  expect_equal(nrow(left_branch), left_nobs)
  expect_equal(nrow(right_branch), right_nobs)
  expect_equal(with(left_branch, mean(ClaimAmount)), left_pred)
  expect_equal(with(right_branch, mean(ClaimAmount)), right_pred)
  expect_equal(gamma_dev(left_branch$ClaimAmount, left_pred), left_dev)
  expect_equal(gamma_dev(right_branch$ClaimAmount, right_pred), right_dev)
})


test_that('Gamma tree with weights is being calculated correctly', {
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  # Fit a gamma regression tree
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 1)
  tree_gamma <- distRforest::rpart(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                   data = ausprivauto0405_claims, method = 'gamma', control = ctrl)
  # Split the data in the left and right branch
  left_branch <- ausprivauto0405_claims[! ausprivauto0405_claims$DrivAge == 'youngest people', ]
  right_branch <- ausprivauto0405_claims[ausprivauto0405_claims$DrivAge == 'youngest people', ]
  # Get number of observations in both branches
  left_nobs <- tree_gamma$frame$n[2]
  right_nobs <- tree_gamma$frame$n[3]
  # Get prediction for both branches
  left_pred <- tree_gamma$frame$yval[2]
  right_pred <- tree_gamma$frame$yval[3]
  # Get sum of deviance in both branches
  left_dev <- tree_gamma$frame$dev[2]
  right_dev <- tree_gamma$frame$dev[3]
  # Define a function to calculate the Poisson deviance
  gamma_dev <- function(ytrue, ypred, wcase = 1) -2 * sum(wcase * (log(ytrue/ypred) - ((ytrue - ypred) / ypred)), na.rm = TRUE)
  # Check whether the observed values match the expected empirical ones
  expect_equal(nrow(left_branch), left_nobs)
  expect_equal(nrow(right_branch), right_nobs)
  expect_equal(with(left_branch, weighted.mean(ClaimAmount, ClaimNb)), left_pred)
  expect_equal(with(right_branch, weighted.mean(ClaimAmount, ClaimNb)), right_pred)
  expect_equal(gamma_dev(left_branch$ClaimAmount, left_pred, left_branch$ClaimNb), left_dev)
  expect_equal(gamma_dev(right_branch$ClaimAmount, right_pred, right_branch$ClaimNb), right_dev)
})


test_that('Log-normal tree without weights is being calculated correctly', {
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  # Fit a log-normal regression tree
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 1)
  tree_lnorm <- distRforest::rpart(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                   data = ausprivauto0405_claims, method = 'lognormal', control = ctrl)
  # Split the data in the left and right branch
  left_branch <- ausprivauto0405_claims[! ausprivauto0405_claims$DrivAge == 'youngest people', ]
  right_branch <- ausprivauto0405_claims[ausprivauto0405_claims$DrivAge == 'youngest people', ]
  # Get number of observations in both branches
  left_nobs <- tree_lnorm$frame$n[2]
  right_nobs <- tree_lnorm$frame$n[3]
  # Get prediction for both branches
  left_pred <- tree_lnorm$frame$yval[2]
  right_pred <- tree_lnorm$frame$yval[3]
  # Get sum of deviance in both branches
  left_dev <- tree_lnorm$frame$dev[2]
  right_dev <- tree_lnorm$frame$dev[3]
  # Define a function to calculate the Poisson deviance
  lnorm_dev <- function(ytrue, ypred, wcase = 1) sum(wcase * (log(ytrue) - ypred)^2, na.rm = TRUE)
  # Check whether the observed values match the expected empirical ones
  expect_equal(nrow(left_branch), left_nobs)
  expect_equal(nrow(right_branch), right_nobs)
  expect_equal(with(left_branch, mean(ClaimAmount)), left_pred)
  expect_equal(with(right_branch, mean(ClaimAmount)), right_pred)
  expect_equal(lnorm_dev(left_branch$ClaimAmount, mean(log(left_branch$ClaimAmount))), left_dev)
  expect_equal(lnorm_dev(right_branch$ClaimAmount, mean(log(right_branch$ClaimAmount))), right_dev)
})


test_that('Log-normal tree with weights is being calculated correctly', {
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  # Fit a log-normal regression tree
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 1)
  tree_lnorm <- distRforest::rpart(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                   data = ausprivauto0405_claims, method = 'lognormal', control = ctrl)
  # Split the data in the left and right branch
  left_branch <- ausprivauto0405_claims[ausprivauto0405_claims$VehAge %in% c('young cars', 'youngest cars'), ]
  right_branch <- ausprivauto0405_claims[! ausprivauto0405_claims$VehAge %in% c('young cars', 'youngest cars'), ]
  # Get number of observations in both branches
  left_nobs <- tree_lnorm$frame$n[2]
  right_nobs <- tree_lnorm$frame$n[3]
  # Get prediction for both branches
  left_pred <- tree_lnorm$frame$yval[2]
  right_pred <- tree_lnorm$frame$yval[3]
  # Get sum of deviance in both branches
  left_dev <- tree_lnorm$frame$dev[2]
  right_dev <- tree_lnorm$frame$dev[3]
  # Define a function to calculate the Poisson deviance
  lnorm_dev <- function(ytrue, ypred, wcase = 1) sum(wcase * (log(ytrue) - ypred)^2, na.rm = TRUE)
  # Check whether the observed values match the expected empirical ones
  expect_equal(nrow(left_branch), left_nobs)
  expect_equal(nrow(right_branch), right_nobs)
  expect_equal(with(left_branch, weighted.mean(ClaimAmount, ClaimNb)), left_pred)
  expect_equal(with(right_branch, weighted.mean(ClaimAmount, ClaimNb)), right_pred)
  expect_equal(lnorm_dev(left_branch$ClaimAmount, weighted.mean(log(left_branch$ClaimAmount), left_branch$ClaimNb), left_branch$ClaimNb), left_dev)
  expect_equal(lnorm_dev(right_branch$ClaimAmount, weighted.mean(log(right_branch$ClaimAmount), right_branch$ClaimNb), right_branch$ClaimNb), right_dev)
})


test_that('Error is produced for gamma and lognormal in case of non-strictly-positive input',{
  expect_error(distRforest::rpart(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                  data = ausprivauto0405, method = 'gamma', control = ctrl),
               'Response variable must be > 0, support of the gamma distribution is strictly positive')
  expect_error(distRforest::rpart(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                  data = ausprivauto0405, method = 'lognormal', control = ctrl),
               'Response variable must be > 0, support of the log-normal distribution is strictly positive')
})