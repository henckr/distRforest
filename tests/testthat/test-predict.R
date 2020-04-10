context('Random forest predict')
library(distRforest)

# Use a gbm fit on the mtpl_be data to test the partial dependence function
if (!requireNamespace('CASdatasets', quietly = TRUE)) {
  stop('Package "CASdatasets" needed for this function to work. Please install it.',
       call. = FALSE)
}
library(CASdatasets)
data(ausprivauto0405)


test_that('an error is generated for missing newdata and keep_data = FALSE', {
  # Set some global settings
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_poiss <- distRforest::rforest(formula = cbind(Exposure, ClaimNb) ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                   data = ausprivauto0405, method = 'poisson', control = ctrl, parms = list('shrink' = 10000000),
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE, keep_data = FALSE)
  expect_error(predict(rf_poiss),
               'The argument newdata must be supplied when the rforest is trained with keep_data = FALSE.')
})


test_that('predictions for a Poisson tree with exposure are being calculated correctly', {
  # Set some global settings
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_poiss <- distRforest::rforest(formula = cbind(Exposure, ClaimNb) ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                   data = ausprivauto0405, method = 'poisson', control = ctrl, parms = list('shrink' = 10000000),
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE)
  # Fit the random forest and keep data
  set.seed(54321)
  rf_poiss_data <- distRforest::rforest(formula = cbind(Exposure, ClaimNb) ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                        data = ausprivauto0405, method = 'poisson', control = ctrl, parms = list('shrink' = 10000000),
                                        ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE, keep_data = TRUE)
  # Get the predictions
  preds <- predict(rf_poiss, newdata = ausprivauto0405) 
  preds_data <- predict(rf_poiss_data)
  # Check whether thepredictions make sense
  expect_true(all(preds == preds_data))
  expect_equal(sum(is.na(preds)), 0)
  expect_equal(sum(preds <= 0), 0)
  expect_equal(length(preds), nrow(ausprivauto0405))
})


test_that('predictions for a gamma tree with weights are being calculated correctly', {
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  # Set some global settings
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_gamma <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                   data = ausprivauto0405_claims, method = 'gamma', control = ctrl,
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE)
  # Fit the random forest and keep data
  set.seed(54321)
  rf_gamma_data <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                        data = ausprivauto0405_claims, method = 'gamma', control = ctrl,
                                        ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE, keep_data = TRUE)
  preds <- predict(rf_gamma, newdata = ausprivauto0405_claims) 
  preds_data <- predict(rf_gamma_data)
  # Check whether thepredictions make sense
  expect_true(all(preds == preds_data))
  expect_equal(sum(is.na(preds)), 0)
  expect_equal(sum(preds <= 0), 0)
  expect_equal(length(preds), nrow(ausprivauto0405_claims))
})


test_that('predictions for a lognormal tree without weights are being calculated correctly', {
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  # Set some global settings
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_lnorm <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                   data = ausprivauto0405_claims, method = 'lognormal', control = ctrl,
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE)
  # Fit the random forest and keep data
  set.seed(54321)
  rf_lnorm_data <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                        data = ausprivauto0405_claims, method = 'lognormal', control = ctrl,
                                        ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE, keep_data = TRUE)
  preds <- predict(rf_lnorm, newdata = ausprivauto0405_claims) 
  preds_data <- predict(rf_lnorm_data)
  # Check whether thepredictions make sense
  expect_true(all(preds == preds_data))
  expect_equal(sum(is.na(preds)), 0)
  expect_equal(sum(preds <= 0), 0)
  expect_equal(length(preds), nrow(ausprivauto0405_claims))
})


test_that('predictions for a classification tree are being calculated correctly', {
  ausprivauto0405_balanced <- rbind(ausprivauto0405[ausprivauto0405$ClaimOcc == 1, ],
                                    ausprivauto0405[ausprivauto0405$ClaimOcc == 0, ][1:5000, ])
  # Set some global settings
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  ncand_val <- 3 ; ntrees_val <- 5 ; subsample_val <- 0.5
  # Fit the random forest
  set.seed(54321)
  rf_class <- distRforest::rforest(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                   data = ausprivauto0405_balanced, method = 'class', control = ctrl,
                                   ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE)
  # Fit the random forest and keep data
  set.seed(54321)
  rf_class_data <- distRforest::rforest(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                        data = ausprivauto0405_balanced, method = 'class', control = ctrl,
                                        ncand = ncand_val, ntrees = ntrees_val, subsample = subsample_val, red_mem = TRUE, keep_data = TRUE)
  preds <- predict(rf_class, newdata = ausprivauto0405_balanced) 
  preds_data <- predict(rf_class_data)
  # Check whether thepredictions make sense
  expect_true(all(preds == preds_data))
  expect_equal(sum(is.na(preds)), 0)
  expect_true(all(preds %in% c(0, 1)))
  expect_equal(length(preds), nrow(ausprivauto0405_balanced))
})