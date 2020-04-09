context('Out of bag error')
library(distRforest)

# Use a gbm fit on the mtpl_be data to test the partial dependence function
if (!requireNamespace('CASdatasets', quietly = TRUE)) {
  stop('Package "CASdatasets" needed for this function to work. Please install it.',
       call. = FALSE)
}
library(CASdatasets)
data(ausprivauto0405)


test_that('the OOB error is tracked properly for a Poisson random forest with exposure in lhs formula',{
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = cbind(Exposure, ClaimNb) ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                 data = ausprivauto0405, method = 'poisson', control = ctrl, parms = list('shrink' = 10000000),
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < 0), 0)
  expect_true(head(rf_obj$oob_error, 1) > tail(rf_obj$oob_error, 1))
})


test_that('the OOB error is tracked properly for a Poisson random forest with exposure in offset',{
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = ClaimNb ~ offset(Exposure) + VehValue + VehAge + VehBody + Gender + DrivAge,
                                 data = ausprivauto0405, method = 'poisson', control = ctrl, parms = list('shrink' = 10000000),
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < 0), 0)
  expect_true(head(rf_obj$oob_error, 1) > tail(rf_obj$oob_error, 1))
})


test_that('the OOB error is tracked properly for a Poisson random forest without exposure',{
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = ClaimNb ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                 data = ausprivauto0405, method = 'poisson', control = ctrl, parms = list('shrink' = 10000000),
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < 0), 0)
  expect_true(head(rf_obj$oob_error, 1) > tail(rf_obj$oob_error, 1))
})


test_that('the OOB error is tracked properly for an anova random forest without weights',{
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                 data = ausprivauto0405_claims, method = 'anova', control = ctrl,
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < 0), 0)
  expect_true(head(rf_obj$oob_error, 1) > tail(rf_obj$oob_error, 1))
})


test_that('the OOB error is tracked properly for an anova random forest with weights',{
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                 data = ausprivauto0405_claims, method = 'anova', control = ctrl,
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < 0), 0)
  expect_true(head(rf_obj$oob_error, 1) > tail(rf_obj$oob_error, 1))
})


test_that('the OOB error is tracked properly for a gamma random forest without weights',{
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                 data = ausprivauto0405_claims, method = 'gamma', control = ctrl,
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < 0), 0)
  expect_true(head(rf_obj$oob_error, 1) > tail(rf_obj$oob_error, 1))
})


test_that('the OOB error is tracked properly for a gamma random forest with weights',{
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                 data = ausprivauto0405_claims, method = 'gamma', control = ctrl,
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < 0), 0)
  expect_true(head(rf_obj$oob_error, 1) > tail(rf_obj$oob_error, 1))
})


test_that('the OOB error is tracked properly for a lognormal random forest without weights',{
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                 data = ausprivauto0405_claims, method = 'lognormal', control = ctrl,
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < 0), 0)
  expect_true(head(rf_obj$oob_error, 1) > tail(rf_obj$oob_error, 1))
})


test_that('the OOB error is tracked properly for a lognormal random forest with weights',{
  ausprivauto0405_claims <- ausprivauto0405[ausprivauto0405$ClaimAmount > 0, ]
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = ClaimAmount ~ VehValue + VehAge + VehBody + Gender + DrivAge, weights = ClaimNb,
                                 data = ausprivauto0405_claims, method = 'lognormal', control = ctrl,
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < 0), 0)
  expect_true(head(rf_obj$oob_error, 1) > tail(rf_obj$oob_error, 1))
})


test_that('the OOB error is tracked properly for a balanced binary classification random forest',{
  ausprivauto0405_balanced <- rbind(ausprivauto0405[ausprivauto0405$ClaimOcc == 1, ],
                                    ausprivauto0405[ausprivauto0405$ClaimOcc == 0, ][1:5000, ])
  ctrl <- rpart.control(minsplit = 20, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                 data = ausprivauto0405_balanced, method = 'class', control = ctrl,
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < -1), 0)
  expect_equal(sum(rf_obj$oob_error > 1), 0)
  expect_true(head(rf_obj$oob_error, 1) < tail(rf_obj$oob_error, 1))
  # Define response as factor and recheck (originally integer)
  ausprivauto0405_balanced$ClaimOcc <- factor(ausprivauto0405_balanced$ClaimOcc)
  set.seed(9876)
  rf_obj_fact <- distRforest::rforest(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                      data = ausprivauto0405_balanced, method = 'class', control = ctrl,
                                      ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_true(all(rf_obj$oob_error == rf_obj_fact$oob_error))
})


test_that('the OOB error is tracked properly for an unbalanced binary classification random forest',{
  ctrl <- rpart.control(minsplit = 2, cp = 0, xval = 0, maxdepth = 5)
  set.seed(9876)
  rf_obj <- distRforest::rforest(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                 data = ausprivauto0405, method = 'class', control = ctrl,
                                 ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_equal(sum(is.na(rf_obj$oob_error)), 0)
  expect_equal(sum(rf_obj$oob_error < -1), 0)
  expect_equal(sum(rf_obj$oob_error > 1), 0)
  expect_true(head(rf_obj$oob_error, 1) < tail(rf_obj$oob_error, 1))
  # Define response as factor and recheck (originally integer)
  ausprivauto0405$ClaimOcc <- factor(ausprivauto0405$ClaimOcc)
  set.seed(9876)
  rf_obj_fact <- distRforest::rforest(formula = ClaimOcc ~ VehValue + VehAge + VehBody + Gender + DrivAge,
                                      data = ausprivauto0405, method = 'class', control = ctrl,
                                      ncand = 3, ntrees = 20, subsample = 0.5, redmem = TRUE, track_oob = TRUE)
  expect_true(all(rf_obj$oob_error == rf_obj_fact$oob_error))
})
