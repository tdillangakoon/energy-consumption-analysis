# Task 5 Notes

## 1. Model Comparison

The candidate regression models were compared using test RMSE, MAE, test R-squared, and training performance measures. The model with the lowest test RMSE provided the best predictive performance.

## 2. Final Model

The final model was selected based on predictive performance on the unseen test period. The model with the lowest test RMSE was selected as the final model.

## 3. Diagnostic Assessment

Model assumptions were assessed using diagnostic plots and statistical tests, including the Breusch–Pagan test for heteroscedasticity and the Shapiro–Wilk test for residual normality.

## 4. Multicollinearity

Variance Inflation Factor (VIF) was examined to identify potential multicollinearity among the predictors. Perfect multicollinearity was also checked using the alias function.

## 5. Influential Observations

Cook's distance was used to identify potentially influential observations. Influential observations were examined but were not automatically removed.

## 6. Cluster-Robust Standard Errors

Cluster-robust standard errors were calculated at the household level to account for possible within-household dependence among repeated monthly observations.