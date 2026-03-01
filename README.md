# autoReg: Automated Regression and Ensemble Learning in R
`autoReg` is an R package designed to streamline the statistical modeling workflow. It provides a comprehensive suite of tools for data pre-screening, fitting various regression models (Standard, Ridge, LASSO, and Elastic Net), and implementing advanced techniques like Bootstrap Bagging and Ensemble Learning.

# Features

- Data Pre-screening: Automatically handle categorical resopnse variables and identify the most informative predictors based on correlation.
- Flexible Modeling: Easily fit and evaluate standard linear/logistic regressions alongside regularized models.
- Bootstrap Bagging: Improve model stability and accuracy by averaging multiple bootstrap replicates.
- Interactive Ensemble Learning: Combine different model types through an interactive selection process to produce a robust final prediction.

# Installation
You can install the development version of `autoReg` from GitHub:
```r
# install.packages("devtools")
devtools::install_github("chaennong/autoReg")
```

# Quick Start

## 1. Data Preparation and Pre-screening
Check your data types and identify the top *K* predictors.
```r
library(autoReg)

# Check and convert response variable
clean_data <- check_data(X = cont.X, y = cont.y)

# Screen for the top 5 most informative predictors
prescreening(X = cont.x, y = cont.y, K = 5)
```

## 2. Fitting and Evaluating Models
Fit a model and evaluate its performance with a single function.
```r
# Evaluate a Standard Regression model
# (Automatically chooses Linear or Logistic based on response)
results <- evaluateModel(X = bin.x, y = bin.y, model = "standard", p = 0.8)

# View accuracy for binary data
print(results$accuracy)
```

## 3. Bootstrap Bagging
Reduce variance by bagging your models
```r
# Bagging a LASSO model with 50 replicates
bagged_lasso <- baggingLasso(cont.x, cont.y, R = 50)

# Check predictor selection frequency
print(bagged_lasso$selection.rate)
```

## 4. Interactive Ensemble Learning
Combine multiple models. The function will prompt you to choose which models to include until you are ready to exit and see the results.
```r
# Start interactive ensemble
ensembleLearning(bin.x, bin.y, p = 0.8)
```

# Available Functions
| Category | Functions |
| :--- | :--- | :--- |
| **Pre-Screening** | `check_data`, `train_test`, `prescreening` | 
| **Model Fitting** | `fitStandard`, `fitRidge`, `fitLasso`, `fitElasticNet` | 
| **Evaluation** | `evaluateModel`, `evaluateBagging` |
| **Bagging** | `baggingStandard`, `bagginRidge`, `baggingLasso`, `baggingElasticNet` |
| **Ensemble** | `ensembleLearning` |

# Author
**Chaeeun Shin** 
