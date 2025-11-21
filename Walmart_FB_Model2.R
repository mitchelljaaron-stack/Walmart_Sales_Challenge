# Walmart Facebook Testing

## Libraries I need
library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)
library(dplyr)
library(glmnet)
library(patchwork)
library(ggplot2) 
library(recipes) 
library(embed) 
library(dials)
library(tune)

## Read in the Data
train_data <- vroom("./train.csv")
test_data <- vroom("./test.csv")
features <- vroom("./features.csv")

#########
## EDA ##
#########
plot_missing(features)
plot_missing(test)

### Impute Missing Markdowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log   = log1p(MarkDown_Total)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe))


train_joined <- train_data %>%
  left_join(imputed_features, by = c("Store", "Date"))

test_joined <- test_data %>%
  left_join(imputed_features, by = c("Store", "Date"))

#beat 0.4322

# Recipe
my_recipe <- recipe(Weekly_Sales ~ ., data = train_joined) %>%
  step_mutate(SD = paste(Store, Dept, sep = "_")) %>%
  step_other(all_nominal_predictors(), threshold = 0.001, other = "other") %>%
  step_mutate(across(where(is.logical), as.integer)) %>%
  step_mutate(Date = as.numeric(Date)) %>%        # ← FIX
  step_lencode_glm(all_nominal_predictors(), outcome = vars(Weekly_Sales)) %>%
  step_normalize(all_predictors())


# mod and wf
rf_mod <- rand_forest( mtry = tune(), min_n = tune(), trees = 500 ) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Workflow
rf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_mod) 

# Tuning grid
tuning_grid <- grid_regular(
  mtry(range = c(1, 10)),
  min_n(range = c(2, 10)),
  levels = 3
)

# CV folds
folds <- vfold_cv(train_joined, v = 3)

# Regression metrics
metrics_reg <- metric_set(rmse, mae, rsq)

# Cross-validation
CV_results <- rf_wf %>%
  tune_grid(
    resamples = folds,
    grid = tuning_grid,
    metrics = metrics_reg,
    control = control_grid(save_pred = TRUE)
  )

## Find Best Tuning Parameters 
bestTune <- CV_results %>% select_best(metric = "rmse")

## Finalize the Workflow & fit it 
final_wf <- rf_wf %>%
  finalize_workflow(bestTune)%>%
  fit(data=train_joined) 

## Predict 
final_predictions <- final_wf %>%
  predict(new_data = test_joined) %>%
  bind_cols(test_data %>% select(Store, Dept, Date)) %>%
  rename(Weekly_Sales = .pred) %>%
  mutate(Id = paste(Store,Dept,Date, sep = "_")) %>% 
  select(Id, Weekly_Sales)


# Export processed dataset 

vroom_write(x = final_predictions, file = "./walmart_rf_model_a.csv", delim = ",")

best_rmse <- CV_results %>%
  show_best(metric = "rmse", n = 1)
best_rmse
