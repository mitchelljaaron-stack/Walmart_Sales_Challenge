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
library(prophet)

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

#beat 4322

# Recipe
my_recipe <- recipe(Weekly_Sales ~ ., data = train_joined) %>%
  step_mutate(SD = paste(Store, Dept, sep = "_")) %>%
  step_other(all_nominal_predictors(), threshold = 0.001, other = "other") %>%
  step_mutate(across(where(is.logical), as.integer)) %>%
  step_mutate(Date = as.numeric(Date)) %>%        # ← FIX
  step_lencode_glm(all_nominal_predictors(), outcome = vars(Weekly_Sales)) %>%
  step_normalize(all_predictors())

## Need to have the column names right for prophet
prophet_df <- train_joined %>%
  rename(y=Weekly_Sales, ds=Date)

test_df <- test_joined %>%
  rename(ds=Date)

## Fit prophet Model with extra regressors
prophet_model <- prophet() %>%
  add_regressor('Store') %>%
  add_regressor('Dept') %>%
  add_regressor('MarkDown_Total') %>%
  fit.prophet(prophet_df)

## Predict using Prophet Model
prof_predict <- predict(prophet_model, test_df)

store <- 17
dept <- 17

## Filter and Rename to match prophet syntax
sd_train <- train_joined %>%
  filter(Store==store, Dept==dept) %>%
  rename(y=Weekly_Sales, ds=Date)

sd_test <- test_joined %>%
  filter(Store==store, Dept==dept) %>%
  rename(ds=Date)


## Fit a prophet model
prophet_model <- prophet() %>%
  add_regressor('Store') %>%
  add_regressor('Dept') %>%
  add_regressor('MarkDown_Total') %>%
  add_regressor('Store') %>%
  add_regressor('IsHoliday.x') %>%
  add_regressor('Temperature') %>%
  add_regressor('CPI') %>%
  add_regressor('Unemployment') %>%
  add_regressor('IsHoliday.y') %>%
  fit.prophet(df=sd_train)


## Predict Using Fitted prophet Model
fitted_vals <- predict(prophet_model, df=sd_train) #For Plotting Fitted Values
test_preds <- predict(prophet_model, df=sd_test) #Predictions are called "yhat"

## Plot Fitted and Forecast on Same Plot
ggplot() +
  geom_line(data = sd_train, mapping = aes(x = ds, y = y, color = "Data")) +
  geom_line(data = fitted_vals, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  geom_line(data = test_preds, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
  labs(color="")



## Predict 

prof_predict <- prof_predict %>%
  mutate(
    Store = test_joined$Store,
    Dept  = test_joined$Dept
  )

first_predictions <- prof_predict %>%
  select(-Store, -Dept) %>%                      # REMOVE old Store/Dept
  bind_cols(test_joined %>% select(Store, Dept, Date)) %>% 
  rename(Weekly_Sales = yhat) %>%
  mutate(Id = paste(Store, Dept, Date, sep = "_")) %>% 
  select(Id, Weekly_Sales)

  
second_predictions <- test_preds %>% 
  select(-Store, -Dept) %>%                      # REMOVE old Store/Dept
  bind_cols(test_joined %>% select(Store, Dept, Date)) %>% 
  rename(Weekly_Sales = yhat) %>%
  mutate(Id = paste(Store, Dept, Date, sep = "_")) %>% 
  select(Id, Weekly_Sales)


vroom_write(x = first_predictions, file = "./walmart_fb_model_a.csv", delim = ",")

