# Walmart_Sales_Analysis_1

library(discrim)
library(glmnet)
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(ggplot2)
library(recipes)
library(embed)
library(themis)

train_data <- vroom("train.csv")

test_data <- vroom("test.csv")

features <- vroom("features.csv")


train_data  <- train_data  %>% mutate(Date = as.Date(as.character(Date)))
test_data   <- test_data   %>% mutate(Date = as.Date(as.character(Date)))
features    <- features    %>% mutate(Date = as.Date(as.character(Date)))

# features dataset prep
markdown_cols <- c("MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5")

features_clean <- features %>%
  mutate(across(all_of(markdown_cols), ~ replace_na(., 0))) %>%
  mutate(
    TotalMarkdown = rowSums(across(all_of(markdown_cols))),     
    MarkdownFlag = if_else(TotalMarkdown > 0, 1, 0)               
  )

# Join to datasets
train_joined <- train_data %>%
  left_join(features_clean, by = c("Store", "Date"))

test_joined <- test_data %>%
  left_join(features_clean, by = c("Store", "Date"))



