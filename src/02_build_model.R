library(caret)
library(janitor)
library(tidyverse)
library(infer)
options(scipen = 999)
set.seed(2019-10-25)

# READ DATA ----

train_features <- read_csv("data/train_features.csv", col_types = cols())
test_features <- read_csv("data/test_features.csv", col_types = cols())


# BUILD MODELS ----

## Linear model ----
model_lm <- train(data = train_features,
                  rushing_yards ~ down,
                  method = "lm")


# ASSESS MODEL ----

## linear model ----
predictions_lm <- predict(model_lm, test_features) %>% round(0)

results_lm <- tibble(
    prediction = predictions_lm,
    truth = test_features$rushing_yards
) %>%
    mutate(delta_amount = prediction - truth,
           delta_percent = prediction / truth)

results_lm_ci <- quantile(results_lm$delta_amount, c(0.025, 0.975))

results_lm %>%
    ggplot(aes(x = delta_amount)) +
    geom_histogram(binwidth = 1) +
    labs(title = "Linear Model Results",
         x = "Prediction yards delta",
         caption = "Positive number represents prediction was greater than actual
                    Negative number represents prediction was less than actual") +
    shade_ci(results_lm_ci)
