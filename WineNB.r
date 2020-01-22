library(dplyr) # for data wrangling
library(ggplot2) # to visualize data
library(gridExtra) # to display multiple graph
library(inspectdf) # for EDA
library(tidymodels) # to build tidy models
library(caret) # to pre-process data
library(rlang)
library(rsample)
library(tidyverse)
library(recipes)
library(e1071)
redwine <- read.csv("~/Downloads/wine-quality-selection/winequality-red.csv")
whitewine <- read.csv("~/Downloads/wine-quality-selection/winequality-white.csv")
redwine['color'] <- 'red'
whitewine['color'] <- 'white'
wines <- rbind(redwine, whitewine)
str(wines)
head(wines)
wines$color <- factor(wines$color)
wines$quality <- as.factor(wines$quality)
table(wines$color)
set.seed(417)
split <- initial_split(data = wines, prop = 0.8, strata = "quality")
train <- training(split)
test <- testing(split)
wine_lables<- train$quality
wine_lable <- test$quality
sms_test_labels  <- sms_data[4170:5559, ]$type
wine_recipe <- recipe(quality~., train) %>% 
  step_upsample(quality, seed = 417) %>%
  step_nzv(all_predictors()) %>% 
  prep()

wine_train <- juice(wine_recipe)
wine_test <- bake(wine_recipe, test) 
# model building
naive <- naiveBayes(wine_train[-12], wine_train$quality, laplace = 1)
naive1 <- naiveBayes(wine_train[-12],wine_train$quality)
# model fitting
naive_pred <- predict(naive, wine_test, type = "class") # for the class prediction
naive_prob <- predict(naive, wine_test, type = "raw") # for the probability
naive_pred1 <- predict(naive1,wine_test,type = "class")
#evaluating model performance
CrossTable(naive_pred,wine_test$quality, prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
CrossTable(naive_pred1,wine_test$quality, prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
confusionMatrix(naive_pred,wine_test$quality)
confusionMatrix(naive_pred1,wine_test$quality)

