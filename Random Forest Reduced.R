

library(tidymodels)
library(tidyverse)
library(vip)

#Exploration data analysis

head(attrition)
str(attrition)

# Initialize the split
split<- initial_split(attrition,prop = 0.8,strata = Attrition)
	
# Extract training set
train<- split %>% training()

# Extract testing set
test<- split %>% testing()

# Create recipe
feature_selection_recipe<- 
  recipe(Attrition ~ .,data = train) %>% 
  step_filter_missing(all_predictors(),threshold = 0.5) %>% 
  step_scale(all_numeric_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  prep()

# Create model
lr_model<- logistic_reg() %>% 
  set_engine("glm")

# Add recipe and model to a workflow
attrition_wflow<- workflow() %>% 
  add_recipe(feature_selection_recipe) %>% 
  add_model(lr_model)

# Fit workflow to train data
attrition_fit<- 
  attrition_wflow %>% fit(data = train)

# Add the test predictions to the test data
attrition_pred_df<- predict(attrition_fit,test) %>% 
  bind_cols(test %>% select(Attrition))

# Evaluate F score
f_meas(attrition_pred_df,Attrition, .pred_class)

# Display model estimates
tidy(attrition_fit)


#Create a Random Forest

# Specify the random forest model
rf_spec<- rand_forest(mode = "classification",trees = 200) %>% 
  set_engine("ranger",importance = "impurity")

# Fit the random forest model with all predictors
rf_fit<- rf_spec %>% 
  fit(Attrition~.,data = train)

# Create the test set prediction data frame
predict_df<- test %>% 
  bind_cols(predict = predict(rf_fit,test))
	
# Calculate F1 performance
f_meas(predict_df,Attrition, .pred_class)


#Reduce data using feature importance

# Extract the top ten features
top_features<- rf_fit %>% 
  vi(rank= TRUE) %>% 
  filter(Importance<= 10) %>% 
  pull(Variable)

# Add the target variable to the feature list
top_features<- c(top_features,"Attrition")

# Reduce and print the data sets
train_reduced<- train[top_features]
test_reduced<- test[top_features]
train_reduced %>% head(5)
test_reduced %>% head(5)


#Reduced Random Forest
# Fit a reduced model
rf_reduced_fit<- rf_spec %>% 
  fit(Attrition~.,data = train_reduced)

# Create test set prediction data frame
predict_reduced_df<- test_reduced %>% 
  bind_cols(predict = predict(rf_reduced_fit,test_reduced))

# Calculate F1 performance
f_meas(predict_reduced_df,Attrition, .pred_class)

##The reduced Random Forest is worst with a 0.908 against the Random Forest with a f-mean of 0.921

##PCA

library(ggfortify)

attrition_df=attrition%>%
  select(MonthlyIncome, Age, TotalWorkingYears, MonthlyRate, DistanceFromHome, DailyRate, 
         HourlyRate, YearsAtCompany, StockOptionLevel, Attrition)

head(attrition_df)

str(attrition_df)

pca_res <- prcomp(attrition_df%>%
                    select(-Attrition), scale. = TRUE)

summary(pca_res)

autoplot(pca_res, 
          data = attrition_df, 
          colour = 'Attrition', 
          alpha = 0.3,
          loadings = TRUE, 
          loadings.label = TRUE, 
          loadings.colour = "black", 
          loadings.label.colour = "black")


pca_res

