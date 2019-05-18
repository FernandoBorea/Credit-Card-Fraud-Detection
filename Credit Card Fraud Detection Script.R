###############
# Description #
###############

# This script will be divided into several sections, following the same structure as the Rmd/report file:

# 1. Data Adquisition
# 2. Data Exploration and Wrangling
# 3. Modeling
# 4. Testing

###############################
# Section 1: Data Adquisition #
###############################

#We will first load the required libraries for the project

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")

if(!require(RCurl)) install.packages("RCurl", 
                                     repos = "http://cran.us.r-project.org")

if(!require(knitr)) install.packages("knitr", 
                                     repos = "http://cran.us.r-project.org")

if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")

if(!require(Rborist)) install.packages("Rborist", 
                                     repos = "http://cran.us.r-project.org")

#And then, if we have the file in our working directory, we will read it from there

if(file.exists("creditcard.csv"))
{
  
  cc_dataset <- read_csv("creditcard.csv")
  
} else {
  
#If we do not have the file in our working directory, we will read it from Git LFS
  
  URL <- "https://media.githubusercontent.com/media/FernandoBorea/Credit-Card-Fraud-Detection/master/creditcard.csv"
  datURL <- getURL(URL)
  
  cc_dataset <- read_csv(datURL)
  
}

#Once this process is finished, we can do some preliminary data exploration, for example

#We can check the structure of the data with str()

str(cc_dataset)

#We also can create two small charts with some information about the data

#First, we will look for the amount of rows and columns

data.frame(Columns = ncol(cc_dataset), Rows = nrow(cc_dataset)) %>% 
  knitr::kable()

#Then we will look for the class of each column

col_classes <- data.frame(Column = colnames(cc_dataset)[1:16],
                          Class = unname(apply(cc_dataset, 2, class))[1:16],
                          Column = c(colnames(cc_dataset)[17:ncol(cc_dataset)],""),
                          Class = c(unname(apply(cc_dataset, 2, class))[17:ncol(cc_dataset)],""))

colnames(col_classes) <- c("Column", "Class", "Column","Class")

col_classes %>% knitr::kable()

#############################################
# Section 2: Data Exploration and Wrangling #
#############################################

#We will transform the Class column from to numeric to factor

cc_dataset <- cc_dataset %>% mutate(Class = as.factor(cc_dataset$Class))

class(cc_dataset$Class)

#Next, we will explore the data

#First, we will look for the distribution of the Class column

cc_dataset %>%
  count(Class) %>%
  ggplot(aes(x = Class, y = n/100,  fill = Class)) + 
  geom_col(col = "Black") + 
  scale_y_log10() + 
  labs(title = "Fraudulent Transactions Distribution", 
       x = "Class (0 = Non-Fraudulent, 1 = Fraudulent)",
       y = "Count in Hundreds")

#Because of the huge difference we saw on the distribution, we will check how many entries we have on each class:

class_dist <- cc_dataset %>%
               group_by(Class) %>%
               summarize(Count = n())
class_dist %>% knitr::kable()

#Due to the highly unbalanced data, we will now create a summary of the averages for each variable grouped by its class:

vars_grouped_avgs <- cc_dataset %>%
                     group_by(Class) %>%
                     summarize_all(list(mean))
vars_grouped_avgs

#We will drop the class column as it is a factor and we cannot perform mathematical operations with it

vars_diff <- vars_grouped_avgs[,-1]

#Then, we will calculate the difference bewtween values and sort them

diff <- abs(vars_diff[1,] - vars_diff[2,])
diff <- sort(diff, decreasing = TRUE)
diff

#We will check the average values for Time and Amount

vars_grouped_avgs[c("Time", "Amount")]

#Because we might have a similar distribution (hypothesis explained on the report at page 7) on Time and Amount variables
#when grouped by the transaction type, we will plot them to check if we are correct:

#We will transform the Amount variable to log10 values and tidy up the data 

time_amount_tidy <- cc_dataset[c("Time", "Amount", "Class")] %>%
  mutate(Amount = if_else(Amount != 0,log10(Amount), Amount)) %>%
  gather(-Class, key = "Variable", value = "Values")

#To then plot it

time_amount_tidy %>% 
  ggplot(aes(x = Class, y = Values, fill = Class)) +
  facet_wrap(~Variable, scales = "free") +
  geom_boxplot() + 
  labs(title = "Time and Amount Variables Distribution", 
       x = "Class (0 = Non-Fraudulent, 1 = Fraudulent)",
       y = "value")

#As we can see, our hyphothesis was correct, so we will drop the Time and Amount variables entries of our vector

diff <- diff[-which(names(diff) %in% c("Time", "Amount"))]

#Now we will plot the top 3 variables we got from the updated diff vector

#We will tidy up the data first

tidy_data <- cc_dataset[c("V3", "V14", "V17", "Class")] %>%
  gather(-Class, key = "Variable", value = "Values")

#And then plot it

tidy_data %>% ggplot(aes(x = Class, y = Values, fill = Class)) + 
  facet_wrap(~Variable, scales = 'free') +
  geom_boxplot() + 
  labs(title = "V3, V14 and V17 Variables Distribution", 
       x = "Class (0 = Non-Fraudulent, 1 = Fraudulent)",
       y = "value")

#As we saw, up to now, our difference of averages approach is still valid

#Now we will plot the last 3 variables we got on our diff vector

#We will again tidy up the data first

tidy_data <- cc_dataset[c("V25", "V23", "V22", "Class")] %>%
  gather(-Class, key = "Variable", value = "Values")

#And then plot it

tidy_data %>% ggplot(aes(x = Class, y = Values, fill = Class)) + 
  facet_wrap(~Variable, scales = 'free') +
  geom_boxplot() + 
  labs(title = "V25, V23 and V22 Variables Distribution", 
       x = "Class (0 = Non-Fraudulent, 1 = Fraudulent)",
       y = "value")

#Now as our approach appears to hold up as well with the last 3 variables as they show a very similar distirbution
#on the Interquartile Range, we will now do one last data wrangling task prior starting the modeling phase. We will
#create a training set as well as a test set.

y <- cc_dataset$Class

#We will use 70% of the data for training and 30% for testing

train_index <-  createDataPartition(y, times = 1, p = 0.7, list = FALSE)

train_set <- cc_dataset %>% slice(train_index)
test_set <- cc_dataset %>% slice(-train_index)


#######################
# Section 3: Modeling #
#######################

#Please note that on this section we will not include a deep explaination about the code we are using as 
#it is included on the report.

#Now we will get our baseline data

all_nonfraud <- rep(0, nrow(test_set)) %>% factor(levels = c("0", "1"))

cm_all_nonfraud <- confusionMatrix(data = all_nonfraud, 
                                   reference = test_set$Class,
                                   positive = "1")

base_accuracy <- cm_all_nonfraud$overal["Accuracy"]
base_b_accuracy <- cm_all_nonfraud$byClass["Balanced Accuracy"]
base_sensitivity <- cm_all_nonfraud$byClass["Sensitivity"]
base_specificity <- cm_all_nonfraud$byClass["Specificity"]

#Also, we will create a data frame to store our results. We will also measure sensitivity and specificity

metrics <- c("Model", "Accuracy", "Balanced Accuracy", "Sensitivity", "Specificity")

results <- data.frame(Model = "All Non-Fraudulent", 
                      Accuracy = unname(base_accuracy), 
                      "Balanced Accuracy" = unname(base_b_accuracy),
                      Sensitivity = unname(base_sensitivity), 
                      Specificity = unname(base_specificity),
                      stringsAsFactors = FALSE)
colnames(results) <- metrics
results %>% knitr::kable()

#We will start by training a GLM model with all the variables

fit_glm_allvar <- glm(Class ~ ., data = train_set, family = "binomial")

predict_glm_allvar <- predict(fit_glm_allvar, test_set, type = "response")

yhat_glm_allvar <- if_else(predict_glm_allvar > 0.5, 1, 0) %>% factor()

cm_glm_allvar <- confusionMatrix(data = yhat_glm_allvar, reference = test_set$Class, 
                                 positive = "1")

#And then storing the results

glm_allvar_acc <- cm_glm_allvar$overall["Accuracy"]
glm_allvar_b_acc <- cm_glm_allvar$byClass["Balanced Accuracy"]
glm_allvar_st <- cm_glm_allvar$byClass["Sensitivity"]
glm_allvar_sp <- cm_glm_allvar$byClass["Specificity"]

glm_allvar_results <-  data.frame(Model = "GLM - All Variables",
                                  Accuracy = unname(glm_allvar_acc),
                                  "Balanced Accuracy" = unname(glm_allvar_b_acc),
                                  Sensitivity = unname(glm_allvar_st),
                                  Specificity = unname(glm_allvar_sp), 
                                  stringsAsFactors = FALSE)
colnames(glm_allvar_results) <- metrics

results <- bind_rows(results, glm_allvar_results)
results %>% knitr::kable()

#Now we will train a Linear Discriminant Analysis Model with all the variables

fit_lda_allvar <- train(Class ~ ., method = "lda", data = train_set)

predict_lda_allvar <- predict(fit_lda_allvar, test_set)

cm_lda_allvar <- confusionMatrix(data = predict_lda_allvar, reference = test_set$Class,
                                positive = "1")

#And then we will store the results

lda_allvar_acc <- cm_lda_allvar$overall["Accuracy"]
lda_allvar_b_acc <- cm_lda_allvar$byClass["Balanced Accuracy"]
lda_allvar_st <- cm_lda_allvar$byClass["Sensitivity"]
lda_allvar_sp <- cm_lda_allvar$byClass["Specificity"]

lda_allvar_results <-  data.frame(Model = "LDA - All Variables",
                                  Accuracy = unname(lda_allvar_acc),
                                  "Balanced Accuracy" = unname(lda_allvar_b_acc),
                                  Sensitivity = unname(lda_allvar_st),
                                  Specificity = unname(lda_allvar_sp), 
                                  stringsAsFactors = FALSE)
colnames(lda_allvar_results) <- metrics

results <- bind_rows(results, lda_allvar_results)
results %>% knitr::kable()

#Now we will train a Quadratic Discriminant Analysis Model with all the variables

fit_qda_allvar <- train(Class ~ ., method = "qda", data = train_set)

predict_qda_allvar <- predict(fit_qda_allvar, test_set)

cm_qda_allvar <- confusionMatrix(data = predict_qda_allvar, reference = test_set$Class,
                                 positive = "1")

#And then we will store the results

qda_allvar_acc <- cm_qda_allvar$overall["Accuracy"]
qda_allvar_b_acc <- cm_qda_allvar$byClass["Balanced Accuracy"]
qda_allvar_st <- cm_qda_allvar$byClass["Sensitivity"]
qda_allvar_sp <- cm_qda_allvar$byClass["Specificity"]

qda_allvar_results <-  data.frame(Model = "QDA - All Variables",
                                  Accuracy = unname(qda_allvar_acc),
                                  "Balanced Accuracy" = unname(qda_allvar_b_acc),
                                  Sensitivity = unname(qda_allvar_st),
                                  Specificity = unname(qda_allvar_sp), 
                                  stringsAsFactors = FALSE)
colnames(qda_allvar_results) <- metrics

results <- bind_rows(results, qda_allvar_results)
results %>% knitr::kable()

#Now, as explained on the report, we will choose the QDA approach to work upon for our final model

#We will start by using our naive average differences approach to select the top 5 variables to train again our model

diff[1:5]

fit_qda_nad5 <- train(Class ~ V3 + V14 + V17 + V12 + V10,
                       method = "qda", data = train_set)

predict_qda_nad5 <- predict(fit_qda_nad5, test_set)

cm_qda_nad5 <- confusionMatrix(data = predict_qda_nad5, reference = test_set$Class,
                                 positive = "1")

#And then we will store the results

qda_nad5_acc <- cm_qda_nad5$overall["Accuracy"]
qda_nad5_b_acc <- cm_qda_nad5$byClass["Balanced Accuracy"]
qda_nad5_st <- cm_qda_nad5$byClass["Sensitivity"]
qda_nad5_sp <- cm_qda_nad5$byClass["Specificity"]

qda_nad5_results <-  data.frame(Model = "QDA - NAD Top 5",
                                  Accuracy = unname(qda_nad5_acc),
                                  "Balanced Accuracy" = unname(qda_nad5_b_acc),
                                  Sensitivity = unname(qda_nad5_st),
                                  Specificity = unname(qda_nad5_sp), 
                                  stringsAsFactors = FALSE)
colnames(qda_nad5_results) <- metrics

results <- bind_rows(results, qda_nad5_results)
results %>% knitr::kable()

#We will use the filterVarImp function to pick another set of predictors

top_10_pred <- filterVarImp(test_set[-31], predict_qda_allvar)

top_10_pred <- top_10_pred[order(top_10_pred$X0, decreasing = TRUE),]

rownames(top_10_pred[1:10,])

#And train again our model using those

fit_qda_fvi10 <- train(Class ~ V1 + V3 + V2 + V17 + V7
                           + V4 + V16 + V8 + V18 + V27,
                      method = "qda", data = train_set)

predict_qda_fvi10 <- predict(fit_qda_fvi10, test_set)

cm_qda_fvi10 <- confusionMatrix(data = predict_qda_fvi10, reference = test_set$Class,
                               positive = "1")

#Then we will store the results

qda_fvi10_acc <- cm_qda_fvi10$overall["Accuracy"]
qda_fvi10_b_acc <- cm_qda_fvi10$byClass["Balanced Accuracy"]
qda_fvi10_st <- cm_qda_fvi10$byClass["Sensitivity"]
qda_fvi10_sp <- cm_qda_fvi10$byClass["Specificity"]

qda_fvi10_results <-  data.frame(Model = "QDA - filterVarImp Top 10",
                                Accuracy = unname(qda_fvi10_acc),
                                "Balanced Accuracy" = unname(qda_fvi10_b_acc),
                                Sensitivity = unname(qda_fvi10_st),
                                Specificity = unname(qda_fvi10_sp), 
                                stringsAsFactors = FALSE)
colnames(qda_fvi10_results) <- metrics

results <- bind_rows(results, qda_fvi10_results)
results %>% knitr::kable()

#We will now sort our results

final_results <- results[order(results$Sensitivity, decreasing = TRUE),]
final_results %>% knitr::kable() 

#And show the confusion matrix for our final model

cm_qda_allvar

#We can run our final model on the entire data set

qda_all_dataset <- predict(fit_qda_allvar, cc_dataset)

cm_qda_all_dataset <- confusionMatrix(data = qda_all_dataset, reference = cc_dataset$Class,
                                      positive = "1")
cm_qda_all_dataset
