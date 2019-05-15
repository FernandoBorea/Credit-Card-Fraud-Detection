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

if(!require(randomForest)) install.packages("randomForest", 
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

#We can check the structure of our data with str()

str(cc_dataset)

#We also can create two small charts with some information about our data

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
  geom_col() + 
  scale_y_log10() + 
  labs(title = "Fraudulent Transactions Distribution", 
       x = "Class (0 = Non-Fraudulent, 1 = Fraudulent)",
       y = "Count in Hundreds")

#Because of the huge difference we saw on the distribution, we will check how many entries we have on  each class:

class_dist <- cc_dataset %>%
               group_by(Class) %>%
               summarize(Count = n())
class_dist %>% knitr::kable()

#Due to the highly unbalanced data, we will now create a summary of the averages for each variable grouped by its class:

vars_grouped_avgs <- cc_dataset %>%
                     group_by(Class) %>%
                     summarize_all(list(mean))
vars_grouped_avgs

#We will drop the class column as it is a factor and we cannot perform operations with it

vars_diff <- vars_grouped_avgs[,-1]

#Then, we will calculate the difference bewtween values and sort them

diff <- abs(vars_diff[1,] - vars_diff[2,])
diff <- sort(diff, decreasing = TRUE)
diff

#We will check the average values for Time and Amount

vars_grouped_avgs[c("Time", "Amount")]

#Because we might have a similar distribution (hyphothesys explained on the report at page 7) on Time and Amount when 
#grouped by the transaction type, we will plot them to check if we are correct:

#We will transform Amount variable to log10 values and tidy up the data 

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

#As we can see, our hyphothesis was correct, so we will drop the Time and Amount entries of our vector

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
