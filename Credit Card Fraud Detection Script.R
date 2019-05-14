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

data.frame(Columns = ncol(cc_dataset), Rows = nrow(cc_dataset)) %>% 
  knitr::kable()

col_classes <- data.frame(Column = colnames(cc_dataset)[1:16],
                          Class = unname(apply(cc_dataset, 2, class))[1:16],
                          Column = c(colnames(cc_dataset)[17:ncol(cc_dataset)],""),
                          Class = c(unname(apply(cc_dataset, 2, class))[17:ncol(cc_dataset)],""))

colnames(col_classes) <- c("Column", "Class", "Column","Class")

col_classes %>% knitr::kable()