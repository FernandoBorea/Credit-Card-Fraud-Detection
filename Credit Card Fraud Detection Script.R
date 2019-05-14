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

#We will first load the required libraries

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")

if(!require(RCurl)) install.packages("RCurl", 
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