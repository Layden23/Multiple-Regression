####Import datasets####
pacman::p_load(readr,rstudioapi,ggplot2,cowplot,GGally,caret)

current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)

products <- read_csv("datasets/existingproductattributes2017.csv")
newproducts <- read_csv("datasets/newproductattributes2017.csv")
####Visualizations####
####Data preprocessing####
#Check duplicated rows
#Check NA
#Check outliers
#Correlation Matrix
#Check for colinearity
#Check abnormalities (x5StarsReview had perfect correlation)
#Normalization, standarization
####Feature Selection####
#Create new features (combining)
##Remove "bad" features (features that are strongly correlated (regression))
##PCA
####Modeling####
##Try different models
##Try different features

