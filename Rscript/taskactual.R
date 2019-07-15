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
sum(duplicated(products[,4:18]))
#6 rows from the extended warranty are duplicated, so we'll remove them
products <- products[-c(35:41),]
#Check NA
any(is.na(products))
summary(products)
#There are missing values. 
#There are 15 missing values in Best Sellers Rank attribute, so we'll remove it.
products <- products[,-12]
#Check outliers
boxplot(products[])
boxplot(products$Volume)
#Dummiffying the data
newDataFrame <- dummyVars(" ~ .", data = products)

products <- data.frame(predict(newDataFrame, newdata = products))
#Correlation Matrix
ggcorr(products, label = TRUE, hjust = 0.85, size = 2, color = "grey50",
       label_size = 2) + ggplot2::labs(title = "Correlation Matrix (prods)")
#Here I remove the columns for the items we don't want to predict.
#We just keep those related to PC,Laptop,Smartphone and Netbook.
existing_products <- products[,-c(1:4,8,9,11:13)]
#We look the correlation matrix again:
ggcorr(existing_products, label = TRUE, hjust = 0.85, size = 2, color = "grey50",
       label_size = 2) + ggplot2::labs(title = "Correlation Matrix (prods)")
#Check for colinearity
corr_products <- cor(products)
View(corr_products[,which(names(products) == "Volume")])
#loop for colinearity?:
findCorrelation(x = corr_products, cutoff = 0.9, names = T)

#Check abnormalities (x5StarsReview had perfect correlation)
#Normalization, standarization
####Feature Selection####
#Create new features (combining)
##Remove "bad" features (features that are strongly correlated (regression))
##PCA
####Modeling####
##Try different models
##Try different features



products <- read_csv("datasets/existingproductattributes2017.csv")
newproducts <- read_csv("datasets/newproductattributes2017.csv")
####Visualizations####

plotprod <- products[, c(3:11, 13:18)]
View(plotprod)

for (i in names(plotprod[, -which(names(plotprod) == "Volume")])) {
  print(ggplot(data = plotprod, 
               aes_string(x=i, y = plotprod$Volume)) 
                 + geom_jitter(color = "darkred")
                   + ylab("Volume")
        )
}

names(plotprod)

sum(is.na(plotprod))

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

