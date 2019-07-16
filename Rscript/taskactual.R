####Import datasets####
pacman::p_load(readr,rstudioapi,ggplot2,cowplot,GGally,caret,dplyr)

current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)

products <- read_csv("datasets/existingproductattributes2017.csv")
newproducts <- read_csv("datasets/newproductattributes2017.csv")
####Visualizations####

####Data preprocessing####

#Check duplicated rows
sum(duplicated(products[,-which(names(products) %in% c("ProductNum","Price"))]))
#6 rows from the extended warranty are duplicated, so we'll remove them
products <- products[-c(35:41),]
#Check NA
any(is.na(products))
summary(products)
#There are missing values. 
#There are 15 missing values in Best Sellers Rank attribute, so we'll remove it.
products <- products[,-which(names(products) %in% "BestSellersRank")]
#Check outliers
boxplot(products$Volume)$out
#Cleaning outliers
products <- filter(products, 
                   products$Volume < 7000)

#Cleaning every outlier?
#attributes <- cbind(products$Volume, products$x3StarReviews)
#View(attributes)
#for (i in attributes) {
#  products <- filter(products, i < min(boxplot(i)$out))
  
#}
#boxplot(products$Volume)
#boxplot(products$x3StarReviews)

newDataFrame <- dummyVars(" ~ .", data = products)

products <- data.frame(predict(newDataFrame, newdata = products))
#Correlation Matrix


#We just keep those related to PC,Laptop,Smartphone and Netbook.
existing_products <- products[,-which(names(products) %in% c("ProductTypeAccessories",
                                                             "ProductTypeDisplay",
                                                             "ProductTypeDisplay",
                                                             "ProductTypeExtendedWarranty",
                                          "ProductTypeGameConsole","ProductTypePrinter",
                                          "ProductTypePrinterSupplies",
                                          "ProductTypeSoftware", "ProductTypeTablet"))]
#We look the correlation matrix again:
#Check for colinearity
corr_products <- cor(products)
#Colinearity:
findCorrelation(x = corr_products, cutoff = 0.80, names = T)


pairwiseCor <- function(dataframe){
  pairs <- combn(names(dataframe), 2, simplify=FALSE)
  df <- data.frame(Vairable1=rep(0,length(pairs)), Variable2=rep(0,length(pairs)),
                   AbsCor=rep(0,length(pairs)), Cor=rep(0,length(pairs)))
  for(i in 1:length(pairs)){
    df[i,1] <- pairs[[i]][1]
    df[i,2] <- pairs[[i]][2]
    df[i,3] <- round(abs(cor(dataframe[,pairs[[i]][1]], dataframe[,pairs[[i]][2]])),4)
    df[i,4] <- round(cor(dataframe[,pairs[[i]][1]], dataframe[,pairs[[i]][2]]),4)
  }
  pairwiseCorDF <- df
  pairwiseCorDF <- pairwiseCorDF[order(pairwiseCorDF$AbsCor, decreasing=TRUE),]
  row.names(pairwiseCorDF) <- 1:length(pairs)
  pairwiseCorDF <<- pairwiseCorDF
  pairwiseCorDF
}

pairwiseCor(products)

#Check abnormalities (x5StarsReview had perfect correlation)
#Normalization, standarization
####Feature Selection####
#Create new features (combining)
#LinearModel to create new variable

lm_model <- train(Volume~x4StarReviews + x3StarReviews + x2StarReviews + x1StarReviews,
                  products, method = "lm")
summary(lm_model)$coefficients
products$"Avg_WghtStar" <-  summary(lm_model)$coefficients[2]*products$x4StarReviews + 
  summary(lm_model)$coefficients[3]*products$x3StarReviews + 
  summary(lm_model)$coefficients[4]*products$x2StarReviews + 
  summary(lm_model)$coefficients[5]*products$x1StarReviews
##Remove "bad" features (features that are strongly correlated (regression))
##We'll do that when training
####Modeling####
#Cross validation:
indexing <- createDataPartition(products$Volume, p = 0.75, list = F)
trainSet <- products[indexing,]
testSet <- products[-indexing,]

form <- c("Volume ~ x4StarReviews + x2StarReviews + PositiveServiceReview",
             "Volume ~ Avg_WghtStar + PositiveServiceReview")
models <- c("gbm", "rf","knn", "svmLinear", "svmRadial","glm")
combined <- c()
cnames <- vector()
ae_errors <- c()
re_errors <- c()

for (i in form){
  for (j in models) {
    model <- train(formula(i), data = trainSet, method = j)
    predictions <- predict(model, testSet)
    results <- postResample(predictions, testSet$Volume)
    combined <- cbind(results, combined)
    cnames <- c(paste(i,j),cnames)
    ae <- abs(predictions - testSet$Volume)
    re <- ae/testSet$Volume
    ae_errors <- cbind(ae,ae_errors)
    re_errors <- cbind(re,re_errors)
  }
}
colnames(combined) <-cnames
colnames(ae_errors) <-cnames
colnames(re_errors) <-cnames


####PCA####
preprocessparams <- preProcess(x = products[-which(colnames(products)==
                                                     c("Volume","Avg_WghtStar"))],
                               method = c("center","scale","pca"))

pca_trainSet <- predict(preprocessparams, 
                        trainSet[,-which(colnames(trainSet) %in% "Avg_WghtStar")])

pca_testSet <- predict(preprocessparams, 
                       testSet[,-which(colnames(trainSet) %in% "Avg_WghtStar")])
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3)

form <- c("Volume ~ x4StarReviews + x2StarReviews + PositiveServiceReview",
          "Volume ~ AvgWght_Star + PositiveServiceReview")
models <- c("rf","knn", "svmLinear", "svmRadial","glm","gbm")

for (i in form){
  for (j in models){
    model <- train(Volume~., data = pca_trainSet, method = i, tuneLength = 2)
    predictions <- predict(model, pca_testSet)
    results <- postResample(predictions, pca_testSet$Volume)
    pca_combined <- cbind(results, pca_combined)
  }
}

#Full PCA
pca_combined <- c()
cnames2 <- vector()

  
for (i in models) {
  pca_model <- train(Volume~., data = pca_trainSet[,-which(
    colnames(pca_trainSet) %in% "Avg_WghtStar")], method = i, tuneLength = 2)
  predictions <- predict(pca_model, pca_testSet)
  results <- postResample(predictions, pca_testSet$Volume)
  pca_combined <- cbind(results, pca_combined)
}
colnames(pca_combined) <- models
#PCA with 6 components
pca6_combined <- c()
for (i in models) {
  pca6_model <- train(Volume~., data = pca_trainSet[,which(
    colnames(pca_trainSet) %in% c("Volume","PC1","PC2","PC3","PC4","PC5","PC6"))],
    method = i, tuneLength = 2)
  predictions <- predict(pca6_model, pca_testSet)
  results <- postResample(predictions, pca_testSet$Volume)
  pca6_combined <- cbind(results, pca6_combined)
}
colnames(pca6_combined) <- models
