####Import datasets####
pacman::p_load(readr,rstudioapi,ggplot2,cowplot,GGally,caret,dplyr,party)

current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)

products <- read_csv("datasets/existingproductattributes2017.csv")
newproducts <- read_csv("datasets/newproductattributes2017.csv")
set.seed(619)
####Visualizations####

####Data preprocessing####

#Check duplicated rows
sum(duplicated(products[,-which(names(products) %in% c("ProductNum","Price"))]))
#6 rows from the extended warranty are duplicated, so we'll remove them
for (i in c(1:ncol(products))){
  a <- any(is.na(products[,i]))
    if (a == T) {
      products[-i]
      
    }
}
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
ProductType <- as.vector(products$ProductType)
products <- products[,-which(colnames(products) %in% "ProductType")]
#Correlation Matrix


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
# decision tree

avg_decisiontree <- ctree(Volume~.,data = 
                        products[,-which(colnames(products) %in% c("x5StarReviews",
                                                                   "x4StarReviews",
                                                                   "x3StarReviews",
                                                                   "x2StarReviews",
                                                                   "x1StarReviews"))], 
                      controls = ctree_control(maxdepth = 3))
plot(avg_decisiontree)
decisiontree <- ctree(Volume~.,data = 
                        products[,-which(colnames(products) %in% c("x5StarReviews",
                                                                   "Avg_WghtStar","x3StarReviews",
                                                                   "x1StarReviews"))], 
                      controls = ctree_control(maxdepth = 3))
plot(decisiontree)

##Remove "bad" features (features that are strongly correlated (regression))
##We'll do that when training
####Modeling####
#Cross validation:
indexing <- createDataPartition(products$Volume, p = 0.75, list = F)
trainSet <- products[indexing,]
testSet <- products[-indexing,]

form <- c("Volume ~ x4StarReviews + PositiveServiceReview",
             "Volume ~ Avg_WghtStar + PositiveServiceReview")
models <- c("gbm", "rf","knn", "svmLinear", "svmRadial","glm")
combined <- c()
cnames <- vector()
for (i in form){
  for (j in models) {
    model <- train(formula(i), data = trainSet, method = j, tuneLength = 3, metric = "MAE")
    predictions <- predict(model, testSet)
    results <- postResample(predictions, testSet$Volume)
    combined <- cbind(results, combined)
    cnames <- c(paste(i,j),cnames)
  }
}
colnames(combined) <-cnames

####PCA####
pca_products <- products[,-which(colnames(products) == "Avg_WghtStar")]
preprocessparams <- preProcess(x = pca_products[-which(colnames(products)=="Volume"
                                                     )],
                               method = c("center","scale","pca"))

pca_trainSet <- predict(preprocessparams, 
                        trainSet[,-which(colnames(trainSet) %in% "Avg_WghtStar")])

pca_testSet <- predict(preprocessparams, 
                       testSet[,-which(colnames(trainSet) %in% "Avg_WghtStar")])
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3)

models <- c("rf","knn", "svmLinear", "svmRadial","glm","gbm")

pca_combined <- c()

for (i in models){
    pca_model <- train(Volume~., data = pca_trainSet, method = i, 
                       tuneLength = 2, metric = "MAE")
    pca_predictions <- predict(pca_model, pca_testSet)
    pca_results <- postResample(pca_predictions, pca_testSet$Volume)
    pca_combined <- cbind(pca_results, pca_combined)
}

colnames(pca_combined) <- models

####RF ####
rf_model <- train(Volume~Avg_WghtStar + PositiveServiceReview, trainSet, 
                  method = "svmLinear",
                  tuneLength = 2, metric = "MAE",preProcess = c("center","scale"))

rf_pred <- predict(rf_model, products)

postResample(rf_pred, products$Volume)


products$Volume[products$Volume == 0] <- 1
rfae_errors <- abs(rf_pred - products$Volume)
rfre_errors <- rfae_errors/products$Volume
mean(rfre_errors)
rferrors <- cbind(rfae_errors,rfre_errors)
rferrors <- as.data.frame(rferrors)
rferrors$Volume <- products$Volume
rferrors$ProductNum <- products$ProductNum
rferrors$ProductType <- ProductType
rferrors <- filter(rferrors, ProductType == "Laptop"|ProductType == "PC" |
                     ProductType == "Netbook" |ProductType == "Smartphone")
ggplot(rferrors, aes(x = Volume, y = rfre_errors, color = ProductType)) + geom_jitter()
ggplot(rferrors, aes(x = Volume, y = rfae_errors)) + geom_jitter()

####Predict####
#Create avg_weighted_star
newproducts$"Avg_WghtStar" <-  summary(lm_model)$coefficients[2]*newproducts$x4StarReviews + 
  summary(lm_model)$coefficients[3]*newproducts$x3StarReviews + 
  summary(lm_model)$coefficients[4]*newproducts$x2StarReviews + 
  summary(lm_model)$coefficients[5]*newproducts$x1StarReviews
newproducts$Volume <- predict(rf_model, newproducts)

