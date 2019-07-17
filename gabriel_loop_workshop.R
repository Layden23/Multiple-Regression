pacman:: p_load(caret, reshape, ggplot2, dplyr)
set.seed(203)
x <- runif(50, min = 0, max = 100) # generate 50 random number from 0 to 100
z <- runif(50, min = 0, max = 100)
a <- runif(50, min = 0, max = 100)
b <- runif(50, min = 0, max = 100)
y <- runif(50, min = 0, max = 100)

df <- as.data.frame(cbind(x,z,a,b,y))
combined <- c()
#Split

indexing <- createDataPartition(df$y, p = 0.75, list = F)
trainSet <- df[indexing,]
testSet <- df[-indexing,]

models <- c("knn","svmLinear","rf","lm")
combined <- c()
for (i in models){
  model <- train(y~.,data = trainSet, method = i)
  predictions <- predict(model, testSet)
  results <- postResample(predictions, testSet$y)
  combined <- cbind(results, combined)
}
colnames(combined) <- models
combined <- reshape::melt(combined)

ggplot(combined, aes(x = X2, y = value)) +  facet_grid(X1~., scales = "free") + geom_col()

ggplot

form <- c("y ~ z","y ~b", "y ~a")
models <- c("knn","svmLinear")
combined2 <- c()
cnames <- c()
for (i in form){
  for (j in models){
    model <- train(formula(i), data = trainSet, method = j)
    predictions <- predict(model, testSet)
    results <- postResample(predictions, testSet$y)
    combined2 <- cbind(results, combined2)
    cnames <- c(paste(i,j),cnames)
  }
}
colnames(combined2) <- cnames
names <- vector()
for (i in form){
  for (j in models){
    names <- append(names,paste(i,j))
  }
}

names 

colnames(combined2) <- names
  

reshape::



for (i in form){
  model <- train(formula(i), data = trainSet, method = "lm")
  predictions <- predict(model, testSet)
  results <- postResample(predictions, testSet$y)
  combined <- cbind(results, combined)
}
colnames(combined) <- form

combined2 <- reshape::melt(combined2)

combined2

ggplot(combined2, aes(x = X2, y = value)) +  facet_grid(X1~., scales = "free") + geom_col()





















methods_1 <- list() 

for (i in models) {
  
  methods_1[[i]] <- train(y~.,data = trainSet, method = i)
  
}


?melt
pred <- lapply(X = methods_1, function(x) predict(x,testSet))

pred$knn


