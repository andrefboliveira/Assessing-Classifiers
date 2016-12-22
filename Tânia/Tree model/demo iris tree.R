set.seed(101)
alpha     <- 0.7 # percentage of training set
inTrain   <- sample(1:nrow(iris), alpha * nrow(iris))
train.set <- iris[inTrain,]
test.set  <- iris[-inTrain,]

library("rpart")

# tree -> rpart

rpart.model <- rpart(Species ~ Sepal.Width + Petal.Width, data=train.set) # tree.model -> model
rpart.model

# summary(rpart.model)

# Distributional prediction
my.prediction <- predict(tree.model, test.set) # gives the probability for each class
head(my.prediction)