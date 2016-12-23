# install.packages("e1071")
library("e1071")

rm(list=ls(all = TRUE))

source("./Code/f_measure.R")


unit_round <- function(val) {
    return(round(val, digits=0))
}

# Comparing error values
count_err <- function(V1, V2) {
  cnt <- 0
  for(i in 1:length(V1)) {
    if (V1[i] != V2[i])  {
      cat(sprintf("%d: %s vs %s\n", i, V1[i], V2[i]));
      cnt <- cnt+1; }
  }
  return(cnt)
}



credit_data_df <- read.csv("./Dataset/project-default-credit-card-clients.csv", sep = ";", header = TRUE)

# Checking to see if there is any missing data
sapply(credit_data_df, function(x) sum(is.na(x)))

Y <- "default.payment.next.month"
X <- "ID"

names <- names(credit_data_df)
use_names <- names(credit_data_df[!names %in% c(Y, X)])

f <- as.formula(paste(paste(Y, "~"),
                      paste(use_names, collapse=" + ")))


credit_data_df$default.payment.next.month <- factor(credit_data_df$default.payment.next.month)

# Splitting data into Testing and Training data set
perct_train <- 2/3
train_index <- sample(1:nrow(credit_data_df), trunc(perct_train*nrow(credit_data_df)))
train_data <- na.omit(credit_data_df[train_index,])
test_data <- na.omit(credit_data_df[-train_index,])



## Train SVM with e1071:
x <- subset(train_data, select = use_names)
y <- train_data$default.payment.next.month

model <- svm(x, y, type="C-classification", kernel="polynomial", cost=10)

print(model)
summary(model)

## Test with test data
xte <- subset(test_data, select = use_names)
pred <- cbind(predict(model, xte))
## (same as:)
##pred <- fitted(model)

# Coorrection of output of SVM
pred[pred==1] <- 0
pred[pred==2] <- 1

predres <- factor(pred)
cmpdata <- data.frame(actual=test_data$default.payment.next.month, predicted=predres)


nerr <- count_err(cmpdata$actual,cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
analyzeCM(cmpdata$actual, cmpdata$predicted)