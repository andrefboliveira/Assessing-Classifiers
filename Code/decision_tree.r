# install.packages("rpart")
library("rpart")

rm(list=ls(all = TRUE))

# Rounding the error value
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


# Splitting data into Testing and Training data set
index <- 1:nrow(credit_data_df)
trainindex <- sample(index, trunc(0.75*nrow(credit_data_df)))
train <- na.omit(credit_data_df[trainindex,])
test <- na.omit(credit_data_df[-trainindex,])

Y <- "default.payment.next.month"
X <- "ID"
names <- names(credit_data_df)
use_names <- names(credit_data_df[!names %in% c(Y, X)])

f <- as.formula(paste(paste(Y, "~"),
                      paste(use_names, collapse=" + ")))

# Train decision tree:
# model <- rpart(f, data = train)
model <- rpart(f, data = train, method="anova", control=rpart.control(minsplit=2, minbucket=1, cp=0.001))
# minsplit: the minimum number of observations that must exist in a node in order for a split to be attempted.
# minbucket: the minimum number of observations in any terminal <leaf> node.
# cp: complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted. 

# printcp(model) # display the results 
# plotcp(model) # visualize cross−validation results 
# summary(model) # detailed summary of splits
# Plot the model
plot(model, uniform = TRUE, branch = 0.6, margin = 0.05)
text(model, use.n = TRUE)
title("Default Payment Next Month")

#plot(model, compress=TRUE)
#text(model, use.n = TRUE)

# Test decision tree:
tstdata <- subset(test, select = use_names)

pred <- cbind(predict(model, tstdata))
predres <-  apply(pred, MARGIN=2, FUN=unit_round)
cmpdata <- data.frame(actual=test$default.payment.next.month, predicted=predres)

# Find percentage of (predicted) errors
nerr <- count_err(cmpdata$actual, cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))