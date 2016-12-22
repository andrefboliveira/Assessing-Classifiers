library("rpart")

# funciona de forma diferente de cada vez q corre

rm(list=ls())

## Rounding the error value
round0 <- function(val) {
    return(round(val, digits=0))
}

## Comparing error values
cnterr <- function(V1, V2) {
    cnt <- 0
    for(i in 1:length(V1)) {
        if (V1[i] != V2[i])  {
            # cat(sprintf("%d: %s vs %s\n", i, V1[i], V2[i]));
            cnt <- cnt+1; }
    }
    return(cnt)
}

## Organizing data
credit_data_df <- read.csv("/Users/taniamaldonado/Desktop/Bioinformática/1º Semestre/Aprendizagem Automática/Assessing-Classifiers/project-default-credit-card-clients.csv", sep = ";", header = TRUE)
credit_data_df = credit_data_df[-2,]

## Fixing what appears to be a typo in the field header PAY_0
names(credit_data_df)[names(credit_data_df) == "PAY_0"] <- "PAY_1"

## Checking to see if there is any missing data
# sapply(credit_data_df, function(x) sum(is.na(x)))

## Splitting data into Testing and Training data set
index <- sample(1:nrow(credit_data_df),round(0.75*nrow(credit_data_df)))
train <- credit_data_df[index,]
test <- credit_data_df[-index,]

## Train decision tree:
# names <- names(train)
# fmt <- paste("default.payment.next.month ~", paste(names[!names %in% "default.payment.next.month"], collapse=" + "))
model <- rpart(default.payment.next.month~., data = train, method="anova", control=rpart.control(minsplit=2, minbucket=1, cp=0.001))
    # minsplit: the minimum number of observations that must exist in a node in order for a split to be attempted.
    # minbucket: the minimum number of observations in any terminal <leaf> node.
    # cp: complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted. 

# printcp(model) # display the results 
# plotcp(model) # visualize cross−validation results 
# summary(model) # detailed summary of splits

## Plot the model
plot(model, uniform = TRUE, branch = 0.6, margin = 0.05)
text(model, use.n = TRUE)
title("Default Payment Next Month")

## Test decision tree:
pred <- cbind(predict(model, test[,1:25]))
predres <-  apply(pred, MARGIN=2, FUN=round0)
cmpdata <- data.frame(actual=test$"default.payment.next.month", predicted=predres)

## Find percentage of (predicted) errors
nerr <- cnterr(cmpdata$actual, cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
