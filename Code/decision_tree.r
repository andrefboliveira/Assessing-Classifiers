# install.packages("rpart")
library("rpart")

rm(list=ls(all = TRUE))

unit_round <- function(val) {
    return(round(val, digits=0))
}

count_err <- function(V1, V2) {
    cnt <- 0
    for(i in 1:length(V1)) {
        if (V1[i] != V2[i])  {
            #cat(sprintf("%d: %s vs %s\n", i, V1[i], V2[i]));
            cnt <- cnt+1; }
    }
    return(cnt)
}

credit_data_df <- read.csv("./Dataset/project-default-credit-card-clients.csv", sep = ";", header = TRUE)

# Checking to see if there is any missing data
sapply(credit_data_df, function(x) sum(is.na(x)))

index <- 1:nrow(credit_data_df)
testindex <- sample(index, trunc(length(index)/3))
test <- na.omit(credit_data_df[testindex,])
train <- na.omit(credit_data_df[-testindex,])

Y <- "default.payment.next.month"
X <- "ID"
names <- names(credit_data_df)
use_names <- names(credit_data_df[!names %in% c(Y, X)])

f <- as.formula(paste(paste(Y, "~"),
                      paste(use_names, collapse=" + ")))

## Train decision tree:
model <- rpart(f, data = train)

plot(model, compress=TRUE)
text(model, use.n = TRUE)

## Test decision tree:
tstdata <- subset(test, select = use_names)

pred <- cbind(predict(model, tstdata))
predres <-  apply(pred, MARGIN=2, FUN=unit_round)
cmpdata <- data.frame(test[Y], predres)
names(cmpdata) <- c("actual", "predicted")

nerr <- count_err(cmpdata$actual, cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
