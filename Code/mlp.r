# install.packages("neuralnet")
library("neuralnet")

rm(list=ls(all = TRUE))

source("./Code/f_measure.R")


unit_round <- function(val) {
    return(round(val, digits=0))
}

count_err <- function(V) {
    cnt <- 0
    for(i in 1:length(V)) {
        if (V[i] != 0) { cnt <- cnt+1; }
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

# Train MLP with neuralnet:
nn <- neuralnet(f, data=train, hidden=(5), act.fct="logistic", linear.output=TRUE, threshold=0.1)

print(nn)
plot(nn)

# Test MLP
cmpv<-data.frame(actual=train$default.payment.next.month,predicted=nn$net.result)
names(cmpv) <- sub("^structure.*", "predicted", names(cmpv))
print(cmpv)

tstdata <- subset(test, select = use_names)
nn.pred <- compute(nn, tstdata)$net.result

predres <- apply(nn.pred, MARGIN=2, FUN=unit_round)
cmpdata <- data.frame(actual=test$default.payment.next.month, predicted=predres)


# Returns number of errors
nerr <- count_err(cmpdata$actual-cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
analyzeCM(cmpdata$actual, cmpdata$predicted)