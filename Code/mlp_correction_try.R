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
Y <- "default.payment.next.month"
X <- "ID"

names <- names(credit_data_df)
use_names <- names(credit_data_df[!names %in% c(Y, X)])

f <- as.formula(paste(paste(Y, "~"),
                      paste(use_names, collapse=" + ")))


yes_data <- subset(credit_data_df, default.payment.next.month==1)
no_data <- subset(credit_data_df, default.payment.next.month==0)
proportion_yes <- nrow(yes_data)/nrow(credit_data_df)

perct_train <- 2/3

train_yes_index <- sample(1:nrow(yes_data), trunc((perct_train/2)*nrow(yes_data)))
train_yes_data <- na.omit(yes_data[train_yes_index,])
test_yes_data <- na.omit(yes_data[-train_yes_index,])

train_no_index <- sample(1:nrow(no_data), trunc((perct_train/2)*nrow(no_data)))
train_no_data <- na.omit(no_data[train_no_index,])
test_no_data <- na.omit(no_data[-train_no_index,])


test_data <- rbind.data.frame(test_no_data, test_yes_data)
train_data <- rbind.data.frame(train_no_data, train_yes_data)



# Train MLP with neuralnet:
nn <- neuralnet(f, data=train_data, hidden=c(9,4), act.fct="logistic", linear.output=FALSE, threshold=0.1)

# print(nn)
# plot(nn)

# Test MLP
cmpv<-data.frame(actual=train_data$default.payment.next.month,predicted=nn$net.result)
names(cmpv) <- sub("^structure.*", "predicted", names(cmpv))
#print(cmpv)

tstdata <- subset(test_data, select = use_names)
nn.pred <- compute(nn, tstdata)$net.result

predres <- apply(nn.pred, MARGIN=2, FUN=unit_round)
cmpdata <- data.frame(actual=test_data$default.payment.next.month, predicted=predres)


# Returns number of errors
nerr <- count_err(cmpdata$actual-cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
analyzeCM(cmpdata$actual, cmpdata$predicted)