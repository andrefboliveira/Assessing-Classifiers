library("rpart")

rm(list=ls())

# Organizing data
credit_data_df <- read.csv("/Users/taniamaldonado/Desktop/Bioinformática/1º Semestre/Aprendizagem Automática/Assessing-Classifiers/project-default-credit-card-clients.csv", sep = ";", header = TRUE)
credit_data_df = credit_data_df[-2,]

## Checking to see if there is any missing data
# sapply(credit_data_df, function(x) sum(is.na(x)))

# Fixing what appears to be a typo in the field header PAY_0
names(credit_data_df)[names(credit_data_df) == "PAY_0"] <- "PAY_1"

## Splitting data into Testing and Training data set
index <- sample(1:nrow(credit_data_df),round(0.75*nrow(credit_data_df)))
train <- credit_data_df[index,]
test <- credit_data_df[-index,]

# ## Train decision tree:
# names <- names(train)
# fmt <- paste("default.payment.next.month ~", paste(names[!names %in% "default.payment.next.month"], collapse=" + "))
model <- rpart(default.payment.next.month~., data = train)

model
# n= 22499 
# 
# node), split, n, deviance, yval
# * denotes terminal node
# 
# 1) root 22499 3870.4580 0.2207654  
# 2) PAY_1< 1.5 20179 2797.8590 0.1663115  
# 4) PAY_2< 1.5 18491 2280.1970 0.1440701  
# 8) PAY_AMT3>=678.5 13372 1358.7940 0.1147921 *
#   9) PAY_AMT3< 678.5 5119  879.9980 0.2205509 *
#   5) PAY_2>=1.5 1688  408.3128 0.4099526 *
#   3) PAY_1>=1.5 2320  492.3272 0.6943966 *

## Distributional prediction
my.prediction <- predict(model, test) # gives the probability for each class
head(my.prediction)
# 4         8        19        27        31        37 
# 0.1144867 0.2184226 0.2184226 0.2184226 0.1144867 0.1144867 

# Point prediction
# Let's translate the probability output to categorical output
maxidx <- function(arr) {
  return(which(arr == max(arr)))
}
idx <- apply(my.prediction, c(1), maxidx)
prediction <- c('PAY_1', 'PAY_2', 'PAY_AMT3')[idx]
table(prediction, test.set$default.payment.next.month~.)

