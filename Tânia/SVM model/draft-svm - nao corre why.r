library("e1071")

rm(list=ls())

#arredondar valor do erro
round0 <- function(val) {
    return(round(val, digits=0))
}

# tratamento dos dados
credit_data_df <- read.csv("/Users/taniamaldonado/Desktop/Bioinformática/1º Semestre/Aprendizagem Automática/Assessing-Classifiers/project-default-credit-card-clients1.csv", sep = ";", header = TRUE)
credit_data_df = credit_data_df[-2,]

# factorizar última coluna
credit_data_df$default.payment.next.month <- factor(credit_data_df$default.payment.next.month)

# for(i in 1:10) {
index <- 1:nrow(credit_data_df)
testindex <- sample(index, trunc(length(index)/3))
testdata <- na.omit(credit_data_df[testindex,])
traindata <- na.omit(credit_data_df[-testindex,])

## Train SVM with e1071:
x <- subset(traindata, select = -default.payment.next.month)
y <- traindata$default.payment.next.month

#linha mais importante. ver kernel, cost e gama 0.1
model <- svm(x, y, type="C-classification", kernel="radial", cost=1000)
print(model)
summary(model)

## Test with test data
xte <- subset(testdata, select = -default.payment.next.month)
pred <- cbind(predict(model, xte))
## (same as:)
##pred <- fitted(model)

# predict
predres <- factor(pred)  ## pred in (1,2)
cmpdata <- data.frame(actual=testdata$default.payment.next.month, predicted=predres)

# compara valores. se forem diferentes conta como erro
cnterr <- function(V1, V2) {
  cnt <- 0
  for(i in 1:length(V1)) {
    if ( (V1[i] == "0" && V2[i] == "2") ||
         (V1[i] == "1" && V2[i] == "1") ) {
      # cat(sprintf("%d: %s vs %s\n", i, V1[i], V2[i]));
      cnt <- cnt+1; } #erro é o valor que vai sair daqui
  }
  return(cnt)
}

# nr erros
nerr <- cnterr(cmpdata$actual,cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
results[i] = errprct
cat(sprintf("Percent errors: %f\n", errprct))

# }

print(results)
summary(results)
