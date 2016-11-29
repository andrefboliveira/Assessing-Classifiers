library("neuralnet")

rm(list=ls())

## Rounding the error value
round0 <- function(val) {
  return(round(val, digits=0))
}

## Comparing error values
cnterr <- function(V) {
  cnt <- 0
  for(i in 1:length(V)) {
    if (V[i] != 0) { cnt <- cnt+1; }
  }
  return(cnt)
}

# Organizing data
credit_data_df <- read.csv("/Users/taniamaldonado/Desktop/Bioinformática/1º Semestre/Aprendizagem Automática/Assessing-Classifiers/project-default-credit-card-clients.csv", sep = ";", header = TRUE)
credit_data_df = credit_data_df[-2,]

## Checking to see if there is any missing data
sapply(credit_data_df, function(x) sum(is.na(x)))

## Splitting data into Testing and Training data set
index <- sample(1:nrow(credit_data_df),round(0.75*nrow(credit_data_df)))
train <- credit_data_df[index,]
test <- credit_data_df[-index,]

## Train MLP with neuralnet:
names <- names(train)
f <- as.formula(paste("default.payment.next.month ~", paste(names[!names %in% "default.payment.next.month"], collapse = " + ")))


## try 1: Percent errors: 22.600000
## nn <- neuralnet(f, data=train, hidden=c(5,3), act.fct="logistic", linear.output=FALSE, threshold=0.1)

## try 2: Percent errors: 22.270000
# nn <- neuralnet(f, data=train, hidden=c(5,3), act.fct="logistic", linear.output=TRUE, threshold=0.1)

## try 3: Percent errors: 21.970000
# nn <- neuralnet(f, data=train, hidden=(5,3), act.fct="logistic", linear.output=TRUE, threshold=0.1)

## try 4: Percent errors: 21.270000     // BEST SO FAR
# nn <- neuralnet(f, data=train, hidden=(5), act.fct="logistic", linear.output=TRUE, threshold=0.1)

## try 5: Percent errors: 21.840000
# nn <- neuralnet(f, data=train, hidden=(6), act.fct="logistic", linear.output=TRUE, threshold=0.1)

## try 6: //NÃO CORRE
# nn <- neuralnet(f, data=train, hidden=(4), act.fct="logistic", linear.output=TRUE, threshold=0.1)

## try 7: Percent errors: 22.270000
# nn <- neuralnet(f, data=train, hidden=(1), act.fct="logistic", linear.output=TRUE, threshold=0.1)

## try 8: Percent errors: 22.010000
# nn <- neuralnet(f, data=train, hidden=(5), act.fct="logistic", linear.output=TRUE, threshold=0.2)

## try 9: Percent errors: 22.670000
nn <- neuralnet(f, data=train, hidden=(5), act.fct="logistic", linear.output=TRUE, threshold=0.05)


# print(nn)
# plot(nn)

## Test MLP
cmpv <- data.frame(actual=train$"default.payment.next.month",predicted=nn$net.result)
names(cmpv) <- sub("^structure.*", "predicted", names(cmpv))
print(cmpv)

tstdata <- subset(test, select = !names(test) %in% c("default.payment.next.month"))
nn.pred <- compute(nn, tstdata)$net.result


predres <- apply(nn.pred, MARGIN=2, FUN=round0)
cmpdata <- data.frame(actual=test$"default.payment.next.month", predicted=predres)

## Returns number of errors
nerr <- cnterr(cmpdata$actual-cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))


