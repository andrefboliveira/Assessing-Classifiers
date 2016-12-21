library("rpart")

rm(list=ls())

round0 <- function(val) {
    return(round(val, digits=0))
}

cnterr <- function(V1, V2) {
    cnt <- 0
    for(i in 1:length(V1)) {
        if (V1[i] != V2[i])  {
            cat(sprintf("%d: %s vs %s\n", i, V1[i], V2[i]));
            cnt <- cnt+1; }
    }
    return(cnt)
}

df <- read.csv("./tic-tac-toe.csv")
index <- 1:nrow(df)
testindex <- sample(index, trunc(length(index)/3))
testdata <- na.omit(df[testindex,])
traindata <- na.omit(df[-testindex,])

## Train decision tree:
names <- names(traindata)
fmt <- paste("Y ~", paste(names[!names %in% c("Y", "X")], collapse=" + "))
model <- rpart(fmt, data = traindata)

plot(model, compress=TRUE)
text(model, use.n = TRUE)

## Test decision tree:
pred <- cbind(predict(model, testdata[,1:9]))
predres <-  apply(pred, MARGIN=2, FUN=round0)
cmpdata <- data.frame(actual=testdata$Y, predicted=predres)

nerr <- cnterr(cmpdata$actual, cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
