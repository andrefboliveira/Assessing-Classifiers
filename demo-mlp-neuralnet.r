library("neuralnet")

rm(list=ls())

round0 <- function(val) {
    return(round(val, digits=0))
}

cnterr <- function(V) {
    cnt <- 0
    for(i in 1:length(V)) {
        if (V[i] != 0) { cnt <- cnt+1; }
    }
    return(cnt)
}

df <- read.csv("/Users/taniamaldonado/Desktop/Bioinformática/1º Semestre/Aprendizagem Automática/Assessing-Classifiers")
index <- 1:nrow(df)
testindex <- sample(index, trunc(length(index)/3))
testdata <- na.omit(df[testindex,])
traindata <- na.omit(df[-testindex,])

## Train MLP with neuralnet:
names <- names(traindata)
nn <- neuralnet(paste("Y ~",
                      paste(names[!names %in% c("Y", "X")],
                            collapse=" + ")),
                traindata, hidden=c(8,4,3), act.fct="logistic",
                linear.output=FALSE, threshold=0.1)
print(nn)
plot(nn)

## Test MLP
cmpv<-data.frame(actual=traindata$Y,predicted=nn$net.result)
names(cmpv) <- sub("^structure.*", "predicted", names(cmpv))
print(cmpv)

tstdata <- subset(testdata, select = !names(testdata) %in% c("Y"))
nn.pred <- compute(nn, tstdata)$net.result

predres <- apply(nn.pred, MARGIN=2, FUN=round0)
cmpdata <- data.frame(actual=testdata$Y, predicted=predres)

nerr <- cnterr(cmpdata$actual-cmpdata$predicted)
errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
cat(sprintf("Percent errors: %f\n", errprct))
