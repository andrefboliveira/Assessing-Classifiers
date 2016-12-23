# install.packages("lme4")
# install.packages("caret")
library("caret")

analyzeCM <- function(observed, predicted){
  confusion_matrix <- confusionMatrix(cmpdata$predicted, cmpdata$actual)$table
  print(confusion_matrix)
  prec <- confusion_matrix[2,2]/sum(confusion_matrix[2,])
  rec <- confusion_matrix[2,2]/sum(confusion_matrix[,2])
  f1 <- 2*((prec*rec)/(prec+rec))
  cat("Precision: ", prec, "; Recall: ", rec, "; f-measure", f1)
}