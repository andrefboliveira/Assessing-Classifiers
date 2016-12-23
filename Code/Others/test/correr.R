# install.packages("neuralnet")
library("neuralnet")

rm(list=ls(all = TRUE))

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

run_MLP <- function(credit_data_df, do_plot, hidden_units, test_sample_size, mlp_threshold) {
  
 
  ## Train MLP with neuralnet:
  nn <- neuralnet(f, data=train, hidden=(hidden_units), act.fct="logistic", linear.output=TRUE, threshold=mlp_threshold)
  
  # Test MLP
  
  cmpv<-data.frame(actual=train[Y],predicted=nn$net.result)
  names(cmpv) <- sub("^structure.*", "predicted", names(cmpv))
  #print(cmpv)
  
  tstdata <- subset(test, select = use_names)
  nn.pred <- compute(nn, tstdata)$net.result
  
  predres <- apply(nn.pred, MARGIN=2, FUN=unit_round)
  cmpdata <- data.frame(test[Y], predres)
  names(cmpdata) <- c("actual", "predicted")
  
  
  nerr <- count_err(cmpdata$actual-cmpdata$predicted)
  errprct <- round(nerr/length(cmpdata$actual)*100, digits=2)
  cat(sprintf("Percent errors: %f\n", errprct))
  
  return(errprct)
  #return(nn)
}

credit_data_df <- read.csv("./Dataset/project-default-credit-card-clients.csv", sep = ";", header = TRUE)
do_plot <- FALSE
hidden_units <- 1
test_sample_size <- 0.25
mlp_threshold <- 0.05

# Checking to see if there is any missing data
NA_values <- sapply(credit_data_df, function(x) sum(is.na(x)))
#print(names(NA_values[NA_values>0]))

index <- 1:nrow(credit_data_df)
testindex <- sample(index, trunc(test_sample_size*nrow(credit_data_df)))
test <- na.omit(credit_data_df[testindex,])
train <- na.omit(credit_data_df[-testindex,])
Y <- "default.payment.next.month"
X <- "ID"

names <- names(credit_data_df)
use_names <- names(credit_data_df[!names %in% c(Y, X)])

f <- as.formula(paste(paste(Y, "~"),
                      paste(use_names, collapse=" + ")))


#run_MLP (credit_data_df, do_plot, hidden_units, test_sample_size, mlp_threshold)
best_result <- -Inf
camadas <- c()
erro <- c()


while(hidden_units < 30) {
  print(hidden_units)
  hidden_units <- hidden_units + 1
  result <- run_MLP (credit_data_df, do_plot, hidden_units, test_sample_size, mlp_threshold)
  
  camadas <- c(camadas, hidden_units)
  erro <- c(erro, result) 
}
df_results <- data.frame(camadas, erro)