rm(list=ls(all = TRUE))


# Runs the gradient descent algorithm with batch backpropagation
# until the maximum number of epochs is reached
mlp <- function(X,
                 Y,
                 net_structure,
                 learning_rate<-0.3,
                 max_epochs<-100,
                 weights<-NA)
{
  if (is.na(weights)) {
    weights <- initialize_random(net_structure)
  }
  
  num_layers <- length(net_structure)
  
  for (t in seq(1:max_epochs)) {
    gradient <- backprop(X, Y, weights)
    for (l in seq(from<-num_layers-1, to<-1)) {
      weights[[l]] <- weights[[l]] - learning_rate*gradient[[l]]
    }
  }  
  return(weights)
}


# Executes a single epoch of the batch backpropagation algorithm
backprop <- function(X, Y, weights) {
  num_layers <- length(weights)+1
  
  # initialize weight_deriv to a list of 0 matrices
  weight_deriv <- list()
  for (l in seq(from<-1, to<-num_layers-1)) {
    # weight_deriv[[l]] should be the same size as weights[[l]]
    rows <- dim(weights[[l]])[1]
    cols <- dim(weights[[l]])[2]
    weight_deriv[[l]] <- matrix(rep(0, rows*cols), nrow<-rows)
  }
  
  # node error
  node_deriv <- list()
  
  # loop over every training example...
  m <- dim(X)[1]
  for (i in seq(1:m)) {
    x <- X[i,]
    y <- Y[i,]
    a <- compute_activations(x, weights)
    # compute node errors at each layer
    node_deriv[[num_layers]] <- a[[num_layers]] - y
    for (l in seq(from<-num_layers-1, to<-1)) {
      node_deriv[[l]] <-  (t(weights[[l]]) %*% node_deriv[[l+1]]) * (a[[l]] * (1 - a[[l]]))
      node_deriv[[l]] <- matrix(node_deriv[[l]][-1,], ncol<-1)
      weight_deriv[[l]] <- weight_deriv[[l]] + node_deriv[[l+1]] %*% t(a[[l]])
    }
  }
  
  gradient <- list()
  for (l in seq(from<-num_layers-1, to<-1)) {
    # apply standard gradient descent update rule
    gradient[[l]] <- (1/m)*weight_deriv[[l]]
    # include regularization component on all non-bias terms
    gradient[[l]][,-1] <- gradient[[l]][,-1] + (regularization_factor/m)*weight_deriv[[l]][,-1]
  }
  return(gradient)
}

# Randomly initializes each of the initial weights of the network
# (this prevents the nodes from learning the same function)
initialize_random <- function(net_structure) {
  num_layers <- length(net_structure)
  weights <- list()
  for (i in seq(from<-1, to<-num_layers-1)) {
    num_rows <- net_structure[i+1] # number of nodes in the next layer
    num_columns <- net_structure[i]+1 # number of nodes in the current layer (+ 1 for the bias term)
    weights[[i]] <- matrix(runif(num_rows*num_columns, min<--.4, max<-.4), nrow<-num_rows)
  }
  return(weights)
}

# Computes a list of vectors, where each vector i contains the activation value for each node in layer i
compute_activations <- function(x, weights) {
  num_layers <- length(weights)+1
  a <- list()
  # input layer activations are just the inputs (+ bias activation of 1)
  a[[1]] <- matrix(c(1,x), ncol<-1)
  for (l in seq(from<-1, to<-num_layers-1)) {
    # computes hidden layer activations (+ bias activation of 1)
    a[[l+1]] <- matrix(c(1, g(a[[l]], weights[[l]])), ncol<-1)
  }
  # output layer has no bias activation, remove it
  a[[num_layers]] <- matrix(a[[num_layers]][-1], ncol<-1)
  return(a)
}

# Computes the feedforward result on feature vector x (with list of weight matrices weights)
feedforward_result <- function(x, weights, a<-NA) {
  num_layers <- length(weights)+1
  if (is.na(a)) {
    a <- compute_activations(x, weights)
  }
  return(a[[num_layers]])
}

# Sigmoid activation function
g <- function(x, params){
  1/(1+exp(-params %*% x))
}

# Predicts the binary classification using the weights for each feature vector in X
predict <- function(X, weights) {
  Y_predicted <- c()
  for (i in seq(1:dim(X)[1])) {
    x <- X[i,]
    y <- feedforward_result(x, weights)
    Y_predicted <- c(Y_predicted, round(y))
  }
  return(Y_predicted)
}

# From a set of predicted binary labels, computes their accuracy from the true labels
compute_accuracy <- function(Y_predicted, Y_actual) {
  1 - (sum(abs(Y_predicted-Y_actual)) / length(Y_predicted))
}



credit_data_df <- read.csv("./Dataset/project-default-credit-card-clients.csv", sep = ";", header = TRUE)



# Splitting data into Testing and Training data set
index <- 1:nrow(credit_data_df)
trainindex <- sample(index, trunc(0.75*nrow(credit_data_df)))
train_data <- na.omit(credit_data_df[trainindex,])
test <- na.omit(credit_data_df[-trainindex,])

names <- names(credit_data_df)
data_mat <- data.matrix(train_data)
X <- matrix(data_mat[,2:(ncol(data_mat)-1)], ncol=23)
Y <- matrix(data_mat[,ncol(data_mat)])

m = dim(X)[1]
n = dim(X)[2]

net_structure <- c(n, 1) # number of nodes in each layer (not including bias node)
learning_rate <- .3 # learning rate for gradient descent
max_epochs = 1000 # maximum number of epochs (passes through the data set)

trained_weights = train(X, Y, net_structure, learning_rate, max_epochs)

Y_predicted = predict(X, trained_weights)
acc = compute_accuracy(Y_predicted, Y)
cat("network test accuracy rate: ", 100*acc, "%", sep="")