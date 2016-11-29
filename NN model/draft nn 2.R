library("neuralnet")
library("MASS")

rm(list=ls())

# Organizing data
credit_data_df <- read.csv("/Users/taniamaldonado/Desktop/Bioinformática/1º Semestre/Aprendizagem Automática/Assessing-Classifiers/project-default-credit-card-clients.csv", sep = ";", header = TRUE)
credit_data_df = credit_data_df[-2,]

## Tidying the data - optional
# names(credit_data_df)[names(credit_data_df) == "PAY_0"] <- "PAY_1" 
# names(credit_data_df)[names(credit_data_df) == "default payment next month"] <- "default_payment_next_month" 

# for(i in 1:nrow(credit_data_df)){   
#   if(credit_data_df$default.payment.next.month[i] == 1){
#     credit_data_df$default[i] <- "yes"
#   }
#   if(credit_data_df$default.payment.next.month[i] == 0){
#     credit_data_df$default[i] <- "no"
#   }
# }

## Checking to see if there is any missing data
sapply(credit_data_df, function(x) sum(is.na(x)))

# ## Removing missing data  //NÃO FUNCIONA
# credit_data_df %<>% remove_missing()

# We proceed by randomly splitting the data into a train and a test set, 
# then we fit a linear regression model and test it on the test set. 
# Note that I am using the gml() function instead of the lm(); 
# this will become useful later when cross validating the linear model.

## Splitting data into Testing and Training data set
index <- sample(1:nrow(credit_data_df),round(0.75*nrow(credit_data_df)))
train <- credit_data_df[index,]
test <- credit_data_df[-index,]


# The sample(x,size) function simply outputs a vector of the specified size 
# of randomly selected samples from the vector x. By default the sampling 
# is without replacement: index is essentially a random vector of indeces.
# Since we are dealing with a regression problem, we are going to use the 
# mean squared error (MSE) as a measure of how much our predictions are far 
# away from the real data. 


## Fitting a linear regression model and testing it on the test set
lm.fit <- glm("default.payment.next.month~.", data=train)
summary(lm.fit)
# Call:
#   glm(formula = "default.payment.next.month~.", data = train)
#
# Deviance Residuals:
#   Min           1Q       Median           3Q          Max
# -1.29323731  -0.23880270  -0.15874626   0.03366375   1.28701657
#
# Coefficients:
#   Estimate        Std. Error  t value               Pr(>|t|)
# (Intercept)  0.33896357377198  0.02099179464000 16.14743 < 0.000000000000000222 ***
#   ID           0.00000006196450  0.00000029995658  0.20658              0.8363411
# LIMIT_BAL   -0.00000009920818  0.00000002472465 -4.01252         0.000060268325 ***
#   SEX         -0.01536663852757  0.00532816442420 -2.88404              0.0039298 **
#   EDUCATION   -0.01577764115538  0.00345635672805 -4.56482         0.000005025745 ***
#   MARRIAGE    -0.03029587640913  0.00546863957006 -5.53993         0.000000030598 ***
#   AGE          0.00102594988371  0.00031686923116  3.23777              0.0012064 **
#   PAY_0        0.09704174243989  0.00319113986347 30.40974 < 0.000000000000000222 ***
#   PAY_2        0.01980757724679  0.00382307264066  5.18106         0.000000222531 ***
#   PAY_3        0.01115494559911  0.00410023553861  2.72056              0.0065221 **
#   PAY_4        0.00209132652906  0.00455564253190  0.45906              0.6461934
# PAY_5        0.00636691674536  0.00492731399239  1.29217              0.1963123
# PAY_6        0.00034705569287  0.00403076100592  0.08610              0.9313863
# BILL_AMT1   -0.00000065021452  0.00000013164179 -4.93927         0.000000789776 ***
#   BILL_AMT2    0.00000015261382  0.00000018705741  0.81587              0.4145853
# BILL_AMT3   -0.00000001665210  0.00000017623720 -0.09449              0.9247232
# BILL_AMT4   -0.00000007412722  0.00000019483098 -0.38047              0.7036006
# BILL_AMT5   -0.00000009886413  0.00000021764461 -0.45425              0.6496564
# BILL_AMT6    0.00000029484700  0.00000016514784  1.78535              0.0742177 .
# PAY_AMT1    -0.00000067027788  0.00000020612810 -3.25175              0.0011486 **
#   PAY_AMT2    -0.00000006346829  0.00000016542935 -0.38366              0.7012356
# PAY_AMT3    -0.00000011984601  0.00000020471439 -0.58543              0.5582643
# PAY_AMT4    -0.00000042592024  0.00000021637128 -1.96847              0.0490263 *
#   PAY_AMT5    -0.00000043193750  0.00000021879378 -1.97418              0.0483739 *
#   PAY_AMT6     0.00000001988349  0.00000015594001  0.12751              0.8985400
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 0.1493151072)
#
# Null deviance: 3844.6748  on 22498  degrees of freedom
# Residual deviance: 3355.7077  on 22474  degrees of freedom
# AIC: 21090.115
#
# Number of Fisher Scoring iterations: 2

## Calculating the mean squared error (MSE) as a measure of 
## how much our predictions are far away from the real data.
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$"default.payment.next.month")^2)/nrow(test)

## Preparing to fit the neural network
## Normalizing the data before the training //NÃO FUNCIONA
# maxs <- apply(data, 2, max) 
# mins <- apply(data, 2, min)
# 
# scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
# 
# train_ <- scaled[index,]
# test_ <- scaled[-index,]

## Train MLP
n <- names(train)
f <- as.formula(paste("default.payment.next.month~", paste(n[!n %in% "default.payment.next.month"], collapse = " + ")))
nn <- neuralnet(f, data=train, hidden=c(5,3), linear.output=TRUE)

# print(nn)
# $result.matrix
# 1
# error                                    1940.573326639029
# reached.threshold                           0.009188154952
# steps                                   18405.000000000000

plot(nn)

## About the plot
# The black lines show the connections between each layer and the weights on each connection 
# while the blue lines show the bias term added in each step. The bias can be thought as 
# the intercept of a linear model. The net is essentially a black box so we cannot say that 
# much about the fitting, the weights and the model. Suffice to say that the training algorithm 
# has converged and therefore the model is ready to be used.


# ## Train MLP with neuralnet:
# creditnet <- neuralnet(paste("Y ~", paste(names[!names %in% c("Y", "X")], collapse=" + ")), train_credit_data_df, hidden = 4, lifesign = "minimal", 
#                        linear.output = FALSE, threshold = 0.1)
# 
# ## plot the NN
# plot(creditnet, rep = "best")









