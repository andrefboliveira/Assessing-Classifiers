library("neuralnet")

rm(list=ls())

# Organizing data
credit_data_df <- read.csv("/Users/taniamaldonado/Desktop/Bioinformática/1º Semestre/Aprendizagem Automática/Assessing-Classifiers/project-default-credit-card-clients.csv", sep = ";", header = TRUE)
credit_data_df = credit_data_df[-2,]

## Checking to see if there is any missing data
sapply(credit_data_df, function(x) sum(is.na(x)))

## Splitting data into Testing and Training data set
index <- sample(1:nrow(credit_data_df),round(0.75*nrow(credit_data_df)))
train <- credit_data_df[index,]
test <- credit_data_df[-index,]

## Fitting a linear regression model and testing it on the test set   //se não necessário, apagar
lm.fit <- glm("default.payment.next.month~.", data=train)
summary(lm.fit)

# Call:
#   glm(formula = "default.payment.next.month~.", data = train)
# 
# Deviance Residuals: 
#   Min           1Q       Median           3Q          Max  
# -1.29018935  -0.24302830  -0.16460963   0.03427044   1.24809190  
# 
# Coefficients:
#   Estimate        Std. Error  t value               Pr(>|t|)    
# (Intercept)  0.32505850300800  0.02123985437007 15.30418 < 0.000000000000000222 ***
#   ID           0.00000025955519  0.00000030314012  0.85622              0.3918842    
# LIMIT_BAL   -0.00000010599601  0.00000002511165 -4.22099          0.00002441871 ***
#   SEX         -0.01461245341338  0.00538786302412 -2.71211              0.0066908 ** 
#   EDUCATION   -0.01579694082241  0.00350094110526 -4.51220          0.00000644842 ***
#   MARRIAGE    -0.02765653520027  0.00554160822868 -4.99071          0.00000060608 ***
#   AGE          0.00129584988466  0.00031861239556  4.06717          0.00004775031 ***
#   PAY_0        0.09490626429674  0.00321198215308 29.54757 < 0.000000000000000222 ***
#   PAY_2        0.02020935532191  0.00388920443790  5.19627          0.00000020510 ***
#   PAY_3        0.01153849002228  0.00416046748710  2.77336              0.0055526 ** 
#   PAY_4        0.00031365616194  0.00465423441672  0.06739              0.9462706    
# PAY_5        0.00860660478863  0.00500622827485  1.71918              0.0855955 .  
# PAY_6        0.00004879513370  0.00410100004361  0.01190              0.9905068    
# BILL_AMT1   -0.00000062286224  0.00000012972326 -4.80147          0.00000158519 ***
#   BILL_AMT2    0.00000015207324  0.00000018846198  0.80692              0.4197226    
# BILL_AMT3    0.00000005787913  0.00000017631312  0.32827              0.7427071    
# BILL_AMT4   -0.00000004911381  0.00000018327536 -0.26798              0.7887185    
# BILL_AMT5   -0.00000004312167  0.00000020975192 -0.20558              0.8371176    
# BILL_AMT6    0.00000012029604  0.00000016605650  0.72443              0.4688103    
# PAY_AMT1    -0.00000078368860  0.00000020126693 -3.89378          0.00009898049 ***
#   PAY_AMT2    -0.00000022282571  0.00000018191228 -1.22491              0.2206229    
# PAY_AMT3    -0.00000014237542  0.00000020284459 -0.70189              0.4827525    
# PAY_AMT4    -0.00000015346685  0.00000021094222 -0.72753              0.4669088    
# PAY_AMT5    -0.00000024990222  0.00000021326771 -1.17178              0.2412990    
# PAY_AMT6    -0.00000002015684  0.00000016319506 -0.12351              0.9017014    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.152586804)
# 
# Null deviance: 3908.7812  on 22498  degrees of freedom
# Residual deviance: 3429.2358  on 22474  degrees of freedom
# AIC: 21577.775
# 
# Number of Fisher Scoring iterations: 2

## Train MLP with neuralnet:
names <- names(train)
f <- as.formula(paste("default.payment.next.month ~", paste(names[!names %in% "default.payment.next.month"], collapse = " + ")))


nn <- neuralnet(f, data=train, hidden=c(5,3), act.fct="logistic", linear.output=FALSE, threshold=0.1) #tentar 1 hidden layer
print(nn)
plot(nn)

## teste 1 - sem fit ao modelo de regressão linear
# error                                   1902.500646642921
# reached.threshold                          0.093219754577
# steps                                    782.000000000000

## teste 2 - com o fit ao modelo de regressão linear
# error                                   1952.454224506203
# reached.threshold                          0.097452007289
# steps                                    589.000000000000

## Calculating the mean squared error (MSE) as a measure of 
## how much our predictions are far away from the real data.      //se não necessário, apagar
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$default.payment.next.month)^2)/nrow(test)

pr.nn <- compute(nn,test[,1:25])
  ##Error in neurons[[i]] %*% weights[[i]] : non-conformable arguments
test.r <- (test$"default.payment.next.month")*(max(data$"default.payment.next.month")-min(data$"default.payment.next.month"))+min(data$"default.payment.next.month")
MSE.nn <- sum((test.r - pr.nn)^2)/nrow(test)
