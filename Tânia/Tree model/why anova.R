# Regression tree:
#   rpart(formula = default.payment.next.month ~ ., data = train, 
#         method = "anova", control = rpart.control(minsplit = 2, minbucket = 1, 
#                                                   cp = 0.001))
# 
# Variables actually used in tree construction:
#   [1] AGE       BILL_AMT1 EDUCATION LIMIT_BAL PAY_0     PAY_2     PAY_3     PAY_4     PAY_5     PAY_6    
# [11] PAY_AMT2  PAY_AMT3 
# 
# Root node error: 3882.7/22499 = 0.17257
Percent errors: 18.290000
Percent errors: 17.450000
Percent errors: 18.160000
Percent errors: 17.960000

# 
# n= 22499 
# Variable importance
# PAY_0     PAY_2     PAY_4     PAY_5  PAY_AMT3 BILL_AMT4     PAY_3     PAY_6 BILL_AMT3 LIMIT_BAL 
# 54        12         5         4         4         3         3         3         2         2 
# BILL_AMT5 BILL_AMT1  PAY_AMT1 BILL_AMT6 BILL_AMT2  PAY_AMT2       AGE 
# 2         2         1         1         1         1         1 





# Classification tree:
#   rpart(formula = default.payment.next.month ~ ., data = train, 
#         method = "class", control = rpart.control(minsplit = 2, minbucket = 1, 
#                                                   cp = 0.001))
# 
# Variables actually used in tree construction:
#   [1] AGE       BILL_AMT1 BILL_AMT2 BILL_AMT6 EDUCATION ID        LIMIT_BAL PAY_0     PAY_2     PAY_5    
# [11] PAY_6     PAY_AMT1  PAY_AMT2  PAY_AMT3  PAY_AMT6 
# 
# Root node error: 5033/22499 = 0.2237

# 
# n= 22499 