## set a 9-month lookback period and a 3-month holdout period ##
    
# lookback #
    
churn_study_period <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE >= as.Date(min(TIC_DATE),
                            format = "%d/%m/%Y"),
         TIC_DATE < as.Date(min(df_7_tic_clean_final$TIC_DATE) %m+% months(9),
                            format = "%d/%m/%Y"))
# holdout #

churn_holdout <- df_7_tic_clean_final %>%
  filter(DIREZIONE == 1,
         TIC_DATE >= as.Date(min(df_7_tic_clean_final$TIC_DATE) %m+% months(9),
                            format = "%d/%m/%Y"),
         TIC_DATE < as.Date(max(TIC_DATE),
                            format = "%d/%m/%Y"))

## identify non-churners (clients who made at least a purchase in the holdout period) ##

no_churner <- unique(churn_holdout$ID_CLI)


## identify latest purchase date per customer ##

churn_recency <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(LAST_PURCHASE_DATE = max(TIC_DATE))

## create variables recency, frequency and monetary (inspired by RFM) to use in the models ##

churn_recency$RECENCY <- as.integer( difftime(as.Date(min(churn_study_period$TIC_DATE),
                                                      format = "%d/%m/%Y"),         
                                              churn_recency$LAST_PURCHASE_DATE,
                                              units = "days") )

churn_frequency <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(TOT_PURCHASE = n()) %>%
  arrange(desc(TOT_PURCHASE))

churn_monetary <- churn_study_period %>%
  filter(DIREZIONE == 1) %>%
  group_by(ID_CLI) %>%
  summarise(IMPORTO_LORDO = sum(IMPORTO_LORDO),
            SCONTO = sum(SCONTO),
            SPESA = IMPORTO_LORDO - SCONTO) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(desc(IMPORTO_LORDO))

churn <- merge(churn_recency, churn_frequency, by = "ID_CLI")
churn <- merge(churn, churn_monetary, by = "ID_CLI") %>%
  select(ID_CLI,
         RECENCY,
         SPESA, 
         TOT_PURCHASE)

## creation of the churn boolean variable: 1 = churner and 0 = non-churner

churn$CHURN <- 1

for (i in c(1:nrow(churn)) ) {
  
  if (churn$ID_CLI[i] %in% no_churner) churn$CHURN[i] <- 0

  }

churn$CHURN <- as.factor(churn$CHURN)

table(churn$CHURN)

## variable pool choice ##

churn <- left_join(churn, df_2_cli_account_clean[, c("ID_CLI", "TYP_JOB")], by = "ID_CLI")

churn <- left_join(churn, df_1_cli_fid_clean[, c("ID_CLI", "LAST_COD_FID")], by = "ID_CLI")

churn <- left_join(churn, df_2_cli_account_clean[, c("ID_CLI", "W_PHONE")], by = "ID_CLI") 

churn <- left_join(churn, df_2_cli_account_clean[, c("ID_CLI", "TYP_CLI_ACCOUNT")], by = "ID_CLI")

region <- left_join(df_2_cli_account_clean[, c("ID_CLI", "ID_ADDRESS")],
                    df_3_cli_address_clean[, c("ID_ADDRESS", "REGION")], by = "ID_ADDRESS") 


churn <- left_join(churn, df_4_cli_privacy_clean[, c("ID_CLI", "FLAG_PRIVACY_1")], by = "ID_CLI") 
churn <- left_join(churn, df_4_cli_privacy_clean[, c("ID_CLI", "FLAG_PRIVACY_2")], by = "ID_CLI")
churn <- left_join(churn, df_4_cli_privacy_clean[, c("ID_CLI", "FLAG_DIRECT_MKT")], by = "ID_CLI")

## creation of a new variable as the ratio of refunds over total purchases ##

refund_frequency <- df_7_tic_clean_final %>%
  filter(DIREZIONE == - 1,
         TIC_DATE >= as.Date(min(TIC_DATE),
                             format = "%d/%m/%Y"),
         TIC_DATE < as.Date(min(df_7_tic_clean_final$TIC_DATE) %m+% months(9),
                            format = "%d/%m/%Y")) %>%
  group_by(ID_CLI) %>%
  summarise(TOT_REFUND = n() ) %>%
  arrange(desc(TOT_REFUND)) %>%
  select(ID_CLI, TOT_REFUND)

churn <- left_join(churn, refund_frequency[, c("ID_CLI", "TOT_REFUND")], by = "ID_CLI")

churn= churn %>%
  mutate( TOT_REFUND = ifelse( is.na(TOT_REFUND),0 , TOT_REFUND) )

churn = churn %>%
  mutate( refund_ratio = TOT_REFUND / (TOT_REFUND+TOT_PURCHASE) )

churn <- left_join(churn, region, by = "ID_CLI")

head(churn)

## train-test split (across both the lookback and holdout datasets) ##

train_index <- createDataPartition(churn$CHURN, 
                                   p = .70, 
                                   list = FALSE, 
                                   times = 1)
train <- churn[train_index,]
test <- churn[-train_index,]
table(train$CHURN)

#!!! NOTE: class imbalance is visible but not extreme: churners are twice the non-churners !!!#
# since churn is the class we are aiming to predict, the fact we have more churners is favorable for the analysis #


###################################
#### EXPLORATIVE DATA ANALYSIS ####
###################################

## CORRELATIONS ##

##########################################
### T-TEST: numeric vs binary (target) ###
##########################################

print(t.test(RECENCY ~ CHURN,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# RECENCY is significantly correlated with the churn response variable #

print(t.test(SPESA ~ CHURN,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# SPESA is significantly correlated with the churn response variable #

print(t.test(TOT_PURCHASE ~ CHURN,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# TOT_PURCHASE is significantly correlated with the churn response variable #

print(t.test(TOT_REFUND ~ CHURN,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# TOT_REFUND is significantly correlated with the churn response variable #

print(t.test(refund_ratio ~ CHURN,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# refund_ratio is significantly correlated with the churn response variable #

###############################################################
### CHI-SQUARED TEST: binary/categorical vs binary (target) ###
###############################################################

print(chisq.test( table(train$TYP_JOB, train$CHURN), simulate.p.value = FALSE ) $p.value < 0.05) # Chi-squared approximation may be incorrect though

# TYP_JOB is most likely NOT significantly correlated with the churn response variable #

print(chisq.test( table(train$LAST_COD_FID, train$CHURN), simulate.p.value = FALSE ) $p.value < 0.05)

# LAST_COD_FID is significantly correlated with the churn response variable #

print(chisq.test( table(train$W_PHONE, train$CHURN), simulate.p.value = FALSE ) $p.value < 0.05)

# W_PHONE is significantly correlated with the churn response variable #

print(chisq.test( table(train$TYP_CLI_ACCOUNT, train$CHURN), simulate.p.value = FALSE ) $p.value < 0.05)

# TYP_CLI_ACCOUNT is significantly correlated with the churn response variable #

print(chisq.test( table(train$FLAG_PRIVACY_1, train$CHURN), simulate.p.value = FALSE ) $p.value < 0.05)

# FLAG_PRIVACY_1 is significantly correlated with the churn response variable #

print(chisq.test( table(train$FLAG_PRIVACY_2, train$CHURN), simulate.p.value = FALSE ) $p.value < 0.05)

# FLAG_PRIVACY_2 is NOT significantly correlated with the churn response variable #

print(chisq.test( table(train$FLAG_DIRECT_MKT, train$CHURN), simulate.p.value = FALSE ) $p.value < 0.05)

# FLAG_DIRECT_MKT is significantly correlated with the churn response variable #

print(chisq.test( table(train$ID_ADDRESS, train$CHURN), simulate.p.value = FALSE ) $p.value < 0.05) # Chi-squared approximation may be incorrect though

# ID_ADDRESS is most likely NOT significantly correlated with the churn response variable #

print(chisq.test( table(train$REGION, train$CHURN), simulate.p.value = FALSE ) $p.value < 0.05)

# REGION is significantly correlated with the churn response variable #


########################################################################################
### REMOVAL OF COLUMNS NOT SIGNIFICANTLY CORRELATED WITH THE CHURN RESPONSE VARIABLE ###
########################################################################################

train = train[,-which(colnames(train) %in% c("TYP_JOB", "FLAG_PRIVACY_2", "ID_ADDRESS"))]

################################################
### IDENTIFY AND REMOVE COLLINEAR COVARIATES ###
################################################

##################################################################
### PEARSON CORRELATION: numeric vs numeric (among covariates) ###
##################################################################

corrplot(cor(train[c('RECENCY','SPESA','TOT_PURCHASE','TOT_REFUND','refund_ratio')]) ,diag = FALSE)

#!!! NOTE: SPESA and TOT_PURCHASE look correlated, let's check the exact value: !!!#

cor(train[c('SPESA','TOT_PURCHASE')])

# a correlation of 0.45 is not particularly strong, but it's better to remove TOT_PURCHASE and TOT_REFUND: #
# both of them were used to calculate the refund_ratio, so they end up being somewhat redundant information # 

train=train[,-which(colnames(train) %in% c("TOT_PURCHASE", "TOT_REFUND") )]

####################################################
### T-TEST: numeric vs binary (among covariates) ###
####################################################

print(t.test(RECENCY ~ W_PHONE,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# RECENCY is significantly correlated with the W_PHONE variable #

print(t.test(RECENCY ~ FLAG_PRIVACY_1,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# RECENCY is significantly correlated with the FLAG_PRIVACY_1 variable #

print(t.test(RECENCY ~ FLAG_DIRECT_MKT,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# RECENCY is significantly correlated with the FLAG_DIRECT_MKT variable #


print(t.test(SPESA ~ W_PHONE,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# SPESA is significantly correlated with the W_PHONE variable #

print(t.test(SPESA ~ FLAG_PRIVACY_1,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# SPESA is NOT significantly correlated with the FLAG_PRIVACY_1 variable #

print(t.test(SPESA ~ FLAG_DIRECT_MKT,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# SPESA is NOT significantly correlated with the FLAG_DIRECT_MKT variable #


print(t.test(refund_ratio ~ W_PHONE,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# refund_ratio is significantly correlated with the W_PHONE variable #

print(t.test(refund_ratio ~ FLAG_PRIVACY_1,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# refund_ratio is NOT significantly correlated with the FLAG_PRIVACY_1 variable #

print(t.test(refund_ratio ~ FLAG_DIRECT_MKT,
             data=train,
             var.equal = FALSE,
             alternative = "two.sided")$p.value < 0.05)

# refund_ratio is NOT significantly correlated with the FLAG_DIRECT_MKT variable #

#####################################################################################
### CHI-SQUARED TEST: binary/categorical vs binary/categorical (among covariates) ###
#####################################################################################

# to avoid making a bruteforce combination of chi-squared tests, let's test only W_PHONE: #
# this is the only covariate that resulted correlated with all the numeric covariates #

print(chisq.test( table(train$W_PHONE, train$LAST_COD_FID), simulate.p.value = FALSE ) $p.value < 0.05)
print(chisq.test( table(train$W_PHONE, train$TYP_CLI_ACCOUNT), simulate.p.value = FALSE ) $p.value < 0.05)
print(chisq.test( table(train$W_PHONE, train$FLAG_PRIVACY_1), simulate.p.value = FALSE ) $p.value < 0.05)
print(chisq.test( table(train$W_PHONE, train$FLAG_DIRECT_MKT), simulate.p.value = FALSE ) $p.value < 0.05)
print(chisq.test( table(train$W_PHONE, train$REGION), simulate.p.value = FALSE ) $p.value < 0.05)

# W_PHONE is also significantly correlated to all the binary/categorical covariates except FLAG_PRIVACY_1 #

# remove the variable W_PHONE #

train = train[,-which(colnames(train) %in% c("W_PHONE"))]

# final check to see if there are null/NA values left

anyNA(train) #TRUE

# the NA values are all in the variable REGION 

nrow(train[is.na(train$REGION),]) / nrow(train) # 4.3% of total rows

# to run some models with REGION we might need to sacrifice 4.3% of total rows due to NA presence #
# we can test whether this will be a profitable trade-off or not #


test = test[,which(colnames(test) %in% colnames(train))] # to match train columns


#################################
#### CHURN PREDICTION MODELS ####
#################################

## 1) Regression Trees

set.seed(1)

tree <- rpart(CHURN ~ RECENCY+SPESA+LAST_COD_FID+TYP_CLI_ACCOUNT+FLAG_PRIVACY_1+FLAG_DIRECT_MKT+refund_ratio+REGION,
              data = train)
rpart.plot(tree)

summary(tree) 

# the tree is actually using just RECENCY and SPESA

## 2) Random Forest

set.seed(1)

tree_rf <- randomForest(CHURN ~ RECENCY+SPESA+LAST_COD_FID+TYP_CLI_ACCOUNT+FLAG_PRIVACY_1+FLAG_DIRECT_MKT+refund_ratio,
                        data = train, ntree = 300)
print(tree_rf)

plot(tree_rf) # error descent is stuck

## 3) Logistic Regression

set.seed(1)

logistic <- train(CHURN ~ RECENCY+SPESA+LAST_COD_FID+TYP_CLI_ACCOUNT+FLAG_PRIVACY_1+FLAG_DIRECT_MKT+refund_ratio,
                  data = train,
                  method = "glm")
summary(logistic)

logistic

# only RECENCY, SPESA, LAST_COD_FID (STANDARD) and FLAG_DIRECT_MKT look statistically significant #
# (with 0.05 confidence and regarding specifically the logistic model) #

logistic_train = train[,which(colnames(train) %in% c("ID_CLI", "CHURN", "RECENCY", "SPESA", "FLAG_DIRECT_MKT", "LAST_COD_FID"))]

# re-encode LAST_COD_FID as binary with STANDARD yes/no #

logistic_train = logistic_train %>% mutate(LAST_COD_FID = ifelse(logistic_train$LAST_COD_FID == "STANDARD", 1, 0))

# retry training #

set.seed(1)

logistic_2 <- train(CHURN ~ 0+RECENCY+SPESA+FLAG_DIRECT_MKT+LAST_COD_FID, #without the intercept which is not significant
                    data = logistic_train,
                    method = "glm")
summary(logistic_2)

logistic_2

# retry with just SPESA and RECENCY, as suggested by the trees #

set.seed(1)

logistic_3 <- train(CHURN ~ 0+SPESA+RECENCY,
                    data = train,
                    method = "glm")
summary(logistic_3)

logistic_3

## 4) Lasso

set.seed(1)

lasso <- train(CHURN ~ RECENCY+SPESA+LAST_COD_FID+TYP_CLI_ACCOUNT+FLAG_PRIVACY_1+FLAG_DIRECT_MKT+refund_ratio,
            data = train,
            method = "glmnet",
            family = "binomial")
lasso

plot(lasso)

# retry with same predictor as logistic_2 #

set.seed(1)

lasso_2 <- train(CHURN ~ 0+RECENCY+SPESA+FLAG_DIRECT_MKT+LAST_COD_FID,
               data = logistic_train,
               method = "glmnet",
               family = "binomial")
lasso_2

plot(lasso_2)

# retry with same predictor as logistic_3 #

set.seed(1)

lasso_3 <- train(CHURN ~ 0+SPESA+RECENCY,
                 data = train,
                 method = "glmnet",
                 family = "binomial")
lasso_3

plot(lasso_3)


#######################################
#### PERFORMANCE OVER TRAINING SET ####
#######################################

## Prediction of the variable churn with the different models

pred_tree <- predict(tree, train[,-which(colnames(train) %in% c("CHURN"))], type = "class")
pred_tree_conf = confusionMatrix(unlist(pred_tree), train$CHURN)

pred_rf <- predict(tree_rf, train[,-which(colnames(train) %in% c("CHURN"))], type = "class")
pred_rf_conf = confusionMatrix(pred_rf, train$CHURN)

pred_logistic <- predict(logistic, train[,-which(colnames(train) %in% c("CHURN"))], type = "raw")
pred_logistic_conf = confusionMatrix(pred_logistic, train$CHURN)

pred_logistic_2 <- predict(logistic_2, logistic_train[,-which(colnames(logistic_train) %in% c("CHURN"))], type = "raw")
pred_logistic_2_conf = confusionMatrix(pred_logistic_2, logistic_train$CHURN)

pred_logistic_3 <- predict(logistic_3, train[,-which(colnames(train) %in% c("CHURN"))], type = "raw")
pred_logistic_3_conf = confusionMatrix(pred_logistic_3, train$CHURN)

pred_lasso <- predict(lasso, train[,-which(colnames(train) %in% c("CHURN"))], type = "raw")
pred_lasso_conf = confusionMatrix(pred_lasso, train$CHURN)

pred_lasso_2 <- predict(lasso_2, logistic_train[,-which(colnames(logistic_train) %in% c("CHURN"))], type = "raw")
pred_lasso_2_conf = confusionMatrix(pred_lasso_2, logistic_train$CHURN)

pred_lasso_3 <- predict(lasso_3, train[,-which(colnames(train) %in% c("CHURN"))], type = "raw")
pred_lasso_3_conf = confusionMatrix(pred_lasso_3, train$CHURN)

## show all confusion matrix in the same dataframe accuracy ##
accuracy <- as.data.frame(t(cbind(
  
  pred_tree_conf$overall[1],
  pred_rf_conf$overall[1],
  pred_logistic_conf$overall[1],
  pred_logistic_2_conf$overall[1],
  pred_logistic_3_conf$overall[1],
  pred_lasso_conf$overall[1],
  pred_lasso_2_conf$overall[1],
  pred_lasso_3_conf$overall[1]
                                )))

accuracy <- as.data.frame(cbind(c("Tree","Random Forest","Logistic_1","Logistic_2","Logistic_3","Lasso_1","Lasso_2","Lasso_3"),
                                accuracy))

## rename the columns of the accuracy dataframe ##
colnames(accuracy) <- c("Models", "Accuracy")

## sort by performance ##

accuracy %>% arrange(desc(Accuracy))

# let's pick the best version of each model (when multiple versions are present)

accuracy = accuracy %>% filter(Models %in% c("Random Forest", "Lasso_1", "Tree", "Logistic_3"))


###################################
#### PERFORMANCE OVER TEST SET ####
###################################

## evaluation

pred_final_tree <- predict(tree, test[,-which(colnames(test) %in% c("CHURN"))], type = "class")
pred_final_rf <- predict(tree_rf, test[,-which(colnames(test) %in% c("CHURN"))], type = "class")
pred_final_logistic <- predict(logistic_3, test[,-which(colnames(test) %in% c("CHURN"))], type = "raw")
pred_final_lasso <- predict(lasso, test[,-which(colnames(test) %in% c("CHURN"))], type = "raw")

test_accuracy = as.data.frame(t(cbind(
  
pred_final_tree_acc = confusionMatrix(unlist(pred_final_tree), test$CHURN)$overall[1],
pred_final_rf_acc = confusionMatrix(pred_final_rf, test$CHURN)$overall[1],
pred_final_logistic_acc = confusionMatrix(pred_final_logistic, test$CHURN)$overall[1],
pred_final_lasso_acc = confusionMatrix(pred_final_lasso, test$CHURN)$overall[1]

)))

test_accuracy <- as.data.frame(cbind(c("Tree","Random Forest","Logistic", "Lasso"),
                                     test_accuracy))

colnames(test_accuracy) <- c("Models", "Accuracy")

test_accuracy %>% arrange(desc(Accuracy))

#!!! NOTE: the top performer is the random forest with 73.19% accuracy, 0.65% better than the second one (logistic) !!!#
# covariates were RECENCY+SPESA+LAST_COD_FID+TYP_CLI_ACCOUNT+FLAG_PRIVACY_1+FLAG_DIRECT_MKT+refund_ratio #
# with a number of trees equal to 300 #

accuracy_plot <- (ggplot(data = test_accuracy,
                         aes(x = Models,
                             y = Accuracy,
                             fill = Models)) +
                    geom_bar(stat = "identity") +
                    coord_cartesian(ylim = c(0.70, 0.75)) +
                    theme_minimal() +
                    guides(fill = FALSE) +
                    labs(title = "Accuracy",
                         x = "Modelli",
                         y = " ") +
                    theme(plot.title = element_text(size=15,hjust = 0.5),
                          axis.text.x = element_text(size=10),
                          axis.text.y = element_text(size=10, hjust=1),
                          legend.text = element_text(size=10),
                          legend.title = element_text(size=10))
)

accuracy_plot

###################################
#### CUMULATIVE GAINS AND LIFT ####
###################################

perf_tree = predict(tree, test[,-which(colnames(train) %in% c("CHURN"))], "prob")[,1]
perf_rf = predict(tree_rf, test[,-which(colnames(train) %in% c("CHURN"))], "prob")[,1]
perf_log = predict(logistic_3, test[,-which(colnames(train) %in% c("CHURN"))], "prob")[,1]
perf_lasso = predict(lasso, test[,-which(colnames(train) %in% c("CHURN"))], "prob")[,1]

perf_tree = predict(tree, test[,-which(colnames(train) %in% c("CHURN"))], "prob")[,1]
perf_rf = predict(tree_rf, test[,-which(colnames(train) %in% c("CHURN"))], "prob")[,1]
perf_log = predict(logistic_3, test[,-which(colnames(train) %in% c("CHURN"))], "prob")[,1]
perf_lasso = predict(lasso, test[,-which(colnames(train) %in% c("CHURN"))], "prob")[,1]

## show all evaluations in the same dataframe data_class

data_class = as.data.frame(cbind(perf_tree, perf_rf, perf_log, perf_lasso))
data_class = cbind(data_class, test$CHURN)
colnames(data_class) <- c("perf_tree", "perf_rf", "perf_log", "perf_lasso", "churn")

head(data_class)

## create lift curve for every models

lift_tree = gain_lift(data = data_class, score = 'perf_tree', target = 'churn')
lift_rf = gain_lift(data = data_class, score = 'perf_rf', target = 'churn')
lift_log = gain_lift(data = data_class, score = 'perf_log', target = 'churn')
lift_lasso = gain_lift(data = data_class, score = 'perf_lasso', target = 'churn')