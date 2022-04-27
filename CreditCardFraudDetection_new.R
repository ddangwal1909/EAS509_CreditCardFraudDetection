#########################################################################
############################### TO DO ###################################
#########################################################################





############################################################################
################ SDM 2 Project #############################################
################ Credit Card Fraud Detection ###############################
############################################################################



#########################LIBRARIES IMPORT ##################################
library(dplyr)
library(stringr)
library(ggplot2)
library(mmtable)
options(dplyr.print_max = 1e9)

##################### VARIABLES DECLARED ###################################

transactions_data_location = "C:\\
Users\\HP\\Desktop\\MS-SUNy Buffalo\\Sem2\\SDM2\\ProjectWork\\transactions.csv"

users_data_location = "C:\\
Users\\HP\\Desktop\\MS-SUNy Buffalo\\Sem2\\SDM2\\ProjectWork\\users_data.csv"

card_data_location = "C:\\
Users\\HP\\Desktop\\MS-SUNy Buffalo\\Sem2\\SDM2\\ProjectWork\\card_data.csv"



##################### LOAD DATASETS ########################################

cards_data = read.csv(file.choose())
View(cards_data)
colnames(cards_data)[2] = 'Card'


state_mapping = read.csv(file.choose())
View(state_mapping)


users_data = read.csv(file.choose())
users_data = users_data %>% mutate(User = row_number()-1)
users_data$Current.Age = as.integer(users_data$Current.Age)
users_data = left_join(users_data,state_mapping, by=c("State"="Code"))
colnames(users_data)[20] = "State_Fullname"

View(users_data)

transactions_data = read.csv(file.choose())
#View(transactions_data)
transactions_data$Amountnew = as.double(substring(transactions_data$Amount,2,))



## getting only spent entries and refunds are ignored
transactions_data = filter(transactions_data,Year>=2015 & Amountnew>0)

transactions_data %>% group_by(Is.Fraud.) %>% tally()

## backup of transactions_data
transactions_bkp <- transactions_data


### treating outliers
Q <- quantile(transactions_data$Amountnew, probs=c(.25, .75), na.rm = FALSE)
iqr = IQR(transactions_data$Amountnew)
Amount_lower = Q[1]-(1.5*iqr)
Amount_upper = Q[2]+(1.5*iqr)
transactions_data = filter(transactions_data,Amountnew>=Amount_lower & Amountnew<=Amount_upper)

transactions_data %>% group_by(Is.Fraud.) %>% tally()



###### moving average of transaction amount

library(tidyverse)
library(zoo)

transactions_data = transactions_data %>%
  group_by(User) %>%
  mutate(dummy_col=1) %>%
  mutate(lag_amountnew = dplyr::lag(Amountnew, n = 1, default = 0),
         max_trans_amt_until_now = cummax(lag_amountnew),
         min_trans_amt_until_now = cummin(lag_amountnew),
         user_trans_num = cumsum(dummy_col),
         running_avg = cumsum(lag_amountnew)/user_trans_num,
         change_prev = (Amountnew-lag_amountnew)/lag_amountnew,
         above_moving_avg = ifelse(Amountnew > running_avg, 1, 0),
         above_max = ifelse(Amountnew > max_trans_amt_until_now, 1, 0),
         change_prev_100perc_flag = ifelse(change_prev > 1 | change_prev < -1, 1, 0),
         change_prev_200perc_flag = ifelse(change_prev > 2 | change_prev < -2, 1, 0)
         )


transactions_data = transactions_data %>%
  group_by(Year,Month,Day,User) %>%
  mutate(dummy_col_velocity=1) %>%
  mutate(num_transactions = cumsum(dummy_col_velocity),
         lag_merchant = dplyr::lag(MCC, n = 1, default = 0),
         same_merchant_transact = ifelse(lag_merchant==MCC,1,0)
         )
  

#View(transactions_data[transactions_data$User==2,])


summary(transactions_data)
transactions_data = filter(transactions_data,change_prev!=Inf)

transactions_data_frauds = filter(transactions_data,Is.Fraud.=='Yes')
transactions_data_nofrauds = filter(transactions_data,Is.Fraud.=='No')

summary(transactions_data_frauds)
summary(transactions_data_nofrauds)





ggplot(data=transactions_data_frauds, aes(x=num_transactions)) + geom_bar(alpha=0.8)+ 
  ggtitle("Distribution of Age")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("perc change prev") + ylab("# txns")


ggplot(data=transactions_data_nofrauds, aes(x=Amountnew)) + geom_histogram(bins=15,alpha=0.8)+ 
  ggtitle("Distribution of Age")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("perc change prev") + ylab("# txns")









####################### DATA TRANSFORMATIONS AND CLEANING #############


## Frauds Dataset ##

transactions_data_frauds = filter(transactions_data,Is.Fraud.=='Yes')
transactions_data_frauds$User = as.integer(transactions_data_frauds$User)
transactions_data_frauds$monthname = month.name[transactions_data_frauds$Month]
transactions_data_frauds$MCC = as.factor(transactions_data_frauds$MCC)


View(transactions_data_frauds)
transactions_data_frauds_customers = left_join(transactions_data_frauds,users_data,by="User")
View(transactions_data_frauds_customers)
transactions_data_frauds = transactions_data_frauds_customers
transactions_data_frauds_customers = left_join(transactions_data_frauds,users_data,by="User")

transactions_data_frauds_cards = left_join(transactions_data_frauds,cards_data,by=c("User","Card"))
View(transactions_data_frauds_cards)


nrow(transactions_data_frauds)
View(users_data)

##################################################################################
###################### Exploratory Data Analysis #################################
##################################################################################




##### USERS ANALYSIS ######

users_data %>% group_by(Gender) %>% tally() %>% 
  ggplot(., aes(x=Gender,y=n,fill=Gender)) + geom_bar(stat='identity')+ 
  ggtitle("Plot of Users by Gender")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Gender") + ylab("# Users")

####Observation: "Seems the Frauds dont depend on Genders and it equally likely to be fraud susceptible"#####







##### TRANSACTIONS ANALYSIS #####
transactions_data %>% group_by(Is.Fraud.) %>% tally()%>% 
  ggplot(., aes(x=Is.Fraud.,y=n,fill=Is.Fraud.)) + geom_bar(stat='identity')+ 
  ggtitle("Plot of #Frauds in Dataset")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Fraud (Yes/No)") + ylab("# Transactions") 

transactions_data %>% group_by(Is.Fraud.) %>% tally() 

transactions_data %>% group_by(Use.Chip) %>% tally() %>% 
  ggplot(., aes(x=Use.Chip,y=n,fill=Use.Chip)) + geom_bar(stat='identity')+ 
  ggtitle("Plot of # Transactions by Chip Status")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Chip Status") + ylab("# Transactions")

### Observation : Swipe Transactions are more as compared to other 2 types of transactions ##


transactions_data %>% group_by(Use.Chip) %>% summarise(avg = mean(Amountnew))%>% 
  ggplot(., aes(x=Use.Chip,y=avg,fill=Use.Chip)) + geom_bar(stat='identity')+ 
  ggtitle("Plot of Avg Transaction Amount by Chip Status")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Chip Status") + ylab("Avg Transaction Amount")

### Observation : Though Avg Amount for transactions for online is higher ###



############################################################################
###################### USERS ANALYTICS #####################################
############################################################################

### GENDER WISE ###
users_data %>% group_by(Gender) %>% tally() %>% 
  ggplot(., aes(x=Gender,y=n,fill=Gender)) + geom_bar(stat='identity')+ 
  ggtitle("Plot of Gender distribution")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Gender") + ylab("# Number of customers")




### AGE Distribution ###

ggplot(data=users_data, aes(x=Current.Age)) + geom_histogram(bins=15,alpha=0.8)+ 
  ggtitle("Distribution of Age")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Age(years)") + ylab("# customers")


### State Wise Distribution ###
library(treemapify)
users_data %>% group_by(State_Fullname) %>% tally()%>%
  ggplot(.,aes(area=n,fill=State_Fullname,label = State_Fullname))+geom_treemap() +geom_treemap_text()

##OBSERVATION: Maximum Transactions are from California ###

### FICO Score with Age ###
ggplot(data=users_data, aes(x=FICO.Score)) + geom_histogram(bins=15)+ 
    ggtitle("Distribution of FICO")+theme(plot.title = element_text(hjust = 0.5))+
    xlab("FICO Score") + ylab("# customers")

############################################################################
################ CARD Analytics ##############################
############################################################################
View(cards_data)

cards_data %>% group_by(Card.Brand) %>% tally() %>%
ggplot(., aes(x=Card.Brand,y=n,fill=Card.Brand)) + geom_bar(stat='identity')+ 
  ggtitle("Plot of Card Brand")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Card Brand") + ylab("# of Cards")

############################################################################
################ FRAUD TRANSACTIONS ANALYTICS ##############################
############################################################################


## Avg Transaction amount ##

transactions_data_frauds %>% group_by(Use.Chip) %>% summarise(avg = mean(Amountnew)) %>% 
  ggplot(., aes(x=Use.Chip,y=avg,fill=Use.Chip)) + geom_bar(stat='identity')+ 
  ggtitle("Avg Fraud Transaction Amount by Chip Use Type")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Chip Type") + ylab("Avg Transaction Amount($) of Fraud Transactions")

library(usmap)
avg_amount_state = transactions_data_frauds %>% group_by(State_Fullname) %>% summarise(avg = mean(Amountnew))
colnames(avg_amount_state)[1]='state'
avg_amount_state[order(avg_amount_state$avg),]


plot_usmap(data = avg_amount_state, values = "avg", color = "red",labels=TRUE) + 
  scale_fill_continuous(
    low = "white", high = "red", name = 'Avg Fraud Transaction Amount', label = scales::comma
  ) + theme(legend.position = "right")


##### Number of Fraudulent transactions by states ###### 
txns_state = transactions_data_frauds %>% group_by(State_Fullname) %>% tally()
colnames(txns_state)[1]='state'
plot_usmap(data = txns_state, values = "n", color = "black",labels=TRUE) + 
  scale_fill_continuous(
    low = "white", high = "blue", name = '# Fraudulent Transactions', label = scales::comma
  ) + theme(legend.position = "right")


#### TODO: DISTINCT USERS FRAUD US MAP###



## Year wise frauds ##

transactions_data_frauds %>% group_by(Year,Use.Chip) %>% tally() %>%
  ggplot(.,aes(x=Year,y=n,group=Use.Chip,colour=Use.Chip)) + geom_line()

## Month wise frauds ##
transactions_data_frauds %>% group_by(Month,monthname) %>% tally() %>%
  ggplot(.,aes(x=reorder(monthname,Month),y=n,fill=reorder(monthname,Month))) + geom_bar(stat = 'identity')+xlab('Month')+ylab('# Fraudulent Txns')

## Month wise avg fraud amount ##
transactions_data_frauds %>% group_by(Month,monthname,Use.Chip) %>% summarise(avg=sum(Amountnew)/10000) %>%
  ggplot(.,aes(x=reorder(monthname,Month),y=avg,fill=Use.Chip)) + geom_bar(stat = 'identity')+xlab('Month')+ylab('Avg Fraud Txn Amount($10K)')

transactions_data_frauds %>% group_by(Month,monthname,Use.Chip) %>% summarise(avg=sum(Amountnew)/10000) %>%
  ggplot(.,aes(x=reorder(monthname,Month),y=avg,fill=Use.Chip)) + geom_bar(stat = 'identity')+
theme(plot.title = element_text(hjust = 0.5))+
  xlab("Age(Years)") + ylab("# Fraud Txns")+facet_grid(Use.Chip ~ .)



## Month wise avg fraud amount ##
transactions_data_frauds %>% group_by(Month,monthname,Use.Chip) %>% summarise(avg=sum(Amountnew)/10000) %>%
  ggplot(.,aes(x=reorder(monthname,Month),y=avg,fill=Use.Chip,group)) + geom_bar(stat = 'identity')+xlab('Month')+ylab('Avg Fraud Txn Amount($10K)')



## Merchant Wise Frauds to identify Highly Risky Merchants ##
transactions_data_frauds %>% group_by(MCC) %>% tally()  %>% slice_max(order_by = n, n = 10) %>%
  ggplot(.,aes(x=reorder(MCC,-n),y=n,fill=MCC)) + geom_bar(stat = 'identity')+xlab('Merchant Number')+ylab('# Fraudulent Txns')


transactions_data_frauds_cards %>% group_by(Card.Brand) %>% tally() %>%
ggplot(.,aes(x=Card.Brand,y=n,fill=Card.Brand)) + geom_bar(stat = 'identity')+xlab('Card Brand')+ylab('# Fraudulent Txns')



transactions_data_frauds %>% group_by(MCC) %>% summarise(avg=mean(Amountnew))  %>% slice_max(order_by = avg, n = 10) %>%
  ggplot(.,aes(x=reorder(MCC,-avg),y=avg,fill=MCC)) + geom_bar(stat = 'identity')+xlab('Merchant Number')+ylab('Avg Fraudulent Amount ($)')

### Average FICO Score of frauds##

ggplot(transactions_data_frauds,aes(x=FICO.Score))+geom_histogram(bins=10)+ 
         ggtitle("Distribution of Fraudulent Transactions by FICO")+theme(plot.title = element_text(hjust = 0.5))+
         xlab("FICO Score") + ylab("# Fraud Txns")
       
### Frauds by Age ##

ggplot(transactions_data_frauds,aes(x=Current.Age,fill=Use.Chip))+geom_histogram(binwidth = 10,alpha=0.9)+ 
  ggtitle("Distribution of Fraudulent Transactions by Age")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Age(Years)") + ylab("# Fraud Txns")+facet_grid(Use.Chip ~ ., scales = "free")

#### Observation: Age between 45 - 60 most susceptible to Fraud ####


fr = transactions_data_frauds %>% group_by(Current.Age,Use.Chip) %>% summarise(sm = sum(Amountnew))
fr = as.data.frame(fr)

ggplot(fr,aes(x=Current.Age,y=sm,fill=Use.Chip))+geom_histogram(alpha=0.9,stat='identity')+ 
  ggtitle("Distribution of Fraudulent Transactions by Age")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Age(Years)") + ylab("# Fraud Txns")+facet_grid(Use.Chip ~ .,scale='free')

ggplot(fr,aes(x=Current.Age,y=sm,fill=Use.Chip))+geom_line(alpha=0.9,stat='identity')+ 
  ggtitle("Distribution of Fraudulent Transactions by Age")+theme(plot.title = element_text(hjust = 0.5))+
  xlab("Age(Years)") + ylab("# Fraud Txns")+facet_grid(Use.Chip ~ .,scale='free')



###################################################################################################################
####################FEATURE ENGINEERING ###########################################################################
###################################################################################################################




##### generate flag of is.Fraud #####

transactions_data <- transactions_data[,!(names(transactions_data) %in% c("Day","Time","Amount","Merchant.Name","Zip","Merchant.City","Errors."))]
transactions_data$Fraud <- 0
transactions_data$Fraud[transactions_data$Is.Fraud.=='Yes'] <- 1


#colnames(transactions_data)
#indx <- apply(transactions_data, 2, function(x) any(is.na(x) | is.infinite(x)))
#colnames(transactions_data)[indx]
## RISKY MERCHANTS
risky_mccs <- c(5311,5300,5310,4829,5912)
transactions_data$mcc_risky <- 0
transactions_data$mcc_risky[transactions_data$MCC %in% risky_mccs] <- 1

## RISKY STATE FILTER ##
transactions_data = left_join(transactions_data,users_data[c("User","Current.Age","Yearly.Income...Person","State_Fullname")],by="User")
risky_state <- c("California","Texas","New York","Florida")
transactions_data$state_risky <- 0
transactions_data$state_risky[transactions_data$State_Fullname %in% risky_state] <- 1

## users avg amount across chips
##transactions_data_frauds = filter(transactions_data,Is.Fraud.=='Yes')
##transactions_data_nofrauds = filter(transactions_data,Is.Fraud.=='No')
##transactions_avg_amt_user_chip <- transactions_data[transactions_data$Is.Fraud.=='No',] %>% group_by(Use.Chip) %>% summarise(avg_nofraud=mean(Amountnew))
###transactions_avg_amt_user_chip_fraud <- transactions_data_frauds %>% group_by(Use.Chip) %>% summarise(avg=mean(Amountnew))

#transactions_data = left_join(transactions_data,transactions_avg_amt_user_chip,by="Use.Chip")

## Transaction ramp up Feature
#transactions_data$avg_rampup = transactions_data$Amountnew - transactions_data$avg_nofraud

### ONE HOT ENCODING OF CHIP TYPE
transactions_data$chip_flag <- 0
transactions_data$swipe_flag <- 0
transactions_data$online_flag <- 0
transactions_data$chip_flag[transactions_data$Use.Chip=='Chip Transaction'] <- 1
transactions_data$swipe_flag[transactions_data$Use.Chip=='Online Transaction'] <- 1
transactions_data$online_flag[transactions_data$Use.Chip=='Swipe Transaction'] <- 1


### High risk Age ###
transactions_data$age_risky <- 0 
transactions_data$age_risky[transactions_data$Current.Age>=40 & transactions_data$Current.Age<=65] <- 1 

### remove columns not needed 
transactions_data_model <- transactions_data[,!(names(transactions_data) %in% c("User","Card","Month","Use.Chip","Merchant.State","MCC","Is.Fraud.","Current.Age","Yearly.Income...Person","State_Fullname"
                    ,"avg_nofraud","Amountnew","dummy_col","lag_amountnew","max_trans_amt_until_now","min_trans_amt_until_now","user_trans_num","running_avg"))]
colnames(transactions_data_model)


############################################## test train split ########################

#set.seed(123)
#smp_siz = floor(0.70*nrow(transactions_data_model))
#train_ind = sample(seq_len(nrow(transactions_data_model)),size = smp_siz)

train_model=filter(transactions_data_model,Year<=2017) #creates the training dataset with row numbers stored in train_ind
test_model=filter(transactions_data_model,Year>=2018 & Year<2020)  # creates the test dataset excluding the row numbers mentioned in train_ind


train_model <- train_model[,!(names(train_model) %in% c("Year"))]
test_model <- test_model[,!(names(test_model) %in% c("Year"))]


train_model %>% group_by(Fraud) %>% tally()
test_model %>% group_by(Fraud) %>% tally()

#train_model = filter(train_model,perc_diff_ma10!=-Inf & change_prev!=-Inf & perc_diff_ma3!=-Inf & perc_diff_ma5!=-Inf)
#test_model = filter(test_model,perc_diff_ma10!=-Inf & change_prev!=-Inf & perc_diff_ma3!=-Inf & perc_diff_ma5!=-Inf)
#summary(test_model)





###################################### OVER SAMPLING ##################################################
#######################################################################################################

##transactions_data %>% group_by(Is.Fraud.) %>% tally()
#transactions_data_model %>% group_by(Fraud) %>% tally()

library(ROSE)

train_model_under_samp <- ovun.sample(Fraud~.,data=train_model,p=0.3, 
                                      seed=123, N=nrow(train_model),method="both")$data

#test_model_under_samp <- ovun.sample(Fraud~.,data=test_model,p=0.3, 
#                                     seed=123, N=100000,method="both")$data

summary(train_model_under_samp)

#train_model_under_samp <- ovun.sample(Fraud~.,data=train_model,p=0.5, 
#                                      seed=123,method="under")$data


train_model_under_samp %>% group_by(Fraud) %>% tally()
test_model_under_samp %>% group_by(Fraud) %>% tally()

View(train_model_under_samp) 

train_model_under_samp %>% group_by(Fraud) %>% summarise(avg=mean(avg_rampup))

rm(transactions_data)
rm(transactions_data_model)
rm(transactions_data_frauds)
rm(transactions_data_nofrauds)
rm(train_model)

######################## correlation plots ################################
library(corrplot)


######################## MODEL CREATION ##############################################################
######################################################################################################

####### LOGISTIC REGRESSION ###############



library(caTools)






#### WITHOUT SAMPLING##################

logistic_model <- glm(Fraud ~ ., 
                      data = train_model, 
                      family = "binomial")


#### with sAMPLING ###############
logistic_model <- glm(Fraud ~ ., 
                      data = train_model_under_samp, 
                      family = "binomial")


summary(logistic_model)

predict_reg <- predict(logistic_model, 
                       test_model, type = "response")
#predict_reg 

# Changing probabilities
predict_reg <- ifelse(predict_reg>0.9, 1, 0)
table(Actual = test_model$Fraud, Predicted = predict_reg)

##### accuracy 
library(MLMetrics)
accuracy(test_model$Fraud, predict_reg)
Precision(test_model$Fraud, predict_reg, positive = 1)
Recall(test_model$Fraud, predict_reg, positive = 1)
F1_Score(test_model$Fraud, predict_reg, positive = 1)
??Metrics


##################### SVM ################
library(e1071)
classifier = svm(formula = Fraud ~ .,
                 data = train_model_under_samp,
                 type = 'C-classification',
                 kernel = 'linear')


################## RFR ####################

library(ranger)

indx <- apply(train_model, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(train_model)[indx]
View(train_model)

train_model[is.na(train_model$change_prev),]
####### wiTHOUT Sampling##################

fit <- ranger(Fraud ~ ., 
              data = train_model, 
              num.trees = 500,
              max.depth = 100,
              classification = TRUE)





####### with sampling ###################

fit <- ranger(Fraud ~ ., 
              data = train_model_under_samp, 
              num.trees = 100,
              max.depth = 10,
              classification = TRUE)

rfr_predict = predict(fit, 
        test_model, type = "response")

table(Actual = test_model$Fraud, Predicted = rfr_predict$predictions)


library(ranger)
fit$variable.importance
varImpRanger(fit)
library(ranger)
varimp = varImpRanger(object = fit, data = train_model_under_samp, target = "Fraud")

rfr_predict$predictions
accuracy(test_model$Fraud, rfr_predict$predictions)

precision(test_model$Fraud, rfr_predict$predictions)


####### XG Boosst #########################

library(xgboost)
colnames(train_model)
## without undersampling

bstSparse <- xgboost(data = as.matrix(train_model[-6]), label = train_model$Fraud, max.depth = 10, eta = 1, nthread = 2, nrounds = 50, objective = "binary:logistic")
xgboost_predict = predict(bstSparse,as.matrix(test_model[-6]), type = "response")
xgboost_predict = predict(bstSparse,as.matrix(train_model[-6]), type = "response")
summary(train_model)

predict_xg <- ifelse(xgboost_predict>0.5, 1, 0)
table(actual = train_model$Fraud, predict = predict_xg)
table(test_model$Fraud, predict_xg)

accuracy(test_model$Fraud,predict_xg)
precision(test_model$Fraud, predict_xg)
recall(test_model$Fraud, predict_xg)

## with undersampling #####
colnames(train_model_under_samp)[6]
bstSparse <- xgboost(data = as.matrix(train_model_under_samp[-6]), label = train_model_under_samp$Fraud, max.depth = 10, eta = 1, nthread = 2, nrounds = 25, objective = "binary:logistic")
xgboost_predict = predict(bstSparse,as.matrix(test_model[-6]), type = "response")

predict_xg_under <- ifelse(xgboost_predict>0.8, 1, 0)
#table(train_model$Fraud, predict_xg_under)
table(Actual = test_model$Fraud, predicted = predict_xg_under)

accuracy(test_model$Fraud,predict_xg_under)
precision(test_model$Fraud, predict_xg_under)
recall(test_model$Fraud, predict_xg_under)
F1_Score(test_model$Fraud, predict_xg, positive = NULL)

### When precision is 50% (no sampling) then recall 0.5%
### When precision sucks (under sampling) then recall is at 50%
### Precision is for customer satisfaction, Recall is for how good is fraud detection
library(MLmetrics)

recall(test_model$Fraud, predict_xg)


################################# NAIVE BAIYES ###########################################
library(e1071)
library(caTools)
library(caret)

### without sampling #####
classifier_naive <- naiveBayes(Fraud ~ ., data = train_model)
predict_naive_unsamp <- predict(classifier_naive, newdata = test_model)
table(Actual = test_model$Fraud, predicted = predict_naive_unsamp)


accuracy(test_model$Fraud,predict_naive_unsamp)
precision(as.factor(test_model$Fraud), predict_naive_unsamp)
recall(as.factor(test_model$Fraud), predict_naive_unsamp)




### with sampling #####
classifier_naive <- naiveBayes(Fraud ~ ., data = train_model_under_samp)
predict_naive <- predict(classifier_naive, newdata = test_model)
table(Actual = test_model$Fraud, predicted = predict_naive_unsamp)

#accuracy(test_model$Fraud,predict_naive)
#precision(factor(test_model$Fraud), predict_naive)
#recall(factor(test_model$Fraud), predict_naive)



sum(predict_naive == predict_naive_unsamp)/length(predict_naive_unsamp)





train_model_under_samp




############################# NEURAL NETWORK #################################

library(tidyverse)
library(keras)
library(fastDummies)
library(caret)
library(tensorflow)
library(kerasR)


## with sampling 
X_train <- train_model_under_samp %>%
  select(-Fraud) %>%
  scale()

summary(X_train)

Y_train <- to_categorical(as.matrix(train_model_under_samp$Fraud))


# Network design
model <- keras_model_sequential()


model %>%
  # Input layer
  layer_dense(units = 256, activation = 'relu', input_shape =  ncol(X_train)) %>% 
  layer_dropout(rate = 0.4) %>% 
  # Hidden layer
  layer_dense(units = 75, activation = 'relu') %>%
  # Output layer
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')


