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
View(transactions_data)
transactions_data$Amountnew = as.double(substring(transactions_data$Amount,2,))


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
transactions_data$Fraud[transactions_data$Is.Fraud.=='Yes'] <- 1
transactions_data$Fraud[transactions_data$Is.Fraud.=='No'] <- 0


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
transactions_data_frauds = filter(transactions_data,Is.Fraud.=='Yes')
transactions_data_nofrauds = filter(transactions_data,Is.Fraud.=='No')
transactions_avg_amt_user_chip <- transactions_data[transactions_data$Is.Fraud.=='No',] %>% group_by(Use.Chip) %>% summarise(avg_nofraud=mean(Amountnew))
transactions_avg_amt_user_chip_fraud <- transactions_data_frauds %>% group_by(Use.Chip) %>% summarise(avg=mean(Amountnew))

transactions_data = left_join(transactions_data,transactions_avg_amt_user_chip,by="Use.Chip")

## Transaction ramp up Feature
transactions_data$avg_rampup = transactions_data$Amountnew - transactions_data$avg_nofraud

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
transactions_data_model <- transactions_data[,!(names(transactions_data) %in% c("User","Card","Year","Month","Use.Chip","Merchant.State","MCC","Is.Fraud.","Current.Age","Yearly.Income...Person","State_Fullname","avg_nofraud"))]




############################################## test train split ########################

set.seed(123)
smp_siz = floor(0.70*nrow(transactions_data_model))
train_ind = sample(seq_len(nrow(transactions_data_model)),size = smp_siz)

train_model=transactions_data_model[train_ind,] #creates the training dataset with row numbers stored in train_ind
test_model=transactions_data_model[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind


train_model %>% group_by(Fraud) %>% tally()
test_model %>% group_by(Fraud) %>% tally()






###################################### OVER SAMPLING ##################################################
#######################################################################################################

##transactions_data %>% group_by(Is.Fraud.) %>% tally()
transactions_data_model %>% group_by(Fraud) %>% tally()

library(ROSE)

train_model_under_samp <- ovun.sample(Fraud~.,data=train_model,p=0.5, 
                                      seed=123, N=nrow(train_model),method="both")$data



train_model_under_samp %>% group_by(Fraud) %>% tally()

View(train_model_under_samp) 

train_model_under_samp %>% group_by(Fraud) %>% summarise(avg=mean(avg_rampup))

rm(transactions_data)
rm(transactions_data_model)
rm(transactions_data_frauds)
rm(transactions_data_nofrauds)
rm(train_model)

######################## MODEL CREATION ##############################################################
######################################################################################################

####### LOGISTIC REGRESSION ###############

library(caTools)

logistic_model <- glm(Fraud ~ ., 
                      data = train_model_under_samp, 
                      family = "binomial")


summary(logistic_model)

predict_reg <- predict(logistic_model, 
                       test_model, type = "response")
#predict_reg 

# Changing probabilities
predict_reg <- ifelse(predict_reg >0.8, 1, 0)
table(test_model$Fraud, predict_reg)

##### accuracy 
library(Metrics)
accuracy(test_model$Fraud, predict_reg)

precision(test_model$Fraud, predict_reg)


??Metrics


##################### SVM ################
library(e1071)
classifier = svm(formula = Fraud ~ .,
                 data = train_model_under_samp,
                 type = 'C-classification',
                 kernel = 'linear')


################## RFR ####################
library(ranger)


fit <- ranger(Fraud ~ ., 
              data = train_model_under_samp, 
              num.trees = 50,
              max.depth = 8,
              classification = TRUE)

rfr_predict = predict(fit, 
        test_model, type = "response")

table(test_model$Fraud, rfr_predict$predictions)

rfr_predict$predictions
accuracy(test_model$Fraud, rfr_predict$predictions)

precision(test_model$Fraud, rfr_predict$predictions)

