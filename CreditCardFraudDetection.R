#########################################################################
############################### TO DO ###################################
#########################################################################


#1 Buckets create for FICO, AGE, RISKY Merchants
#2 






############################################################################
################ SDM 2 Project #############################################
################ Credit Card Fraud Detection ###############################
############################################################################



#########################LIBRARIES IMPORT ##################################
library(dplyr)
library(stringr)
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

users_data = read.csv(file.choose())
users_data = users_data %>% mutate(User = row_number()-1)
View(users_data)

transactions_data = read.csv(file.choose())
View(transactions_data)


####################### DATA TRANSFORMATIONS AND CLEANING #############
transactions_data$Amountnew = as.double(substring(transactions_data$Amount,2,))

## Frauds Dataset ##

transactions_data_frauds = filter(transactions_data,Is.Fraud.=='Yes')
transactions_data_frauds$User = as.integer(transactions_data_frauds$User)

View(transactions_data_frauds)

transactions_data_frauds_customers = left_join(transactions_data_frauds,users_data,by="User")
View(transactions_data_frauds_customers)
transactions_data_frauds = transactions_data_frauds_customers
###################### Exploratory Data Analysis #########################


##### USERS ANALYSIS ######
users_data %>% group_by(Gender) %>% tally()


##### TRANSACTIONS ANALYSIS #####
transactions_data %>% group_by(Is.Fraud.) %>% tally()
transactions_data %>% group_by(Use.Chip) %>% summarise(avg = mean(Amountnew)) 



############################################################################
###################### USERS ANALYTICS #####################################
############################################################################

### GENDER WISE ###
users_data %>% group_by(Gender) %>% tally()

### AGE Distibution ###
users_data %>% group_by(Current.Age) %>% tally()

### State Wise Distribution ###
users_data %>% group_by(State) %>% tally()

### FICO Score with Age ###





############################################################################
################ FRAUD TRANSACTIONS ANALYTICS ##############################
############################################################################





## Avg Transaction amount ##
transactions_data_frauds %>% group_by(Use.Chip) %>% summarise(avg = mean(Amountnew))
transactions_data_frauds %>% group_by(State) %>% summarise(avg = mean(Amountnew))

## Year wise frauds ##
transactions_data_frauds %>% group_by(Year) %>% tally()

## Month wise frauds ##
transactions_data_frauds %>% group_by(Month) %>% tally()

## Merchant Wise Frauds to identify Highly Risky Merchants ##
transactions_data_frauds %>% group_by(MCC) %>% tally()  %>% slice_max(order_by = n, n = 10)

### Average FICO Score of frauds##
transactions_data_frauds %>% summarise(avg=mean(FICO.Score))
transactions_data_frauds %>% group_by(Use.Chip) %>% summarise(avg=mean(FICO.Score))

### Frauds by gender ###
transactions_data_frauds %>% group_by(Gender) %>% tally()

### Average Age for fraud ###
transactions_data_frauds %>% group_by(Current.Age) %>% tally()
transactions_data_frauds %>% group_by(Use.Chip) %>% summarise(median = median(Current.Age),avg=mean(Current.Age))



#### State Wise Frauds ####
transactions_data_frauds %>% group_by(State) %>% tally()

### Errors and Fraud ###
transactions_data_frauds %>% group_by(Errors.) %>% tally()






