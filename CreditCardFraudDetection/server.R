server <- function(input, output, session) {
  
  #Plot 
  output$plot <- renderPlot({
if (input$sel_SalesRep=='Age')
    {
      ggplot(data=users_data, aes(x=Current.Age)) + geom_histogram(bins=15,alpha=0.8)+ 
        ggtitle("Distribution of Age")+theme(plot.title = element_text(hjust = 0.5))+
        xlab("Age(years)") + ylab("# customers")
    }
    else if (input$sel_SalesRep=='FICO')
    {
      ggplot(data=users_data, aes(x=FICO.Score)) + geom_histogram(bins=15)+ 
        ggtitle("Distribution of FICO")+theme(plot.title = element_text(hjust = 0.5))+
        xlab("FICO Score") + ylab("# customers")
    }
  })
  output$plot1 <- renderPlot({
    if(input$sel_SalesRep == 'Age')
    {
      ggplot(fr,aes(x=Current.Age,y=sm,fill=Use.Chip))+geom_histogram(alpha=0.9,stat='identity')+ 
        ggtitle("Distribution of Fraudulent Transactions by Age")+theme(plot.title = element_text(hjust = 0.5))+
        xlab("Age(Years)") + ylab("# Fraud Txns")+facet_grid(Use.Chip ~ .,scale='free')
    }
    else if(input$sel_SalesRep == 'FICO')
    {
      ggplot(transactions_data_frauds,aes(x=FICO.Score))+geom_histogram(bins=10)+ 
        ggtitle("Distribution of Fraudulent Transactions by FICO")+theme(plot.title = element_text(hjust = 0.5))+
        xlab("FICO Score") + ylab("# Fraud Txns")
    }
  })
  output$plot7<-renderPlot({
    if(input$sel_user == 'Gender')
    {
      users_data %>% group_by(Gender) %>% tally() %>% 
        ggplot(., aes(x=Gender,y=n,fill=Gender)) + geom_bar(stat='identity')+ 
        ggtitle("Plot of Gender distribution")+theme(plot.title = element_text(hjust = 0.5))+
        xlab("Gender") + ylab("# Number of customers")
    }
    else if (input$sel_user=='State')
    {
      users_data %>% group_by(State_Fullname) %>% tally()%>%
        ggplot(.,aes(area=n,fill=State_Fullname,label = State_Fullname))+geom_treemap() +geom_treemap_text()
    }
    else if (input$sel_user == 'Year')
    {
      transactions_data_frauds %>% group_by(Year,Use.Chip) %>% tally() %>%
        ggplot(.,aes(x=Year,y=n,group=Use.Chip,colour=Use.Chip)) + geom_line()
    }
  })
  output$plot2<-renderPlot({
    cards_data %>% group_by(Card.Brand) %>% tally() %>%
      ggplot(., aes(x=Card.Brand,y=n,fill=Card.Brand)) + geom_bar(stat='identity')+ 
      ggtitle("Plot of Card Brand")+theme(plot.title = element_text(hjust = 0.5))+
      xlab("Card Brand") + ylab("# of Cards")
  })
  output$plot3<-renderPlot({
    transactions_data_frauds_cards %>% group_by(Card.Brand) %>% tally() %>%
      ggplot(.,aes(x=Card.Brand,y=n,fill=Card.Brand)) + geom_bar(stat = 'identity')+xlab('Card Brand')+ylab('# Fraudulent Txns')
  })
  output$plot4<-renderPlot({
    if (input$sel_fraud=='Merchant Number')
    {
      transactions_data_frauds %>% group_by(MCC) %>% tally()  %>% slice_max(order_by = n, n = 10) %>%
        ggplot(.,aes(x=reorder(MCC,-n),y=n,fill=MCC)) + geom_bar(stat = 'identity')+xlab('Merchant Number')+ylab('Number of Fraudulent Txns')
    }
    else if (input$sel_fraud=='Month')
    {
      transactions_data_frauds %>% group_by(Month,monthname) %>% tally() %>%
        ggplot(.,aes(x=reorder(monthname,Month),y=n,fill=reorder(monthname,Month))) + geom_bar(stat = 'identity')+xlab('Month')+ylab('Number of Fraudulent Txns')
    }
    else if (input$sel_fraud=='State')
    {
      plot_usmap(data = txns_state, values = "n", color = "black",labels=TRUE) + 
        scale_fill_continuous(
          low = "white", high = "blue", name = 'Number of Fraudulent Transactions', label = scales::comma
        ) + theme(legend.position = "right")
    }
  })
  output$plot5<-renderPlot({
    if (input$sel_fraud=='Merchant Number')
    {
      transactions_data_frauds %>% group_by(MCC) %>% summarise(avg=mean(Amountnew))  %>% slice_max(order_by = avg, n = 10) %>%
        ggplot(.,aes(x=reorder(MCC,-avg),y=avg,fill=MCC)) + geom_bar(stat = 'identity')+xlab('Merchant Number')+ylab('Avg Fraudulent Amount ($)')
    }
    else if (input$sel_fraud=='Month')
    {
      transactions_data_frauds %>% group_by(Month,monthname,Use.Chip) %>% summarise(avg=sum(Amountnew)/10000) %>%
        ggplot(.,aes(x=reorder(monthname,Month),y=avg,fill=Use.Chip)) + geom_bar(stat = 'identity')+xlab('Month')+ylab('Avg Fraud Txn Amount($10K)') 
    }
    else if (input$sel_fraud=='State')
    {
      plot_usmap(data = avg_amount_state, values = "avg", color = "red",labels=TRUE) + 
        scale_fill_continuous(
          low = "white", high = "red", name = 'Avg Fraud Transaction Amount', label = scales::comma
        ) + theme(legend.position = "right")
    }
  })
  output$plot6<-renderPlot({
    if (input$sampling=='Sampling')
    {
      results %>% select(Model,MetricType,Metricsampled) %>% filter(MetricType==input$metrics) %>%
        ggplot(., aes(x=Model, y=Metricsampled,fill=Model,label = Metricsampled )) %>% + geom_bar(stat = 'identity')+xlab('Model')+ylab(input$metrics)
    }
    else if (input$sampling=='No-Sampling')
    {
      results %>% select(Model,MetricType,MetricUnsampled) %>% filter(MetricType==input$metrics) %>%
        ggplot(., aes(x=Model, y=MetricUnsampled,fill=Model,label = MetricUnsampled )) %>% + geom_bar(stat = 'identity')+xlab('Model')+ylab(input$metrics)
    }
  })
  
  ######## XGBOOST MODEL #########
  #bstSparse <- xgboost(data = as.matrix(train_model_under_samp[-8]), label = train_model_under_samp$Fraud, max.depth = 10, eta = 1, nthread = 2, nrounds = 25, objective = "binary:logistic")
  change_prev = reactive({input$change_prev})
  above_moving_avg = reactive({input$above_moving_avg})
      above_max = reactive({input$above_max})
      change_prev_100perc_flag = reactive({input$change_prev_100perc_flag})
      change_prev_200perc_flag = reactive({input$change_prev_200perc_flag})
      same_merchant_transact = reactive({input$same_merchant_transact})
      above_moving_avg_user_flag = reactive({input$above_moving_avg_user_flag})
      mcc_risky = reactive({input$mcc_risky})
      state_risky = reactive({input$state_risky})
      age_risky = reactive({input$age_risky})
      chip_flag = reactive({ifelse(input$type == 'chip', 1, 0)})
      swipe_flag = reactive({ifelse(input$type == 'swipe', 1, 0)})
      online_flag = reactive({ifelse(input$type == 'online', 1, 0)})
      
     # df <- reactive({ 
      #  data.frame(change_prev(), above_moving_avg(),above_max(),change_prev_100perc_flag(),change_prev_200perc_flag(),same_merchant_transact(),
     #              above_moving_avg_user_flag(),mcc_risky(),state_risky(),age_risky(),chip_flag(),swipe_flag(),online_flag())
     # })
      
      df <- data.frame(matrix(ncol = 13, nrow = 0))
      x <- c("change_prev", "above_moving_avg", "change_prev_100perc_flag","change_prev_200perc_flag","same_merchant_transact","above_moving_avg_user_flag",
             "mcc_risky","state_risky","age_risky","chip_flag","chip_flag","swipe_flag","online_flag")
      colnames(df) <- x

      newrow <- reactive({
        c(as.numeric(input$change_prev),as.integer(input$above_moving_avg), as.integer(input$above_max),as.integer(input$change_prev_100perc_flag),as.integer(input$change_prev_200perc_flag),as.integer(input$same_merchant_transact),as.integer(input$above_moving_avg_user_flag),
          as.integer(input$mcc_risky),as.integer(input$state_risky),as.integer(input$age_risky),as.integer(ifelse(input$type == 'chip', 1, 0)),as.integer(ifelse(input$type == 'swipe', 1, 0)),as.integer(ifelse(input$type == 'online', 1, 0)))
      })
    modelpred = reactive({
      mpgInput = input$age_risky
      predict(bstSparse,as.matrix(structure(rbind(df, newrow()), .Names = names(df))), type = "response")
    })
  
    output$pred <- renderPrint({
      if(input$predict)
      {
        modelpred()
      }
      })
}