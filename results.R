library(dplyr)
library(tidyverse)
library(ggplot2)

results <- read.csv(file.choose())

View(results)



##Unsampled

### take metric Type as a input through dropdown and pass it as variable
results %>% select(Model,MetricType,MetricUnsampled) %>% filter(MetricType=="Accuracy") %>%
  ggplot(., aes(x=Model, y=MetricUnsampled,fill=Model,label = MetricUnsampled )) %>% + geom_bar(stat = 'identity')+xlab('Model')+ylab('Accuracy')

##sampled
results %>% select(Model,MetricType,Metricsampled) %>% filter(MetricType=="Accuracy") %>%
  ggplot(., aes(x=Model, y=Metricsampled,fill=Model,label = Metricsampled )) %>% + geom_bar(stat = 'identity')+xlab('Model')+ylab('Accuracy')
