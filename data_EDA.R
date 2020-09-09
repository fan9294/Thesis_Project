library(tidyverse)
library(data.table)
library(ggplot2)
library(rworldmap)
library(ggsci)

radio <- fread('radio_contest_2019_ssb_processed.csv', check.names = FALSE)
str(radio)

radio$`CATEGORY-OPERATOR` <- as.factor(radio$`CATEGORY-OPERATOR`)
radio$`CATEGORY-ASSISTED` <- as.factor(radio$`CATEGORY-ASSISTED`)
radio$`CATEGORY-POWER` <- as.factor(radio$`CATEGORY-POWER`)
radio$`CATEGORY-MODE` <- as.factor(radio$`CATEGORY-MODE`)
radio$`CATEGORY-TRANSMITTER` <- as.factor(radio$`CATEGORY-TRANSMITTER`)

radio$`CATEGORY-OPERATOR` <- factor(radio$`CATEGORY-OPERATOR`, levels = c('SINGLE-OP', 'MULTI-OP'))
radio$`CATEGORY-ASSISTED` <- factor(radio$`CATEGORY-ASSISTED`, levels = c('NON-ASSISTED', 'ASSISTED'))
radio$`CATEGORY-POWER` <- factor(radio$`CATEGORY-POWER`, levels = c('QRP', 'LOW', 'HIGH'))

summary(radio)

ggplot(data = radio, aes(x =Numbers)) +
  geom_histogram(bins = 50, fill='#d1495b', color='black') +
  ggtitle('Distribution of radio contacts') + 
  xlab('Number of contacts')

ggplot(data = radio, aes(x = Numbers %>% log)) +
  geom_histogram(bins = 50, fill='#d1495b', color='black') +
  ggtitle('Distribution of log (Number of Contacts)') + 
  xlab('LOG(No. contacts)')


ggplot(data=radio, aes(x=`CATEGORY-OPERATOR`)) + 
  geom_bar(stat = 'count', fill='#d1495b') + xlab('Operating type')



ggplot(data=radio, aes(x=`CATEGORY-ASSISTED`)) + 
  geom_bar(stat = 'count', fill='#d1495b') + 
  xlab('Assistance')
ggplot(data = radio, aes(x =log(Numbers), y = ..density..,
                         color=`CATEGORY-ASSISTED`, fill=`CATEGORY-ASSISTED`)) +
  geom_histogram(bins = 50,
                 alpha=.5, position="identity") +
  xlab('LOG(No. contacts)')

ggplot(data=radio, aes(x=`CATEGORY-OPERATOR`, fill=`CATEGORY-ASSISTED`)) + 
  geom_bar(stat = 'count')

ggplot(data = radio, aes(x =log(Numbers), y = ..density..,
                         color=`CATEGORY-OPERATOR`, fill=`CATEGORY-OPERATOR`)) +
  geom_histogram(bins = 50,
                 alpha=.5, position="identity") + 
  xlab('LOG(No. contacts)')



ggplot(data=radio, aes(x=`CATEGORY-POWER`)) + 
  geom_bar(stat = 'count', fill='#d1495b') 

ggplot(data = radio, aes(y =log(Numbers), 
                         x=`CATEGORY-POWER`, fill=`CATEGORY-POWER`)) +
  geom_boxplot(alpha=.5) +
  xlab('Power type') +
  ylab('LOG(No. contacts)')

ggplot(data=radio, aes(x=`CATEGORY-TRANSMITTER`)) + 
  geom_bar(stat = 'count', fill='#d1495b')
ggplot(data=radio, aes(x=`CATEGORY-POWER`, fill=`CATEGORY-TRANSMITTER`)) + 
  geom_bar(stat = 'count')

ggplot(data = radio, aes(y =log(Numbers), 
                         x=`CATEGORY-TRANSMITTER`, fill=`CATEGORY-TRANSMITTER`)) +
  geom_boxplot(alpha=.5) +
  xlab('Power type') +
  ylab('LOG(No. contacts)')


countries <- radio$Country_code_3c %>% table() %>% as.data.frame()
colnames(countries) <- c('code', 'freq')
countries$ratio <- countries$freq / sum(countries$freq)
mapped_data <- joinCountryData2Map(countries, joinCode="ISO3", nameJoinColumn="code")
mapped_data$ratio <- cut(mapped_data$ratio, breaks = c(0, 0.01, 0.05, .1, .2, .3))
levels(mapped_data$ratio) <- c('1%', '5%', '10%', '20%', '30%')
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(mapped_data, nameColumnToPlot="ratio", catMethod = 'categorical', colourPalette = 'rainbow',
               mapTitle = 'Geographic distribution of participants')




