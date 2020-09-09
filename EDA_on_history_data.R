library(tidyverse)
library(data.table)

years <- seq(2019, 2006)
GetContestHistory <- function(years){
  require(tidyverse)
  require(data.table)
  contest <- list()
  contest$Year = years
  for (i in 1:length(years)){
    radio <- fread(paste0('radio_contest_', years[i], '_processed.csv'))
    contest$No_participants <- append(contest$No_participants, nrow(radio))
    contest$Most_contacts <-  append(contest$Most_contacts, max(radio$Numbers, na.rm=TRUE))
    contest$Total_contacts <-  append(contest$Total_contacts, sum(radio$Numbers, na.rm = TRUE))
    qso <- fread(paste0('qso_data_', years[i], '_processed.csv'))
    band <- table(qso[Qualified == 1, Bandwith]) %>% as.list()
    contest$`10m` <- append(contest$`10m`, band$`10m`)
    contest$`15m` <- append(contest$`15m`, band$`15m`)
    contest$`20m` <- append(contest$`20m`, band$`20m`)
    contest$`40m` <- append(contest$`40m`, band$`40m`)
    contest$`80m` <- append(contest$`80m`, band$`80m`)
    contest$`160m` <- append(contest$`160m`, band$`160m`)
  }
  contest <- contest %>% as.data.table()
  contest[, `10m` := `10m` / Total_contacts]
  contest[, `15m` := `15m` / Total_contacts]
  contest[, `20m` := `20m` / Total_contacts]
  contest[, `40m` := `40m` / Total_contacts]
  contest[, `80m` := `80m` / Total_contacts]
  contest[, `160m` := `160m` / Total_contacts]
  sunspot <- fread('SN_m_tot_V2.0.csv')
  sunspot[, V1 := floor(V1)]
  sunspot <- sunspot[V2 == 11]
  contest <- left_join(contest, sunspot[V1 %in% years, V4, V1], by = c('Year' = 'V1'))
  setnames(contest, c('V4'), c('Sunspot'))
  return(contest)
}
contest <- GetContestHistory(years)


ggplot(contest_data, aes(No_participants, Total_contacts)) +
  geom_point()+xlab('Number of participants')+ ylab('Number of total contacts')

ggplot(contest_data, aes(Year, No_participants)) +
  geom_point() + xlab('Year') + ylab('Number of participants')

ggplot(contest_data, aes(Sunspot, Total_contacts)) +
  geom_point() + xlab('Sunspot number') + ylab('Number of total contacts')

contest_data[,.(Year, `10m`, `15m`, `20m`, `40m`, `80m`, `160m`)] %>% melt(id = 'Year') %>%
  ggplot(aes(x=Year, y=value, fill=variable)) +
  geom_bar(position='stack', stat='identity')+
  ylab('Ratio') + xlab('Year') + scale_fill_discrete(name = "Radio band")

ggplot(contest_data, aes(Year, Sunspot)) +
  geom_point() + xlab('Year') + ylab('Sunspot number')
