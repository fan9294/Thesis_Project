library(tidyverse)
library(data.table)
library(DirichletReg)
library(quantreg)

source('GetHistoricalData.R')

years = seq(2006, 2019)

contest_data_cq <- GetContestHistory(years, TRUE, ssb = FALSE)
contest_data_ssb <- GetContestHistory(years, TRUE, ssb = TRUE)


model <- lm(log(Total_contacts) ~ No_participants, data = contest_data)
summary(model)

par(mfrow = c(2,2))
model_10m <- lm(log(`10m`) ~ Sunspot, data = contest_data)
summary(model_10m)
plot(model_10m)

model_15m <- update(model_10m, log(`15m`) ~ .)
summary(model_15m)
plot(model_15m)

model_20m <- update(model_10m, log(`20m`) ~ .)
summary(model_20m)
plot(model_20m)

model_40m <- update(model_10m, log(`40m`) ~ .)
summary(model_40m)
plot(model_40m)

model_80m <- update(model_10m, log(`80m`) ~ .)
summary(model_80m)
plot(model_80m)

model_160m <- update(model_10m, log(`160m`) ~ .)
summary(model_160m)
plot(model_160m)



data_to_fit <- as.data.frame(contest_data)
data_to_fit$Y <- DR_data(data_to_fit[, 5:10])
model_dirichlet2 <- DirichReg(Y ~ Sunspot, data = data_to_fit, model = 'common')
summary(model_dirichlet2)
plot(model_dirichlet2)
model_dirichlet_null <- DirichReg(Y ~ 1, data = data_to_fit, model = 'common')
summary(model_dirichlet_null)

qr10 <- rq(Total_contacts*`10m` ~ Sunspot, data = contest_data, tau = 0.5)
summary(qr10)
qr15 <- update(qr10, Total_contacts*`15m` ~ Sunspot)
summary(qr15)
qr20 <- update(qr10, Total_contacts*`20m` ~ Sunspot)
summary(qr20)
qr40 <- update(qr10, Total_contacts*`40m` ~ Sunspot)
summary(qr40)
qr80 <- update(qr10, Total_contacts*`80m` ~ Sunspot)
summary(qr80)
qr160 <- update(qr10, Total_contacts*`160m` ~ Sunspot)
summary(qr160)

prediction <- as.data.table(predict(model_dirichlet2))
prediction$Year = seq(2006, 2019)
prediction[,.(Year, `10m`, `15m`, `20m`, `40m`, `80m`, `160m`)] %>% melt(id = 'Year') %>%
  ggplot(aes(x=Year, y=value, fill=variable)) +
  geom_bar(position='stack', stat='identity')+
  ylab('Ratio') + xlab('Year') + scale_fill_discrete(name = "Radio band")
contest_data
model = lm(log(Total_contacts) ~ No_participants, data = contest_data)
summary(model)
par(mfrow = c(2,2))
plot(model)

predict(model_dirichlet2, data.frame(Sunspot = 10))
predict(model_dirichlet2, data.frame(Sunspot = 100))
predict(model_dirichlet2, data.frame(Sunspot = 150))


ggplot(contest_data, aes(Sunspot,Total_contacts*`10m`)) + 
  geom_point() + 
  geom_quantile(quantiles = 0.25)
