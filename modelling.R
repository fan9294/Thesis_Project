library(data.table)
radio <- fread('radio_contest_2019_ssb_processed.csv', check.names = FALSE)

radio$`CATEGORY-OPERATOR` <- factor(radio$`CATEGORY-OPERATOR`, levels = c('SINGLE-OP', 'MULTI-OP'))
radio$`CATEGORY-ASSISTED` <- factor(radio$`CATEGORY-ASSISTED`, levels = c('NON-ASSISTED', 'ASSISTED'))
radio$`CATEGORY-POWER` <- factor(radio$`CATEGORY-POWER`, levels = c('QRP', 'LOW', 'HIGH'))

model1 <- glm(log(Numbers) ~ `CATEGORY-OPERATOR` + `CATEGORY-ASSISTED` + `CATEGORY-POWER`, data = radio,
              family = gaussian)
summary(model1)

model2 <- glm(log(Numbers)~ `CATEGORY-OPERATOR` * `CATEGORY-ASSISTED` +
         `CATEGORY-POWER`, data = radio, family = gaussian)
summary(model2)

drop.terms(model2)

anova(model1, model2, test = 'Chisq')
