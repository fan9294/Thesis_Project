library(data.table)
library(tidyverse)
library(spatstat)
require(lubridate)
library(wesanderson)


source('GetTime.R')

dat = GetLocalTime(2019, ssb = TRUE)



radio <- dat$radio
qso_data <- dat$qso[!is.na(Arrival) & !is.na(Freq)]
qso_data[, Freq := Freq %>% as.numeric]
qso_data <- qso_data[!is.na(Freq)]
setnames(qso_data, 'Bandwith', 'Band')

bandnames = c('10', '15', '20', '40', '80', '160')
for (name in bandnames){
  assign(paste0('data_to_fit_', name), qso_data[Band == paste0(name, 'm')])
  
}

Getppp_helper <- function(data_to_fit){
  rep1 = data_to_fit
  rep2 = data_to_fit
  rep1$Arrival = rep1$Arrival - 3600*24
  rep2$Arrival = rep2$Arrival + 3600*24
  ret <- rbind(rep1, data_to_fit, rep2)
  return(ret)
}


Getppp <- function(data_to_fit, plot = FALSE, standardize = TRUE, range = c(0, 5)){
  
  factor <- nrow(data_to_fit)
  data_to_fit <- Getppp_helper(data_to_fit)
  #f = densityfun(ppp(x = data_to_fit[,Arrival], y = data_to_fit[, Freq],
  #                   window = owin(xrange = c(0, 24*3600),
  #                                 yrange = c(min(data_to_fit[,Freq]), max(data_to_fit[,Freq])))))
  scaling <- (max(data_to_fit[,Freq]) - min(data_to_fit[,Freq])) / 24
  ret <- ppp(x = data_to_fit[, Arrival] / 3600 * scaling,
             y = data_to_fit[,Freq],
             window = owin(xrange = c(-24* scaling, 48*scaling),
                           yrange = c(min(data_to_fit[,Freq]), max(data_to_fit[,Freq]))))
  model <- density.ppp(ret, kernel = 'gaussian', diggle = TRUE)
  if (standardize){
    model$v <- log(model$v * scaling / factor * 1e5)
  }
  else{
    model$v <- log(model$v * scaling)
  }
  
  len = diff(model$xrange)
  x_min = 1/3 * len
  x_max = 2/3 * len
  
  if (plot){
    plot(model[owin(xrange = c(model$xrange[1] + x_min, model$xrange[1] + x_max), 
                    yrange = model$yrange)], 
         main = '', ribside = 'right', riblab = 'log(Intensity) / 10^5',
         ribsep = 0.05,
         col = colourmap(wes_palette("Zissou1", 100, type = "continuous"), range = c(range[1], range[2])))
    axis(1, at = seq(0, 24*scaling, scaling*2), labels = c(seq(0,24,2)))
    axis(2)
    title(xlab = 'Hour of a day', ylab = 'Frequency (Hz)')
    }
  return(model)
}


model = Getppp(data_to_fit_10, TRUE, TRUE)
mean(exp(model$v))
model = Getppp(data_to_fit_15, TRUE, TRUE)
mean(exp(model$v))
model = Getppp(data_to_fit_20, TRUE, TRUE)
mean(exp(model$v))
model = Getppp(data_to_fit_40, TRUE, TRUE)
mean(exp(model$v))
model = Getppp(data_to_fit_80, TRUE, TRUE)
mean(exp(model$v))
model = Getppp(data_to_fit_160, TRUE, TRUE)
mean(exp(model$v))

dat2 <- GetLocalTime(2019, ssb = FALSE)
radio2 <- dat$radio
qso_data2 <- dat2$qso[!is.na(Arrival) & !is.na(Freq)]
qso_data2[, Freq := Freq %>% as.numeric]
qso_data2 <- qso_data2[!is.na(Freq)]
setnames(qso_data2, 'Bandwith', 'Band')
ggplot(data = qso_data2[Band == '10m'], aes(x =Freq, y=..density..)) +
  geom_histogram(bins = 50, fill='#d1495b', color='black')
ggplot(data = data_to_fit_10, aes(x =Freq, y=..density..)) +
  geom_histogram(bins = 50, fill='#d1495b', color='black')


ggplot(data = data_to_fit_10, aes(x =Freq, y=..density..)) +
  geom_histogram(bins = 50, fill='#d1495b', color='black')
  

