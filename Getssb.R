source('data_extraction.R')
years = 2019
Getdata(years, 'ssb')

source('data_preprocessing.R')

years = seq(2006, 2018)
  qso_preprocess(year, TRUE)
  radio_preprocess(years, TRUE)  

