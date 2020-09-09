GetTimezone <- function(radio_data) {
  # Load the required packages
  require(rvest)
  require(xml2)
  require(maps)
  require(lutz)
  library(data.table)
  library(tidyverse)

  # We first deal with the callsigns in Russia, as there are multiple timezones in Russia.
  radio_RU <- data.table()
  radio_RU[, CALLSIGN := radio_data[Country_code == 'RU', CALLSIGN]]
  radio_RU[, Suffix := sub(CALLSIGN, pattern = '.*(\\d.).*', replacement = '\\1')]
  url <- 'https://en.wikipedia.org/wiki/Call_signs_in_Russia'
  temp <- url %>%
    read_html %>%
    html_nodes("table")
  table_list <- html_table(temp[3:8])
  russia <- table_list %>% reduce(full_join)
  colnames(russia) <- c('Suffix', 'City')
  russia <- left_join(radio_RU, russia, by = c('Suffix' = 'Suffix'))
  data(world.cities)
  russia <-
    left_join(russia, world.cities[world.cities$country.etc == 'Russia', ][c('name', 'lat', 'long')], by = c('City' = 'name'))
  russia <- russia[!duplicated(CALLSIGN), ]
  russia[is.na(lat), lat := russia[City == 'Moscow', lat] %>% unique()]
  russia[is.na(long), long := russia[City == 'Moscow', long] %>% unique()]
  russia[, Timezone := tz_lookup_coords(
    lat = lat,
    lon = long,
    method = 'accurate'
  )]
  russia <- russia[, CALLSIGN, Timezone]


  # Next, Canada
  radio_CA <- radio_data[Country_code == 'CA', .(CALLSIGN)]
  radio_CA[, Prefix := sub(CALLSIGN, pattern = '(*.\\d).*', replacement = '\\1')]
  ca = fread('Canada.csv')
  ca <- left_join(radio_CA, ca, by = c('Prefix' = 'Prefix'))
  ca <-
    left_join(ca, world.cities[world.cities$country.etc == 'Canada', ][c('name', 'lat', 'long')],
      by = c('City' = 'name')
    )
  ca <- ca[!duplicated(CALLSIGN), ]
  ca[is.na(lat), lat := ca[City == 'Toronto', lat] %>% unique()]
  ca[is.na(long), long := ca[City == 'Toronto', long] %>% unique()]
  ca[, Timezone := tz_lookup_coords(
    lat = lat,
    lon = long,
    method = 'accurate'
  )]
  ca <- ca[, CALLSIGN, Timezone]

  # Next, US
  radio_US <- radio_data[Country_code == 'US', CALLSIGN, LOCATION]
  url <- 'https://cqww.com/locations.htm'
  temp <- url %>%
    read_html %>%
    html_nodes("table")
  us <-
    html_table(temp, fill = TRUE)[[1]][, c('LOCATION', 'Description')]
  us <- left_join(radio_US, us, by = c('LOCATION' = 'LOCATION'))
  us_location <- fread('us_location.csv')
  us <-
    left_join(us, us_location[, LOCATION, City], by = c('LOCATION' = 'LOCATION'))
  data("us.cities")
  us <-
    left_join(us, us.cities[c('name', 'lat', 'long')], by = c('City' = 'name'))
  us[is.na(lat)]$lat <-
    world.cities[world.cities$name == 'San Juan' &
      world.cities$country.etc == 'Puerto Rico', 'lat']
  us[is.na(long)]$long <-
    world.cities[world.cities$name == 'San Juan' &
      world.cities$country.etc == 'Puerto Rico', 'long']
  us[, Timezone := tz_lookup_coords(
    lat = lat,
    lon = long,
    method = 'accurate'
  )]
  us <- us[, CALLSIGN, Timezone]

  # Next, Australia
  radio_AU <- data.table()
  radio_AU[, CALLSIGN := radio_data[Country_code == 'AU', CALLSIGN]]
  radio_AU[grepl('.*1.*|.*2.*', CALLSIGN), Timezone := 'Australia/Sydney']
  radio_AU[grepl('.*3.*', CALLSIGN), Timezone := 'Australia/Melbourne']
  radio_AU[grepl('.*4.*', CALLSIGN), Timezone := 'Australia/Brisbane']
  radio_AU[grepl('.*5.*', CALLSIGN), Timezone := 'Australia/Adelaide']
  radio_AU[grepl('.*6.*', CALLSIGN), Timezone := 'Australia/Perth']
  radio_AU[grepl('.*7.*', CALLSIGN), Timezone := 'Australia/Hobart']
  radio_AU[grepl('.*8.*', CALLSIGN), Timezone := 'Australia/Darwin']
  radio_AU[grepl('VK9C.*', CALLSIGN), Timezone := 'Indian/Cocos']
  radio_AU[grepl('VK9N.*', CALLSIGN), Timezone := 'Pacific/Norfolk']
  radio_AU[grepl('VK9X.*', CALLSIGN), Timezone := 'Indian/Christmas']
  radio_AU[grepl('VK9L.*', CALLSIGN), Timezone := 'Australia/Lord_Howe']
  radio_AU[grepl('VK9M.*', CALLSIGN), Timezone := 'Australia/Brisbane']
  au <- radio_AU[, CALLSIGN, Timezone]

  # Next, China
  radio_CN <- data.table()
  radio_CN[, CALLSIGN := radio_data[Country_code == 'CN', CALLSIGN]]
  radio_CN[, Timezone := 'Asia/Shanghai']
  radio_CN[grepl('0', CALLSIGN), Timezone := 'Asia/Urumqi']
  radio_CN[grepl('BV', CALLSIGN), Timezone := 'Asia/Shanghai']
  cn <- radio_CN[, CALLSIGN, Timezone]

  # Finally, all the other countries
  radio_others <- data.table()
  radio_others <-
    radio_data[!Country_code %in% c('CN', 'RU', 'US', 'CA', 'AU')]
  timezone <- fread('zone.tab')[, 1:3]
  timezone <- timezone[!duplicated(`#code`)]
  others <-
    left_join(radio_others, timezone, by = c('Country_code' = '#code'))
  others <- others[, CALLSIGN, TZ]
  colnames(others) <- c('Timezone', 'CALLSIGN')

  return(rbind(russia, us, cn, ca, au, others))
}

GetLocalTime <- function(year, ssb = FALSE) {
  # Load the packages
  require(data.table)
  require(tidyverse)
  require(lubridate)

  # Read in the data of the inputted year
  if (ssb == TRUE) {
    qso_data <- fread(paste0('qso_data_', year, '_ssb_processed.csv'))
    radio <-
      fread(paste0('radio_contest_', year, '_ssb_processed.csv'))
  }
  else {
    qso_data <- fread(paste0('qso_data_', year, '_processed.csv'))
    radio <- fread(paste0('radio_contest_', year, '_processed.csv'))
  }
  # Use GetTimezone function to get the timezones
  timezone <- GetTimezone(radio)
  radio <-
    left_join(radio, timezone, by = c('CALLSIGN' = 'CALLSIGN'))
  qso_data <-
    left_join(qso_data, timezone, by = c('Callsign' = 'CALLSIGN'))
  qso_data <-
    left_join(qso_data, timezone, by = c('Contact' = 'CALLSIGN'))
  setnames(
    qso_data,
    c('Timezone.x', 'Timezone.y'),
    c('Timezone_callsign', 'Timezone_contact')
  )

  # Convert the UTC time to the local time at the corresponding timezones.
  local_time_callsign <-
    force_tzs(qso_data[!is.na(Timezone_callsign), Time] %>% ymd_hms(tz = 'UTC'),
      tzones = qso_data[!is.na(Timezone_callsign), Timezone_callsign]
    )
  local_time_contact <-
    force_tzs(qso_data[!is.na(Timezone_contact), Time] %>% ymd_hms(tz = 'UTC'),
      tzones = qso_data[!is.na(Timezone_contact), Timezone_contact]
    )
  qso_data[!is.na(Timezone_callsign), Localtime_callsign := local_time_callsign]
  qso_data[!is.na(Timezone_contact), Localtime_contact := local_time_contact]

  # Omit the na values
  qso_data[, Time := ymd_hms(Time, tz = 'UTC')]
  qso_data <-
    qso_data[!is.na(Timezone_callsign) & !is.na(Timezone_callsign), ]

  # Create a column called AverageTime, which is the local time at the mid point of between two contacted callsigns.
  qso_data[, AverageTime := mean(c(Localtime_callsign, Localtime_contact)), by = seq_len(nrow(qso_data))]
  qso_data[, AverageTime := AverageTime + runif(nrow(qso_data), min = -30, max = 30)]

  # Convert the AverageTime into pure seconds, which will be used as arrival time later in the
  # Poisson process model.
  qso_data[, Arrival := hour(AverageTime) * 3600 + minute(AverageTime) * 60 + second(AverageTime)]
  x = as.POSIXct(strptime(
    c(
      "050000", "105959", "110000", "155959", "160000",
      "185959"
    ),
    "%H%M%S"
  ))

  # Create another column, Time_of_day, which is a factor of 4 levels,
  # indicating the time of day at the mid point of the contact.
  breaks <-
    lubridate::hour(hm("00:00", "5:00", "11:00", "18:00", "23:59"))
  labels <- c("Night", "Morning", "Afternoon", "Evening")
  qso_data$Time_of_day <-
    cut(
      x = hour(qso_data$AverageTime),
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE
    )
  qso_data[, Time_of_day := factor(Time_of_day,
    levels = c('Morning', "Afternoon", "Evening", 'Night')
  )]
  qso_data[, Bandwith := factor(Bandwith, level = c('10m', '15m', '20m', '40m', '80m', '160m'))]
  qso_data[, Bandwith := as.factor(Bandwith)]

  # Return a list of two newly processed datasets.
  return(list(radio = radio, qso = qso_data))
}
