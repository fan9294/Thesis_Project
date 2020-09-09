# Load the packages
library(data.table)
library(tidyverse)
library(countrycode)
library(lubridate)

# qso_preprocess is the function which preprocesses the data of qso logs.
qso_preprocess <- function(years, ssb = FALSE) {
  for (year in years) {
    # First, read in the datasets.
    if (ssb == TRUE) {
      qso_data <- fread(paste0("qso_data_", year, "_ssb.csv"))
      radio <- fread(paste0("radio_contest_", year, "_ssb.csv"))
    }
    else {
      qso_data <- fread(paste0("qso_data_", year, ".csv"))
      radio <- fread(paste0("radio_contest_", year, ".csv"))
    }
    # Rename some of the columns, and convert some columns to the correct types.
    qso_data[, c("X6", "X9")] <- NULL
    colnames(qso_data) <- c(
      "Freq",
      "Type",
      "Time",
      "Callsign",
      "Location_local",
      "Contact",
      "Location_contact"
    )
    qso_data$Freq <- as.numeric(as.character(qso_data$Freq))
    qso_data$Time <- ymd_hm(qso_data$Time)
    qso_data$Callsign <- as.character(qso_data$Callsign) %>%
      toupper() %>%
      sub(pattern = "-", replacement = "/")
    qso_data$Contact <- as.character(qso_data$Contact) %>%
      toupper() %>%
      sub(pattern = "-", replacement = "/")
    qso_data$Location_local <- as.factor(qso_data$Location_local)
    qso_data$Bandwith <- cut(
      qso_data$Freq,
      breaks = c(
        1799,
        2000,
        3499,
        4000,
        4999,
        6999,
        7300,
        10099,
        10150,
        13999,
        14350,
        18067,
        18168,
        20999,
        21450,
        24889,
        24990,
        27999,
        29700
      ),
      labels = c(
        "160m",
        NA,
        "80m",
        NA,
        "60m",
        "40m",
        NA,
        "30m",
        NA,
        "20m",
        NA,
        "17m",
        NA,
        "15m",
        NA,
        "12m",
        NA,
        "10m"
      )
    )

    # Here, we perform a symmetric check on every row of the qso logs.
    # The goal here is that, for each row we want to see if there is
    # another row in the dataset so that there is a matched pair.
    participants <- radio$CALLSIGN # Extract all the callsigns.
    indices_callsign <-
      list() # Build two indices dictionaries, so for each callsign we
    indices_contact <- list() # know which rows to look at.
    for (item in participants) {
      indices_callsign[[item]] <- which(qso_data$Callsign == item)
    }
    for (item in participants) {
      indices_contact[[item]] <- which(qso_data$Contact == item)
    }
    # Initialise the values for symmetric check
    qso_data[, Qualified := rep(0, nrow(qso_data))]
    qso_data[, Checked := rep(0, nrow(qso_data))]
    # Set a time threshold. There is a matched pair
    # if the time difference is within the time threshold.
    delta_time <- 10

    # Performing the symmetric check
    for (i in 1:nrow(qso_data)) {
      if (qso_data[i, Checked] == 1) {
        next
      }
      caller <- qso_data[i, Callsign] %>% toupper()
      contact <- qso_data[i, Contact] %>% toupper()
      claimed_time <- qso_data[i, Time]
      claimed_bandwith <- qso_data[i, Bandwith]

      indices <-
        intersect(indices_contact[[caller]], indices_callsign[[contact]])
      if (length(indices) > 0) {
        index <-
          indices[which(qso_data[indices][, abs(Time - claimed_time) <= delta_time &
            Bandwith == claimed_bandwith])]
        qso_data[index, Checked := 1]
        qso_data[index, Qualified := length(index)]
        qso_data[i, Qualified := length(index)]
      }
      qso_data[i, Checked := 1]
    }

    # Save the preprocessed dataset.
    if (ssb == TRUE) {
      write.csv(qso_data,
        paste0("qso_data_", year, "_ssb_processed.csv"),
        row.names = FALSE
      )
    }
    else {
      write.csv(qso_data,
        paste0("qso_data_", year, "_processed.csv"),
        row.names = FALSE
      )
    }
  }
}

# radio_preprocess is the function which preprocesses the overall data about the competition.
radio_preprocess <- function(years, ssb = FALSE) {
  for (year in years) {
    # Read in the data, the ITU_CODE.csv helps us to identify the country
    # where each callsign is operating in.
    code <- fread("ITU_CODE.csv", header = TRUE)
    if (ssb == TRUE) {
      radio <- fread(paste0("radio_contest_", year, "_ssb.csv"))
      qso_data <-
        fread(paste0("qso_data_", year, "_ssb_processed.csv"))
    }
    else {
      radio <- fread(paste0("radio_contest_", year, ".csv"))
      qso_data <- fread(paste0("qso_data_", year, "_processed.csv"))
    }


    # Extract the prefix of each callsign, which is related to the country.
    # Then left join with ITU code so we have a country column.
    radio$prefix <-
      radio$CALLSIGN %>% sub(pattern = "(.*\\d)\\d.*", replacement = "\\1")
    radio$prefix <-
      radio$prefix %>% sub(pattern = "(.)\\d..*", replacement = "\\1")
    radio$prefix <-
      radio$prefix %>% sub(pattern = "(.*)-.*", replacement = "\\1")
    radio <- left_join(radio, code, by = c("prefix" = "Prefix"))
    radio$Country_code <- radio$Country_code %>% as.factor()
    radio[is.na(radio$Country_code), "prefix"] <-
      sapply(
        radio[is.na(radio$Country_code), "prefix"],
        FUN = substring,
        first = 1,
        last = 1,
        simplify = TRUE,
        USE.NAMES = FALSE
      )
    radio$Country_code <- NULL
    radio <- left_join(radio, code, by = c("prefix" = "Prefix"))
    radio$Country_code <- radio$Country_code %>% as.factor()
    radio[is.na(radio$Country_code), "prefix"] <-
      sapply(
        radio[is.na(radio$Country_code), "CALLSIGN"],
        FUN = substring,
        first = 1,
        last = 2,
        simplify = TRUE,
        USE.NAMES = FALSE
      )
    radio$Country_code <- NULL
    radio <- left_join(radio, code, by = c("prefix" = "Prefix"))
    radio$Country_code <- radio$Country_code %>% as.factor()

    # Finally, convert the iso2c country code to iso3c standard.
    radio$Country_code_3c <- radio$Country_code %>%
      countrycode(origin = "iso2c", destination = "iso3c") %>%
      as.factor()

    # Left join with pre-processed qso_data so we can see how many valid contacts each callsign has made.
    radio$CALLSIGN <-
      radio$CALLSIGN %>% sub(pattern = "-", replacement = "/")
    qso_data[, Callsign := qso_data$Callsign %>% sub(pattern = "-", replacement = "/")]
    radio <-
      left_join(radio,
        qso_data[Qualified == TRUE, Callsign] %>% table() %>% as.data.table(),
        by = c("CALLSIGN" = ".")
      )
    colnames(radio)[12] <- "Numbers"

    # Save the preprocessed data locally.
    if (ssb == TRUE) {
      write.csv(radio,
        paste0("radio_contest_", year, "_ssb_processed.csv"),
        row.names = FALSE
      )
    }
    else {
      write.csv(radio,
        paste0("radio_contest_", year, "_processed.csv"),
        row.names = FALSE
      )
    }
  }
}
