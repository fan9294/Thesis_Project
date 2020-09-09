# Load the packages
library(tidyverse)
library(xml2)
library(rvest)
library(XML)
library(httr)



Getdata <- function(years, ssb = FALSE) {
  for (year in years) {
    # First, create the URL link of the data.
    if (ssb == FALSE) {
      URL <- paste0("https://cqww.com/publiclogs/", year, "cw/")
    }
    else {
      URL <- paste0("https://cqww.com/publiclogs/", year, "ph/")
    }

    # Then extract all the participants of that year
    participants <-
      html_attr(html_nodes(read_html(URL), "a"), "href")
    participants <- participants[grepl("\\.log", participants)]

    # Initialise the dataframe, radio_contest and qso_log.
    col_names <- c(
      "CALLSIGN",
      "CATEGORY-OPERATOR",
      "CATEGORY-ASSISTED",
      "CATEGORY-POWER",
      "CLAIMED-SCORE",
      "CATEGORY-MODE",
      "CATEGORY-TRANSMITTER",
      "LOCATION"
    )
    radio_contest <-
      data.frame(matrix(nrow = 0, ncol = length(col_names)))
    colnames(radio_contest) <- col_names
    qso_log <- list()

    # For each participant, extract relevant information
    for (item in participants) {
      # First extract the overall information for each participant
      # Read all the lines of that web page
      raw <- readLines(paste0(URL, item)) %>% toupper()
      # Increase the row number by 1
      radio_contest[nrow(radio_contest) + 1, ] <-
        rep(NA, ncol(radio_contest))
      # Some processing on the extracted text
      for (name in col_names) {
        entry <- raw[grepl(name, raw)] %>%
          sub(pattern = ".*:", replacement = "") %>%
          trimws() %>%
          toupper()
        if (length(entry) > 0) {
          radio_contest[nrow(radio_contest), name] <- entry[1]
        }
      }

      # Then extract the qso logs
      qsos <- raw[grepl("QSO:.*\\d", raw)] %>%
        sub(pattern = ".*QSO:(.*)", replacement = "\\1") %>%
        gsub(pattern = "\\s+", replacement = " ") %>%
        trimws(which = "left") %>%
        strsplit(split = " ") %>%
        lapply("[", c(1:10))
      qso_log <- c(qso_log, qsos)
    }

    # Save the radio_contest locally.
    if (ssb == TRUE) {
      write.csv(radio_contest,
        paste0("radio_contest_", year, "_ssb.csv"),
        row.names = FALSE
      )
    }
    else {
      write.csv(radio_contest,
        paste0("radio_contest_", year, ".csv"),
        row.names = FALSE
      )
    }
    # Save the qso_data locally.
    qso_data <-
      data.frame(matrix(unlist(qso_log), ncol = 10, byrow = TRUE))
    qso_data$X3 <- paste(qso_data$X3, qso_data$X4)
    qso_data$X4 <- NULL
    if (ssb == TRUE) {
      write.csv(qso_data,
        paste0("qso_data_", year, "_ssb.csv"),
        row.names = FALSE
      )
    }
    else {
      write.csv(qso_data,
        paste0("qso_data_", year, ".csv"),
        row.names = FALSE
      )
    }
  }
}
