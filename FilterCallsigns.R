
getIndices <- function(qso, radio){
  callsigns <- radio$CALLSIGN
  indices <-  list()
  for (callsign in callsigns){
    indices[[callsign]] = qso[Callsign == callsign, which = TRUE]
  }
  return(indices)
}

filterCallsgn <- function(indices, qso_data, filter_rate = 0.8){
  callsigns = c()
  for (callsign in indices %>% names){
    if (nrow(qso_data[indices[[callsign]]]) <= 0){
      next
    }
    if (((qso_data[indices[[callsign]], duplicated(Freq)] %>% sum) / 
         indices[[callsign]] %>% length()) < filter_rate){
      callsigns = append(callsigns, callsign)
    }
  }
  return(callsigns)
}
