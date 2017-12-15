time.stratified <- function(data, id, covariate=F, admit_date, 
                            start_date, end_date, interval){
  # if id value is given
  if(is.character(id)){
    # vector of ids
    id_vec <- data[,id]
  } else { # else if no value provided, create an id variable
    id_vec <- seq(1, nrow(data), by=1)
  }
  # vector of admit dates joined to the id vector
  admit_date <- data[,admit_date]
  id_date <- data.frame(id_vec, admit_date)
  
  # create list of vectors of referent dates and admission date
  referent_date_list <- apply(id_date, 1, function(x){
    date_prior <- as.character(seq(as.Date(x[2]),as.Date(start_date),
                                   by = (-interval)))
    date_posterior <- as.character(seq(as.Date(x[2]),as.Date(end_date),
                                       by = (interval)))
    date <- sort(unique(c(date_prior, date_posterior)))
    identifier <- rep(x[1], length(date))
    # outcome
    outcome <- ifelse(date %in% x[2], 1, 0)
    id_date_vec <- cbind(identifier, date, outcome)
    return(id_date_vec)
  }) # end apply
  # timestrat dataframe
  ts_data <- do.call(rbind, referent_date_list)
  rownames(ts_data) <- NULL
  ts_data <- as.data.frame(ts_data)
  # if covariates provided, join to the dataframe
  if(is.character(covariate)){
    cov_data <- data[,c(id,covariate)]
    ts_data <- merge(ts_data, cov_data, by.x = "identifier", by.y = id[])
  }
  # return dataframe
  return(ts_data) 
}
