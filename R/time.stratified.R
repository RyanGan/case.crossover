#' Time-stratified case-crossover
#'
#' Function creates a time-stratified case-crossover dataframe from a case-only
#' dataframe where the outcome on a particular date can be compared to referent
#' periods on the same day of the week.
#' @param data Case-only data you wish to create referent observations for
#' @param id String of variable of strata identifier. Leave blank if you do not have a known identifier.
#' @param covariate Strings of covariates you'd like to include in the time-stratified dataframe
#' @param admit_date Date of admission. This is also sets the outcome variable to 1
#' @param start_date Start date of the period you wish to create referent observations over
#' @param end_date End date of the period you with to create referent observations over
#' @param interval Numberic value. Day == 1, Week = 7, Bi-weekly == 14, etc...
#' @keywords time-stratified case-crossover, case-crossover
#' @export
#' @examples
#' time.stratified()


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
