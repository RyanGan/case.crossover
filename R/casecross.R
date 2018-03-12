#' Time-stratified case-crossover
#'
#' Function creates a time-stratified case-crossover dataframe from a case-only
#' dataframe where the outcome on a particular date can be compared to referent
#' periods on the same day of the week.
#' @param data Case-only observations you wish to create referent observations for
#' @param id String of variable of strata identifier. Leave blank if you do not have a known identifier.
#' @param covariate Strings of covariates you'd like to include in the time-stratified dataframe
#' @param date Date of admission. This is also sets the outcome variable to 1
#' @param period Character value based on lubridates floor and ceiling unit. Default is "month".
#' @keywords time-stratified case-crossover, case-crossover
#' @export
#' @examples
#' casecross()
#'
casecross <- function(data, id, date, covariate=F, period = "month"){
  # if id value is given
  if(is.character(id)){
    # vector of ids
    id_vec <- data[,id]
  } else { # else if no value provided, create an id variable
    id_vec <- seq(1, nrow(data), by=1)
  }
  # vector of admit dates joined to the id vector
  event_date <- data[,date]
  id_date <- data.frame(id_vec, event_date)

  # find ref dates
  date_list <- apply(id_date, 1, function(x) {
    # output event date
    event_date <- as.Date(x[2])
    # day of week
    day_week <- wday(event_date)
    # create sequence of dates based on ref period
    date_seq <- seq.Date(floor_date(event_date, unit = period),
                         ceiling_date(event_date, unit = period), "days")
    # find dates on the same day of event date
    ref_dates <- as.character(date_seq[wday(date_seq)==day_week])

    # identifier
    identifier <- rep(x[1], length(ref_dates))
    # outcome
    outcome <- ifelse(as.Date(ref_dates) == x[2],1,0)
    id_date_vec <- cbind(identifier, ref_dates, outcome)
  })
  # bind lists together
  strat_data <- do.call(rbind, date_list)
  # remove row names
  rownames(strat_data) <- NULL
  colnames(strat_data) <- c("id", date, "outcome")
  # convert to dataframe
  strat_data <- data.frame(strat_data)
  # if covariates provided, join to the dataframe
  if(is.character(covariate)){
    cov_data <- as.data.frame(cbind(id_vec, data[,covariate]))
    # names of cov data
    colnames(cov_data) <- c("id", covariate)
    # conver identifier to character to merge
    cov_data$id <- as.character(cov_data$id)
    # merge with ts_data
    strat_data <- merge(strat_data, cov_data, by = "id")
  }
  # return dataframe
  return(strat_data)
} # end function
