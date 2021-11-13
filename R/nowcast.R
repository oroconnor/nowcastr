# nowcast function

#' @export
nowcast <- function (
  df # dataframe that contains the past 12 hours of PM observations. First column timestamp. Second column PM2.5
) {

  hourly_avgs <- hour_munge(df) #takes the last 12 hours of data and creates 12 hourly concentration averages

  range <- max(hourly_avgs$PM2.5) - min(hourly_avgs$PM2.5)
  scaled_rate_of_change <- range / max(hourly_avgs$PM2.5)
  weight_factor <- 1 - scaled_rate_of_change
  if (weight_factor < .5)
    weight_factor <- .5


  hourly_avgs_weighted <- hourly_avgs %>%
    mutate(
      PM2.5 = PM2.5 * (weight_factor ^ time_from_recent),
      weights = (weight_factor ^ time_from_recent)
    )

  nowcast_num <- sum(hourly_avgs_weighted$PM2.5) / sum(hourly_avgs_weighted$weights)

  nowcast_num <-  trunc(nowcast_num*10^2)/10^2 # truncate to 2 decimal places


  return(nowcast_num)

}


#' @export
nowcast10 <- function (
  df # dataframe that contains the past 12 hours of PM observations. First column timestamp. Second column PM2.5
) {

  hourly_avgs <- hour_munge(df) #takes the last 12 hours of data and creates 12 hourly concentration averages

  range <- max(hourly_avgs$PM10) - min(hourly_avgs$PM10)
  scaled_rate_of_change <- range / max(hourly_avgs$PM10)
  weight_factor <- 1 - scaled_rate_of_change
  if (weight_factor < .5)
    weight_factor <- .5


  hourly_avgs_weighted <- hourly_avgs %>%
    mutate(
      PM10 = PM10 * (weight_factor ^ time_from_recent),
      weights = (weight_factor ^ time_from_recent)
    )

  nowcast_num10 <- sum(hourly_avgs_weighted$PM10) / sum(hourly_avgs_weighted$weights)

  nowcast_num10 <-  trunc(nowcast_num10*10^2)/10^2 # truncate to 2 decimal places


  return(nowcast_num10)

}
