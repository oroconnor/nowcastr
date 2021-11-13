# hour_munge function


#takes the last 12 hours of data and creates 12 hourly concentration averages
hour_munge <- function (
  df # dataframe that contains the past 12 hours of PM observations. First column timestamp. Second column PM2.5,
  #third column PM10
  ) {

  most_recent_time <- max(df$time) # calculate most recent time in dataset
  twelve_hours_ago <- most_recent_time - hours(12) # calculate 12 hours before that

  df <- df %>%
    filter(
      time >= twelve_hours_ago
    ) %>%
    mutate(
      time_from_recent = floor(as.numeric(as.duration(most_recent_time-time), "hours"))
    )

  #Round values on the edge that are 12 hours down into the "11th hour"
  df$time_from_recent[df$time_from_recent == 12] <- 11

  hourly_avgs <- df %>%
    group_by(
      time_from_recent
    ) %>%
    summarise(
      PM2.5 = mean(PM2.5,na.rm = TRUE),
      PM10 = mean(PM10,na.rm = TRUE),
    )

  return(hourly_avgs)
}
