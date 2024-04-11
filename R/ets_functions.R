#' ETS Modeling functions
#'
#' This module provides a collection of functions for performing predictions
#' using ETS (Error-Trend-Seasonality) models. ETS models are time series models
#' that capture the underlying components of a time series, including error,
#' trend, and seasonality. The functions in this module facilitate the
#' implementation and evaluation of ETS models for predictive modeling tasks.

build_data_ets <- function(challenge, challenges_info, data_info) {
  code_variable_interest <- challenges_info[[challenge]]$principal_code
  countries <- challenges_info[[challenge]]$countries

  data <- data_info[[challenge]]$data |>
    dplyr::filter((siec %in% code_variable_interest) & (geo %in% countries)) |>
    to_tsibble() |>
    tsibble::fill_gaps() |>
    group_by(geo) |>
    # when missing values they are replaced by the last value
    mutate(values = zoo::na.locf(values)) |>
    ungroup()

  return(data)
}

estimate_ets <- function(challenge, challenges_info, data_info, initial_year, last_year) {
  ets <- build_data_ets(challenge, challenges_info, data_info) |>
    filter((year(time) >= initial_year) & (year(time) < last_year)) |>
    fabletools::model(
      ETS = fable::ETS(values)
    )

  return(ets)
}

run_ETS <- function(challenge, challenges_info, data_info, models_info) {
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)
  parameters <- c(list(challenge = challenge, challenges_info = challenges_info, data_info = data_info), models_info$ETS[[challenge]])
  ets <- do.call(estimate_ets, parameters)

  preds_ets <- ets |>
    # forecast over 12 months and then filter to the expected forecast date
    fabletools::forecast(h = "12 months") |>
    filter(as.Date(time) == date_to_pred) |>
    mutate(
      Country = geo,
      Date = as.Date(time),
      value = round(.mean, 1)
    ) |>
    as_tibble() |>
    select(Country, Date, value)

  resid_ets <- ets |>
    residuals() |>
    mutate(
      Country = geo,
      Date = as.Date(time),
      value = .resid
    ) |>
    as_tibble() |>
    select(Country, Date, value)

  return(list(
    "preds" = preds_ets,
    "resids" = resid_ets
  ))
}
