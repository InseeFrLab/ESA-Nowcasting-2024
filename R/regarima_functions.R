#########################################
# Building database for Regarima for each country
# (using parameters of the configuration files data.yaml and models.yaml)
#########################################

build_data_regarima <- function(challenge, challenges_info, data, models, country) {
  selected_data <- Filter(function(x) (challenge %in% x$challenge) & ("REGARIMA" %in% x$model), data)

  code_variable_interest <- challenges_info[[challenge]]$principal_code
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)

  # Target variable
  y <- data[[challenge]]$data |>
    dplyr::filter((siec %in% code_variable_interest) & (geo == country)) |>
    tsbox::ts_ts()

  if (models$REGARIMA[[challenge]]$SA) {
    # y <- replace(y, (floor(time(y)) %in% c(2020, 2021)) & is.na(y), 1) # replace NA by 1 during covid period
    # y <- replace(y, (floor(time(y)) %in% c(2020, 2021)) & y == 0, 1) # replace 0 by 1 during covid period

    # specific treatment for challenges with seasonality : seasonal-adjustment of the target series
    result_x13 <- desaiso(y, challenge, models)
    y_sa <- result_x13$final$series[, "sa"]
    # Projected seasonal coefficient at the predicted date : necessary to produce the non-SA prediction
    coef_sa <- window(
      result_x13$final$forecasts[, "sa_f"] / result_x13$final$forecasts[, "y_f"],
      start = c(year(date_to_pred), month(date_to_pred)),
      end = c(year(date_to_pred), month(date_to_pred))
    )
  }

  # Selection of regressors for the model and the country
  X <- create_regressors(challenge, challenges_info, selected_data, models, country)

  # Differenciate the target series
  if (models$REGARIMA[[challenge]]$SA) {
    dy_sa <- window(log(y_sa) - stats::lag(log(y_sa), -1))
    DB <- list("y" = dy_sa, "X" = X, "Historical" = y, "Historical_sa" = y_sa, "coef_sa" = coef_sa)
  } else {
    dy <- window(log(y) - stats::lag(log(y), -1), start = c(2010, 1))
    DB <- list("y" = dy, "X" = X, "Historical" = y)
  }
  return(DB)
}

#########################################
# function for seasonal adjustement of some series
# Using X13 in the RJdemetra package
#########################################

desaiso <- function(serie, challenge, models) {
  specification_sa <- do.call(RJDemetra::x13_spec, models$REGARIMA[[challenge]]$sa_spec)
  serie_sa <- RJDemetra::x13(window(serie, start = c(2014, 1)), specification_sa)
  return(serie_sa)
}

#########################################
# Creation of the regressors list for a given challenge x country occurrence
# Some exceptions (fine-tuning) are authorized to adapt the regressors to countries specificities
#########################################

create_regressors <- function(challenge, challenges_info, data, models, country) {
  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)

  # Defining regressors for challenges
    brent <- reshape_daily_data(data, "Yahoo") |>
      mutate(BRENT = BRENT.Adjusted / EUR_USD.Adjusted) |>
      select(time, BRENT) |>
      tsbox::ts_ts()

    brent_1 <- stats::lag(brent, -1)
    brent_2 <- stats::lag(brent, -2)

    dlbrent <- log(brent) - stats::lag(log(brent), -1)
    dlbrent_1 <- stats::lag(dlbrent, -1)
    dlbrent_2 <- stats::lag(dlbrent, -2)

    # Minimal regressors list for challenges
    X <- ts.union(dlbrent, dlbrent_1, dlbrent_2)

  return(X)
}

#########################################
# Estimation of parameters
#########################################

estimate_regarima <- function(challenge, data, models, country, h) {
  parameters <- c(models$REGARIMA[[challenge]]$estim_spec, list(fcst.horizon = h))

  parameters <- c(parameters, list(usrdef.var = data$X))

  # Fine-tuning on the length of the estimation period (especially due to contraints on availability or the quality)
  # if (challenge == "PPI") {
  #   if (country %in% c("EE")) {
  #     parameters$estimate.from <- "2012-01-01"
  #   }
  #   if (country %in% c("LT")) {
  #     parameters$estimate.from <- "2011-01-01"
  #   }
  #   if (country %in% c("LV")) {
  #     parameters$estimate.from <- "2015-01-01"
  #   }
  # }
  # 
  # if (challenge == "PVI") {
  #   if (country %in% c("DE")) {
  #     parameters$estimate.from <- "2015-01-01"
  #   }
  #   if (country %in% c("AT")) {
  #     parameters$estimate.from <- "2021-06-01"
  #   }
  # }

  # Call of the Regarima (tramo version) function implemented in the JDemetra package
  specification <- do.call(RJDemetra::regarima_spec_tramoseats, parameters)
  regarima <- RJDemetra::regarima(data$y, specification)

  # Exception if estimation gives NA for the prediction
  if (any(is.na(regarima$forecast))) {
    parameters$usrdef.var <- NULL
    parameters$usrdef.varEnabled <- NULL

    specification <- do.call(RJDemetra::regarima_spec_tramoseats, parameters)
    regarima <- RJDemetra::regarima(data$y, specification)
  }

  return(regarima)
}

#########################################
# Calculation of the final prediction
#########################################

run_regarima <- function(challenge, challenges_info, data, models) {
  preds_regarima <- tibble(
    Country = character(),
    Date = as.POSIXct(NA),
    value = numeric()
  )

  resid_regarima <- tibble(
    Country = character(),
    Date = as.POSIXct(NA),
    value = numeric()
  )

  date_to_pred <- ymd(challenges_info$DATES$date_to_pred)

  for (country in challenges_info[[challenge]]$countries) {
    DB <- build_data_regarima(challenge, challenges_info, data, models, country)

    h <- lubridate::interval(last(index(tsbox::ts_xts(DB$y))), date_to_pred) %/% months(1)

    regarima <- estimate_regarima(challenge, DB, models, country, h)

    # The prediction is either derived from predictions (differenciated) applied to the 
    # last observation of our interest variable or=a final correction is applied by 
    # dividing by implicit seasonal coefficient

    if (models$REGARIMA[[challenge]]$SA) {
      pred <- (last(DB$Historical_sa) * prod(exp(regarima$forecast[, 1]))) / DB$coef_sa
    } else {
      pred <- last(DB$Historical) * prod(exp(regarima$forecast[, 1]))
    }

    # Storing the predictions
    preds_regarima <- preds_regarima |>
      add_row(
        Country = country,
        Date = date_to_pred,
        value = round(as.numeric(pred), 1)
      )

    # Storing the residuals
    resid_regarima <- rbind(
      resid_regarima,
      tsbox::ts_xts(DB$Historical - exp(DB$y - resid(regarima)) * (stats::lag(DB$Historical, -1))) |>
        as_tibble() |>
        mutate(
          Date = zoo::index(tsbox::ts_xts(resid(regarima))),
          Country = country
        ) |>
        select(Country, Date, value)
    )
  }
  return(list(
    "preds" = preds_regarima,
    "resids" = resid_regarima
  ))
}
