#' Pipeline for ELECTRICITY challenge
#'
#' This pipeline executes five different models (Reg-Arima, DFM, XGBoost, ETS, 
#' LSTM) that were utilized in the ESA Nowcasting Challenge. The purpose is to 
#' perform nowcasting of the Number of nights spent at tourist accommodation 
#' establishments based on these models. 
#' If the `SAVE_TO_S3` variable is set to TRUE, the submission can be saved in 
#' a S3 bucket.

library(targets)

# Set target options:
tar_option_set(
  packages = c(
    "xts", "lubridate", "dplyr", "tidyr", "data.table",
    "dfms", "jsonlite", "styler", "visNetwork"
  ),
  memory = "transient",
  garbage_collection = TRUE
)
options(dplyr.summarise.inform = FALSE)

# Execute files stored in R/
tar_source(files = "R")

# Saving flag to S3 (TOKEN NEEDED)
SAVE_TO_S3 <- TRUE

# Pipeline
list(
  tar_target(
    name = challenge,
    command = "ELECTRICITY"
  ),
  tar_target(
    name = data_info_file,
    command = "data.yaml",
    format = "file"
  ),
  tar_target(
    name = challenges_file,
    command = "challenges.yaml",
    format = "file"
  ),
  tar_target(
    name = models_file,
    command = "models.yaml",
    format = "file"
  ),
  tar_target(
    name = data_info,
    command = yaml::read_yaml(data_info_file),
  ),
  tar_target(
    name = challenges,
    command = yaml::read_yaml(challenges_file),
  ),
  tar_target(
    name = models,
    command = yaml::read_yaml(models_file),
  ),
  tar_target(
    name = data,
    command = read_data_from_s3(challenges, data_info),
  ),
  tar_target(
    name = regarima_electricity,  # Entry 1
    command = run_regarima(challenge, challenges, data, models)
  ),
  tar_target(
    name = dfms_electricity,  # Entry 2
    command = run_DFMs(challenge, challenges, data, models)
  ),
  tar_target(
    name = ets_electricity,  # Entry 3
    command = run_ETS(challenge, challenges, data, models)
  ),
  tar_target(
    name = xgboost_electricity,  # Entry 4
    command = run_xgboost_per_country(
      data = data,
      config_models = models,
      config_env = challenges,
      challenge = challenge
    )
  ),
  tar_target(
    name = lstm_electricity,  # Entry 5
    command = run_lstm_per_country(
      data = data,
      config_models = models,
      config_env = challenges,
      challenge = challenge
    )
  ),
  tar_target(
    name = predictions_electricity,
    command = bind_rows(list(
      "entry_1" = regarima_electricity$preds %>% mutate(Entries = "REG-ARIMA"),
      "entry_2" = dfms_electricity$preds %>% mutate(Entries = "DFM"),
      "entry_3" = ets_electricity$preds %>% mutate(Entries = "ETS"),
      "entry_4" = xgboost_electricity$preds %>% mutate(Entries = "XGBOOST"),
      "entry_5" = lstm_electricity$preds %>% mutate(Entries = "LSTM")
    ))
  ),
  tar_target(
    name = save_electricity,
    command = save_entries(
      "ELECTRICITY", list(
        "entry_1" = regarima_electricity,
        "entry_2" = dfms_electricity,
        "entry_3" = ets_electricity,
        "entry_4" = xgboost_electricity,
        "entry_5" = lstm_electricity
      ),
      challenges,
      SAVE_TO_S3
    )
  )
)
