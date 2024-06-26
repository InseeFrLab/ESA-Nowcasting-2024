#' Pipeline for GAS challenge
#'
#' This pipeline executes five different models (Reg-Arima, DFM, XGBoost, ETS, 
#' LSTM) that were utilized in the ESA Nowcasting Challenge. 
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
    command = "GAS"
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
    name = regarima_gas,  # Entry 1
    command = run_regarima(challenge, challenges, data, models)
  ),
  tar_target(
    name = dfms_gas,  # Entry 2
    command = run_DFMs(challenge, challenges, data, models)
  ),
  tar_target(
    name = ets_gas,  # Entry 3
    command = run_ETS(challenge, challenges, data, models)
  ),
  tar_target(
    name = large_data_xgboost_gas,
    command = build_data_ml(
      data, models, challenges, challenge, "XGBOOST"
    )
  ),
  tar_target(
    name = xgboost_gas,  # Entry 4
    command = train_pred_xgboost_per_country(
      large_data = large_data_xgboost_gas,
      config_models = models,
      config_env = challenges,
      challenge = challenge
    )
  ),
  tar_target(
    name = large_data_gas_saved,
    command = save_large_data(large_data_xgboost_gas, challenges, challenge),
    format = "file"
  ),
  # tar_target(
  #   name = xgboost_gas,  # Entry 4
  #   command = run_xgboost_per_country(
  #     data = data,
  #     config_models = models,
  #     config_env = challenges,
  #     challenge = challenge
  #   )
  # ),
  tar_target(
    name = lstm_gas,  # Entry 5
    command = run_lstm_per_country(
      data = data,
      config_models = models,
      config_env = challenges,
      challenge = challenge
    )
  ),
  tar_target(
    name = predictions_gas,
    command = bind_rows(list(
      "entry_1" = regarima_gas$preds %>% mutate(Entries = "REG-ARIMA"),
      "entry_2" = dfms_gas$preds %>% mutate(Entries = "DFM"),
      "entry_3" = ets_gas$preds %>% mutate(Entries = "ETS"),
      "entry_4" = xgboost_gas$preds %>% mutate(Entries = "XGBOOST"),
      "entry_5" = lstm_gas$preds %>% mutate(Entries = "LSTM")
    ))
  ),
  tar_target(
    name = save_gas,
    command = save_entries(
      challenge, list(
        "entry_1" = regarima_gas,
        "entry_2" = dfms_gas,
        "entry_3" = ets_gas,
        "entry_4" = xgboost_gas,
        "entry_5" = lstm_gas
      ),
      challenges,
      SAVE_TO_S3
    )
  )
)
