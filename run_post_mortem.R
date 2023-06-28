#' Pipeline for Post-Mortem
#'
#' This pipeline is designed to conduct a comprehensive post-mortem analysis of 
#' our previous forecast by performing a series of computations and data 
#' manipulations. Its purpose is to thoroughly evaluate the accuracy and 
#' performance of the forecasted results and provide valuable insights for 
#' retrospective examination.

library(targets)

# Set target options:
tar_option_set(
  packages = c(
    "xts", "lubridate", "dplyr", "tidyr", "data.table", "tibble",
    "jsonlite", "styler", "visNetwork"
  ),
  memory = "transient",
  garbage_collection = TRUE
)
options(dplyr.summarise.inform = FALSE)

# Execute files stored in R/
tar_source(files = "R")

# Pipeline
list(
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
    name = submitted_models_file,
    command = "submitted_models.yaml",
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
    name = data,
    command = read_data_from_s3(challenges, data_info),
  ),
  tar_target(
    name = submitted_models,
    command = yaml::read_yaml(submitted_models_file),
  ),
  tar_target(
    name = past_submissions_gas,
    command = create_table_past_submissions(submitted_models, "GAS", submissions_folder = "Submissions", by_entry = FALSE)
  ),
  tar_target(
    name = past_submissions_gas_by_entry,
    command = create_table_past_submissions(submitted_models, "GAS", submissions_folder = "Submissions", by_entry = TRUE)
  ),
  tar_target(
    name = recent_data_gas,
    command = get_recent_data(data, challenges, submitted_models, "GAS")
  ),
  tar_target(
    name = past_errors_gas,
    command = get_residuals_past_months(past_submissions_gas, recent_data_gas)
  ),
  tar_target(
    name = past_errors_gas_by_entry,
    command = get_residuals_past_months(past_submissions_gas_by_entry, recent_data_gas)
  ),
  tar_target(
    name = past_submissions_oil,
    command = create_table_past_submissions(submitted_models, "OIL", submissions_folder = "Submissions", by_entry = FALSE)
  ),
  tar_target(
    name = past_submissions_oil_by_entry,
    command = create_table_past_submissions(submitted_models, "OIL", submissions_folder = "Submissions", by_entry = TRUE)
  ),
  tar_target(
    name = recent_data_oil,
    command = get_recent_data(data, challenges, submitted_models, "OIL")
  ),
  tar_target(
    name = past_errors_oil,
    command = get_residuals_past_months(past_submissions_oil, recent_data_oil)
  ),
  tar_target(
    name = past_errors_oil_by_entry,
    command = get_residuals_past_months(past_submissions_oil_by_entry, recent_data_oil)
  ),
  tar_target(
    name = past_submissions_electricity,
    command = create_table_past_submissions(submitted_models, "ELECTRICITY", submissions_folder = "Submissions", by_entry = FALSE)
  ),
  tar_target(
    name = past_submissions_electricity_by_entry,
    command = create_table_past_submissions(submitted_models, "ELECTRICITY", submissions_folder = "Submissions", by_entry = TRUE)
  ),
  tar_target(
    name = recent_data_electricity,
    command = get_recent_data(data, challenges, submitted_models, "ELECTRICITY")
  ),
  tar_target(
    name = past_errors_electricity,
    command = get_residuals_past_months(past_submissions_electricity, recent_data_electricity)
  ),
  tar_target(
    name = past_errors_electricity_by_entry,
    command = get_residuals_past_months(past_submissions_electricity_by_entry, recent_data_electricity)
  )
)
