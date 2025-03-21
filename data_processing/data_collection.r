# first, make sure that we are in the correct environment to run the script
script_env <- Sys.getenv("GP_SCRIPT_FOLDER")
if (nchar(script_env) == 0 && !file.exists("environment_setup.r")) {
  stop(paste0(
    "To run this script please navigate to the directory that ",
    "contains 'environment_setup.r' or set the GP_SCRIPT_FOLDER environment",
    "variable"
  ))
  # load the environment setters
} else if (script_env == "") {
  # check for the local directory
  source(file.path("environment_setup.r"))
} else {
  source(file.path(script_env, "environment_setup.r"))
}

# make sure we have a proper environment for this script
load_environment_if_necessary()

# require libs
require(dplyr, quietly = TRUE)
require(tidyr, quietly = TRUE)


script_dir <- Sys.getenv(script_env)
data_dir <- Sys.getenv(input_env)
output_dir <- Sys.getenv(output_env)

# auxiliary scripts
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "experiment_helpers.r"))
source(file.path(script_dir, "data_mapping.r"))

# set environment and define paths
input_data_dir <- file.path(data_dir, "input")
result_dir <- file.path(data_dir, "generated")

replacement_file_name <-
  file.path(data_dir, "processing_config/column_mappings.csv")
replacement_values_file_name <-
  file.path(data_dir, "processing_config/value_mappings.csv")

augment_flag <- "no-augment"
impute_flag <- "impute-na"
perform_augmentation <- TRUE
perform_impute <- FALSE
# augmentation takes a while, it can be skipped if necessary
if (args_contain_option(commandArgs(trailingOnly = TRUE), augment_flag)) {
  print("Skipping augmentation step")
  perform_augmentation <- FALSE
} else {
  print("Performing augmentation step")
}
if (args_contain_option(commandArgs(trailingOnly = TRUE), impute_flag)) {
  print("Performing imputation step")
  perform_impute <- TRUE
} else {
  print("Skipping imputation step")
}

## load the replacement strings, these are used for all input data
column_mapping_data <- REPLACEMENT_DATA_FROM_FILE(replacement_file_name)
value_mapping_data <- REPLACEMENT_VALUES_FROM_FILE(replacement_values_file_name)

# constant that indicates all lines from the data should be read
READ_ALL_LINES <- -1
# parameter dictating how many lines to read from the data files
lines_to_read <- READ_ALL_LINES
files_to_read <- c(dir(input_data_dir, full.names = TRUE))

# read the files and combine the data
result <- load_combined_data(
  files_to_read, value_mapping_data, column_mapping_data, lines_to_read
)

# augment the combined data with additional energy labels if possible
if (perform_augmentation) {
  # load the national energy label dataset
  # NOTE: this is a very big dataset, ensure availability of memory
  energy_label_data_path <-
    file.path(data_dir, "energielabels", "rno_data_set.csv")
  energy_label_data <- read.csv(energy_label_data_path, skip = 2, sep = ";")
  result_augmented <- augment_energylabel_data(result, energy_label_data)
  # a view into only the new fields
  result_filtered <- filter_combined_data(
    result_augmented, column_mapping_data$replacement
  )
} else {
  # a view into only the new fields
  result_filtered <- filter_combined_data(
    result, column_mapping_data$replacement
  )
}

# TODO: Add the dwelling factor to the data

# add the factor levels to the filtered data
result_levels <- set_factor_levels(result_filtered)

# create an additional view containing only the columns used for HUMAT
# initialization
target_columns <- c(
  "id_in_dataset",
  "woonsituatie",
  "energielabel",
  "bouwjaar",
  "aanwezigheid_duurzame_interventies",
  "houding_verduurzaming",
  "file_name"
  # missing: household income
)

# make sure to not accidentally select any of the pre-replacement columns
result_targets <- result_levels |>
  select(all_of(target_columns) & !contains("prerep"))

# optionally perform imputation
if (perform_impute) {
  # NOTE: this also writes the imputed data to file
  source(file.path(script_dir, "data_processing.r"))
}

# write the data to a file (and to archive)
output_file_name_base <- "combined_data"
bulk_data_path <- file.path(
  output_dir, generate_file_name(output_file_name_base)
)
levels_data_path <- file.path(
  output_dir, generate_file_name(output_file_name_base, "levels")
)
augmented_data_path <- file.path(
  output_dir, generate_file_name(output_file_name_base, "augmented")
)
cleaned_data_path <- file.path(
  output_dir, generate_file_name(output_file_name_base, "filtered")
)
humat_target_data_path <- file.path(
  output_dir, generate_file_name(output_file_name_base, "humat_targets")
)

# write the outputs to file
print(paste0("Writing resultant datasets: ", output_dir))
write.csv(result, bulk_data_path, row.names = FALSE)
write.csv(result_levels, levels_data_path, row.names = FALSE)
if (perform_augmentation) {
  write.csv(result_augmented, augmented_data_path, row.names = FALSE)
}
write.csv(result_filtered, cleaned_data_path, row.names = FALSE)
write.csv(result_targets, humat_target_data_path, row.names = FALSE)

# copy the outputs to archive
archive_folder <- file.path(output_dir, "archive", archive_date())
print(paste0("Copying resultant datasets to archive at: ", archive_folder))
create_folder_if_needed(archive_folder)
file.copy(
  from = c(
    bulk_data_path,
    levels_data_path,
    augmented_data_path,
    cleaned_data_path,
    humat_target_data_path
  ),
  to = archive_folder,
  overwrite = TRUE
)
