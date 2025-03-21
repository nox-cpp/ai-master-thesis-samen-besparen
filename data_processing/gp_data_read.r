load_lib_quietly("tidyr")
# this gives 'glue' syntax '{}' which can be used to convert strings to literals
load_lib_quietly("glue")
load_lib_quietly("stringr")
load_lib_quietly("purrr")

# use a global constant for now
GP_FILE_ENCODING <- "UTF-8"

# Central factor levels, to be used everywhere
GP_BUILD_YEAR_LEVELS <<- c("pre-1930", "1930-1949", "1950-1969", "1970-1999", "post-2000")
GP_TENANCY_LEVELS <<- c("eigenaar", "huurder")
GP_BUILD_STATE_LEVELS <<- c("Als zeer slecht", "Als slecht", "Als matig", "Als redelijk", "Als goed", "Als zeer goed")
GP_ENERGY_LABEL_LEVELS <<- rev(c("A++++", "A+++", "A++", "A+", "A", "B", "C", "D", "E", "F", "G"))
GP_ATTITUDE_LEVELS <<- c("negative", "neutral", "positive")
GP_PRESENT_INT_LEVELS <<- c("ja", "nee")
GP_COMPOSITION_LEVELS <<- c("alleen", "partners", "samengesteld")
GP_FORM_STATUS_LEVELS <<- c("open", "afgesloten")
GP_EDUCATION_LEVELS <<- c("basis", "voortgezet", "middelbaar", "hoger")

# NOTE: duplicated function
args_contain_option <- function(args, target) {
  length(grep(target, args, value = TRUE)) > 0
}

# gets the two column headers
read_col_names <- function(file_path) {
  col_names_1 <- scan(
    file_path,
    nlines = 1,
    what = character(),
    sep = ",",
    skip = 1,
    encoding = GP_FILE_ENCODING
  )
  col_names_2 <- scan(
    file_path,
    nlines = 1,
    what = character(),
    sep = ",",
    skip = 2,
    encoding = GP_FILE_ENCODING
  )
  rbind(col_names_1, col_names_2)
}

# replace empty columns in the header with the last populated values
fill_missing_col_names <- function(col_names_header) {
  for (i in seq_along(col_names_header)) {
    if (col_names_header[i] == "" && i > 1) {
      col_names_header[i] <- col_names_header[i - 1]
    }
  }
  c(col_names_header)
}

# combines the double column headers into a single array of names
combine_col_names_headers <- function(col_names_headers) {
  paste(col_names_headers[1, ], col_names_headers[2, ], sep = "")
}

# read in a dataframe
# skip a single row (contains chapter/section headers)
## set num_rows = -1 to read the whole file
read_gp_data <- function(file_path, column_names, num_rows = 10) {
  read.csv(
    file_path,
    header = FALSE,
    sep = ",",
    nrows = num_rows,
    skip = 3,
    col.names = column_names,
    encoding = GP_FILE_ENCODING,
    colClasses = "character",
    na.strings = ""
  )
}

# store the columns that were combined
# takes as argument the first column header
combined_columns <- function(col_names_header) {
  all_duplicated_names <- unique(
    col_names_header[duplicated(col_names_header)]
  )
  make.names(all_duplicated_names)
}

# combine columns that start with the same name
# argument is a list of target column name prefixes
# keeps the original columns
combine_columns_in_frame <- function(target_prefixes, data) {
  # TODO: make this a function argument
  REMOVE_ORIGINALS <- TRUE
  for (column_name in target_prefixes)
  {
    # the original columns are kept
    data <- data |> unite(
      col = {{ column_name }},
      grep(column_name, colnames(data)),
      na.rm = TRUE,
      sep = "|",
      remove = REMOVE_ORIGINALS
    )
  }
  data
}

# reads the column headers present in the file and fills any missing headers
read_column_headers_and_fill_missing <- function(file_path) {
  # get the headers from file and get the targets for combination
  column_headers <- read_col_names(file_path)
  column_headers[1, ] <- fill_missing_col_names(column_headers[1, ])
  column_headers
}

get_column_names_from_file <- function(file_path) {
  column_headers <- read_column_headers_and_fill_missing(file_path)

  # create column names for frame from the headers
  make.names(combine_col_names_headers(column_headers))
}

load_gp_data <- function(file_path, number_of_lines_to_read = -1) {
  # get the headers from file and get the targets for combination
  column_headers <- read_column_headers_and_fill_missing(file_path)

  # stores which columns have been combined
  combined_headers <- combined_columns(column_headers[1, ])

  # create column names for frame
  column_names <- make.names(combine_col_names_headers(column_headers))

  # read the data file into a frame
  data <- read_gp_data(file_path, column_names, number_of_lines_to_read)

  # combine columns that go together
  data <- combine_columns_in_frame(combined_headers, data)

  # add a column with the file name
  data$file_name <- basename(file_path)

  # return the finished product
  data
}

load_combined_data <- function(input_files, value_mappings, column_mappings, lines_to_read) {
  result <- data.frame()
  # go over all files in the data directory
  for (data_file_name in files_to_read)
  {
    # check if the file is available, log if not
    if (!file.exists(data_file_name)) {
      warning(paste0("Failed to open: ", data_file_name, ". Skipping it."))
    } else {
      # read one of the files
      current_data <- read_gp_data_and_replace(
        file.path(data_file_name),
        column_mapping_data,
        value_mapping_data,
        lines_to_read
      )

      # join the new data with the rest
      result <- bind_rows(result, current_data)
    }
  }
  result
}

# reports true if the data is NA or an empty string
empty_level <- function(data) {
  is.na(data) | data == ""
}

# returns number of available energy labels in data
energy_label_availability <- function(data) {
  data |>
    filter(!empty_level(energielabel)) |>
    summarise(n())
}

# returns number of new energy labels that could be added based on address data
# returns number of available energy labels in data
energy_label_augmentation_availability <- function(data) {
  data |>
    filter(!is.na(postcode) & !is.na(huisnummer) & empty_level(energielabel)) |>
    summarise(n())
}

available_length <- function(data) {
  length(data[!empty_level(data)])
}

# Matches addresses in `data' against those in `energy_label_data' and retrieves
# and updates or adds the energy label in data. Labels are only updated if they
# were collected at an earlier date than the labels registered in `energy_label_data'
# TODO: Conditional update of data, add it as a function parameter to determine whether or not to do it
augment_energylabel_data <- function(data, energy_label_data) {
  # check if there is a possibility for augmentation, otherwise exit early
  if (energy_label_augmentation_availability(data) == 0) {
    return(data)
  }

  # try to do a lookup of the energy labels
  # we need to make sure the postcode format is consistent between sets
  # also make it so the house numbers are of a numerical type
  data <- data |>
    mutate(postcode_upper = toupper(postcode)) |>
    # this effectively strips all non-numerical characters
    mutate(huisnummer = as.integer(gsub("[^0-9]", "", huisnummer))) |>
    left_join(
      # get rid of all the data we don't care about and make sure the energy
      # label columns match
      energy_label_data |>
        select(Postcode, Huisnummer, Energieklasse, Registratiedatum) |>
        rename(energielabel = Energieklasse, energylabeldate = Registratiedatum),
      # match the address
      # NOTE: join_by would be cleaner, but is not available in my environment
      by = c("postcode_upper" = "Postcode", "huisnummer" = "Huisnummer")
    )

  # conditionally take the energielabel from the national set if it is available
  data <- data |>
    mutate(energielabel = energielabel.x)
  # all rows in the original set that are missing their energy label take the one from the national set (if it is available)
  data$energielabel[is.na(data$energielabel.x) | data$energielabel.x == ""] <-
    data$energielabel.y[is.na(data$energielabel.x) | data$energielabel.x == ""]

  return(data)
}

# quick convenience function for processing the categorical data
levels_as_factor <- function(x) {
  levels(as.factor(x))
}

set_factor_levels <- function(dataset) {
  # TODO: check if there is a less clumsy way of setting columns without knowing
  # if they are present or not
  # TODO: consider doing something with the ages (leeftijd) field to standardize
  # the data NOTE: setting the factors implicitly sets values that do not have a
  # proper factor as NA some of these columns may get removed from later
  # iterations of the dataset, so we check for them
  if ("opleidingsniveau" %in% colnames(dataset)) {
    dataset$opleidingsniveau <- factor(dataset$opleidingsniveau,
      levels = GP_EDUCATION_LEVELS,
      ordered = TRUE
    )
  }
  if ("invulstatus_formulier" %in% colnames(dataset)) {
    dataset$invulstatus_formulier <- factor(dataset$invulstatus_formulier, GP_FORM_STATUS_LEVELS)
  }
  # ordering here is from lesser to greater number of individuals
  if ("samenstelling_huishouden" %in% colnames(dataset)) {
    dataset$samenstelling_huishouden <- factor(dataset$samenstelling_huishouden,
      levels = GP_COMPOSITION_LEVELS,
      ordered = TRUE
    )
  }
  if ("staat" %in% colnames(dataset)) {
    dataset$staat <- factor(dataset$staat, levels = GP_BUILD_STATE_LEVELS, ordered = TRUE)
  }
  if ("woonsituatie" %in% colnames(dataset)) {
    dataset$woonsituatie <- factor(dataset$woonsituatie, levels = GP_TENANCY_LEVELS)
  }
  if ("energielabel" %in% colnames(dataset)) {
    dataset$energielabel <- factor(dataset$energielabel, levels = GP_ENERGY_LABEL_LEVELS, ordered = TRUE)
  }
  if ("bouwjaar" %in% colnames(dataset)) {
    dataset$bouwjaar <- factor(dataset$bouwjaar, levels = GP_BUILD_YEAR_LEVELS, ordered = TRUE)
  }
  if ("houding_verduurzaming" %in% colnames(dataset)) {
    dataset$houding_verduurzaming <- factor(dataset$houding_verduurzaming,
      levels = GP_ATTITUDE_LEVELS,
      ordered = TRUE
    )
  }
  if ("aanwezigheid_duurzame_interventies" %in% colnames(dataset)) {
    dataset$aanwezigheid_duurzame_interventies <- factor(
      dataset$aanwezigheid_duurzame_interventies,
      GP_PRESENT_INT_LEVELS
    )
  }
  # TODO: columns that require work:
  # need value mappings and then a factorization
  # dataset$bekend_met_x
  # dataset$soort_woning

  # return the dataset
  dataset
}

get_newest_data_file_path <- function(folder, variant = "") {
  get_newest_data_file_with_base_path(folder, "combined_data", variant)
}

# Given a folder path, returns the newest data file that matches the variant
# Variant is a suffix that is searched for in addition to the combined_data
# prefix.
# The newest file is selected based on the time encoded in the file name, not
# the modification time of the file. Assumes a format of yyyy-mm-dd for the date
get_newest_data_file_with_base_path <- function(folder, base_name, variant = "") {
  # make sure the correct file suffix is present
  if (variant != "" & !(stringr::str_ends(variant, ".csv"))) {
    variant <- stringr::str_c(variant, ".csv")
  }

  data_files_in_folder <- dir(folder)

  # filter all the non-targets
  data_files_in_folder <- data_files_in_folder[stringr::str_ends(data_files_in_folder, ".csv")]
  data_files_in_folder <- data_files_in_folder[stringr::str_starts(data_files_in_folder, base_name)]
  if (variant != "") {
    data_files_in_folder <- data_files_in_folder[stringr::str_ends(data_files_in_folder, variant)]
  }

  # given the data format, we can select the newest file by finding the 'max' on
  # the filtered file list
  # NOTE: uses purrr to shut the max function up
  best_match <- purrr::quietly(max)(data_files_in_folder)$result
  if (is.na(best_match) || best_match == -Inf) {
    return(NA)
  }

  file.path(folder, best_match)
}

# Assuming the correct environment is available, this will read the latest set of data into this session
load_latest_data_from_file <- function() {
  data_folder_path <- Sys.getenv(output_env)

  # for the different variants of the dataset, get the newest each time
  result_path <- get_newest_data_file_path(data_folder_path)
  result_targets_path <- get_newest_data_file_path(data_folder_path, "targets")
  result_levels_path <- get_newest_data_file_path(data_folder_path, "levels")
  result_imputation_path <- get_newest_data_file_path(data_folder_path, "imputed")

  # if we have no files, exit
  if (is.na(result_path) || is.na(result_levels_path) || is.na(result_targets_path)) {
    stop(paste0(
      "Could not find any of the required data files. Either",
      "generate them or otherwise add them to the data folder."
    ))
  }

  print(paste0("Reading data from: ", result_path))

  # load the data into the global environment
  result <<- read.csv(result_path)
  result_targets <<- read.csv(result_targets_path, stringsAsFactors = TRUE)
  result_levels <<- read.csv(result_levels_path, stringsAsFactors = TRUE)
  if (!is.na(result_imputation_path)) {
    result_imputed <<- read.csv(result_imputation_path, stringsAsFactors = TRUE)
    result_imputed <<- set_factor_levels(result_imputed)
  }

  # we need to set the factor levels (they are not available if the data was)
  result <<- set_factor_levels(result)
  result_targets <<- set_factor_levels(result_targets)
  result_levels <<- set_factor_levels(result_levels)
}

# sources the data either from existing files or using the data_collection script
# adds them to the environment as a family of objects named result(_*)
load_data_to_environment <- function(script_dir, read_from_file = FALSE) {
  if (!exists("result_targets") && !exists("result_levels") && !exists("result")) {
    if (read_from_file) {
      load_latest_data_from_file()
    } else {
      # run the data collection script to get our results
      source(file.path(script_dir, "data_collection.r"))
    }
  }
}

# checks the positional arguments to the script to see if the data needs for the
# presence of a flag used to determine if data should be regenerated
read_from_file_option_enabled <- function() {
  regenerate_flag <- "--regenerate"
  if (args_contain_option(commandArgs(trailingOnly = TRUE), regenerate_flag)) {
    return(FALSE)
  }
  TRUE
}

generate_file_name_base_dated <- function(prefix) {
  paste0(prefix, "_", Sys.Date())
}

generate_file_name <- function(prefix, suffix = "", file_ext = ".csv") {
  bridge <- if (suffix != "") "_" else ""
  paste0(prefix, bridge, suffix, file_ext)
}

generate_file_name_dated <- function(prefix, suffix = "", file_ext = ".csv") {
  bridge <- if (suffix != "") "_" else ""
  paste0(generate_file_name_base_dated(prefix), bridge, suffix, file_ext)
}

# takes in a combined (complete) dataset and returns only the columns of
# interest and filters data records that were not complete in their original
# dataset
filter_combined_data <- function(dataset, target_columns) {
  dataset |>
    select(any_of(target_columns), file_name) |>
    filter(invulstatus_formulier != "open")
}

# Creates a folder if it is not yet present, recursively
create_folder_if_needed <- function(file_path) {
  if (dir.exists(file_path)) {
    # folder exists
    return(TRUE)
  }
  # create it
  return(dir.create(file_path, recursive = TRUE))
}
