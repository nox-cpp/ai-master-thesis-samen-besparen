# functions that aid in experiment setup

# NOTE: There seems to be a dependency of this on the data_mapping and humat_mapping script file.
# TODO: Evaluate the dependency flow and fix if necessary

load_lib_quietly("dplyr")
load_lib_quietly("tidyr")

default_humat_replacement_path <- file.path(data_dir, "processing_config", "value_mappings_poc.csv")

archive_date <- function() {
  format(Sys.Date(), "%Y-%m-%d")
}

# loads a humat input dataset, sets the factor levels (including replacement!) and generates agent id's
# also generates the dwelling factor if it is not yet present
load_humat_input_set <- function(file_path) {
  # read the base file
  humat_input <- read.csv(file_path)

  # trim the fat
  humat_input <- humat_filter_columns(humat_input)

  # perform general humat factor replacement
  humat_input <- replace_values_in_frame(
    humat_input,
    REPLACEMENT_VALUES_FROM_FILE(default_humat_replacement_path),
    FALSE
  )

  # set the humat factor levels
  humat_input <- set_humat_factor_levels(humat_input)

  # generate agent id's
  humat_input <- humat_generate_agent_ids(humat_input)

  # TODO: filter out the renters

  # if the dwelling factor is not yet added, add it now
  if (!("dwelling_factor" %in% colnames(humat_input))) {
    humat_input$dwelling_factor <-
      dwelling_factor(humat_input$energielabel, humat_input$bouwjaar)
  }

  # return the data object
  humat_input
}


# function that loads a single result population file as exported in some HUMAT
# experiments. This way, the population attributes can be analyzed post-hoc.
load_humat_population_file <- function(file_path, num_lines = -1) {
  read.csv(
    file_path,
    header = TRUE,
    sep = ",",
    nrows = num_lines,
    skip = 0
  )
}

# NOTE: The following population loader functions make assumptions on the file
# name structure that need to be honored in the source data generators (HUMAT Model)

# function that loads the humat population file and augments the data with some
# of the information that is stored in the file name.
# NOTE: makes strong assumptions on the file-name format. For generic use, use
# ``load_humat_population_file``
load_augmented_humat_pop_file <- function(file_name, data_dir, num_lines = -1) {
  population <- load_humat_population_file(file.path(data_dir, file_name), num_lines)
  # regex that retrieves the run nr from the file name and adds it to the data
  run_nr_regex <- "humats_([[:alnum:]]+)_(\\d+)"
  # get the run nr from the file name
  run_nr <- regmatches(file_name, regexec(run_nr_regex, file_name))[[1]][3]
  # get the experiment name from the file name
  exp_name <- regmatches(file_name, regexec(run_nr_regex, file_name))[[1]][2]
  # add the additional data to the table
  population <- population |>
    mutate(file = file_name) |>
    mutate(run_nr = as.integer(run_nr)) |>
    mutate(exp_name = exp_name)
  # return the table
  population
}

# regex to decode the file name
humat_file_filter_regex <- function(experiment_name, suffix = "") {
  paste0("humats_", experiment_name, ".*", suffix)
}

# loads the combined populations of a given named experiment with the given suffix
# being part of the name and returns it as a dataframe
# NOTE: omitting the suffix assumes that all populations in the folder belong together
load_experiment_population <- function(data_dir, experiment_name, suffix = "") {
  # filter the directory contents for our targets
  file_list <- grep(humat_file_filter_regex(experiment_name, suffix), dir(data_dir), ignore.case = TRUE, value = TRUE)
  # get the first target
  if (is.na(file_list[1])) {
    stop("Failed to find any files that match the target")
  }
  population <- load_augmented_humat_pop_file(file_list[1], data_dir)
  # loop over the rest of the files and add them to the table
  for (file_name in file_list[-1]) {
    # combine the tables
    population <- bind_rows(population, load_augmented_humat_pop_file(file_name, data_dir))
  }
  # return the population
  population
}

humat_date_string <- function() {
  date_str <- format(Sys.Date(), "%d-%b-%Y")
}

# Searches in the given data directory for populations that match the current
# date and returns the combined population as a dataframe
load_latest_experiment_population <- function(data_dir, experiment_name) {
  load_experiment_population(data_dir, experiment_name)
}
