# first, make sure that we are in the correct environment to run the script
script_env <- Sys.getenv("GP_SCRIPT_FOLDER")
if (nchar(script_env) == 0 && !file.exists("environment_setup.r")) {
  stop(paste0(
    "To run this script please navigate to the directory that ",
    "contains 'environment_setup.r' or set the GP_SCRIPT_FOLDER environment",
    "variable"
  ))
}

# load the environment setters
if (script_env == "") {
  # check for the local directory
  source(file.path("environment_setup.r"))
} else {
  source(file.path(script_env, "environment_setup.r"))
}


script_dir <- Sys.getenv(script_env)
data_dir <- Sys.getenv(input_env)

# load the data reading scripts
source(file.path(script_dir, "gp_data_read.r"))

# read the positional arguments for some settings
read_from_file <- TRUE
regenerate_flag <- "--regenerate"
if (args_contain_option(commandArgs(trailingOnly = TRUE), regenerate_flag)) {
  read_from_file <- FALSE
}

# load the data to this session
load_data_to_environment(script_dir, read_from_file)

# load the columns that we account for
expected_columns <- read.csv(file.path(data_dir, "processing_config", "columns.csv"))$column

# list all the columns that are not replaced in the large result table
print(colnames(select(result, !(
  contains("_orig") |
    contains("_prerep") | any_of(expected_columns)
))))
