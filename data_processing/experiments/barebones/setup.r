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

# make sure we have a proper environment for this script
load_environment_if_necessary()

script_dir <- Sys.getenv(script_env)
data_dir <- Sys.getenv(input_env)
output_dir <- Sys.getenv(output_env)

# required libs
load_lib_quietly("dplyr")
load_lib_quietly("tidyr")
load_lib_quietly("ggplot2")

# Loads the scripts that do the reading and replacing
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "data_mapping.r"))
source(file.path(script_dir, "humat_mapping_tools.r"))
source(file.path(script_dir, "experiment_helpers.r"))
source(file.path(script_dir, "parameterized_humat_mapping.r"))

# input data folder (base populations)
base_data_dir <- file.path(data_dir, "generated")

# for each of our three base populations, load the most recent one,
baseline_default_path <- get_newest_data_file_path(file.path(base_data_dir), "targets")
baseline_imputed_path <- get_newest_data_file_path(file.path(base_data_dir), "imputed")
baseline_ipf_path <- get_newest_data_file_with_base_path(file.path(base_data_dir), "Wijert_Comb", "ipf")

# check if the input data is valid
if (is.na(baseline_default_path) || is.na(baseline_imputed_path) || is.na(baseline_ipf_path)) {
  stop("Input data is not available, please run the data collection and data processing scripts prior to running this experiment setup.")
}

# load the default dataset
default_set <- load_humat_input_set(baseline_default_path)
# this set has many missing values, so we account for that
default_set <- humat_default_missing_values(default_set)
# convert the input set to a HUMAT population using the baseline mapping
default_humats <- humat_convert_data_to_agents(default_set)

# load the imputed dataset
imputed_set <- load_humat_input_set(baseline_imputed_path)
# this set should be complete, so convert directly to HUMATS
imputed_humats <- humat_convert_data_to_agents(imputed_set)

# load the IPF dataset
ipf_set <- load_humat_input_set(baseline_ipf_path)
# convert to HUMATS
ipf_humats <- humat_convert_data_to_agents(ipf_set)

# make the output folder if it is not there yet
experiment_dir <- file.path(output_dir, "experiments", "barebones")
population_output_dir <- file.path(experiment_dir, "archive", archive_date(), "populations")
# we make an additional output folder that holds the latest pops and has a
# fixed path (simplifies other scripts, older versions are for dev mostly)
latest_population_output_dir <- file.path(experiment_dir, "populations")
if (!create_folder_if_needed(population_output_dir) || !create_folder_if_needed(latest_population_output_dir)) {
  stop("Failed to create output directory, exiting")
}

# write the populations to file (to be used by other scripts or model users)
for (path in c(latest_population_output_dir, population_output_dir)) {
  # create files for the agent sets
  # TODO: this results in invalid agents, see where the bug is
  # default_output_path <- file.path(path, "default_agents.csv")
  imputed_output_path <- file.path(path, "imputed_agents.csv")
  ipf_output_path <- file.path(path, "ipf_agents.csv")

  # report the file paths to the user
  print(paste0("Writing to: ", default_output_path, ", ", imputed_output_path, " and ", ipf_output_path))

  # write to file
  write.csv(default_humats, default_output_path, row.names = FALSE)
  write.csv(imputed_humats, imputed_output_path, row.names = FALSE)
  write.csv(ipf_humats, ipf_output_path, row.names = FALSE)
}
