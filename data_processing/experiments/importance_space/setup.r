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

# TODO: this experiment setup is almost a duplicate of that of the mapping space
# experiment, factor this into functions or a template file
experiment_name <- "importance_space"

# Loads the scripts that do the reading and replacing
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "data_mapping.r"))
source(file.path(script_dir, "humat_mapping_tools.r"))
source(file.path(script_dir, "experiment_helpers.r"))
source(file.path(script_dir, "parameterized_humat_mapping.r"))
source(file.path(script_dir, "experiments", "mapping_space", "helpers.r"))

# input data folder (base populations)
base_data_dir <- file.path(data_dir, "generated")

# load the most recent IPF dataset as our basis
baseline_ipf_path <- get_newest_data_file_with_base_path(
  file.path(base_data_dir), "Wijert_Comb", "ipf")

# check if the input data is valid
if (is.na(baseline_ipf_path)) {
  stop(paste0("Input data is not available, please run the data collection ",
    "and data processing scripts prior to running this experiment setup.")
  )
}

# load the default dataset
default_set <- load_humat_input_set(baseline_ipf_path)

# filter out renters
default_set <- default_set |> filter(woonsituatie == "eigenaar")

# convert the input set to a HUMAT population using the baseline mapping
default_humats <- humat_convert_data_to_agents(default_set)

# make the output folder if it is not there yet
experiment_dir <- file.path(output_dir, "experiments", experiment_name)
# NOTE: archival copies need to be made elsewhere, as it seems R refuses to
# copy files to paths containing date-format directory names
# archive_dir <- file.path(experiment_dir, "archive", archive_date())
# population_output_dir <- file.path(archive_dir, "populations")

# we make an additional output folder that holds the latest pops and has a
# fixed path (simplifies other scripts, older versions are for dev mostly)
latest_population_output_dir <- file.path(experiment_dir, "populations")

# try to make the folders
if (!create_folder_if_needed(latest_population_output_dir)) {
  stop("Failed to create output directory, exiting")
}

# write the populations to file (to be used by other scripts or model users)
for (path in c(latest_population_output_dir)) {
  # create files for the agent sets
  default_output_path <- file.path(path, "default_agents.csv")

  # report the file paths to the user
  print(paste0("Writing to: ", default_output_path))

  # write to file
  write.csv(default_humats, default_output_path, row.names = FALSE)
}

# sample mapping config location
mapping_cfg_folder <- file.path(data_dir, "humat_meta_config")
mapping_cfg_path <- file.path(mapping_cfg_folder, "sample.csv")

# read the mapping config base
mapping_cfg <- read.csv(mapping_cfg_path)

# TODO: make this an input argument
REGENERATE_MAPPINGS <- TRUE
if (REGENERATE_MAPPINGS) {
  # generate the mappings for the variable inclusion experiment
  mapping_cfg <- generate_mapping_configurations(mapping_cfg)
  # generate the mappings for the importance permutations
  mapping_cfg <- mapping_cfg |>
    bind_rows(generate_importance_mapping_configurations(mapping_cfg[1, ]))
  # generate the mappings for the aspiration level subexperiment
  mapping_cfg <- mapping_cfg |>
    bind_rows(generate_aspiration_mapping_configurations(mapping_cfg[1, ]))
} else {
  # TODO: load a cached version if available, or exit
}

# remove any duplicate conditions (selects one of them using <unique>)
if (sum(duplicated(mapping_cfg$cfg_id)) > 0) {
  print("Removing duplicate mapping entries:")
  print(mapping_cfg$cfg_id[duplicated(mapping_cfg$cfg_id)])
  mapping_cfg <- unique(mapping_cfg)
}

# write all the loaded configurations to disk
mapping_cfg_output <- file.path(
  mapping_cfg_folder, generate_file_name(paste0(experiment_name, "_cfg")))
print(paste0("Writing meta config to: ", mapping_cfg_output))
write.csv(mapping_cfg, mapping_cfg_output, row.names = FALSE)
# copy the mapping to the experiment and the archive
print(paste0("Copying meta config to: ", experiment_dir))
file.copy(from = mapping_cfg_output, to = experiment_dir, overwrite = TRUE)

# check if any filters are enabled (we skip pop-gen for filtered configs)
filter_flag <- "--filter"
if (args_contain_option(commandArgs(trailingOnly = TRUE), filter_flag)) {
  filter_str <- args_get_value_of_flag(
    commandArgs(trailingOnly = TRUE), filter_flag)
  print(paste0("Using filter: ", filter_str))
  mapping_cfg <- mapping_cfg |> filter(grepl(filter_str, cfg_id))
}

# iterate over the mappings and generate a population for each
for (row in rownames(mapping_cfg)) {
  # apply the mapping to the default set
  humat_input_set <- default_set

  # select the mapping config
  humat_config <- mapping_cfg[row, ]

  # generate the agent descriptions
  humat_agents <-
    humat_convert_data_to_agents_with_cfg(humat_input_set, humat_config)

  # write the populations to file (to be used by other scripts or model users)
  for (path in c(latest_population_output_dir)) {
    humat_pop_name <- generate_file_name(humat_config$cfg_id)
    print(paste0("Writing to: ", path, "/", humat_pop_name))
    write.csv(humat_agents, file.path(path, humat_pop_name), row.names = FALSE)
  }
}
