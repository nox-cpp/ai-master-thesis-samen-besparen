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
figure_dir <- Sys.getenv("GP_PLOTTING_FOLDER")
table_dir <- ""

# required libs
load_lib_quietly("dplyr")
load_lib_quietly("tidyr")

# Loads the scripts that do the reading and replacing
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "aggregate_data_read.r"))
source(file.path(script_dir, "data_mapping.r"))
source(file.path(script_dir, "humat_mapping_tools.r"))
source(file.path(script_dir, "parameterized_humat_mapping.r"))
source(file.path(script_dir, "ipf_generation_main_function.r"))

# if we have a figure output folder, set the script to write to it
if (nchar(figure_dir) != 0) {
  table_dir <- file.path(figure_dir, "tables")
  if (create_folder_if_needed(table_dir)) {
    print(paste0("Writing tables to: ", table_dir))
  }
}

# use the most recent imputed data path
data_file_name <- get_newest_data_file_path(
  file.path(data_dir, "generated"), "imputed"
)

# if we can't find a file, exit early
if (is.na(data_file_name)) {
  stop(
    paste0(
      "Could not find the required base population for IPF. Please generate it",
      " prior to running this script (data_processing.sh --impute-na)"
    )
  )
}

# report the used source
print(paste0("Reading data from: ", data_file_name))

# load aggregates (constraints) for the zones of interest
zones_of_interest <- load_zones_of_interest(file.path(data_dir, "marginals"))

# read in the mappings required to go from the raw population data to data that
# can be used for fitting (filtering, renaming columns, factor levels etc.)
mappings_file <- file.path(
  data_dir, "processing_config", "value_mappings_poc.csv"
)

# get the basis population data from disk and perform some initial processing
source_data <- gp_read_and_process_generated_data(data_file_name, mappings_file)

table(source_data |> select(bouwjaar, energielabel))

# read the marginals from file
targets <- zones_of_interest

# generate the ipf populations (in two batches)
generate_ipf_populations(
  source_data,
  targets,
  pop_output_dir = file.path(data_dir, "generated", "humat_config"),
  table_output_file_suffix = "ipf_batch_one",
  population_file_suffix = "ipf_batch_one"
)

# second batch gets separate output tables, otherwise this is the same as above
batch_two_targets <-
  load_zones_of_interest_batch_two(file.path(data_dir, "marginals"))

generate_ipf_populations(
  source_data,
  batch_two_targets,
  pop_output_dir = file.path(data_dir, "generated", "humat_config"),
  table_output_file_suffix = "ipf_batch_two",
  population_file_suffix = "ipf_batch_two"
)

# TODO: write to archive too
