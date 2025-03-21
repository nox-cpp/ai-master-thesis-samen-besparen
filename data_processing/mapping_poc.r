# This script contains the code to do an initial proof of concept mapping that
# takes real data and uses it to generate agent input for the HUMAT NetLogo
# model

# required libs
load_lib_quietly("dplyr")
load_lib_quietly("tidyr")

script_dir <- "~/thesis_code/data_processing/"
data_dir <- "~/thesis_data/"
mappings_file <- file.path(
  data_dir, "processing_config", "value_mappings_poc.csv"
)

## DEBUGGING ENABLED
# Loads the scripts that do the reading and replacing
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "data_mapping.r"))
source(file.path(script_dir, "humat_mapping_tools.r"))
source(file.path(script_dir, "parameterized_humat_mapping.r"))

# read/generate the source data
# TODO: make it so this uses the most recently generated data instead
source_data_path <- file.path(data_dir, "generated", "filtered_24_07.csv")

# get the data from the file and perform some initial processing
source_data <-
  gp_read_and_process_generated_data(source_data_path, mappings_file)

# generate the agent id's
source_data <- humat_generate_agent_ids(source_data)

# filter rows with too many NA's
na_dropped_data <- source_data |> filter(!(
  is.na(energielabel) &
    is.na(woonsituatie) & is.na(houding_verduurzaming)
))

# after the initial filter, make sure there are no NA fields left
imputed_data <- humat_impute_missing_values(na_dropped_data)

# TODO: (extra) check if the values are consistent with the levels supplied in
# the replacement data. This is to check the consistency of the data with the
# spec

# map the complete data set to HUMAT variables
humat_data <- humat_convert_data_to_agents(imputed_data)

# TODO: (optional) validate values are in range
print(paste0("Remaining NA's in dataset: ", as.character(sum(
  na_count_data(humat_data)
))))

# get the humat agent data required for output
output_data <- humat_data
output_path <- file.path(data_dir, "humat_config", "agent_generation_poc.csv")

# write all the humats to the output file
write.csv(output_data, output_path, row.names = FALSE)
