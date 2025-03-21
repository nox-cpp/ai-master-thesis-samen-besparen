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
source(file.path(script_dir, "experiment_helpers.r"))
source(file.path(script_dir, "data_mapping.r"))
source(file.path(script_dir, "humat_mapping_tools.r"))
source(file.path(script_dir, "parameterized_humat_mapping.r"))
source(file.path(script_dir, "ipf_functions.r"))
source(file.path(script_dir, "analysis_helpers.r"))
source(file.path(script_dir, "bn_generation_main_function.r"))

# if we have a figure output folder, set the script to write to it
if (nchar(figure_dir) != 0) {
  table_dir <- file.path(figure_dir, "tables")
  if (create_folder_if_needed(table_dir)) {
    print(paste0("Writing tables to: ", table_dir))
  }
}

# use the most recent imputed data path
# TODO: check the difference between the imputed an unimputed datasets
data_file_name <- get_newest_data_file_path(
  file.path(data_dir, "generated"), "imputed"
)

plotting_folder <- getwd()
if (!is.na(Sys.getenv(plot_env, unset = NA))) {
  plotting_folder <- Sys.getenv(plot_env)
}

# if we can't find a file, exit early
if (is.na(data_file_name)) {
  stop(
    paste0(
      "Could not find the required base population for IPF. Please generate it",
      " prior to running this script."
    )
  )
}

# report the used source
print(paste0("Reading data from: ", data_file_name))

# get the data from the file and perform some initial processing
source_data <- load_humat_input_set(data_file_name)
zones_of_interest <- load_zones_of_interest(file.path(data_dir, "marginals"))


# perform the generations
generate_bn_populations(
  source_data, zones_of_interest, file.path(data_dir, "generated"),
  table_output_file_suffix = "bn_batch_one",
  population_file_suffix = "bn_batch_one"
)

# perform the second batch
zones_of_interest_two <-
  load_zones_of_interest_batch_two(file.path(data_dir, "marginals"))
generate_bn_populations(
  source_data, zones_of_interest_two, file.path(data_dir, "generated"),
  table_output_file_suffix = "bn_batch_two",
  population_file_suffix = "bn_batch_two"
)

# TODO: copy populations to archive

# generate a plot separately from the populations
bn_seed <- source_data |>
  select(
    houding_verduurzaming,
    woonsituatie,
    aanwezigheid_duurzame_interventies,
    dwelling_factor
  ) |>
  rename(
    tenancy = woonsituatie,
    attitude = houding_verduurzaming,
    interventions = aanwezigheid_duurzame_interventies
  )

# generate white/blacklists
blacklist <- data.frame(
  from = c("interventions", "dwelling_factor"),
  to = c("tenancy", "tenancy")
)

# fit the network structure
fitted <- generate_bn_structure(bn_seed, blacklist)

# visualize the network graph and write it to file
pdf(file.path(plotting_folder, "bayesian_network.pdf"))
graphviz.chart(fitted)
dev.off()

# also add a copy to the archive
archive_name <- generate_file_name("bayesian_network", file_ext = ".pdf")
pdf(file.path(plotting_folder, "archive", archive_name))
graphviz.chart(fitted)
dev.off()
