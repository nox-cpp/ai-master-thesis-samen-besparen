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

# Loads the scripts that do the reading and replacing
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "data_mapping.r"))
source(file.path(script_dir, "humat_mapping_tools.r"))
source(file.path(script_dir, "experiment_helpers.r"))
source(file.path(script_dir, "parameterized_humat_mapping.r"))

# Generate the IPF and BN population bases
# TODO: perform the generation using setup and zones of choice
source(file.path(script_dir, "ipf_generation.r"))
source(file.path(script_dir, "bn_generation.r"))

experiment_name <- "synth_pops_two"

# Load the humat meta configs (it's the same for both synth. pop. experiments)
meta_cfg_file <- file.path(
  data_dir, "humat_meta_config", "synth_pop_exp_cfg.csv"
)
meta_cfg <- read.csv(meta_cfg_file)

# Load the IPF and BN population bases from file
input_population_dir <- file.path(data_dir, "generated")
input_populations <- grep(
  "(bn_batch_two|ipf_batch_two)", dir(input_population_dir, full.names = TRUE),
  value = TRUE
)

# prepare our output directory
output_population_dir <-
  file.path(
    data_dir, "generated", "experiments", experiment_name, "populations"
  )
if (!create_folder_if_needed(output_population_dir)) {
  stop("Failed to create output directory, exiting")
}

# TODO: some input populations snuck in here that we do not want, check upstream
# where these are coming from and remove them from setup

# Apply the meta configs and generate populations
# TODO: Vectorize this
for (population_file in input_populations) {
  # load the population
  population_base <- load_humat_input_set(population_file)

  # filter out renters
  population_base <- population_base |> filter(woonsituatie == "eigenaar")

  # loop over the variable mapping configs
  for (idx in 1:seq(nrow(meta_cfg))) {
    # convert population to humats
    config <- meta_cfg[idx, ]
    humats <- humat_convert_data_to_agents_with_cfg(population_base, config)

    # write the population to the experiment folder
    humat_output_file_name <-
      generate_file_name(
        paste0(config$cfg_id, "_", basename(population_file)),
        file_ext = ""
      )
    humat_output_path <-
      file.path(output_population_dir, humat_output_file_name)
    print(paste0("Writing to: ", humat_output_path))
    write.csv(humats, humat_output_path, row.names = FALSE)
  }
}

# the populations are archived by the shell script
