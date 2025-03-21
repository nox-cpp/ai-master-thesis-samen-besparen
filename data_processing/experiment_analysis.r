# script that reads in the results of an experiment and provides some analysis

# first, make sure that we are in the correct environment to run the script
script_env <- Sys.getenv("GP_SCRIPT_FOLDER")
if (nchar(script_env) == 0 && !file.exists("environment_setup.r")) {
  stop(
    paste0(
      "To run this script please navigate to the directory that ",
      "contains 'environment_setup.r' or set the GP_SCRIPT_FOLDER environment",
      "variable"
    )
  )
  # load the environment setters
} else if (script_env == "") {
  # check for the local directory
  source(file.path("environment_setup.r"))
} else {
  source(file.path(script_env, "environment_setup.r"))
}

load_lib_quietly("dplyr")
load_lib_quietly("ggplot2")

# make sure we have a proper environment for this script
load_environment_if_necessary()
script_dir <- Sys.getenv("GP_SCRIPT_FOLDER")
data_dir <- Sys.getenv("GP_INPUT_DATA_FOLDER")

# load the required functions
source(file.path(script_dir, "behaviourspace_helpers.r"))
source(file.path(script_dir, "experiment_helpers.r"))

# load the data into a table
data_path <- file.path(
  data_dir,
  "experiments",
  "barebones",
  "behaviourspace_output",
  "barebones.csv"
)
bspace <- read_behaviourspace_data(data_path)

# get the final opt-ins per group
optins <- bspace |>
  group_by(agent.file.path, ui.campaign.enabled) |>
  group_by(X.run.number.) |>
  slice_max(order_by = X.step.)

# this plot shows that introducing the campaign mechanism results in a slight
# bias towards more positive evaluations and more agents that do not experience
# a dilemma
optins |> ggplot(aes(x = as.factor(ui.campaign.enabled), y = Average.Population.Evaluation, fill = ui.campaign.enabled)) +
  geom_violin()
optins |> ggplot(aes(x = as.factor(ui.campaign.enabled), y = Non.Dilemma.Count, fill = ui.campaign.enabled)) +
  geom_violin()

# this plot shows the population evaluation average dependent on whether or not the campaign has been enabled or not
bspace |> ggplot(aes(x = X.step., y = Average.Population.Evaluation, color = ui.campaign.enabled)) +
  geom_smooth()

# this plot show the final opt-in rate for the campaign condition
# surprisingly, it seems to show a greater opt-in rate when the campaign is disabled
optins |> ggplot(aes(x = as.factor(ui.campaign.enabled), y = Optin.Count / Household.Count, fill = ui.campaign.enabled)) +
  geom_violin() +
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1))

# a parameter to skip the combined population data collection
COMBINE_POPULATION_OUTPUTS <- TRUE
# a parameter to look for existing combined population data
LOOKUP_POPULATION_OUPUT <- TRUE

# location for population data output from this experiment
population_file_dir <- file.path(data_dir, "experiments", "barebones", "world_states")
# name of the combined population file
combined_file_name <- paste0("humats_barebones_all_", humat_date_string(), ".csv")
combined_file_path <- file.path(population_file_dir, combined_file_name)

# check if we want to do the lookup
if (LOOKUP_POPULATION_OUPUT && file.exists(combined_file_path)) {
    # read the population from the existing file
    population <- read.csv(combined_file_path)
} else if (COMBINE_POPULATION_OUTPUTS)
{
  # load the combined humat population file for all experiment runs
  population <- load_latest_experiment_population(population_file_dir, "barebones")    
  # write it to file for later reuse
  write.csv(population, combined_file_path)
}

# cross-reference the populations with the model run data for source pops
reference_data <- optins |> select(X.run.number., agent.file.path, ui.campaign.enabled)
cross_ref <- left_join(population, reference_data, by = c("run_nr" = "X.run.number."))

# show a summary of the data
print(summary(population))

# print a density plot of the population satisfactions
cross_ref |> group_by(run_nr) |> filter(ui.campaign.enabled == "true") |>  ggplot(aes(x = overall.evaluation)) + geom_density()
