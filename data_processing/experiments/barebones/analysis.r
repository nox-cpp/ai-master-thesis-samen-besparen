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
output_dir <- Sys.getenv("GP_OUTPUT_DATA_FOLDER")
data_dir <- Sys.getenv("GP_INPUT_DATA_FOLDER")
figure_dir <- Sys.getenv("GP_PLOTTING_FOLDER")

# load the required functions
source(file.path(script_dir, "behaviourspace_helpers.r"))
source(file.path(script_dir, "experiment_helpers.r"))
source(file.path(script_dir, "gp_data_read.r"))

# if we have a figure output folder, set the script to write to it
if (nchar(figure_dir) != 0) {
  # TODO: put the time-stamped figures in an archive folder
  figure_file <- file.path(figure_dir, paste0("barebones_figures_", archive_date(), ".pdf"))
  print(paste("Writing figures to:", figure_file))
  pdf(file = figure_file)
}

# load the data into a table
experiment_dir <- file.path(output_dir, "experiments", "barebones")
data_path <- file.path(experiment_dir, "barebones.csv")

# get the behaviour space data (and enrich it)
bspace$pop_name <- pop_name(read_behaviourspace_data(data_path))

# get the final opt-ins per group
optins <- subset_run_ends(bspace)

# this plot shows that introducing the campaign mechanism results in a slight
# bias towards more positive evaluations and more agents that do not experience
# a dilemma
print(optins |>
  ggplot(
    aes(
      x = as.factor(ui.campaign.enabled),
      y = Average.Population.Evaluation,
      fill = ui.campaign.enabled
    )
  ) +
  geom_violin())

print(optins |>
  ggplot(
    aes(
      x = as.factor(ui.campaign.enabled),
      y = Non.Dilemma.Count,
      fill = ui.campaign.enabled
    )
  ) +
  geom_violin())

# reusable plot elements
geom_campaign_events <-
  geom_vline(xintercept = c(2, 5, 10))
geom_household_counts <-
  geom_hline(yintercept = as.numeric(unique(bspace$Household.Count)))
n_annotation <-
  paste0(unique(bspace$Household.Count), "_", unique(bspace$pop_name), collapse = "/")

# get one run sample for each unique population
runs <- sample_population_runs(bspace)
run_space <- bspace |> filter(X.run.number. %in% runs)

# this plot shows the population evaluation average dependent on whether or not
# the campaign has been enabled or not.
# it also plots lines that indicate when the campaign events were active
print(bspace |> ggplot(aes(x = X.step., y = Average.Population.Evaluation, )) +
  aes(color = pop_name, linetype = ui.campaign.enabled) +
  geom_smooth() +
  geom_campaign_events +
  xlim(0, 15))

# same for the opt-in
print(bspace |> optin_count_plot_base() +
  aes(linetype = ui.campaign.enabled, color = pop_name) +
  geom_smooth() +
  xlim(0, 15) +
  labs(caption = n_annotation) +
  geom_campaign_events +
  geom_household_counts)

# opt-in ratios
print(bspace |> optin_ratio_plot_base() +
  aes(color = pop_name, linetype = ui.campaign.enabled) +
  geom_smooth() +
  geom_campaign_events +
  labs(caption = n_annotation) +
  xlim(0, 15) +
  ylim(0, 1))

# highlight the population evaluation for single cases
print(run_space |>
  ggplot(
    aes(
      x = X.step.,
      y = Average.Population.Evaluation,
      color = pop_name,
      linetype = ui.campaign.enabled
    )
  ) +
  geom_line() +
  geom_campaign_events)

# same for the opt-in
print(run_space |> optin_count_plot_base() +
  aes(color = pop_name, linetype = ui.campaign.enabled) +
  geom_line() +
  geom_campaign_events +
  xlim(0, 15) +
  labs(caption = n_annotation) +
  geom_household_counts)

# same for the opt-in (ratios)
print(run_space |> optin_ratio_plot_base() +
  aes(color = pop_name, linetype = ui.campaign.enabled) +
  geom_line() +
  geom_campaign_events +
  xlim(0, 15) +
  labs(caption = n_annotation))

# dilemma counts
print(run_space |> dilemma_count_plot() +
  aes(color = pop_name) +
  geom_campaign_events +
  labs(caption = n_annotation) +
  geom_household_counts +
  xlim(0, 5))

# dilemma ratios
print(run_space |> dilemma_ratio_plot() +
  aes(color = pop_name) +
  geom_campaign_events +
  xlim(0, 5) +
  ylim(0, 1) +
  labs(caption = n_annotation))

# this plot show the final opt-in rate for the campaign condition
# surprisingly, it seems to show a greater opt-in rate when the campaign is disabled
print(optins |> ggplot(aes(x = as.factor(ui.campaign.enabled), y = Optin.Count / Household.Count, fill = ui.campaign.enabled)) +
  geom_violin() +
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1)))

# a parameter to skip the combined population data collection
COMBINE_POPULATION_OUTPUTS <- TRUE
# a parameter to look for existing combined population data
LOOKUP_POPULATION_OUPUT <- TRUE

# location for population data output from this experiment
population_file_dir <- file.path(experiment_dir, "world_states")
# name of the combined population file
combined_file_name <- paste0("humats_barebones_all", ".csv")
combined_file_path <- file.path(population_file_dir, combined_file_name)

# check if we want to do the lookup
if (LOOKUP_POPULATION_OUPUT && file.exists(combined_file_path)) {
  # read the population from the existing file
  population <- read.csv(combined_file_path)
} else if (COMBINE_POPULATION_OUTPUTS) {
  # load the combined humat population file for all experiment runs
  population <- load_latest_experiment_population(population_file_dir, "barebones")
  # write it to file for later reuse
  write.csv(population, combined_file_path)
  archive_path <- file.path(
    experiment_dir, "archive", archive_date(), "world_states"
  )
  if (create_folder_if_needed(archive_path)) {
    write.csv(population, file.path(archive_path, combined_file_name))
  } else {
    warning("Failed to write combined population to archive")
  }
}

# get the wide form of a given variable
bspace_wide(bspace, "Optin.Count")

# cross-reference the populations with the model run data for source pops
cross_ref <- cross_ref_pop_bspace(optins, population)

# show a summary of the data
print(summary(population))

# print a density plot of the population satisfactions
print(cross_ref |>
  group_by(run_nr) |>
  filter(ui.campaign.enabled == "true") |>
  ggplot(aes(x = overall.evaluation)) +
  geom_density())

# if we have a figure output folder, set the script to write to it
if (nchar(figure_dir) != 0) {
  print(paste("Wrote figures to:", figure_dir))
  dev.off()
}
