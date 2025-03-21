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


# load the data reading scripts
script_dir <- Sys.getenv(script_env)
source(file.path(script_dir, "gp_data_read.r"))

# read the positional arguments for some settings
read_from_file <- TRUE
regenerate_flag <- "--regenerate"
if (args_contain_option(commandArgs(trailingOnly = TRUE), regenerate_flag)) {
  read_from_file <- FALSE
}

# load the data to this session
load_data_to_environment(script_dir, read_from_file)

# if the reading went alright, start loading the required libraries
# TODO: set library loads to quiet
load_lib_quietly("dplyr")
load_lib_quietly("ggplot2")

# look at the date range of the data
print("Data sourced within time period: ")
print(range(as.Date(result$datum_van_invullen, "%d-%m-%Y")))

# set the path where plots are stored (may be set in environment but not required)
plotting_folder <- getwd()
if (!is.na(Sys.getenv(plot_env, unset = NA))) {
  plotting_folder <- Sys.getenv(plot_env)
}

# code copied from:
# https://stackoverflow.com/questions/8317231/elegant-way-to-report-missing-values-in-a-data-frame
load_lib_quietly("naniar")
load_lib_quietly("UpSetR")

# subset the data to remove the id and source file columns
result_targets_from_data <- result_targets |> select(-id_in_dataset, -file_name)
# upset plot needs to be plotted with generic r functionality
# shows missing values by count and their column interactions
pdf(file.path(plotting_folder, "upset.pdf"))
result_targets_from_data |>
  as_shadow_upset() |>
  upset()
dev.off()

# this plot type visualizes missing column values by record
vis_labeler <- function(name) {
  abbreviate(gsub("_", " ", name))
}

vis_miss_plot <- vis_miss(result_targets_from_data) + scale_x_discrete(labels = vis_labeler)
ggsave(file.path(plotting_folder, "vis_miss.pdf"), vis_miss_plot)

# print a table of the missing rows for each variable
print("Availability of data tabulated")
available_records <- nrow(result_targets_from_data) - colSums(is.na(result_targets_from_data))
print(available_records)
print(available_records / nrow(result_targets_from_data))

# Visualize the availability of postal codes and housenumbers (for energy labels)
energy_label_availability <- result |> select(postcode, huisnummer, energielabel)
vis_miss_plot <- vis_miss(energy_label_availability)
ggsave(file.path(plotting_folder, "vis_miss_elabels.pdf"), vis_miss_plot)

# report on the size of the data set and build year
print("Size of base dataset")
print(result_targets |> summarise(n()))

print("Number of build year data in base dataset")
print(result_targets |> filter(!is.na(bouwjaar)) |> summarise(n()))

# report the number of data records that report the energy label
print("Number of energy labels in base dataset")
print(energy_label_availability |> filter(!is.na(energielabel)) |> summarise(n()))

# Look at which data rows may possibly be augmented with a postal code
print("Upper bound of energy labels that could be added to the dataset by lookup")
print(energy_label_availability |> filter(!is.na(postcode) & !is.na(huisnummer) & is.na(energielabel)) |> summarise(n()))

# Number of records that have either the build year or the energy label information
print("Number of records with build year OR energy label")
print(result_targets |> filter(!is.na(bouwjaar) | !is.na(energielabel)) |> summarise(n()))

# Print and visualize correlations of selected columns in the main dataset
source(file.path(script_dir, "data_correlation.r"))

# correlation summary on combined data
print("Categorical correlations on base")
correlation_summary_on_dataset(result_targets)

# Combine some of the sparser factors and recompute the correlation
# NOTE: this can be done by using the mappings as they are done for the HUMAT conversion
source(file.path(script_dir, "humat_mapping_tools.r"))
# the humat processing combines some of the factors, making it easier to get correlation statistics that are accurate**
data_dir <- "~/thesis_data/"
mappings_file <- file.path(
  data_dir, "processing_config", "value_mappings_poc.csv"
)

# correlation summary on the data after combining certain bins
result_humat_factors <- process_generated_data(result_targets, mappings_file)
print("Categorical correlations after combining factor bins")
correlation_summary_on_dataset(result_humat_factors)

# correlation summary on the data after imputation
if (exists("result_imputed")) {
  print("Categorical correlations after imputing data")
  correlation_summary_on_dataset(result_imputed)
}

# highlight post-hoc observations
## no present interventions but good energy labels
print("Deeper dive into presence of interventions and energy labels")
result_targets |>
  select(energielabel, woonsituatie, bouwjaar, aanwezigheid_duurzame_interventies) |>
  filter(!is.na(energielabel), !is.na(bouwjaar), energielabel > "C") |>
  group_by(aanwezigheid_duurzame_interventies) |>
  count(energielabel, bouwjaar)

# TODO: try to use one-hot encoding (model.matrix) and calculate regular correlation from those
