# Functions that can be used in converting a dataset to humat model input

# NOTE: There seems to be a dependency of this on the data_mapping script file.
# TODO: Evaluate the dependency flow and fix if necessary

load_lib_quietly("dplyr")
load_lib_quietly("tidyr")
load_lib_quietly("EnvStats")

# factors to be used:
# TODO: dwelling factor levels
energielabel_factors <- rev(c("good", "average", "poor"))
bouwjaar_factors <- c("old", "average", "new")
tenancy_factors <- c("huurder", "eigenaar")
interventie_factors <- rev(c("some", "none"))
verduurzamings_factors <- rev(c("positive", "neutral", "negative"))

# filter all the unused columns
humat_target_columns <- c(
  "agent_id",
  "woonsituatie",
  "dwelling_factor",
  "aanwezigheid_duurzame_interventies",
  "houding_verduurzaming",
  # these columns are _maybe_ used
  "energielabel",
  "bouwjaar",
  "id_in_dataset",
  "file_name"
)

# TODO: add factor mapping here, combine into data frame
# TODO: add reading factor mapping from file

# convenience
levels_as_factor <- function(x) {
  levels(as.factor(x))
}

# determine missing values
missing_value <- function(entry) {
  is.na(entry) | entry == ""
}

# check for missing values in the data
na_count_data <- function(data_frame) {
  # NOTE: possibly slow for large data frames
  summarize(data_frame, across(everything(), ~ sum(missing_value(.x))))
}

# generates importance distribution according to specs
normal_importance_distribution <- function(nr_of_observations, mu, sigma) {
  # we allow sigma of 0, but can't use the rnormtrunc function
  if (sigma <= 0) {
    return(rep(max(min(sigma, 1), 0), nr_of_observations))
  }

  rnormTrunc(nr_of_observations, mu, sigma, 0, 1)
}

# sample distribution for value importances (social, financial, comfort,
# climate)
# NOTE: normal distribution centered around 0.5 (the mid-range value)
balanced_importance_distribution <- function(nr_of_observations) {
  rnormTrunc(nr_of_observations, 0.5, 0.15, 0, 1)
}

baseline_importance_major <- function(nr_of_observations) {
  rnormTrunc(nr_of_observations, 0.8, 0.2, 0, 1)
}

baseline_importance_minor <- function(nr_of_observations) {
  rnormTrunc(nr_of_observations, 0.3, 0.2, 0, 1)
}

# applies the humat input factor levels to the dataset
set_humat_factor_levels <- function(dataset) {
  dataset$woonsituatie <- factor(dataset$woonsituatie, tenancy_factors)
  if ("energielabel" %in% colnames(dataset)) {
    dataset$energielabel <-
      factor(dataset$energielabel, energielabel_factors, ordered = TRUE)
  }
  if ("bouwjaar" %in% colnames(dataset)) {
    dataset$bouwjaar <-
      factor(dataset$bouwjaar, bouwjaar_factors, ordered = TRUE)
  }
  if ("dwelling_factor" %in% colnames(dataset)) {
    dataset$dwelling_factor <- factor(
      dataset$dwelling_factor,
      labels = GP_DWELLING_FACTOR_LEVELS, ordered = TRUE
    )
  }
  dataset$houding_verduurzaming <-
    factor(dataset$houding_verduurzaming, verduurzamings_factors, ordered = TRUE)
  dataset$aanwezigheid_duurzame_interventies <-
    factor(dataset$aanwezigheid_duurzame_interventies, interventie_factors, ordered = TRUE)
  dataset
}

# keep only those columns needed for HUMAT parameter computation
humat_filter_columns <- function(data) {
  # get rid of the columns that we don't care about
  data |> select(
    any_of(humat_target_columns) & !contains("prerep")
  )
}

process_generated_data <- function(
  data, mapping_file_path = "", resolve_na = TRUE) {
  # get rid of unneeded columns
  filtered_data <- humat_filter_columns(data)

  # map the data values to buckets
  if (mapping_file_path != "") {
    PRESERVE_DUPLICATES <- FALSE
    mapping_data <- REPLACEMENT_VALUES_FROM_FILE(mapping_file_path)
    filtered_data <- replace_values_in_frame(
      filtered_data, mapping_data, PRESERVE_DUPLICATES
    )
  }

  # set the factors
  refactored_data <- set_humat_factor_levels(filtered_data)

  # if at this point we still have missing values, replace these with defaults
  if (sum(na_count_data(refactored_data)) & resolve_na) {
    print("Missing values found in HUMAT conversion input data")
    print(summary(refactored_data))
    print("Replacing missing values with defaults")
    refactored_data <- humat_default_missing_values(refactored_data)
  }

  # check if the dwelling factor is available, add it if not
  if (!("dwelling_factor" %in% colnames(refactored_data))) {
    refactored_data$dwelling_factor <- dwelling_factor(
      refactored_data$energielabel, refactored_data$bouwjaar
    )
  }

  # return our object
  refactored_data
}

# Function that reads in the data generated by gp_data_read functions and
# returns it as a dataframe. First argument is the data file path and the second
# the file that contains the desired replacement mappings.
# If <resolve_na> is set to TRUE, missing files will be defaulted
gp_read_and_process_generated_data <- function(
  file_path, mapping_file_path = "", resolve_na = TRUE) {
  # read the base file
  source_data <- read.csv(file_path)

  # process the input data
  source_data <- process_generated_data(
    source_data, mapping_file_path, resolve_na)

  source_data
}

# Takes an generated input data set that contains the required source columns
# and generates an additional column with standardized agent id's
humat_generate_agent_ids <- function(source_data) {
  # map identifying data to an agent id
  # TODO: make the id generation happen at a consistent point in processing
  # and set expectations for humat input data
  if (!("agent_id" %in% colnames(source_data))) {
    source_data <- source_data |>
      unite(agent_id, id_in_dataset, file_name, sep = "_", remove = FALSE) |>
      mutate(agent_id = make.names(agent_id))
  }
  source_data
}

# uses simple rules and sampling to replace all missing humat values with valid
# ones.
# NOTE: This function is to be eliminated by imputing data prior to converting
# it to humat inputs
humat_impute_missing_values <- function(humat_data) {
  # for now; just select randomly
  # TODO: supply some (conditional) probabilities for these values
  # energielabel
  imputed_data <- humat_data |>
    mutate(across(energielabel, ~ replace(
      .x, missing_value(.x), sample(energielabel_factors, 1)
    )))
  # bouwjaar
  imputed_data <- imputed_data |>
    mutate(across(bouwjaar, ~ replace(
      .x, missing_value(.x), sample(bouwjaar_factors, 1)
    )))
  # woonsituatie
  imputed_data <- imputed_data |>
    mutate(across(woonsituatie, ~ replace(
      .x, missing_value(.x), sample(tenancy_factors, 1)
    )))
  # aanwezigheid interventies
  imputed_data <- imputed_data |>
    mutate(across(
      aanwezigheid_duurzame_interventies,
      ~ replace(.x, missing_value(.x), sample(interventie_factors, 1))
    ))
  # houding verduurzaming
  imputed_data <- imputed_data |>
    mutate(across(
      houding_verduurzaming,
      ~ replace(.x, missing_value(.x), sample(verduurzamings_factors, 1))
    ))

  imputed_data
}

# replaces humat data missing values with a default value
# @deprecated
humat_default_missing_values <- function(humat_data) {
  # energielabel
  imputed_data <- humat_data |>
    mutate(across(energielabel, ~ replace(
      .x, missing_value(.x), median(energielabel_factors)
    )))
  # bouwjaar
  imputed_data <- imputed_data |>
    mutate(across(bouwjaar, ~ replace(
      .x, missing_value(.x), median(bouwjaar_factors)
    )))
  # woonsituatie
  imputed_data <- imputed_data |>
    mutate(across(woonsituatie, ~ replace(
      .x, missing_value(.x), tail(tenancy_factors, 1)
    )))
  # aanwezigheid interventies
  imputed_data <- imputed_data |>
    mutate(across(
      aanwezigheid_duurzame_interventies,
      ~ replace(.x, missing_value(.x), tail(interventie_factors, 1))
    ))
  # houding verduurzaming
  imputed_data <- imputed_data |>
    mutate(across(
      houding_verduurzaming,
      ~ replace(.x, missing_value(.x), median(verduurzamings_factors))
    ))

  imputed_data
}

# Selects from the humat_data only those fields that are required by the model
# to generate humats in the model
humat_get_agent_description_data <- function(humat_data) {
  # NOTE: this depends on the order of fields being a certain way
  # TODO: Use manual selection, as this is likely to break at some point
  humat_data |>
    select(
      agent_id,
      dissonance_tolerance:motive_count,
      social_importance:climate_importance,
      social_satisfaction_a:value_satisfaction_b
    )
}

# Converts the data to HUMAT attributes and returns only the resultant agents
# Expects all the variables selected in the methodology to be available and
# complete within the dataframe. If this is not the case, defaults may be used
# This version uses a hard-coded baseline mapping
humat_convert_data_to_agents <- function(humat_input) {
  intermediates <- generate_humat_data_with_cfg(
    humat_input, baseline_mapping_config()
  )

  # remove the non-HUMAT data columns
  humat_get_agent_description_data(intermediates)
}

# Converts the data to HUMAT attributes and returns only the resultant agents
# using the given mapping configuration for the HUMAT attributes
# Expects all the variables selected in the methodology to be available and
# complete within the dataframe. If this is not the case, defaults may be used
# TODO: comprehensive description
humat_convert_data_to_agents_with_cfg <- function(humat_input, config) {
  # quick sanity check
  if (nrow(humat_input) == 0) {
    warning("Cannot convert empty base to humat population")
    return(0)
  }
  # generate the HUMAT attribute columns
  humat_output <- generate_humat_data_with_cfg(humat_input, config)

  # filter non-humat parameters
  # NOTE: The following line makes some assumptions about the shape of the humat data
  humat_agents <- humat_get_agent_description_data(humat_output)
}

baseline_mapping_config <- function() {
  header <- c("cfg_id", "mu_a", "sigma_a", "mu_d", "sigma_d", "symm_flag", "mu_f", "sigma_f", "mu_c", "sigma_c", "mu_v", "sigma_v", "mu_s", "sigma_s", "f_df_l_a", "f_df_m_a", "f_df_h_a", "f_si_y_a", "f_si_n_a", "c_df_l_a", "c_df_m_a", "c_df_h_a", "c_si_y_a", "c_si_n_a", "v_l_a", "v_m_a", "v_h_a", "f_df_l_b", "f_df_m_b", "f_df_h_b", "f_si_y_b", "f_si_n_b", "c_df_l_b", "c_df_m_b", "c_df_h_b", "c_si_y_b", "c_si_n_b", "v_l_b", "v_m_b", "v_h_b")
  config <- c("baseline", 0.5, 0.25, 0.5, 0.25, "false", 0.8, 0.2, 0.3, 0.2, 0.3, 0.2, 0.3, 0.2, 1, 0, -1, 1, -1, 1, 0.5, 0, 0.25, 0.75, -1, 0, 1, -1, 0, 1, -1, 1, -1, -0.5, 0, 0, -0.5, 1, 0, -1)

  mapping <- as.list(config)
  names(mapping) <- header
  as.data.frame(mapping, stringsAsFactors = FALSE) |>
    mutate(across(!(cfg_id | symm_flag), as.numeric))
}
