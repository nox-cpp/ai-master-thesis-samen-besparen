load_lib_quietly("dplyr")
load_lib_quietly("tidyr")

# generates humat importances from normal distributions in the config
generate_humat_importances_from_cfg <- function(humat_input, config) {
  humat_output <- humat_input |> 
    mutate(social_importance = normal_importance_distribution(n(), config$mu_s, config$sigma_s))
  humat_output <- humat_output |> 
    mutate(financial_importance = normal_importance_distribution(n(), config$mu_f, config$sigma_f))
  humat_output <- humat_output |> 
    mutate(comfort_importance = normal_importance_distribution(n(), config$mu_c, config$sigma_c))
  humat_output <- humat_output |> 
    mutate(climate_importance = normal_importance_distribution(n(), config$mu_v, config$sigma_v))
  humat_output
}

# TODO: split this code across alternatives
# NOTE: All these mappers depend on the shape of the input mapping and do not
# generalize to different variables

# Takes in a factor and returns a numerical vector mapping the ordered levels of
# the factor to the values in map. Assumes the map holds enough values to map
# all the levels of the variable
map_factor_to_number_from_cfg <- function(variable, map) {
  map[as.numeric(variable)]
}

# takes in a dual_config vector of 5, where the first 3 items is the mapping for
# the dwelling factor and the second two the mapping for sustainable 
# interventions
generate_motive_from_df_si_cfg <- function(humat_input, sub_config) {
  motive <- map_factor_to_number_from_cfg(humat_input$dwelling_factor, sub_config[1:3])
  motive <- motive + map_factor_to_number_from_cfg(humat_input$aanwezigheid_duurzame_interventies, sub_config[4:5])
  # both variables are weighted equally (weighting is assumed to be part of the
  # factor mapping)
  motive / 2
}

add_financial_motives_from_cfg <- function(humat_input, config) {
  # map the fin motive for a
  a_sub_config <- as.numeric(config |> select(f_df_l_a:f_si_n_a))
  humat_input <- humat_input |> 
    mutate(financial_satisfaction_a = 
             generate_motive_from_df_si_cfg(humat_input, a_sub_config))
  
  # map the fin motive for b (use the mirror of a if symmetrical mappings)
  b_sub_config <- a_sub_config * -1
  if (tolower(config$symm_flag) == "false")  {
    b_sub_config <- as.numeric(config |> select(f_df_l_b:f_si_n_b))
  }
  humat_input <- humat_input |> 
    mutate(financial_satisfaction_b = 
             generate_motive_from_df_si_cfg(humat_input, b_sub_config))
  
  humat_input
}

add_comfort_motives_from_cfg <- function(humat_input, config) {
  # map the fin motive for a
  a_sub_config <- as.numeric(config |> select(c_df_l_a:c_si_n_a))
  humat_input <- humat_input |> 
    mutate(comfort_satisfaction_a = 
             generate_motive_from_df_si_cfg(humat_input, a_sub_config))
  
  # map the fin motive for b (use the mirror of a if symmetrical mappings)
  b_sub_config <- a_sub_config * -1
  if (tolower(config$symm_flag) == "false")  {
    b_sub_config <- as.numeric(config |> select(c_df_l_b:c_si_n_b))
  }
  humat_input <- humat_input |> 
    mutate(comfort_satisfaction_b = 
             generate_motive_from_df_si_cfg(humat_input, b_sub_config))
  humat_input
}

add_climate_motives_from_cfg <- function(humat_input, config) {
  # map the climate motive for a
  a_sub_config <- as.numeric(config |> select(v_l_a:v_h_a))
  humat_input <- humat_input |> 
    mutate(value_satisfaction_a = map_factor_to_number_from_cfg(humat_input$houding_verduurzaming, a_sub_config))
  
  # map the climate motive for b
  b_sub_config <- a_sub_config * -1
  if (tolower(config$symm_flag) == "false")  {
    sub_config <- as.numeric(config |> select(v_l_b:v_h_b))
  }
  humat_input <- humat_input |> 
    mutate(value_satisfaction_b = map_factor_to_number_from_cfg(humat_input$houding_verduurzaming, b_sub_config))
  
  humat_input
}

# generates the motive satisfaction initialization based on the config
generate_humat_motives_from_cfg <- function(humat_input, config) {
  # social satisfaction is overridden later but we set it to a default
  humat_input <- humat_input |>
    mutate(social_satisfaction_a = 0.5) |>
    mutate(social_satisfaction_b = 0.5)
  
  humat_input <- add_financial_motives_from_cfg(humat_input, config)
  humat_input <- add_comfort_motives_from_cfg(humat_input, config)
  humat_input <- add_climate_motives_from_cfg(humat_input, config)
  humat_input
}

generate_standard_humat_basics <- function(humat_input, config) {
  humat_input <- humat_input |> mutate(
    dissonance_tolerance = normal_importance_distribution(n(), config$mu_d, config$sigma_d)
  )
  humat_input <- humat_input |> mutate(
    aspiration_level = normal_importance_distribution(n(), config$mu_a, config$sigma_a)
  )
  humat_input <- humat_input |>
    mutate(selected_alternative = "B")
  
  # set the number of motives (fixed)
  humat_input <- humat_input |> mutate(motive_count = 4)
  
  humat_input
}

# takes in a config (a single named list, i.e. a row from a dataframe)
# and adds HUMAT data based on the input and the config
generate_humat_data_with_cfg <- function(humat_input, config) {
  # generate the motive importances
  humat_input <- generate_humat_importances_from_cfg(humat_input, config)
  
  # map the variables to motives
  humat_input <- generate_humat_motives_from_cfg(humat_input, config)
  
  # generate the dissonance and aspiration values
  humat_input <- generate_standard_humat_basics(humat_input, config)
  
  humat_input
}

# Converts the data to HUMAT attributes and returns only the resultant agents
# using the given mapping configuration for the HUMAT attributes
# Expects all the variables selected in the methodology to be available and
# complete within the dataframe. If this is not the case, defaults may be used
# TODO: comprehensive description
humat_convert_data_to_agents_with_cfg <- function(humat_input, config) {
  # generate the HUMAT attribute columns
  humat_output <- generate_humat_data_with_cfg(humat_input, config)
  
  # filter non-humat parameters
  # NOTE: The following line makes some assumptions about the shape of the humat data
  humat_get_agent_description_data(humat_output)
}