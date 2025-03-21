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

# load the data itself
load_data_to_environment(script_dir, read_from_file_option_enabled())

# there are several libraries that implement KNN, the implementation from the VIM
# package seems to be the most feature rich and robust
load_lib_quietly("VIM")
load_lib_quietly("multiUS")
load_lib_quietly("dplyr")

# get the filtered input dataset for KNN
imputation_target_columns <- c(
  # "leeftijd",
  # "opleidingsniveau",
  # "buurt_dorp",
  "id_in_dataset",
  "energielabel",
  "samenstelling_huishouden",
  "woonsituatie",
  "bouwjaar",
  "houding_verduurzaming",
  "aanwezigheid_duurzame_interventies"
)
imputation_input_set <- result_levels |> select(all_of(imputation_target_columns))

# apply KNN to the filtered dataset
# multiUS::KNNimp has unspecified limitations
# NOTE: KNN generally requires sufficient complete cases, which is likely why this fails
# imputation_output <- KNNimp(imputation_input_set, k = 1, meth = "median", scale = FALSE)

# VIM KNN
# just werks
# NOTE: by default uses imputed data values for distance calculations
imputation_output <- VIM::kNN(imputation_input_set, imp_var = FALSE, useImputedDist = FALSE)

# combine the imputed fields with the existing data and store the result
result_imputed <- result_levels
for (imputed_col in colnames(imputation_output)) {
  result_imputed[imputed_col] <- imputation_output[imputed_col]
}

# generate the output file
data_dir <- Sys.getenv(input_env)
result_dir <- file.path(data_dir, "generated")
output_file_name_base <- generate_file_name("combined_data", suffix = "imputed")
imputation_data_path <- file.path(result_dir, output_file_name_base)
print(paste0("Writing data to ", imputation_data_path))
write.csv(result_imputed, imputation_data_path, row.names = FALSE)
