require(dotenv, quietly = TRUE)

# NOTE: duplicated function
args_contain_option <- function(args, target) {
  length(grep(target, args, value = TRUE)) > 0
}

# looks for the given flag in the command args and returns any value passed with
# the flag 
# e.g. if arg contains "--filter=string" this returns "string"
# if the flag has no value (e.g. --flag) this returns ""
# only returns the last passed value
args_get_value_of_flag <- function(args, target) {
  arg_str <- args[tail(grep(target, args), 1)]
  if (length(arg_str) == 0)
    return("")

  # select the last passed arg that matches and return only the value
  str_replace(arg_str, paste0(target, "="), "")
}

is_empty_string <- function(arg) {
  nchar(arg) == 0
}

root_env <- "GP_ROOT_FOLDER"
script_env <- "GP_SCRIPT_FOLDER"
input_env <- "GP_INPUT_DATA_FOLDER"
output_env <- "GP_OUTPUT_DATA_FOLDER"
plot_env <- "GP_PLOTTING_FOLDER"

is_environment_available <- function() {
  # We need all environments to be set, otherwise we cannot start
  !is_empty_string(base::Sys.getenv(script_env)) &&
    !is_empty_string(base::Sys.getenv(input_env)) &&
    !is_empty_string(base::Sys.getenv(output_env))
}

load_environment_or_stop <- function(env_file_path = NULL) {
  # Check the environment and try to load it if it is not yet available
  if (!is_environment_available()) {
    if (is.null(env_file_path)) {
      if (!is_empty_string(base::Sys.getenv(root_env))) {
        env_file_path <- file.path(Sys.getenv(root_env), ".env")
      } else {
        # assume we are running from the script folder (under the project root)
        env_file_path <- "../.env"
      }
    }
    if (file.exists(env_file_path)) {
      dotenv::load_dot_env(env_file_path)
      if (!is_environment_available()) {
        stop("Script cannot be run, please add environment or environment file.")
      }
    } else {
      warning(
        paste0(
          "Running the scripts requires a valid environment. Either add .env",
          " to the current folder or set the environment variables in the executing",
          "environment. If you expected a .env to be available, check if the .env",
          "is in the current working directory."
        )
      )
      stop("Script cannot be run, please add environment or environment file.")
    }
  }
}

load_environment_if_necessary <- function(env_file_path = NULL) {
  # when this function is called, check the environment and try to set it up if it is not
  if (!is_environment_available()) {
    load_environment_or_stop(env_file_path)
  }
}

# loads a library without any messages (useful for scripts)
load_lib_quietly <- function(lib_name) {
  suppressPackageStartupMessages({
    require(lib_name, character.only = TRUE)
  })
}
