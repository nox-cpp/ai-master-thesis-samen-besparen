load_lib_quietly("dplyr")
# this gives 'glue' syntax '{}' which can be used to convert strings to literals
load_lib_quietly("glue")

GP_DWELLING_FACTOR_LEVELS <<- c("low", "medium", "high")

# reads in a file where each row corresponds to a target column name and its
# replacement. A replacement may occur multiple times, indicating that columns
# with that target will be combined into one.
REPLACEMENT_DATA_FROM_FILE <- function(replacement_file_path) {
  # try using the targets/replacements from file
  framepy <- data.frame(read.csv(
    replacement_file_path,
    header = TRUE,
    strip.white = TRUE
  ))

  # ensure a proper format for the replacements and targets
  framepy$replacement <- make.names(framepy$replacement)
  framepy$targets <- make.names(framepy$targets)

  framepy
}

# for each unique replacement, collapses all targets into a single regex lookup
unique_target_lookups <- function(replacement_data) {
  replacement_data |>
    group_by(replacement) |>
    mutate(collapsed_string = paste(targets, collapse = "|")) |>
    select(replacement, collapsed_string) |>
    unique()
}

# returns a list of vectors with the indices of the matched targets for each
# replacement lookup string
lookup_target_columns <- function(target_lookups, columns) {
  lapply(target_lookups, grep, columns)
}

# deprecated
get_replacement_col_names <- function(column_names, replacement_data) {
  # collapse the replacement lookup into single regexes
  unique_replacements <- unique_target_lookups(replacement_data)

  # adds a column containing the indices of each target hit
  unique_replacements$matches <-
    lookup_target_columns(unique_replacements$collapsed_string, column_names)

  # loop over all the replacements entries and perform the replacements of the
  # column names.
  ## TODO: Find a way to vectorize this
  for (entry in 1:nrow(unique_replacements))
  {
    # look for matching columns for each unique replacement
    # collect the indices we want to replace
    matches <- unlist(unique_replacements$matches[entry])
    # replace all matched names with the replacement
    column_names <-
      replace(
        column_names,
        as.vector(matches),
        unique_replacements$replacement[entry]
      )
  }

  # make sure the names are valid (no duplicates)
  make.names(column_names, unique = TRUE)
}

# replaces the column names mentioned in the replacement data in-place
# deprecated
replace_col_names_in_frame <- function(data, replacement_data) {
  setNames(
    data,
    get_replacement_col_names(colnames(data), replacement_data)
  )
}

# creates duplicates of the columns listed in the replacement data with the
# target names
replace_add_col_names_in_frame <- function(data, replacement_data) {
  # collapse the replacement data to unique replacements
  replacement_lookups <- unique_target_lookups(replacement_data)

  # get the column indices of the targets in the data
  replacement_lookups$matches <-
    lookup_target_columns(replacement_lookups$collapsed_string, colnames(data))

  # duplicate each matched row and replace the original
  # TODO: Find a way to vectorize this
  for (entry in 1:nrow(replacement_lookups))
  {
    # collect the indices we want to replace
    matches <- unlist(replacement_lookups$matches[entry])

    # matched column names
    matched_names <- as.list(colnames(data)[as.vector(matches)])

    # replacement name
    replacement_name <- replacement_lookups$replacement[entry]

    # it might be that we get an empty string back in the matches
    if (length(matched_names) > 0 && length(matched_names[[1]]) > 0) {
      # duplicate the columns that are matched with a new name
      for (inner_entry in matched_names)
      {
        new_name <- paste0(inner_entry, "_orig")
        data <- duplicate_column_with_name(data, inner_entry, new_name)
      }

      # replace all matched names with the replacement
      colnames(data) <-
        make.names(
          replace(
            colnames(data),
            as.vector(matches),
            replacement_name
          ),
          # make sure the names are valid (no duplicates)
          unique = TRUE
        )
    }
  }

  # TODO: make this an argument
  # only keep the first column (combined) with the replacement header
  KEEP_ALL <- FALSE
  if (!KEEP_ALL) {
    # we loop over the column names again to reduce duplication somewhat. We get
    # rid of all the replacement columns except one, which should hold the
    # combination of all columns starting with the same name (we don't do it in
    # the previous loop because it would invalidate the indices retrieved
    # earlier)
    # TODO: this is not a very nice function, make it look better
    for (entry in 1:nrow(replacement_lookups))
    {
      # only do this for columns we actually matched
      replacement_name <- replacement_lookups$replacement[entry]
      if (replacement_name %in% colnames(data)) {
        # this removes all columns that match the replacement name, except the
        # one that matches it exactly this assumes that the combined column is
        # in front, and the source columns follow it
        data <- data |>
          select({{ replacement_name }}, (everything() & !matches(replacement_name)))
      }
    }
  }

  data
}

REPLACEMENT_VALUES_FROM_FILE <- function(replacement_file_path) {
  data.frame(read.csv(
    replacement_file_path,
    header = TRUE,
    strip.white = TRUE
  ))
}

# returns the mutated data column (vector) with all targets replaced by the replacement string
# replaces entries only in the given column
# We use fixed matching, no regex
replace_value_in_column <- function(data_column, target, replacement) {
  # if the column is a factor, we need to make sure the replacement is available as a factor
  if (is.factor(data_column)) {
    # if the replacement is not a factor, add it as a factor
    if (!(replacement %in% levels(data_column))) {
      levels(data_column) <- c(levels(data_column), replacement)
    }
  }
  data_column[grep(target, data_column, fixed = TRUE)] <- replacement
  data_column
}

# returns the mutated dataframe with all targets in the named column replaced by the replacement string
# replaces entries only in the given column.
# if no column is given, then replacement is applied in all columns
replace_value_in_column_with_name <- function(data, target, replacement, column) {
  # if no column is specified, apply replacement to all columns and return
  if (is.na(column) | column == "") {
    return(replace_value_in_frame(data, target, replacement))
  }

  # check if the column is in the data, otherwise return early
  if (!(column %in% colnames(data))) {
    return(data)
  }

  # we do have a specified column, apply only to that
  data[[column]] <- replace_value_in_column(data[[column]], target, replacement)
  data
}

# returns the mutated dataframe with the values in the named column replaced
# according to the replacement_data
# replacement data is expected in the form of a map of replacements to factors
replace_values_in_column_with_name <- function(data, targets, replacements, column) {
  # TODO: Vectorize this
  for (entry in seq_len(length(replacements))) {
    data <- replace_value_in_column_with_name(
      data,
      targets[entry],
      replacements[entry],
      column
    )
  }
  data
}

# returns the mutated dataframe with the values in the named column replaced
# according to the replacement_data
# replacement data is expected in the form of a map of replacements to factors
map_column_to_new_column <- function(data, targets, replacements, column, new_column) {
  # simplest way I can think of
  data[[new_column]] <- data[[column]]
  data <- replace_values_in_column_with_name(data, targets, replacements, new_column)
  data
}

# takes a column and returns a new one where all targets are replaced with the
# matching replacement
map_column_to_vector <- function(column, targets, replacements) {
  # simplest way I can think of
  # NOTE: We convert to character data here, as otherwise the results might be
  # unexpected if the input data is a factor
  result <- as.character(column)
  for (entry in seq_len(length(targets))) {
    result <- replace_value_in_column(result, targets[entry], replacements[entry])
  }
  result
}

# returns the mutated dataframe with all targets replaced by the replacement
# replaces entries in any column
replace_value_in_frame <- function(data, target, replacement) {
  data <- data |> mutate(across(
    everything(),
    ~ replace_value_in_column(.x, target, replacement)
  ))
  data
}

# duplicates the column with name `target' and sets `replacement' as its name
duplicate_column_with_name <- function(data, target, replacement) {
  # unusual syntax, but it duplicates a column with the `replacement' header
  # the := is called the interpolation syntax, from the glue package
  # the rhs is the data masking format suggested in the dplyr docs
  mutate(data, "{replacement}" := .data[[target]])
}

duplicate_replacement_targets <- function(data, replacement_data) {
  # get all unique replacement targets
  targets <- unique(replacement_data$column)

  # iterate over the target columns
  # TODO: vectorize this
  for (entry in targets)
  {
    if (entry %in% colnames(data)) {
      new_name <- paste0(entry, "_prerep")
      data <- duplicate_column_with_name(data, entry, new_name)
    }
  }

  data
}

# returns the mutated dataframe with all targets replaced by the replacement
replace_values_in_frame <- function(data, replacement_data, preserve_duplicates = TRUE) {
  # first, duplicate all columns that are targeted for replacement
  if (preserve_duplicates) {
    data <- duplicate_replacement_targets(data, replacement_data)
  }

  # TODO: Vectorize this
  for (entry in seq_len(nrow(replacement_data)))
  {
    data <-
      replace_value_in_column_with_name(
        data,
        replacement_data$target[entry],
        replacement_data$replacement[entry],
        replacement_data$column[entry]
      )
  }
  data
}

read_gp_data_and_process_columns <- function(data_file_name,
                                             replacement_data,
                                             number_of_lines_to_read) {
  # load one of the files
  current_data <- load_gp_data(data_file_name, number_of_lines_to_read)

  # duplicate targeted columns with new name
  current_data <- replace_add_col_names_in_frame(current_data, replacement_data)

  # we now may have duplicate columns, merge them and return the data frame
  combine_columns_in_frame(combined_columns(colnames(current_data)), current_data)
}

# reads the data from a GP source data file and replaces column names and data
# values given the spec in the replacement data. Will keep the original columns
# that are present in the replacement data as _orig
read_gp_data_and_replace <- function(data_file_name,
                                     column_replacement_data,
                                     value_replacement_data,
                                     number_of_lines_to_read) {
  # read in the data and process its columns
  current_data <- read_gp_data_and_process_columns(data_file_name, column_replacement_data, lines_to_read)

  # replace selected values in the data (original values will be preserved in new columns)
  current_data <- replace_values_in_frame(current_data, value_replacement_data)

  # return the data
  current_data
}

# Report the dwelling factor given the build year and energy label (post-replacement)
# NOTE: this requires the energy label and build year factors to be set correctly
# NOTE: the order of the arguments does not actually matter
dwelling_factor <- function(energy_label, build_year) {
  # check for invalid input
  if (length(build_year) != length(energy_label))
    error("Vector sizes do not match")
  
  # initialize the vector
  result <- rep(NA, length(energy_label))
  
  # if it's present in both, set it to the more extreme value, or the middle value
  result[!is.na(build_year) & !is.na(energy_label)] <- as.numeric(build_year[!is.na(build_year) & !is.na(energy_label)]) + as.numeric(energy_label[!is.na(build_year) & !is.na(energy_label)])
  result[result < 4] <- 1
  result[result > 4] <- 3
  result[result == 4] <- 2
  
  # set anything that is unavailable as NA
  result[is.na(build_year) & is.na(energy_label)] <- NA
  
  # set anything present in only one vector to that vector's value
  result[is.na(build_year) & !is.na(energy_label)] <- as.numeric(energy_label[is.na(build_year) & !is.na(energy_label)])
  result[!is.na(build_year) & is.na(energy_label)] <- as.numeric(build_year[!is.na(build_year) & is.na(energy_label)])
  
  result <- factor(result, levels = c(1, 2, 3), labels = GP_DWELLING_FACTOR_LEVELS)
  
  result
}
