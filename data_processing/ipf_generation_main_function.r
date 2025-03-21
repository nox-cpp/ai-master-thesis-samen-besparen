# required libs
load_lib_quietly("dplyr")
load_lib_quietly("tidyr")
load_lib_quietly("ggplot2")
load_lib_quietly("mipfp")
load_lib_quietly("purrr") # provides reduce
load_lib_quietly("tibble")
load_lib_quietly("ggcorrplot")
load_lib_quietly("xtable")
load_lib_quietly("data.table")

# sourcing this as is is not a great way to do it, but this needs to be 
# available in the functions
# TODO: figure out a neater way of doing this, this potentially overrides things
script_dir <- Sys.getenv(script_env)
data_dir <- Sys.getenv(input_env)
output_dir <- Sys.getenv(output_env)

# Loads the scripts that do the reading and replacing
source(file.path(script_dir, "gp_data_read.r"))
source(file.path(script_dir, "aggregate_data_read.r"))
source(file.path(script_dir, "data_mapping.r"))
source(file.path(script_dir, "humat_mapping_tools.r"))
source(file.path(script_dir, "parameterized_humat_mapping.r"))
source(file.path(script_dir, "ipf_functions.r"))
source(file.path(script_dir, "analysis_helpers.r"))

# Returns the configred table directory. Returns an empty string if no directory
# has been configured.
get_table_dir_from_env <- function() {
  figure_dir <- Sys.getenv("GP_PLOTTING_FOLDER")
  # if we have a figure output folder, set the script to write to it
  if (nchar(figure_dir) != 0) {
    return(file.path(figure_dir, "tables"))
  }
  return("")
}

# generates populations using IPF, using <base_population> as the micro-data and
# <target_marginals> as the population fit constraints. <value_mappings> is
# used to replace/sanitize the <base_population> data where necessary.
# <pop_output_dir> determines where the fitted populations are placed.
generate_ipf_populations <- function(
    base_population, 
    target_marginals,
    pop_output_dir = "",
    write_summary_tables = TRUE,
    table_output_file_suffix = "ipf",
    population_file_suffix = "ipf")
{
  # output folder
  humat_output_folder <- file.path(pop_output_dir)
  if (!create_folder_if_needed(humat_output_folder)) {
    print(paste0("Could not create folder: ", humat_output_folder))
    error("Unable to create directory for humat output, exiting.")
  }

  synth_aggs <- tibble()
  # TODO: use vectorized function instead
  for (target_idx in seq_len(nrow(target_marginals))) {
    # get the ipf population
    ipf_population <- ipfp_population(
      base_population, target_marginals[target_idx, ])

    # sanity check
    if (nrow(ipf_population) == 0) {
      error("IPF population is empty, cannot proceed")
    }

    # combine aggregate statistics for the fitted population for validation
    synth_aggs <- synth_aggs |> bind_rows(
      rbind(
        ipf_population |> rename(group = woonsituatie) |> count(group),
        # can't have two factors with the same levels later
        ipf_population |>
          mutate(bouwjaar = recode(bouwjaar, average = "average_by")) |>
          rename(group = bouwjaar) |>
          count(group),
        ipf_population |> rename(group = energielabel) |> count(group)
      ) |>
        mutate(source = target_marginals$name[target_idx]) |>
        mutate(households_fit = nrow(ipf_population)) |>
        spread(group, n)
    )

    # write the IPF population basis to file
    ipf_output_file_name <- generate_file_name(
      target_marginals[target_idx, "name"], suffix = population_file_suffix)
    ipf_output_path <- file.path(data_dir, "generated", ipf_output_file_name)
    print(paste0("Writing IPF population to: ", ipf_output_path))
    write.csv(ipf_population, ipf_output_path, row.names = FALSE)

    # convert the population basis to HUMATs
    humat_data <- humat_convert_data_to_agents(ipf_population)

    # validate values are in range
    print(paste0("Remaining NA's in dataset: ", as.character(sum(
      na_count_data(humat_data)
    ))))

    # generate the HUMAT population
    output_data <- humat_get_agent_description_data(humat_data)
    output_file_name <- generate_file_name(
      paste0(
        "agent_generation_ipf_", 
        target_marginals[target_idx, "name"], 
        suffix = population_file_suffix)
    )
    output_path <- file.path(humat_output_folder, output_file_name)
    print(paste0("Writing output to: ", output_path))

    # write the fitted population to an output file
    write.csv(output_data, output_path, row.names = FALSE)
  }

  if (write_summary_tables)
  {
    ipf_write_summary_tables(
      synth_aggs, target_marginals, table_output_file_suffix)
  }
}

# <table_output_file_suffix> is used in the table file names to distinguish
# the result of different IPF population generation procedures
ipf_write_summary_tables <- function (
  synth_aggs, target_marginals, table_output_file_suffix = "ipf") {
  table_dir <- get_table_dir_from_env()
  print(paste0("Writing table output about IPF generations in: ",table_dir))

  # if a segment is not included in a generated population, the synthetic
  # aggregates may contain NA's. Those are set to 0
  synth_aggs[is.na(synth_aggs)] <- 0

  # combine the fitted aggregates with the constraints
  validation_table <- target_marginals |>
    select(
      name, households, renter, owner,
      pre_1970, mid_1970_1999, post_2000,
      e_minus, c_d, b_plus
    ) |>
    inner_join(synth_aggs, by = c("name" = "source"))

  # separate the constraints and fitted values as a convenience
  constraints <- validation_table |>
    select(
      households, renter, owner,
      pre_1970, mid_1970_1999, post_2000,
      e_minus, c_d, b_plus
    )
  fitted <- synth_aggs |>
    select(
      households_fit, huurder, eigenaar,
      old, average_by, new,
      poor, average, good
    )

  ## Error metrics per zone
  # this is essentially just a loop
  zonewise_cors <- unlist(lapply(1:nrow(constraints), function(idx) {
    cor(t(constraints[idx, ]), t(fitted[idx, ]), method = "pearson")
  })) |> setNames(validation_table$name)

  # calculate TAE for constraints/aggregates
  zonewise_tae <- unlist(lapply(1:nrow(constraints), function(idx) {
    TAE(constraints[idx, ], fitted[idx, ])
  })) |> setNames(validation_table$name)

  # calculate RE for constraints/aggregates
  zonewise_re <- unlist(lapply(1:nrow(constraints), function(idx) {
    RE(constraints[idx, ], fitted[idx, ], constraints$households[idx])
  })) |> setNames(validation_table$name)

  # calculate the RMSE of the constraints/fitted
  zonewise_rmse <- unlist(lapply(1:nrow(constraints), function(idx) {
    RMSE(constraints[idx, ], fitted[idx, ])
  })) |> setNames(validation_table$name)

  # combine the fit metrics and export to a table
  combined_zone_metrics <- data.frame(
    zonewise_cors, zonewise_tae, zonewise_re, zonewise_rmse)
  colnames(combined_zone_metrics) <- c("Pearson's $\\rho$", "TAE", "RE", "RMSE")
  combined_zone_metrics <- rownames_to_column(combined_zone_metrics, "zone")
  zone_fit_table_path <- file.path(
    table_dir, paste0("zone_fit_errors_", table_output_file_suffix, ".tex"))
  print(
    xtable(
      combined_zone_metrics |> arrange(zone),
      caption = paste0(
        "Table of goodness-of-fit measures for IPF synthetic populations ",
        " as compared to the constraint aggregates computed per zone. ",
        "Listed are the total absolute error, relative error, residual mean ",
        "square error and Pearson's $\rho$"
      ),
      label = paste0("tbl:ipf_gof"),
      align = "llrrrr"
    ),
    file = zone_fit_table_path,
    include.rownames = FALSE,
    sanitize.colnames.function = identity
  )

  # give a table as an example of what the fit is based on
  fit_example <- rbind(
    c(name = validation_table$name[1], constraints[1, ]),
    c(name = paste0(validation_table$name[1], " fitted"), fitted[1, ])
  ) |> data.frame()
  example_table_path <- 
    file.path(
      table_dir, paste0("example_fit_", table_output_file_suffix, ".tex"))
  print(xtable(
    fit_example |> 
    rename(zone = name) |>
     select(zone, households, renter, owner, pre_1970, e_minus),
    caption = paste0(
      "Partial example of zone constraints and IPF fitted values."),
    label = paste0("tbl:ipf_fit_example"),
    align = "llrrrrr"
  ), file = example_table_path, include.rownames = FALSE)

  # Error metrics by variable
  # this is essentially just a loop
  varwise_cors <- unlist(lapply(1:ncol(constraints), function(idx) {
    cor((constraints[, idx]), (fitted[, idx]), method = "pearson")
  })) |> setNames(colnames(constraints))

  # calculate RE for constraints/aggregates
  varwise_re <- unlist(lapply(1:ncol(constraints), function(idx) {
    RE(constraints[, idx], fitted[, idx], sum(constraints[, idx]))
  })) |> setNames(colnames(constraints))

  # calculate the RMSE of the constraints/fitted
  varwise_rmse <- unlist(lapply(1:ncol(constraints), function(idx) {
    RMSE(constraints[, idx], fitted[, idx])
  })) |> setNames(colnames(constraints))

  combined_var_metrics <- data.frame(varwise_cors, varwise_re, varwise_rmse)
  var_fit_table_path <- file.path(
    table_dir, paste0("var_fit_errors", table_output_file_suffix, ".tex"))
  print(xtable(combined_var_metrics), file = var_fit_table_path)
}